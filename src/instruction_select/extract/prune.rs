use crate::halide_ir::ast::Instr;
use crate::instruction_select::Learned;
use crate::{instruction_select::lang::PatternConvert, HalideLang};
use crate::{utils, HalideExprOp, Instructions};
use egg::Searcher;
use indicatif::ParallelProgressIterator;
use itertools::Itertools;
use rayon::iter::ParallelBridge;
use rayon::iter::ParallelIterator;
use smallvec::{smallvec, SmallVec};
use std::collections::{HashMap, HashSet};

use super::instruction_covering::{ChoiceSet, Covering};
use super::minimal_isa::IsaPruner;

pub struct StrictOrdering<'a, N = ()>
where
    N: egg::Analysis<HalideLang>,
{
    /// An element in this set `(i, j)` means that `i < j`
    ordering: HashSet<(Instr, Instr)>,
    egraph: &'a egg::EGraph<HalideLang, N>,
    candidates: HashMap<Instr, egg::Pattern<HalideLang>>,
}

impl<'a> StrictOrdering<'a> {
    pub fn new(learned: &'a Instructions<Learned>) -> Self {
        let candidates = learned.instructions().collect();

        let mut s = Self {
            ordering: HashSet::default(),
            egraph: &learned.egraph,
            candidates,
        };

        // build ordering by looking at all pairs of instructions
        s.ordering = s
            .candidates
            .keys()
            .cartesian_product(s.candidates.keys())
            .par_bridge()
            .progress_with(
                utils::progress_bar(s.candidates.len() * s.candidates.len())
                    .with_message("Building ordering over instructions"),
            )
            // don't consider comparison with self
            .filter(|(i, j)| i != j)
            .filter_map(|(i, j)| {
                // if i less general than j
                s.less_general_than(*i, *j).map(|subst| {
                    // and j is only used in the same context
                    // that i is used in
                    if s.covers(*j, subst) {
                        // then j < i
                        (*j, *i)
                    } else {
                        // else i < j
                        // (including the more strict instruction, means that we would also need the more
                        // general instruction, so we should just include the more general instruction)
                        (*i, *j)
                    }
                })
            })
            .collect();

        s
    }

    /// Returns true if `i < j`
    fn better_than(&self, i: &Instr, j: &Instr) -> bool {
        self.ordering.contains(&(*i, *j))
    }
}

#[derive(Default)]
pub struct PairwisePrune {
    msg: String,
}

impl IsaPruner for (StrictOrdering<'_>, PairwisePrune) {
    fn prune(&self, choices: ChoiceSet) -> ChoiceSet {
        let better =
            |((idx_a, a), (idx_b, b)): ((usize, &Covering), (usize, &Covering))| -> SmallVec<[usize; 2]> {
                for ia in a.iter() {
                    for ib in b.iter() {
                        if self.0.better_than(ia, ib) {
                            return smallvec![idx_a];
                        } else if self.0.better_than(ib, ia) {
                            return smallvec![idx_b];
                        }
                    }
                }

                smallvec![idx_a, idx_b]
            };

        let partial_order: HashSet<usize> = choices
            .iter()
            .enumerate()
            .tuple_combinations()
            .par_bridge()
            .map(better)
            .fold_with(HashSet::default(), |mut acc, sv| {
                acc.extend(sv);
                acc
            })
            .reduce_with(|mut acc, hset| {
                acc.extend(hset);
                acc
            })
            .unwrap();

        choices
            .into_iter()
            .enumerate()
            .filter_map(move |(i, set)| {
                if !partial_order.contains(&i) {
                    Some(set)
                } else {
                    None
                }
            })
            .collect()
    }

    fn set_message(&mut self, msg: String) {
        self.1.msg = msg;
    }
}

#[derive(Default)]
pub struct Experimental {
    msg: String,
}

impl IsaPruner for (StrictOrdering<'_>, Experimental) {
    fn prune(&self, mut choices: ChoiceSet) -> ChoiceSet {
        // find pairs in ordering such that if i < j, j exists in some set in choices
        let relevant_ordering: HashSet<Instr> = self
            .0
            .ordering
            .iter()
            .filter(|(_, j)| choices.iter().any(|cover| cover.contains(j)))
            .map(|(i, _)| *i)
            .collect();

        // remove all covers that contain an instruction in relevant ordering
        choices.retain(|cover| !cover.iter().any(|i| relevant_ordering.contains(i)));

        choices
    }

    fn set_message(&mut self, msg: String) {
        self.1.msg = msg;
    }
}

impl StrictOrdering<'_> {
    /// Check if `instr0` is less general than `instr1`. If so, produce a substitution
    /// that describes how to produce `instr0` from `instr1`.
    fn less_general_than(
        &self,
        instr0: Instr,
        instr1: Instr,
    ) -> Option<HashMap<egg::Var, egg::Pattern<HalideLang>>> {
        let (root, instr_egraph) = self.pattern_egraph(instr0);

        let pat1 = &self.candidates[&instr1];
        let m = pat1.search_eclass(&instr_egraph, root)?;

        let extractor = egg::Extractor::new(&instr_egraph, egg::AstSize);

        let substs: HashMap<_, _> = m
            .substs
            .into_iter()
            .cartesian_product(pat1.vars())
            .map(|(s, v)| {
                let (_, sub_prog) = extractor.find_best(s[v]);
                (v, babble::AstNode::to_pattern(&sub_prog))
            })
            .collect();
        Some(substs)
    }

    fn covers(&self, instr: Instr, subst: HashMap<egg::Var, egg::Pattern<HalideLang>>) -> bool {
        let pat = self.candidates[&instr].clone();

        let egraph = self.egraph;

        for m in pat.search(egraph) {
            for s in m.substs {
                for v in pat.vars() {
                    if subst[&v].search_eclass(egraph, s[v]).is_none() {
                        return false;
                    }
                }
            }
        }

        true
    }

    fn pattern_egraph(&self, instr: Instr) -> (egg::Id, egg::EGraph<HalideLang, ()>) {
        use babble::{AstNode, Expr, PartialExpr};

        // get instruction pattern
        let pat = self.candidates[&instr].clone();

        // map holes in partial expression to halide identifiers
        let partial_expr: PartialExpr<HalideExprOp, HalideExprOp> = PartialExpr::from(pat)
            .fill(|hole| PartialExpr::Node(AstNode::leaf(HalideExprOp::PatternVar(hole))));

        // flatten into an expression
        let expr: Expr<HalideExprOp> = partial_expr.try_into().expect("still some holes");

        // finally, make an egraph and add this expression to it
        let mut egraph: egg::EGraph<HalideLang, ()> = egg::EGraph::new(());
        let root = egraph.add_expr(&egg::RecExpr::from(expr));
        egraph.rebuild();
        (root, egraph)
    }
}
