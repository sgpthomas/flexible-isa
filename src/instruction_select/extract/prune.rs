use crate::halide_ir::ast::Instr;
use crate::instruction_select::Learned;
use crate::{instruction_select::lang::PatternConvert, HalideLang};
use crate::{HalideExprOp, Instructions};
use egg::Searcher;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

use super::covering_options::ChoiceSet;
use super::minimal_isa::IsaPruner;

pub struct StrictOrdering<'a, N = ()>
where
    N: egg::Analysis<HalideLang>,
{
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

        for i in s.candidates.keys() {
            for j in s.candidates.keys() {
                if i == j {
                    continue;
                }

                if let Some(subst) = s.less_general_than(*i, *j) {
                    if s.covers(*j, subst) {
                        s.ordering.insert((*i, *j));
                        // println!("{i} is strictly better than {j}");
                    } else {
                        s.ordering.insert((*j, *i));
                    }
                }
            }
        }

        // println!("ordering: {:#?}", s.ordering);

        s
    }
}

impl IsaPruner for StrictOrdering<'_> {
    fn prune(&self, choices: ChoiceSet) -> ChoiceSet {
        choices
        // println!("{choices:?}");
        // let mut partial_order: HashMap<Instr, Instr> = HashMap::default();

        // for ((idx_a, a), (idx_b, b)) in choices.iter().enumerate().tuple_combinations() {
        //     // println!("{a:?} <? {b:?}");

        //     if a.iter()
        //         .any(|ia| b.iter().any(|ib| self.ordering.contains(&(*ia, *ib))))
        //     {
        //         // println!("a better than b");
        //         partial_order.insert(Instr(idx_b), Instr(idx_a));
        //     } else if b
        //         .iter()
        //         .any(|ib| a.iter().any(|ia| self.ordering.contains(&(*ib, *ia))))
        //     {
        //         // println!("b better than a");
        //         partial_order.insert(Instr(idx_a), Instr(idx_b));
        //     }
        // }

        // choices
        //     .into_iter()
        //     .enumerate()
        //     .filter_map(move |(i, set)| {
        //         if !partial_order.contains_key(&Instr(i)) {
        //             Some(set)
        //         } else {
        //             None
        //         }
        //     })
        //     .collect()
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

// impl<'a> MinimalIsa<'a> for BruteForcePruneIsa<'a> {
//     fn new(learned: &'a Instructions<Learned>) -> Self {
//         learned
//             .egraph
//             .named_dot()
//             .to_dot("simple.dot")
//             .expect("dooootttt!!");
//         println!("{:?}", learned.egraph.dump());
//         BruteForcePruneIsa {
//             brute_force: BruteForceIsa::new(learned),
//             strict_ordering: HashSet::default(),
//         }
//     }

//     fn minimize(&mut self) {
//         let roots = &self.learned.roots;
//         let memo = &self.eclass_memo;

//         // let mut strict_ordering: HashMap<usize, usize> = HashMap::default();
//         for i in self.candidates.keys() {
//             for j in self.candidates.keys() {
//                 if i == j {
//                     continue;
//                 }

//                 if let Some(subst) = self.less_general_than(*i, *j) {
//                     if self.covers(*j, subst) {
//                         self.strict_ordering.insert((*i, *j));
//                         // println!("{i} is strictly better than {j}");
//                     } else {
//                         self.strict_ordering.insert((*j, *i));
//                     }
//                 }
//             }
//         }

//         println!("ordering: {:#?}", self.strict_ordering);

//         // decide if this will be computationally feasible
//         let options: Vec<_> = roots
//             .iter()
//             .map(|r| memo.get(r).expect("No ISA for root."))
//             .filter(|r| !r.is_empty())
//             .map(|iset| iset.clone().into_iter())
//             // .map(|iset| self.prune(iset.clone()))
//             .collect();

//         let number_options = options
//             .iter()
//             .map(|r| r.clone().count())
//             .fold(1usize, usize::saturating_mul);

//         if number_options > 100_000_000 {
//             panic!("Too many options to compute minimal ISA by brute force.");
//         }

//         self.isa = options
//             .into_iter()
//             .inspect(|iset| {
//                 let reified: Vec<_> = iset.clone().collect();
//                 println!(
//                     "== {:?} {} ==",
//                     &reified[..(std::cmp::min(10, reified.len()))],
//                     reified.len()
//                 )
//             })
//             .multi_cartesian_product()
//             .map(|isets| {
//                 // union over cartesian product
//                 isets
//                     .iter()
//                     .fold(HashSet::default(), |mut acc: HashSet<_>, iset| {
//                         acc.extend(iset);
//                         acc
//                     })
//             })
//             .min_by_key(|iset| iset.len())
//             .unwrap_or_default();
//         // .for_each(|opts| {
//         //     println!("final: {opts:?}");
//         //     println!("====");
//         //     println!();
//         // });
//     }

//     fn isa(&self) -> &HashSet<Instr> {
//         self.isa()
//     }
// }
