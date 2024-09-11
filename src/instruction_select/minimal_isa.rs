use egg::Searcher;
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};

use crate::{halide_ir::ast, HalideExprOp, HalideLang, Instructions};

use super::Learned;

pub struct MinimalIsa<'a> {
    pub instructions: HashSet<usize>,
    learned: &'a Instructions<Learned>,
}

impl<'a> MinimalIsa<'a> {
    pub fn new(learned: &'a Instructions<Learned>) -> Self {
        let mut isa = Self {
            instructions: HashSet::default(),
            learned,
        };

        isa.minimize();
        isa
    }

    fn minimize(&mut self) {
        let rewrite_graph = self.rewritability();
        let instructions: HashMap<usize, _> = self.learned.instructions().collect();

        // initialize isa to instructions that don't have dependencies
        let mut initial_isa: HashSet<_> = rewrite_graph
            .iter()
            .filter_map(|(i, deps)| if deps.is_empty() { Some(*i) } else { None })
            .filter(|i| {
                // only choose instructions that have matches in the graph
                !instructions[i].search(&self.learned.egraph).is_empty()
            })
            .collect();

        // remove empty items from set
        let mut queue: VecDeque<_> = rewrite_graph
            .into_iter()
            .filter(|(_, set)| !set.is_empty())
            .map(|(i, deps)| (i, deps, 0))
            .collect();

        let mut eliminated: HashSet<usize> = HashSet::default();
        while let Some((i, deps, checked_times)) = queue.pop_front() {
            // if we've come back around
            // XXX this isn't the right check, but will be fine for now
            if checked_times > 10 {
                unreachable!("I think we shouldn't need this check, but not sure")
            }

            // if all the deps aren't in the isa, push to back of list
            if !deps
                .iter()
                .filter(|d| !eliminated.contains(d))
                .all(|d| initial_isa.contains(d))
            {
                queue.push_back((i, deps, checked_times + 1));
                continue;
            }

            // check if the things that the dependencies match exactly correspond to the things
            // `i` matches. If this is the case, then we can replace all instances of the
            // dependencies with `i`, thus shrinking the ISA.
            let mut dep_matches = Matches::default();
            deps.iter().for_each(|j| {
                let pat = &instructions[j];
                dep_matches.add_match(pat, &self.learned.egraph);
            });

            let ipat = &instructions[&i];
            let mut i_matches = Matches::default();
            i_matches.add_match(ipat, &self.learned.egraph);

            if i_matches.actuals() == dep_matches.actuals() {
                // remove all the dependencies from the isa
                initial_isa.retain(|i| !deps.contains(i));
                // add this instruction to the isa
                initial_isa.insert(i);
            } else {
                // keep track of instructions that failed to replace their deps
                eliminated.insert(i);
            }
        }

        self.instructions = initial_isa;
    }

    fn rewritability(&self) -> HashMap<usize, HashSet<usize>> {
        // compute rewritable graph
        println!("computing rewritability graph...");
        let mut map: HashMap<usize, HashSet<usize>> = HashMap::new();
        for (i, ipat) in self.learned.instructions() {
            // initialize hashset for this instruction to the empty set
            map.insert(i, HashSet::new());

            // some conversion nonsense
            let partial_expr: babble::PartialExpr<HalideExprOp, HalideExprOp> =
                babble::PartialExpr::from(ipat.clone()).fill(|hole| {
                    babble::PartialExpr::Node(babble::AstNode::leaf(HalideExprOp::Ident(
                        ast::Id::new(hole.to_string()),
                    )))
                });
            let expr: babble::Expr<HalideExprOp> =
                partial_expr.try_into().expect("still some holes");

            // add instruction to e-graph
            let mut i_egraph: egg::EGraph<HalideLang, ()> = egg::EGraph::new(());
            i_egraph.add_expr(&egg::RecExpr::from(expr));
            i_egraph.rebuild();

            for (j, jpat) in self.learned.instructions() {
                // don't check if we are checking against ourselves
                if i == j {
                    continue;
                };

                // if j matches against something in `i`, then add it to `i`'s set
                if !jpat.search(&i_egraph).is_empty() {
                    map.entry(i).and_modify(|set| {
                        set.insert(j);
                    });
                }
            }
        }
        map
    }
}

/// Signature of a pattern match in an e-graph.
/// Used to deduplicate equivalent patterns.
#[derive(PartialEq, Eq, Debug, Default)]
struct Matches {
    match_set: HashMap<egg::Id, HashSet<egg::Id>>,
}

impl Matches {
    fn add_match<L, A>(&mut self, pattern: &egg::Pattern<L>, egraph: &egg::EGraph<L, A>)
    where
        L: egg::Language,
        A: egg::Analysis<L>,
    {
        for m in pattern.search(egraph) {
            for sub in m.substs {
                let actuals: HashSet<_> = pattern.vars().iter().map(|v| sub[*v]).collect();
                self.match_set
                    .entry(m.eclass)
                    .and_modify(|set| set.extend(actuals.clone()))
                    .or_insert(actuals);
            }
        }
    }

    /// A match set is canonicalized if every a actual does not point to something else
    /// in the match set.
    fn is_canonicalized(&self) -> bool {
        self.match_set
            .values()
            .all(|actuals| !actuals.iter().any(|m| self.match_set.contains_key(m)))
    }

    fn canonicalize(&mut self) {
        while !self.is_canonicalized() {
            let copy = self.match_set.clone();
            for (_eclass, actuals) in self.match_set.iter_mut() {
                *actuals = actuals
                    .iter()
                    .flat_map(|id| copy.get(id).cloned().unwrap_or(HashSet::from([*id])))
                    .collect();
            }
        }
    }

    fn actuals(&mut self) -> HashSet<egg::Id> {
        self.canonicalize();
        self.match_set.values().flatten().copied().collect()
    }
}
