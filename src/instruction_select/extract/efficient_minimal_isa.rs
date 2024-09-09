use std::collections::{HashMap, HashSet};

use egg::Searcher;

use crate::{
    halide_ir::ast::{self, Instr},
    instruction_select::Learned,
    HalideExprOp, HalideLang, Instructions,
};

use super::minimal_isa::MinimalIsa;

pub struct EfficientIsa<'a> {
    isa: HashSet<Instr>,
    learned: &'a Instructions<Learned>,
    candidates: HashMap<Instr, egg::Pattern<HalideLang>>,
}

impl<'a> MinimalIsa<'a> for EfficientIsa<'a> {
    fn new(learned: &'a Instructions<Learned>) -> Self {
        let candidates = learned.instructions().collect();
        Self {
            isa: HashSet::default(),
            learned,
            candidates,
        }
    }
    fn minimize(&mut self) {
        let rewrite_graph = self.rewritability();
        let lattice0 = self.lattice(&rewrite_graph);
        let lattice = self.lattice_covers(&lattice0);
        let together = AlwaysTogether::new(&self.learned.egraph);

        println!("{rewrite_graph:#?}");
        println!("{lattice0:#?}");
        println!("{lattice:#?}");
        println!("{together:#?}",);

        self.learned
            .egraph
            .dot()
            .to_pdf("simple.pdf")
            .expect("failed to produce graph");
        println!("{:?}", self.learned.egraph.dump());

        let instructions: HashMap<Instr, _> = self.learned.instructions().collect();
        // initialize isa to instructions that don't have dependencies
        let mut initial_isa: HashSet<_> = rewrite_graph
            .0
            .iter()
            .filter_map(|(i, deps)| if deps.is_empty() { Some(*i) } else { None })
            .filter(|i| {
                // only choose instructions that have matches in the graph
                !instructions[i].search(&self.learned.egraph).is_empty()
            })
            .collect();

        loop {
            let mut additions: Vec<Instr> = vec![];
            let mut removals: Vec<Instr> = vec![];
            for i in &initial_isa {
                if let Some(replacements) = lattice.0.get(i) {
                    additions.extend(replacements);
                    removals.push(*i);

                    for r in replacements {
                        if let Some(bffs) = together.map.get(r) {
                            removals.extend(bffs);
                        }
                    }
                }
            }

            additions.dedup();
            removals.dedup();

            if additions.is_empty() && removals.is_empty() {
                break;
            }

            // we haven't made the ISA any smaller
            // this is a greedy bit
            if additions.len() >= removals.len() {
                break;
            }

            initial_isa.extend(additions);
            for r in removals {
                initial_isa.remove(&r);
            }
        }

        // // remove empty items from set
        // let mut queue: VecDeque<_> = rewrite_graph
        //     .0
        //     .into_iter()
        //     .filter(|(_, set)| !set.is_empty())
        //     .map(|(i, deps)| (i, deps, 0))
        //     .collect();

        // let mut eliminated: HashSet<usize> = HashSet::default();
        // while let Some((i, deps, checked_times)) = queue.pop_front() {
        //     println!("current isa: {initial_isa:?}");
        //     // if we've come back around
        //     // XXX this isn't the right check, but will be fine for now
        //     if checked_times > 10 {
        //         unreachable!("I think we shouldn't need this check, but not sure")
        //     }

        //     let filtered_deps: HashSet<_> =
        //         deps.iter().filter(|d| !eliminated.contains(d)).collect();

        //     println!("checking {i}: {filtered_deps:?}");
        //     if filtered_deps.is_empty() && !initial_isa.contains(&i) {
        //         continue;
        //     }

        //     // if all the deps aren't in the isa, push to back of list
        //     if !filtered_deps.iter().all(|d| initial_isa.contains(d)) {
        //         queue.push_back((i, deps, checked_times + 1));
        //         continue;
        //     }

        //     // check if the things that the dependencies match exactly correspond to the things
        //     // `i` matches. If this is the case, then we can replace all instances of the
        //     // dependencies with `i`, thus shrinking the ISA.
        //     let dep_matches = self.matches(filtered_deps.iter().map(|j| &instructions[j]));

        //     let ipat = &instructions[&i];
        //     let i_matches = self.matches([ipat]);
        //     println!("checking {i}: {ipat}");
        //     println!("i matches: {i_matches:#?}");
        //     println!("dep matches: {dep_matches:#?}");

        //     if i_matches.actuals() == dep_matches.actuals() {
        //         println!("yay! shrinking isa");
        //         // remove all the dependencies from the isa
        //         initial_isa.retain(|i| !filtered_deps.contains(i));
        //         eliminated.extend(filtered_deps);
        //         // add this instruction to the isa
        //         initial_isa.insert(i);
        //     } else {
        //         // keep track of instructions that failed to replace their deps
        //         eliminated.insert(i);
        //     }
        // }

        self.isa = initial_isa;
    }

    fn isa(&self) -> &HashSet<Instr> {
        &self.isa
    }
}

impl<'a> EfficientIsa<'a> {
    fn rewritability(&self) -> Rewritability {
        // compute rewritable graph
        println!("computing rewritability graph...");
        let mut map: HashMap<Instr, HashSet<Instr>> = HashMap::new();
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
        Rewritability(map)
    }

    fn lattice(&self, _rewritability: &Rewritability) -> Lattice {
        let mut map: HashMap<Instr, HashSet<Instr>> = HashMap::new();
        let instructions: HashMap<Instr, _> = self.learned.instructions().collect();

        for (i, ipat) in &instructions {
            map.insert(*i, HashSet::new());

            let i_matches = self.matches([ipat]);

            for (j, jpat) in &instructions {
                if i == j {
                    continue;
                }

                let j_matches = self.matches([jpat]);
                if i_matches.intersects(&j_matches) {
                    map.entry(*i).and_modify(|set| {
                        set.insert(*j);
                    });
                }
            }
        }

        // for (i, deps) in &rewritability.0 {
        //     let i_matches = Matches::new(&self.learned.egraph, [&instructions[i]]);
        //     for d in deps {
        //         let d_matches = Matches::new(&self.learned.egraph, [&instructions[d]]);
        //         if i_matches.intersects(&d_matches) {
        //             map.entry(*i)
        //                 .and_modify(|set| {
        //                     set.insert(*d);
        //                 })
        //                 .or_insert_with(|| HashSet::from_iter([*d]));
        //         }
        //     }
        // }

        Lattice(map)
    }

    fn lattice_covers(&self, lattice: &Lattice) -> Lattice {
        Lattice(
            lattice
                .0
                .iter()
                .filter(|(a, bs)| {
                    let a_matches = self.matches([&self.candidates[a]]);
                    let bs_matches = self.matches(bs.iter().map(|b| &self.candidates[b]));
                    // println!("a: {a}");
                    // println!("  {}", format!("{:#?}", a_matches).prefix_lines("  "));
                    // println!("bs: {bs:?}");
                    // println!("  {}", format!("{:#?}", bs_matches).prefix_lines("  "));
                    a_matches.actuals().is_subset(&bs_matches.actuals())
                })
                .map(|(a, bs)| (*a, bs.clone()))
                .filter(|(_, bs)| !bs.is_empty())
                .collect(),
        )
    }

    fn matches<'p, P>(&self, patterns: P) -> Matches
    where
        P: IntoIterator<Item = &'p egg::Pattern<HalideLang>>,
    {
        Matches::new(&self.learned.egraph, patterns)
    }
}

#[derive(Debug)]
struct Rewritability(HashMap<Instr, HashSet<Instr>>);

#[derive(Debug)]
struct Lattice(HashMap<Instr, HashSet<Instr>>);

/// Signature of a pattern match in an e-graph.
/// Used to deduplicate equivalent patterns.
#[derive(PartialEq, Eq, Debug, Default)]
pub struct Matches {
    match_set: HashMap<egg::Id, HashSet<egg::Id>>,
}

impl Matches {
    pub fn new<'a, L, A, P>(egraph: &egg::EGraph<L, A>, patterns: P) -> Self
    where
        L: egg::Language + 'a,
        A: egg::Analysis<L>,
        P: IntoIterator<Item = &'a egg::Pattern<L>>,
    {
        let mut matches = Matches::default();
        for p in patterns {
            matches.add_match(egraph, p);
        }
        // matches.canonicalize();
        matches
    }

    fn add_match<L, A>(&mut self, egraph: &egg::EGraph<L, A>, pattern: &egg::Pattern<L>)
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

    // /// A match set is canonicalized if every a actual does not point to something else
    // /// in the match set.
    // fn is_canonicalized(&self) -> bool {
    //     self.match_set
    //         .values()
    //         .all(|actuals| !actuals.iter().any(|m| self.match_set.contains_key(m)))
    // }

    // fn canonicalize(&mut self) {
    //     while !self.is_canonicalized() {
    //         let copy = self.match_set.clone();
    //         for (_eclass, actuals) in self.match_set.iter_mut() {
    //             *actuals = actuals
    //                 .iter()
    //                 .flat_map(|id| copy.get(id).cloned().unwrap_or(HashSet::from([*id])))
    //                 .collect();
    //         }
    //     }
    // }

    fn actuals(&self) -> HashSet<egg::Id> {
        self.match_set.values().flatten().copied().collect()
    }

    fn intersects(&self, other: &Self) -> bool {
        let self_vals = self.actuals();
        let other_vals = other.actuals();
        let intersection: Vec<_> = self_vals.intersection(&other_vals).collect();
        !intersection.is_empty()
    }
}

#[derive(Debug)]
struct AlwaysTogether {
    map: HashMap<Instr, HashSet<Instr>>,
}

impl AlwaysTogether {
    fn new<A>(egraph: &egg::EGraph<HalideLang, A>) -> Self
    where
        A: egg::Analysis<HalideLang>,
    {
        let mut map = HashMap::new();

        for eclass in egraph.classes() {
            let instrs: HashSet<_> = eclass
                .nodes
                .iter()
                .filter_map(|node| {
                    if let HalideExprOp::Instruction(i) = node.operation() {
                        Some(*i)
                    } else {
                        None
                    }
                })
                .collect();

            for i in &instrs {
                // if a mapping exists already for this instruction
                if let Some(shared) = map.get_mut(i) {
                    *shared = instrs.intersection(shared).copied().collect();
                } else {
                    let mut shared = instrs.clone();
                    shared.remove(i);
                    if !shared.is_empty() {
                        map.insert(*i, shared);
                    }
                }
            }
        }

        AlwaysTogether { map }
    }
}
