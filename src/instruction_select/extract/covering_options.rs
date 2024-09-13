//! Module for computing all possible instruction sets that can cover each e-class
//! in an e-graph.

use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use rayon::iter::{ParallelBridge, ParallelIterator};

use crate::{halide_ir::ast::Instr, HalideExprOp, HalideLang};

use super::minimal_isa::{IsaPruner, MinimalIsaDump};

pub struct EGraphCovering<'a, N = ()>
where
    N: egg::Analysis<HalideLang>,
{
    egraph: &'a egg::EGraph<HalideLang, N>,
    pruner: &'a dyn IsaPruner,
    covering: HashMap<egg::Id, ChoiceSet>,
    seen: HashSet<egg::Id>,
}

impl<'a, N> std::fmt::Debug for EGraphCovering<'a, N>
where
    N: egg::Analysis<HalideLang>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();
        for id in self.covering.keys().sorted() {
            map.entry(id, &self.covering[id]);
        }
        map.finish()
    }
}

impl<'a, N> EGraphCovering<'a, N>
where
    N: egg::Analysis<HalideLang>,
{
    pub fn new(egraph: &'a egg::EGraph<HalideLang, N>, pruner: &'a dyn IsaPruner) -> Self {
        EGraphCovering {
            egraph,
            pruner,
            covering: HashMap::default(),
            seen: HashSet::default(),
        }
    }

    pub fn compute_from_root(&mut self, root: egg::Id) {
        // if we have already checked this eclass, just return
        if self.covering.contains_key(&root) || self.seen.contains(&root) {
            return;
        }

        // insert default element so that we don't follow loops infinitely
        self.seen.insert(root);

        // There are two cases:
        //     1) This eclass contains some instructions. We know that one of these
        //     instructions has to be picked, so just look at the children of the
        //     instructions.
        //     2) This eclass contains no instructions. We need to recurse into all
        //     children, and then take the cross product across the children.

        // XXX(sam): collapse these two cases into one. I don't think two cases is necessary
        if self.egraph[root]
            .nodes
            .iter()
            .any(|enode| enode.operation().instruction().is_some())
        {
            let instrs: Vec<(Instr, &[egg::Id])> = self.egraph[root]
                .nodes
                .iter()
                .filter_map(|enode| {
                    enode
                        .operation()
                        .instruction()
                        .map(|instr| (instr, enode.args()))
                })
                .collect();

            let mut choices = ChoiceSet::default();
            for (instr, children) in instrs {
                for child in children {
                    self.compute_from_root(*child);
                }

                choices.extend(
                    children
                        .iter()
                        .map(|child| {
                            if root == *child {
                                let mut choices = ChoiceSet::from(&self.egraph[root]);
                                choices.retain(|covering| !covering.contains(&instr));
                                choices
                            } else {
                                self.covering[child].clone()
                            }
                        })
                        .map(|covering| self.pruner.prune(covering))
                        .fold(ChoiceSet::from([instr.0]), |acc, el| acc.cross_product(&el)),
                );
            }

            self.covering.insert(root, choices);
        } else {
            // gather all unique children of the nodes in self.egraph[root]
            let children: Vec<egg::Id> = self.egraph[root]
                .nodes
                .iter()
                .flat_map(|enode| enode.args())
                .copied()
                .sorted()
                .dedup()
                .collect();

            // recurse through all children
            for child in &children {
                self.compute_from_root(*child);
            }

            // gather instructions from current root
            // update this roots covering with the cross-product over it's children
            self.covering.insert(
                root,
                children
                    .iter()
                    .map(|child| &self.covering[child])
                    .map(|covering| self.pruner.prune(covering.clone()))
                    .fold(ChoiceSet::from([]), |acc, el| acc.cross_product(&el)),
            );
        }
    }

    pub fn get(&self, root: &egg::Id) -> Option<&ChoiceSet> {
        self.covering.get(root)
    }
}

/// Represents a set of instructions that must be choosen together, in order to
/// cover a sub-tree.
#[derive(derive_more::Debug, Clone, PartialEq, Eq, Default)]
#[debug("{_0:?}")]
pub struct Covering(HashSet<Instr>);

impl Covering {
    pub fn union(self, other: &Self) -> Self {
        Covering(self.0.union(&other.0).copied().collect())
    }

    pub fn into_inner(self) -> HashSet<Instr> {
        self.0
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<'_, Instr> {
        self.0.iter()
    }

    pub fn contains(&self, value: &Instr) -> bool {
        self.0.contains(value)
    }

    pub fn dump<'a>(
        &'a self,
        instructions: &'a HashMap<Instr, egg::Pattern<HalideLang>>,
    ) -> MinimalIsaDump<'a> {
        MinimalIsaDump {
            isa: &self.0,
            instructions,
        }
    }
}

/// Each element represents a possible choice to cover some sub-tree.
#[derive(derive_more::Debug, Clone, Default)]
#[debug("{_0:?}")]
pub struct ChoiceSet(Vec<Covering>);

impl PartialEq for ChoiceSet {
    fn eq(&self, other: &Self) -> bool {
        self.0.iter().all(|cov| other.0.contains(cov))
            && other.0.iter().all(|cov| self.0.contains(cov))
    }
}

impl Eq for ChoiceSet {}

impl ChoiceSet {
    pub fn cross_product(self, other: &Self) -> Self {
        if self.0.is_empty() {
            return other.clone();
        }

        if other.0.is_empty() {
            return self;
        }

        ChoiceSet(
            self.0
                .into_iter()
                .cartesian_product(&other.0)
                .par_bridge()
                .map(|(s, o)| s.union(o))
                .collect(),
        )
    }

    #[allow(unused)]
    fn or<C>(mut self, value: C) -> Self
    where
        C: Into<Covering>,
    {
        self.0.push(value.into());
        self
    }

    pub fn extend<I>(&mut self, iter: I) -> &mut Self
    where
        I: IntoIterator<Item = Covering>,
    {
        self.0.extend(iter);
        self.0.dedup();
        self
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Covering> + Clone {
        self.0.iter()
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Covering) -> bool,
    {
        self.0.retain(f)
    }
}

impl<const N: usize> From<[usize; N]> for Covering {
    fn from(value: [usize; N]) -> Self {
        Covering(HashSet::from_iter(value.into_iter().map(Instr)))
    }
}

impl<C> From<C> for ChoiceSet
where
    C: Into<Covering>,
{
    fn from(value: C) -> Self {
        ChoiceSet(vec![value.into()])
    }
}

impl<'a, D> From<&'a egg::EClass<HalideLang, D>> for ChoiceSet {
    fn from(value: &'a egg::EClass<HalideLang, D>) -> Self {
        ChoiceSet(
            value
                .nodes
                .iter()
                .filter_map(|enode| {
                    if let HalideExprOp::Instruction(i) = enode.operation() {
                        Some(*i)
                    } else {
                        None
                    }
                })
                .map(|i| Covering(HashSet::from([i])))
                .collect(),
        )
    }
}

impl IntoIterator for ChoiceSet {
    type Item = Covering;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Covering> for ChoiceSet {
    fn from_iter<T: IntoIterator<Item = Covering>>(iter: T) -> Self {
        ChoiceSet(iter.into_iter().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cross_product() {
        let a = ChoiceSet::from([0]).or([1]);
        let b = ChoiceSet::from([2]).or([3]);

        assert_eq!(
            a.cross_product(&b),
            ChoiceSet::from([0, 2]).or([0, 3]).or([1, 2]).or([1, 3])
        )
    }

    #[test]
    fn test_children_cross_product() {
        let a = ChoiceSet::from([0]).or([1]);
        let b = ChoiceSet::from([2]).or([3]);
        let c = ChoiceSet::from([4]);

        let children = a.cross_product(&b);

        assert_eq!(
            children.cross_product(&c),
            ChoiceSet::from([0, 2, 4])
                .or([0, 3, 4])
                .or([1, 2, 4])
                .or([1, 3, 4])
        )
    }

    #[test]
    fn test_empty_cross_product0() {
        let a = ChoiceSet::from([0]).or([1]);
        let b = ChoiceSet::from([]);

        assert_eq!(a.clone().cross_product(&b), a)
    }

    #[test]
    fn test_empty_cross_product1() {
        let a = ChoiceSet::from([0]).or([1]);
        let b = ChoiceSet::from([]);
        let c = ChoiceSet::from([2]);

        assert_eq!(
            a.clone().cross_product(&b).cross_product(&c),
            a.cross_product(&c)
        )
    }

    #[test]
    fn test_empty_cross_product2() {
        let a = ChoiceSet::from([0]).or([1]);
        let b = ChoiceSet(vec![]);

        assert_eq!(b.clone().cross_product(&a), a)
    }

    #[test]
    fn test_egraph0() {
        let expr: egg::RecExpr<HalideLang> = "(/ (* a b) (+ c d))".parse().unwrap();

        let instrs: Vec<egg::Rewrite<HalideLang, ()>> = vec![
            egg::rewrite!("rw0"; "(* ?a ?b)" => "(inst<0> ?a ?b)"),
            egg::rewrite!("rw1"; "(* a ?b)" => "(inst<1> ?b)"),
            egg::rewrite!("rw2"; "(+ ?a ?b)" => "(inst<2> ?a ?b)"),
            egg::rewrite!("rw3"; "(+ c ?b)" => "(inst<3> ?b)"),
        ];

        let runner = egg::Runner::default().with_expr(&expr).run(&instrs);
        let root = runner.roots[0];

        let mut covering = EGraphCovering::new(&runner.egraph, &());
        covering.compute_from_root(root);

        assert_eq!(
            covering.covering[&root],
            ChoiceSet::from([0, 2]).or([0, 3]).or([1, 2]).or([1, 3])
        )
    }

    #[test]
    fn test_egraph1() {
        let expr: egg::RecExpr<HalideLang> = "(/ (* a b) (+ c d))".parse().unwrap();

        let instrs: Vec<egg::Rewrite<HalideLang, ()>> = vec![
            egg::rewrite!("rw0"; "(* ?a ?b)" => "(inst<0> ?a ?b)"),
            egg::rewrite!("rw1"; "(* a ?b)" => "(inst<1> ?b)"),
            egg::rewrite!("rw2"; "(+ ?a ?b)" => "(inst<2> ?a ?b)"),
            egg::rewrite!("rw3"; "(+ c ?b)" => "(inst<3> ?b)"),
            egg::rewrite!("rw4"; "(/ ?a ?b)" => "(inst<4> ?a ?b)"),
        ];

        let runner = egg::Runner::default().with_expr(&expr).run(&instrs);
        let root = runner.roots[0];

        let mut covering = EGraphCovering::new(&runner.egraph, &());
        covering.compute_from_root(root);

        assert_eq!(
            covering.covering[&root],
            ChoiceSet::from([0, 2, 4])
                .or([0, 3, 4])
                .or([1, 2, 4])
                .or([1, 3, 4])
        )
    }
}
