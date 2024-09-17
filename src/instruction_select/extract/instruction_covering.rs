//! Module for computing all possible instruction sets that can cover each e-class
//! in an e-graph.

use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
};

use indicatif::ProgressIterator;
use itertools::Itertools;
use rayon::iter::{ParallelBridge, ParallelIterator};

use crate::{halide_ir::ast::Instr, utils, HalideExprOp, HalideLang};

use super::minimal_isa::{BestIsaDump, IsaPruner};

pub struct EGraphCovering<'a, T = (), N = ()>
where
    N: egg::Analysis<HalideLang>,
{
    egraph: &'a egg::EGraph<HalideLang, N>,
    pruner: &'a dyn IsaPruner<T>,
    covering: HashMap<egg::Id, ChoiceSet<T>>,
    seen: HashSet<egg::Id>,
}

impl<'a, T, N> std::fmt::Debug for EGraphCovering<'a, T, N>
where
    T: std::fmt::Debug,
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

impl<'a, T, N> EGraphCovering<'a, T, N>
where
    N: egg::Analysis<HalideLang>,
    T: Clone + Send + Sync + CoveringDataMerge,
{
    pub fn new(egraph: &'a egg::EGraph<HalideLang, N>, pruner: &'a dyn IsaPruner<T>) -> Self {
        EGraphCovering {
            egraph,
            pruner,
            covering: HashMap::default(),
            seen: HashSet::default(),
        }
    }

    pub fn with_roots(mut self, roots: &[egg::Id]) -> Self {
        for r in roots.iter().progress_with(
            utils::progress_bar(roots.len())
                .with_message("Computing valid instruction coverings..."),
        ) {
            self.compute_from_root(*r);
        }

        self
    }

    pub fn compute_from_root(&mut self, root: egg::Id) {
        // if we have already checked this eclass, just return
        if self.covering.contains_key(&root) || self.seen.contains(&root) {
            return;
        }

        // mark this root as seen, so that we don't follow loops infinitely
        self.seen.insert(root);

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

        // There are two cases:
        //     1) This eclass contains some instructions. We know that one of these
        //     instructions has to be picked, so just look at the children of the
        //     instructions.
        //     2) This eclass contains no instructions. We need to recurse into all
        //     children, and then take the cross product across the children.

        // XXX(sam): collapse these two cases into one. I don't think two cases is necessary
        let choices = if !instrs.is_empty() {
            let mut choices = ChoiceSet::default();
            for (instr, children) in instrs {
                for child in children {
                    self.compute_from_root(*child);
                }

                choices.extend(
                    children
                        .iter()
                        .map(|child| {
                            // if there is a cycle, the choice set contains all the
                            // instructions in the eclass besides the current instruction
                            if root == *child {
                                let mut choices =
                                    ChoiceSet::from(&self.egraph[root]).map_data(T::zero());
                                choices.retain(|covering| !covering.contains(&instr));
                                choices
                            } else {
                                self.covering[child].clone()
                            }
                        })
                        .fold(ChoiceSet::from([instr.0]).map_data(T::zero()), |acc, el| {
                            acc.cross_product(&el)
                        }),
                );
            }

            choices
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
            children
                .iter()
                .map(|child| &self.covering[child])
                .fold(ChoiceSet::from([]).map_data(T::one()), |acc, el| {
                    acc.cross_product(el)
                })
        };

        self.covering.insert(root, self.pruner.prune(choices));
    }

    pub fn get(&self, root: &egg::Id) -> Option<&ChoiceSet<T>> {
        self.covering.get(root)
    }
}

pub trait CoveringDataMerge {
    fn merge(self, other: &Self) -> Self;
    fn one() -> Self;
    fn zero() -> Self;
}

impl CoveringDataMerge for () {
    fn merge(self, _other: &Self) -> Self {}
    fn one() -> Self {}
    fn zero() -> Self {}
}

/// Represents a set of instructions that must be choosen together, in order to
/// cover a sub-tree.
#[derive(Clone, Default)]
pub struct Covering<T = ()> {
    set: HashSet<Instr>,
    pub data: T,
}

impl<T> PartialEq for Covering<T> {
    fn eq(&self, other: &Self) -> bool {
        self.set == other.set
    }
}

impl<T> Eq for Covering<T> {}

impl<T> PartialOrd for Covering<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Covering<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        // first compare by length
        match self.len().cmp(&other.len()) {
            x @ (Ordering::Less | Ordering::Greater) => return x,
            Ordering::Equal => (),
        }

        // if they are exactly the same, then they are equal of course
        if self.set == other.set {
            return Ordering::Equal;
        }

        // compare element-wise until we find a difference
        for (s, o) in self.set.iter().sorted().zip(other.set.iter().sorted()) {
            if s == o {
                continue;
            }

            return s.cmp(o);
        }

        unreachable!()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Covering<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut set_fmtter = f.debug_set();
        for item in self.set.iter().sorted() {
            set_fmtter.entry(item);
        }
        set_fmtter.entry(&self.data);
        set_fmtter.finish()
    }
}

impl<T> Covering<T>
where
    T: CoveringDataMerge,
{
    pub fn union(self, other: &Self) -> Self {
        Covering {
            set: self.set.union(&other.set).copied().collect(),
            data: self.data.merge(&other.data),
        }
    }
}

impl<T> Covering<T> {
    pub fn into_inner(self) -> HashSet<Instr> {
        self.set
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<'_, Instr> {
        self.set.iter()
    }

    pub fn contains(&self, value: &Instr) -> bool {
        self.set.contains(value)
    }

    pub fn dump<'a>(
        &'a self,
        instructions: &'a HashMap<Instr, egg::Pattern<HalideLang>>,
    ) -> BestIsaDump<'a> {
        BestIsaDump {
            isa: &self.set,
            instructions,
        }
    }

    pub fn map_data<U>(self, data: U) -> Covering<U> {
        Covering {
            set: self.set,
            data,
        }
    }
}

/// Each element represents a possible choice to cover some sub-tree.
#[derive(derive_more::Debug, Clone, PartialEq, Eq)]
#[debug("{_0:?}")]
pub struct ChoiceSet<T = ()>(BTreeSet<Covering<T>>);

impl<T> Default for ChoiceSet<T> {
    fn default() -> Self {
        ChoiceSet(BTreeSet::default())
    }
}

impl<T> ChoiceSet<T>
where
    T: Clone + Send + Sync + CoveringDataMerge,
{
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
}

impl<T> ChoiceSet<T> {
    #[allow(unused)]
    fn or<C>(mut self, value: C) -> Self
    where
        C: Into<Covering<T>>,
    {
        self.0.insert(value.into());
        self
    }

    pub fn extend<I>(&mut self, iter: I) -> &mut Self
    where
        I: IntoIterator<Item = Covering<T>>,
    {
        self.0.extend(iter);
        self
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Covering<T>> + Clone {
        self.0.iter()
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Covering<T>) -> bool,
    {
        self.0.retain(f)
    }

    #[allow(unused)]
    pub fn empty() -> Self {
        Self(BTreeSet::new())
    }

    pub fn map_data<U: Clone>(self, data: U) -> ChoiceSet<U> {
        ChoiceSet(
            self.0
                .into_iter()
                .map(|cov| cov.map_data(data.clone()))
                .collect(),
        )
    }
}

impl<const N: usize> From<[usize; N]> for Covering<()> {
    fn from(value: [usize; N]) -> Self {
        Covering {
            set: HashSet::from_iter(value.into_iter().map(Instr)),
            data: (),
        }
    }
}

impl<C, T> From<C> for ChoiceSet<T>
where
    C: Into<Covering<T>>,
    T: Default,
{
    fn from(value: C) -> Self {
        ChoiceSet(BTreeSet::from([value.into()]))
    }
}

impl<'a, D> From<&'a egg::EClass<HalideLang, D>> for ChoiceSet<()> {
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
                .map(|i| Covering {
                    set: HashSet::from([i]),
                    data: (),
                })
                .collect(),
        )
    }
}

impl<T> IntoIterator for ChoiceSet<T> {
    type Item = Covering<T>;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> FromIterator<Covering<T>> for ChoiceSet<T> {
    fn from_iter<I: IntoIterator<Item = Covering<T>>>(iter: I) -> Self {
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
        let b = ChoiceSet::empty();

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

    #[test]
    fn test_covering_ordering() {
        let cov1 = Covering::from([0, 1, 2]);
        let cov2 = Covering::from([2, 1, 3, 0]);

        assert!(cov1 < cov2);

        let cov1 = Covering::from([]);
        let cov2 = Covering::from([]);

        assert!(cov1 == cov2);

        let cov1 = Covering::from([0, 1, 2]);
        let cov2 = Covering::from([1, 2, 3]);

        assert!(cov1 < cov2);

        let cov1 = Covering::from([0, 1, 2]);
        let cov2 = Covering::from([2, 1, 0]);

        assert!(cov1 == cov2);
    }
}
