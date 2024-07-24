use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use super::{HalideExprOp, HalideLang};
use egg::Language;
use itertools::Itertools;

pub struct InstructionSelect<Op>
where
    Op: Debug + Hash,
{
    op_count: HashMap<Op, usize>,
}

impl<Op> Debug for InstructionSelect<Op>
where
    Op: Debug + Hash,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("InstructionSelect");
        s.field(
            "op_count",
            &self
                .op_count
                .iter()
                .sorted_by_key(|(_op, count)| *count)
                .collect_vec(),
        );
        s.finish()
    }
}

impl<Op> InstructionSelect<Op>
where
    Op: Ord + Debug + Clone + Hash,
{
    pub fn new<N: egg::Analysis<babble::AstNode<Op>>>(
        egraph: &egg::EGraph<babble::AstNode<Op>, N>,
    ) -> Self {
        // count the corrences of every node in the graph
        let mut op_count = HashMap::new();
        for eclass in egraph.classes() {
            for node in &eclass.nodes {
                let op = node.operation().clone();
                op_count.entry(op).and_modify(|x| *x += 1).or_insert(1);
            }
        }

        Self { op_count }
    }
}

impl egg::CostFunction<HalideLang> for InstructionSelect<HalideExprOp> {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &babble::AstNode<HalideExprOp>, mut costs: C) -> Self::Cost
    where
        C: FnMut(egg::Id) -> Self::Cost,
    {
        // operations that occur more frequently are less expensive
        let max_occurence = self.op_count.values().max().unwrap();
        let op_cost = match enode.operation() {
            inst @ HalideExprOp::Instruction(_) => max_occurence - self.op_count[inst],
            // prefer instructions over anything else
            _ => max_occurence * 2,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}
