use std::fmt::Debug;
use std::hash::Hash;
use std::{collections::HashMap, fmt::Display};

use super::{HalideExprOp, HalideLang};
use egg::Language;
use itertools::Itertools;

/// `egg::CostFunction` that weights instructions based on how much they are used.
pub struct InstructionSelect<Op>
where
    Op: Debug + Hash,
{
    op_count: HashMap<Op, usize>,
}

impl<Op> Display for InstructionSelect<Op>
where
    Op: Debug + Hash,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (op, count) in self.op_count.iter().sorted_by_key(|(_op, count)| *count) {
            writeln!(f, "{op:?}: {count}")?;
        }
        Ok(())
    }
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
        // count the occurences of every node in the graph
        let mut op_count = HashMap::new();
        for eclass in egraph.classes() {
            for node in &eclass.nodes {
                let op = node.operation().clone();
                op_count.entry(op).and_modify(|x| *x += 1).or_insert(1);
            }
        }

        Self { op_count }
    }

    pub fn from_recexpr(expr: &egg::RecExpr<babble::AstNode<Op>>) -> Self {
        let mut op_count = HashMap::new();

        fn count<Op: Ord + Debug + Clone + Hash>(
            expr: &egg::RecExpr<babble::AstNode<Op>>,
            op_count: &mut HashMap<Op, usize>,
            id: egg::Id,
        ) {
            if !expr[id].is_empty() {
                op_count
                    .entry(expr[id].operation().clone())
                    .and_modify(|x| *x += 1)
                    .or_insert(1);
            }

            expr[id].for_each(|id| count(expr, op_count, id))
        }

        let slice: &[babble::AstNode<Op>] = expr.as_ref();
        let head = &slice[slice.len() - 1];

        head.for_each(|id| count(expr, &mut op_count, id));
        Self { op_count }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Op, &usize)> {
        self.op_count.iter()
    }

    pub fn len(&self) -> usize {
        self.op_count.len()
    }

    pub fn is_empty(&self) -> bool {
        self.op_count.is_empty()
    }
}

impl InstructionSelect<HalideExprOp> {
    pub fn with_limit(self, limit: Option<usize>) -> Self {
        let allowed_ops: HashMap<_, _> = if let Some(limit) = &limit {
            println!("limiting");
            self.op_count
                .into_iter()
                .filter(|(op, _count)| matches!(op, HalideExprOp::Instruction(_)))
                .sorted_by_key(|(_op, count)| *count)
                .rev()
                .take(*limit)
                .collect()
        } else {
            self.op_count
        };

        println!("{allowed_ops:#?}");

        Self {
            op_count: allowed_ops,
        }
    }
}

impl egg::CostFunction<HalideLang> for InstructionSelect<HalideExprOp> {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &babble::AstNode<HalideExprOp>, mut costs: C) -> Self::Cost
    where
        C: FnMut(egg::Id) -> Self::Cost,
    {
        // println!("{:#?}", allowed_ops);

        // operations that occur more frequently are less expensive
        let max_occurence = self.op_count.values().max().unwrap();

        let op_cost = match enode.operation() {
            inst @ HalideExprOp::Instruction(_) if self.op_count.contains_key(inst) => {
                max_occurence - self.op_count[inst]
            }
            HalideExprOp::Instruction(_) => 1_000_000,
            // prefer instructions over anything else
            _ => max_occurence * 2,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}
