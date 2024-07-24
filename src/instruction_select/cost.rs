use super::lang;
use egg::Language;

pub struct InstructionSelect;

impl egg::CostFunction<babble::AstNode<lang::HalideExprOp>> for InstructionSelect {
    type Cost = u64;

    fn cost<C>(&mut self, enode: &babble::AstNode<lang::HalideExprOp>, mut costs: C) -> Self::Cost
    where
        C: FnMut(egg::Id) -> Self::Cost,
    {
        let op_cost = match enode.operation() {
            lang::HalideExprOp::Instruction(_) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}
