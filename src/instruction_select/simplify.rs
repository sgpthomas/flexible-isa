use std::str::FromStr;

use super::{
    lang::{BabbleOp, PatternConvert},
    learn::LibraryPattern,
    HalideExprOp,
};

pub trait Simplify {
    fn simplify(&self) -> Self;
}

impl Simplify for LibraryPattern {
    fn simplify(&self) -> Self {
        // first convert the pattern into a babble expression
        let expr: babble::Expr<_> = babble::AstNode::from_pattern(self).into();
        // then recursively simplify the expression
        let simp = expr.simplify().into();
        // and finally turn back into a pattern
        babble::AstNode::to_pattern(&simp)
    }
}

impl Simplify for babble::Expr<HalideExprOp> {
    fn simplify(&self) -> Self {
        match self.as_ref().as_parts() {
            (HalideExprOp::Babble(BabbleOp::Lib(_lib_id)), [bound_value, _body]) => {
                bound_value.simplify()
            }
            (HalideExprOp::Babble(BabbleOp::Lambda), [body]) => body.simplify(),
            (HalideExprOp::Babble(BabbleOp::LambdaVar(idx)), []) => {
                babble::Expr(babble::AstNode::leaf(HalideExprOp::PatternVar(
                    egg::Var::from_str(&format!("?x{}", idx.0)).unwrap(),
                )))
            }
            (op, args) => babble::Expr(babble::AstNode::new(
                op.clone(),
                args.iter().map(|x| x.simplify()),
            )),
        }
    }
}
