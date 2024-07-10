//! Convert Halide IR into `babble::AstNode` so that we can use egraphs and library learning

use crate::{halide_ir::ast, instruction_select::HalideExprOp};

impl From<ast::Expr> for babble::Expr<HalideExprOp> {
    fn from(value: ast::Expr) -> Self {
        babble::Expr(match value {
            ast::Expr::Number(n) => babble::AstNode::leaf(HalideExprOp::Number(n)),
            ast::Expr::Ident(id) => babble::AstNode::leaf(HalideExprOp::Ident(id)),
            ast::Expr::Neg(val) => babble::AstNode::new(HalideExprOp::Neg, [val.into()]),
            ast::Expr::Add(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Add, [lhs.into(), rhs.into()])
            }
            ast::Expr::Sub(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Sub, [lhs.into(), rhs.into()])
            }
            ast::Expr::Mul(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Mul, [lhs.into(), rhs.into()])
            }
            ast::Expr::Div(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Div, [lhs.into(), rhs.into()])
            }
            ast::Expr::Modulo(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Modulo, [lhs.into(), rhs.into()])
            }
            ast::Expr::Lt(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Lt, [lhs.into(), rhs.into()])
            }
            ast::Expr::Lte(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Lte, [lhs.into(), rhs.into()])
            }
            ast::Expr::Eq(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Eq, [lhs.into(), rhs.into()])
            }
            ast::Expr::Neq(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Neq, [lhs.into(), rhs.into()])
            }
            ast::Expr::Gte(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Gte, [lhs.into(), rhs.into()])
            }
            ast::Expr::Gt(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Gt, [lhs.into(), rhs.into()])
            }
            ast::Expr::And(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::And, [lhs.into(), rhs.into()])
            }
            ast::Expr::Or(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::Or, [lhs.into(), rhs.into()])
            }
            ast::Expr::If(lhs, rhs) => {
                babble::AstNode::new(HalideExprOp::If, [lhs.into(), rhs.into()])
            }
            ast::Expr::FunCall(id, args) => {
                let mut babble_args = vec![];
                babble_args.push(id.into());
                babble_args.extend(args.into_iter().map(babble::Expr::from));
                babble::AstNode::new(HalideExprOp::FunCall, babble_args)
            }
            ast::Expr::Reinterpret(typs, args) => babble::AstNode::new(
                HalideExprOp::Reinterpret(typs),
                args.into_iter().map(babble::Expr::from),
            ),
            ast::Expr::Cast(typs, expr) => {
                babble::AstNode::new(HalideExprOp::Cast(typs), [expr.into()])
            }
            ast::Expr::Access(ast::Access { var, idx, .. }) => {
                babble::AstNode::new(HalideExprOp::Access, [var.into(), idx.into()])
            }
            ast::Expr::LetIn(_, _, _) => unreachable!("Let in's should be removed by now"),
        })
    }
}

impl From<Box<ast::Expr>> for babble::Expr<HalideExprOp> {
    fn from(value: Box<ast::Expr>) -> Self {
        (*value).into()
    }
}

impl From<ast::Id> for babble::Expr<HalideExprOp> {
    fn from(value: ast::Id) -> Self {
        babble::Expr(babble::AstNode::leaf(HalideExprOp::Ident(value)))
    }
}
