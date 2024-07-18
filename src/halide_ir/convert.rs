//! Convert Halide IR into `babble::AstNode` so that we can use egraphs and library learning

use crate::{halide_ir::ast, instruction_select::HalideExprOp};

impl<T> From<ast::Expr<T>> for babble::Expr<HalideExprOp> {
    fn from(value: ast::Expr<T>) -> Self {
        babble::Expr(match value {
            ast::Expr::Number(n, _) => babble::AstNode::leaf(HalideExprOp::Number(n)),
            ast::Expr::Ident(id, _) => babble::AstNode::leaf(HalideExprOp::Ident(id)),
            ast::Expr::Unop(op, inner, _) => babble::AstNode::new(op.into(), [inner.into()]),
            ast::Expr::ArithBinop(op, lhs, rhs, _) => {
                babble::AstNode::new(op.into(), [lhs.into(), rhs.into()])
            }
            ast::Expr::CompBinop(op, lhs, rhs, _) => {
                babble::AstNode::new(op.into(), [lhs.into(), rhs.into()])
            }
            ast::Expr::If(lhs, rhs, _) => {
                babble::AstNode::new(HalideExprOp::If, [lhs.into(), rhs.into()])
            }
            ast::Expr::FunCall(id, args, _) => babble::AstNode::new(
                HalideExprOp::FunCall(id),
                args.into_iter().map(babble::Expr::from),
            ),
            ast::Expr::Reinterpret(typs, args, _) => babble::AstNode::new(
                HalideExprOp::Reinterpret(typs),
                args.into_iter().map(babble::Expr::from),
            ),
            ast::Expr::Cast(typ, expr, _) => {
                babble::AstNode::new(HalideExprOp::Cast(vec![typ]), [expr.into()])
            }
            ast::Expr::PtrCast(mut typs, expr, _) => {
                // TODO: this is hacky
                typs.push(ast::Id::new("*"));
                babble::AstNode::new(HalideExprOp::Cast(typs), [expr.into()])
            }
            ast::Expr::Access(ast::Access { var, idx, .. }, _) => {
                babble::AstNode::new(HalideExprOp::Access, [var.into(), idx.into()])
            }
            ast::Expr::LetIn(_, _, _, _) => unreachable!("Let in's should be removed by now"),
        })
    }
}

impl<T> From<Box<ast::Expr<T>>> for babble::Expr<HalideExprOp> {
    fn from(value: Box<ast::Expr<T>>) -> Self {
        (*value).into()
    }
}

impl From<ast::Id> for babble::Expr<HalideExprOp> {
    fn from(value: ast::Id) -> Self {
        babble::Expr(babble::AstNode::leaf(HalideExprOp::Ident(value)))
    }
}

impl From<ast::Unop> for HalideExprOp {
    fn from(value: ast::Unop) -> Self {
        match value {
            ast::Unop::Neg => HalideExprOp::Neg,
        }
    }
}

impl From<ast::ArithBinop> for HalideExprOp {
    fn from(value: ast::ArithBinop) -> Self {
        match value {
            ast::ArithBinop::Add => HalideExprOp::Add,
            ast::ArithBinop::Sub => HalideExprOp::Sub,
            ast::ArithBinop::Mul => HalideExprOp::Mul,
            ast::ArithBinop::Div => HalideExprOp::Div,
            ast::ArithBinop::Modulo => HalideExprOp::Modulo,
        }
    }
}

impl From<ast::CompBinop> for HalideExprOp {
    fn from(value: ast::CompBinop) -> Self {
        match value {
            ast::CompBinop::Lt => HalideExprOp::Lt,
            ast::CompBinop::Lte => HalideExprOp::Lte,
            ast::CompBinop::Eq => HalideExprOp::Eq,
            ast::CompBinop::Neq => HalideExprOp::Neq,
            ast::CompBinop::Gte => HalideExprOp::Gte,
            ast::CompBinop::Gt => HalideExprOp::Gt,
            ast::CompBinop::And => HalideExprOp::And,
            ast::CompBinop::Or => HalideExprOp::Or,
        }
    }
}

impl<T> From<ast::Expr<T>> for egg::RecExpr<babble::AstNode<HalideExprOp>> {
    fn from(value: ast::Expr<T>) -> Self {
        egg::RecExpr::from(babble::Expr::from(value))
    }
}
