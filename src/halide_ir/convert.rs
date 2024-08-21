//! Convert Halide IR into `babble::AstNode` so that we can use egraphs and library learning

use crate::{
    halide_ir::{ast, Annotation},
    instruction_select::HalideExprOp,
};
use anyhow::anyhow;

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
            ast::Expr::StructMember(struct_expr, thing, _) => babble::AstNode::new(
                HalideExprOp::StructMember,
                [struct_expr.into(), thing.into()],
            ),
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
            ast::Expr::Instruction { num, args, data: _ } => babble::AstNode::new(
                HalideExprOp::Instruction(num),
                args.into_iter().map(babble::Expr::from),
            ),
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

impl From<ast::Expr<u64>> for egg::RecExpr<babble::AstNode<HalideExprOp>> {
    fn from(value: ast::Expr<u64>) -> Self {
        // wrap every expression with it's id, so that we can map
        // them back into the original AST
        let id = *value.data();
        let raw = babble::Expr::from(value);
        let wrapped = babble::AstNode::new(HalideExprOp::Named(id), vec![raw]);
        egg::RecExpr::from(babble::Expr(wrapped))
    }
}

impl TryFrom<&babble::Expr<HalideExprOp>> for ast::Expr<()> {
    type Error = anyhow::Error;

    fn try_from(value: &babble::Expr<HalideExprOp>) -> Result<Self, Self::Error> {
        value.clone().try_into()
    }
}

impl TryFrom<babble::Expr<HalideExprOp>> for ast::Expr<()> {
    type Error = anyhow::Error;

    fn try_from(value: babble::Expr<HalideExprOp>) -> anyhow::Result<Self> {
        let (op, children) = value.into_inner().into_parts();
        match (op, children.as_slice()) {
            (HalideExprOp::Number(n), []) => Ok(ast::Expr::Number(n, ())),
            (HalideExprOp::Number(_), _) => Err(anyhow!("Wrong number of arguments for `Number`")),
            (HalideExprOp::Ident(id), []) => Ok(ast::Expr::Ident(id, ())),
            (HalideExprOp::Ident(_), _) => Err(anyhow!("Wrong number of arguments for `Ident`")),
            (HalideExprOp::Neg, [inner]) => Ok(ast::Expr::neg(inner.try_into()?)),
            (HalideExprOp::Neg, _) => Err(anyhow!("Wrong number of arguments for `Neg`")),
            (HalideExprOp::Add, [lhs, rhs]) => Ok(ast::Expr::add(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Add, _) => Err(anyhow!("Wrong number of arguments for `Add`")),
            (HalideExprOp::Sub, [lhs, rhs]) => Ok(ast::Expr::sub(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Sub, _) => Err(anyhow!("Wrong number of arguments for `Sub`")),
            (HalideExprOp::Mul, [lhs, rhs]) => Ok(ast::Expr::mul(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Mul, _) => Err(anyhow!("Wrong number of arguments for `Mul`")),
            (HalideExprOp::Div, [lhs, rhs]) => Ok(ast::Expr::div(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Div, _) => Err(anyhow!("Wrong number of arguments for `Div`")),
            (HalideExprOp::Modulo, [lhs, rhs]) => {
                Ok(ast::Expr::modulo(lhs.try_into()?, rhs.try_into()?))
            }
            (HalideExprOp::Modulo, _) => Err(anyhow!("Wrong number of arguments for `Modulo`")),
            (HalideExprOp::Lt, [lhs, rhs]) => Ok(ast::Expr::lt(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Lt, _) => Err(anyhow!("Wrong number of arguments for `Lt`")),
            (HalideExprOp::Lte, [lhs, rhs]) => Ok(ast::Expr::lte(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Lte, _) => Err(anyhow!("Wrong number of arguments for `Lte`")),
            (HalideExprOp::Eq, [lhs, rhs]) => Ok(ast::Expr::eq(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Eq, _) => Err(anyhow!("Wrong number of arguments for `Eq`")),
            (HalideExprOp::Neq, [lhs, rhs]) => Ok(ast::Expr::neq(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Neq, _) => Err(anyhow!("Wrong number of arguments for `Neq`")),
            (HalideExprOp::Gte, [lhs, rhs]) => Ok(ast::Expr::gte(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Gte, _) => Err(anyhow!("Wrong number of arguments for `Geq`")),
            (HalideExprOp::Gt, [lhs, rhs]) => Ok(ast::Expr::gt(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Gt, _) => Err(anyhow!("Wrong number of arguments for `Gt`")),
            (HalideExprOp::And, [lhs, rhs]) => Ok(ast::Expr::and(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::And, _) => Err(anyhow!("Wrong number of arguments for `And`")),
            (HalideExprOp::Or, [lhs, rhs]) => Ok(ast::Expr::or(lhs.try_into()?, rhs.try_into()?)),
            (HalideExprOp::Or, _) => Err(anyhow!("Wrong number of arguments for `Or`")),
            (HalideExprOp::If, [lhs, rhs]) => Ok(ast::Expr::If(
                Box::new(lhs.try_into()?),
                Box::new(rhs.try_into()?),
                (),
            )),
            (HalideExprOp::If, _) => Err(anyhow!("Wrong number of arguments for `If`")),
            (HalideExprOp::StructMember, [struct_expr, thing]) => Ok(ast::Expr::StructMember(
                struct_expr.try_into()?,
                thing.try_into()?,
                (),
            )),
            (HalideExprOp::StructMember, _) => {
                Err(anyhow!("Wrong number of arguments for `StructMember`"))
            }
            (HalideExprOp::FunCall(id), args) => Ok(ast::Expr::FunCall(
                id,
                args.iter()
                    .map(ast::Expr::try_from)
                    .collect::<Result<_, _>>()?,
                (),
            )),
            (HalideExprOp::Reinterpret(typs), args) => Ok(ast::Expr::Reinterpret(
                typs,
                args.iter()
                    .map(ast::Expr::try_from)
                    .collect::<Result<_, _>>()?,
                (),
            )),
            (HalideExprOp::Cast(mut typs), [expr]) if typs.len() == 1 => Ok(ast::Expr::Cast(
                typs.swap_remove(0),
                Box::new(expr.try_into()?),
                (),
            )),
            (HalideExprOp::Cast(mut typs), [expr]) => {
                // pop the last element because it is *
                let _ = typs.pop();
                Ok(ast::Expr::PtrCast(typs, Box::new(expr.try_into()?), ()))
            }
            (HalideExprOp::Cast(_), _) => Err(anyhow!("Wrong number of arguments for `Cast`")),
            (HalideExprOp::Access, [var, idx]) => Ok(ast::Expr::Access(
                ast::Access {
                    var: var.try_into()?,
                    idx: Box::new(idx.try_into()?),
                    align: None,
                },
                (),
            )),
            (HalideExprOp::Access, _) => Err(anyhow!("Wrong number of arguments for `Access`")),
            (HalideExprOp::Instruction(i), args) => Ok(ast::Expr::Instruction {
                num: i,
                args: args
                    .iter()
                    .map(ast::Expr::try_from)
                    .collect::<Result<_, _>>()?,
                data: (),
            }),
            (HalideExprOp::Named(_), _) => Err(anyhow!("Can't convert `Named`")),
            (HalideExprOp::Babble(_), _) => Err(anyhow!("Can't convert babble ops")),
            (HalideExprOp::PatternVar(_), _) => Err(anyhow!("Can't convert pattern vars")),
        }
    }
}

impl TryFrom<&babble::Expr<HalideExprOp>> for ast::Id {
    type Error = anyhow::Error;

    fn try_from(value: &babble::Expr<HalideExprOp>) -> Result<Self, Self::Error> {
        match value.0.operation() {
            HalideExprOp::Ident(x) => Ok(x.clone()),
            _ => Err(anyhow!("Not an ident")),
        }
    }
}
