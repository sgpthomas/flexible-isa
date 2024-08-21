use super::{ast, SetData, Visitor};
use crate::{
    halide_ir::Annotation,
    instruction_select::{BabbleOp, HalideExprOp, HalideLang},
};
use anyhow::{anyhow, Context};
use std::collections::HashMap;

pub struct Rewrite {
    map: HashMap<u64, ast::Expr<u64>>,
}

impl Rewrite {
    pub fn new(prog: egg::RecExpr<HalideLang>) -> anyhow::Result<Self> {
        // unflatten the program
        let unflattened = babble::Expr::from(prog.clone());
        let mut set_data = SetData::new(0);

        let children = if let (HalideExprOp::Babble(BabbleOp::List), children) =
            unflattened.into_inner().into_parts()
        {
            Ok(children)
        } else {
            Err(anyhow!("Expr didn't have top-level list"))
        }?;

        let map = children
            .into_iter()
            .map(|op| {
                let (op, mut expr) = op.into_inner().into_parts();
                if let HalideExprOp::Named(id) = op {
                    let ast_expr = ast::Expr::try_from(expr.swap_remove(0))?;
                    Ok((id, set_data.do_pass_expr(ast_expr)))
                } else {
                    Err(anyhow!("Unnamed expression: {op:?}"))
                }
            })
            .collect::<anyhow::Result<_>>()
            .context("translating children into map")?;

        Ok(Rewrite { map })
    }
}

impl Visitor<u64> for Rewrite {
    type Output = u64;

    fn default_u(&mut self, data: u64) -> Self::Output {
        data
    }

    fn make_expr(&mut self, expr: ast::Expr<Self::Output>) -> ast::Expr<Self::Output> {
        if let Some(replacement) = self.map.get(expr.data()) {
            replacement.clone()
        } else {
            expr
        }
    }
}
