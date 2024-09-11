use super::{ast, Annotation, HalideType, Visitor};

#[derive(Default)]
pub struct InsertCasts;

impl Visitor<HalideType> for InsertCasts {
    type Output = HalideType;

    fn default_u(&mut self, data: HalideType) -> Self::Output {
        data
    }

    fn expr_stmt(
        &mut self,
        expr: ast::Expr<Self::Output>,
        data: HalideType,
    ) -> ast::Stmt<Self::Output> {
        let expr = match expr {
            ast::Expr::Cast(_, inner, _) => *inner,
            ast::Expr::PtrCast(_, inner, _) => *inner,
            x => x,
        };

        ast::Stmt::Expr(expr, data)
    }

    fn make_expr(&mut self, expr: ast::Expr<Self::Output>) -> ast::Expr<Self::Output> {
        let typ = expr.data().clone();

        match expr {
            x @ ast::Expr::Cast(..) => x,
            x @ ast::Expr::PtrCast(..) => x,
            x => ast::Expr::Cast(typ.to_id(), Box::new(x), typ),
        }
    }
}

#[derive(Default)]
pub struct RemoveCasts;

impl<T> Visitor<T> for RemoveCasts {
    type Output = T;

    fn default_u(&mut self, data: T) -> Self::Output {
        data
    }

    fn make_cast_expr(
        &mut self,
        _typ: ast::Id,
        expr: ast::Expr<Self::Output>,
        _data: T,
    ) -> ast::Expr<Self::Output> {
        expr
    }
}
