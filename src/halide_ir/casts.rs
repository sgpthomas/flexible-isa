use super::{ast, Annotation, HalideType, Visitor};

#[derive(Default)]
pub struct InsertCasts;

impl Visitor<HalideType> for InsertCasts {
    type Output = HalideType;

    fn default_u(&mut self, data: HalideType) -> HalideType {
        data
    }

    fn expr_stmt(
        &mut self,
        expr: ast::Expr<HalideType>,
        data: HalideType,
    ) -> ast::Stmt<HalideType> {
        let expr = match expr {
            ast::Expr::Cast(_, inner, _) => *inner,
            ast::Expr::PtrCast(_, inner, _) => *inner,
            x => x,
        };

        ast::Stmt::Expr(expr, data)
    }

    fn make_expr(&mut self, expr: ast::Expr<HalideType>) -> ast::Expr<HalideType> {
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
