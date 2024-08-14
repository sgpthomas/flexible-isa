use super::{ast, Annotation, HalideType, Visitor};

pub struct InsertCasts;

impl Visitor<HalideType, HalideType> for InsertCasts {
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
