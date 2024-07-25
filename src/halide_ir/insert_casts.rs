use super::{ast, type_annotator::HalideType, Annotation, Visitor};

pub struct InsertCasts;

impl Visitor<HalideType, HalideType> for InsertCasts {
    fn default_u(&mut self, data: HalideType) -> HalideType {
        data
    }

    fn make_expr(&mut self, expr: ast::Expr<HalideType>) -> ast::Expr<HalideType> {
        let typ = expr.data().clone();

        if let HalideType::Unknown = typ {
            expr
        } else {
            match expr {
                x @ ast::Expr::Cast(..) => x,
                x @ ast::Expr::PtrCast(..) => x,
                x => ast::Expr::Cast(typ.to_id(), Box::new(x), typ),
            }
        }
    }
}
