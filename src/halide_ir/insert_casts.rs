use super::{ast, type_annotator::HalideType};

#[derive(Default)]
pub struct InsertCasts;

impl InsertCasts {
    pub fn cast_module(&mut self, module: ast::Module<HalideType>) -> ast::Module<HalideType> {
        todo!()
    }

    pub fn cast_func(&mut self, func: ast::Func<HalideType>) -> ast::Func<HalideType> {
        todo!()
    }

    pub fn cast_block(&mut self, block: ast::Block<HalideType>) -> ast::Block<HalideType> {
        todo!()
    }

    pub fn cast_stmt(&mut self, stmt: ast::Stmt<HalideType>) -> ast::Stmt<HalideType> {
        todo!()
    }

    pub fn cast_expr(&mut self, expr: ast::Expr<HalideType>) -> ast::Expr<HalideType> {
        todo!()
    }
}
