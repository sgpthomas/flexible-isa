use super::ast;

#[derive(Default)]
pub struct MineExpressions<'a> {
    exprs: Vec<&'a ast::Expr>,
}

impl<'a> MineExpressions<'a> {
    pub fn mine_func<'s>(&'s mut self, func: &'a ast::Func)
    where
        'a: 's,
    {
        self.mine_block(&func.stmts);
    }

    pub fn mine_block<'s>(&'s mut self, block: &'a ast::Block)
    where
        'a: 's,
    {
        for stmt in block {
            self.mine_stmt(stmt);
        }
    }

    pub fn mine_stmt<'s>(&'s mut self, stmt: &'a ast::Stmt)
    where
        'a: 's,
    {
        match stmt {
            ast::Stmt::Let { var: _, expr } => self.exprs.push(expr),
            ast::Stmt::Produce { var: _, body } => self.mine_block(body),
            ast::Stmt::Consume { var: _, body } => self.mine_block(body),
            ast::Stmt::Store { access, value } => {
                self.exprs.push(&access.idx);
                self.exprs.push(value)
            }
            ast::Stmt::Allocate {
                access,
                loc: _,
                condition,
            } => {
                self.exprs.push(&access.idx);
                if let Some(expr) = condition {
                    self.exprs.push(expr)
                }
            }
            ast::Stmt::Free { .. } => (),
            ast::Stmt::For {
                var,
                low,
                high,
                device: _,
                body,
            } => {
                self.exprs.push(var);
                self.exprs.push(low);
                self.exprs.push(high);
                self.mine_block(body);
            }
            ast::Stmt::If { cond, tru, fls } => {
                self.exprs.push(cond);
                self.mine_block(tru);
                if let Some(fls) = fls {
                    self.mine_block(fls);
                }
            }
            ast::Stmt::Predicate { cond, stmt } => {
                self.exprs.push(cond);
                self.mine_stmt(stmt);
            }
            ast::Stmt::Expr(expr) => self.exprs.push(expr),
        }
    }
}

impl<'a> IntoIterator for MineExpressions<'a> {
    type Item = &'a ast::Expr;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.exprs.into_iter()
    }
}
