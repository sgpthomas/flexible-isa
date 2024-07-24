use super::ast;

pub struct MineExpressions<'a, T> {
    exprs: Vec<&'a ast::Expr<T>>,

    /// signals whether or not we should mine the current expressions. this is false
    /// until we hit the first produce block
    should_mine: bool,
}

impl<'a, T> Default for MineExpressions<'a, T> {
    fn default() -> Self {
        Self {
            exprs: vec![],
            should_mine: false,
        }
    }
}

impl<'a, T> MineExpressions<'a, T> {
    pub fn mine_module<'s>(&'s mut self, module: &'a ast::Module<T>)
    where
        'a: 's,
    {
        for func in &module.funcs {
            self.mine_func(func);
        }
    }

    pub fn mine_func<'s>(&'s mut self, func: &'a ast::Func<T>)
    where
        'a: 's,
    {
        self.mine_block(&func.stmts);
    }

    pub fn mine_block<'s>(&'s mut self, block: &'a ast::Block<T>)
    where
        'a: 's,
    {
        for stmt in block {
            self.mine_stmt(stmt);
        }
    }

    pub fn mine_stmt<'s>(&'s mut self, stmt: &'a ast::Stmt<T>)
    where
        'a: 's,
    {
        match stmt {
            ast::Stmt::Let { expr, .. } if self.should_mine => self.mine_expr(expr),
            ast::Stmt::Produce { body, .. } => {
                self.should_mine = true;
                self.mine_block(body)
            }
            ast::Stmt::Consume { body, .. } => self.mine_block(body),
            ast::Stmt::Store { access, value, .. } if self.should_mine => {
                self.mine_expr(&access.idx);
                self.mine_expr(value)
            }
            ast::Stmt::Allocate {
                condition: Some(expr),
                ..
            } if self.should_mine => self.mine_expr(expr),
            ast::Stmt::Free { .. } => (),
            ast::Stmt::For {
                low, high, body, ..
            } if self.should_mine => {
                self.mine_expr(low);
                self.mine_expr(high);
                self.mine_block(body);
            }
            ast::Stmt::If { cond, tru, fls, .. } if self.should_mine => {
                self.mine_expr(cond);
                self.mine_block(tru);
                if let Some(fls) = fls {
                    self.mine_block(fls);
                }
            }
            ast::Stmt::Predicate { cond, stmt, .. } if self.should_mine => {
                self.mine_expr(cond);
                self.mine_stmt(stmt);
            }
            ast::Stmt::Expr(expr, _) if self.should_mine => self.exprs.push(expr),
            _ => (),
        }
    }

    pub fn mine_expr<'s>(&'s mut self, expr: &'a ast::Expr<T>)
    where
        'a: 's,
    {
        // exlude ramp calls from top-level expressions
        // TODO: exclude it altogether. not totally sure
        // waht the right way to do this is
        match expr {
            ast::Expr::FunCall(id, _, _) if id.name == "ramp" => (),
            expr => self.exprs.push(expr),
        }
    }
}

impl<'a, T> IntoIterator for MineExpressions<'a, T> {
    type Item = &'a ast::Expr<T>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.exprs.into_iter()
    }
}
