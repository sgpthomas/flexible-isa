use std::collections::HashMap;

use super::{ast, Visitor};

/// Inlines `let x = v in y` expressions by replacing all instances of `x` in `y`
/// with `v`
#[derive(Default)]
pub struct Inline<T> {
    /// keep track of the variables that we want to substitute
    substs: HashMap<ast::Id, ast::Expr<T>>,

    /// should I inline let stmts
    let_stmts: bool,

    /// keep track of produce blocks we've seen
    produce: Vec<ast::Id>,
}

impl<T> Inline<T> {
    pub fn new(let_stmts: bool) -> Self {
        Self {
            substs: HashMap::default(),
            let_stmts,
            produce: vec![],
        }
    }
}

impl<T> Visitor<T> for Inline<T>
where
    T: Clone,
{
    type Output = T;

    fn default_u(&mut self, data: T) -> T {
        data
    }

    fn start_produce_stmt(&mut self, var: &ast::Id, _body: &ast::Block<T>, _data: &T) {
        self.produce.push(var.clone());
    }

    fn make_produce_stmt(
        &mut self,
        var: ast::Id,
        body: ast::Block<Self::Output>,
        data: T,
    ) -> ast::Stmt<Self::Output> {
        let x = self.produce.pop();
        debug_assert_eq!(Some(&var), x.as_ref());
        ast::Stmt::Produce {
            var,
            body,
            data: self.default_u(data),
        }
    }

    fn make_let_stmt(
        &mut self,
        var: ast::Id,
        expr: ast::Expr<Self::Output>,
        data: T,
    ) -> Vec<ast::Stmt<Self::Output>> {
        // we only insert a binding for this let statement, after we have finishing
        // visiting its expression. this ensures that if there are variables inside of this
        // let statements' expression, we record their fully inlined version
        if self.let_stmts && !self.produce.is_empty() {
            self.substs.insert(var.clone(), expr.clone());
            vec![]
        } else {
            vec![ast::Stmt::Let {
                var,
                expr,
                data: self.default_u(data),
            }]
        }
    }

    fn start_letin_expr(
        &mut self,
        var: &ast::Id,
        binding: &ast::Expr<T>,
        _body: &ast::Expr<T>,
        _data: &T,
    ) {
        // add a substition for this let in expression
        self.substs.insert(var.clone(), binding.clone());
    }

    fn make_ident_expr(&mut self, id: ast::Id, data: T) -> ast::Expr<T> {
        // if the ident that we are looking at is in the substs map, use the substition
        // instead of the variable
        if let Some(expr) = self.substs.get(&id) {
            expr.clone()
        } else {
            ast::Expr::Ident(id, data)
        }
    }

    fn make_letin_expr(
        &mut self,
        var: ast::Id,
        _binding: ast::Expr<T>,
        body: ast::Expr<T>,
        _data: T,
    ) -> ast::Expr<T> {
        // remove the substition when we are coming back out of the let-in expression
        self.substs.remove(&var);

        // we have inlined `var` in `body`, so we can just return `body`.
        // this will remove all instances of `let .. in` expressions
        body
    }
}
