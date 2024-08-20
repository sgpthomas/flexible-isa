use crate::halide_ir::Annotation;

use super::{ast, Visitor};

/// Extract all nested expressions into assignments.
///
/// Implementation notes:
/// When we enter a stmt node (`start_stmt`), we record the name of something about
/// this statement from which we can derive unique identifiers. We only ever lift
/// expressions if we have some base `var` from which to make new names.
///
/// Then, we start entering expressions. If we hit some expression that is
/// `complex`, meaning that is has nested children, we record it's expression id so
/// that we know when we hit it on the way back up the tree. This is what the
/// `nested` field represents. While the `nested` field contains some node
/// identifier, we record all sub-expressions that aren't leafs (so not `Ident` or
/// `Number`). When we return to the expressions that set `nested`, we unset
/// `nested` and resume the process.
#[derive(Default)]
pub struct Flatten {
    var: Option<ast::Id>,
    count: usize,
    nested: Option<u64>,
    stack: Vec<ast::Stmt<u64>>,
}

impl Flatten {
    /// Consume the stack of expressions, and reset state back to default
    fn consume(&mut self) -> Vec<ast::Stmt<u64>> {
        self.var = None;
        self.count = 0;
        self.nested = None;
        self.stack.drain(..).collect()
    }

    /// Create a new let stmt for a given expression with a var
    /// name based on the current context. If there is no `self.var`
    /// set, this method does nothing.
    fn push_expr(&mut self, expr: ast::Expr<u64>) -> ast::Expr<u64> {
        // don't push identifiers, we can just return them
        if matches!(expr, ast::Expr::Ident(..) | ast::Expr::Number(..)) {
            return expr;
        }

        if let Some(var) = &self.var {
            let new_var = ast::Id::new(format!("{var}_{}", self.count));
            self.count += 1;
            self.stack.push(ast::Stmt::Let {
                var: new_var.clone(),
                expr,
                data: 0,
            });
            ast::Expr::Ident(new_var, 0)
        } else {
            expr
        }
    }
}

impl Visitor<u64> for Flatten {
    type Output = u64;

    fn default_u(&mut self, data: u64) -> Self::Output {
        data
    }

    fn start_stmt(&mut self, stmt: &ast::Stmt<u64>) {
        let var = match stmt {
            ast::Stmt::Let { var, .. } => var.clone(),
            ast::Stmt::Produce { var, .. } => var.clone(),
            ast::Stmt::Consume { var, .. } => var.clone(),
            ast::Stmt::Store { access, .. } => access.var.clone(),
            ast::Stmt::Allocate { name, .. } => name.clone(),
            ast::Stmt::Free { var, .. } => var.clone(),
            ast::Stmt::For { var, .. } => var.clone(),
            ast::Stmt::If { data, .. } => ast::Id::new(format!("if_{data}")),
            ast::Stmt::Predicate { data, .. } => ast::Id::new(format!("predicate_{data}")),
            ast::Stmt::Expr(_, data) => ast::Id::new(format!("expr_{data}")),
        };
        self.var = Some(var.clone());
    }

    fn make_stmt(&mut self, stmt: ast::Stmt<Self::Output>) -> Vec<ast::Stmt<Self::Output>> {
        let mut res = self.consume();
        res.push(stmt);
        res
    }

    fn start_expr(&mut self, expr: &ast::Expr<u64>) {
        if expr.is_complex() && self.nested.is_none() {
            self.nested = Some(*expr.data());
        }
    }

    fn make_expr(&mut self, expr: ast::Expr<Self::Output>) -> ast::Expr<Self::Output> {
        match self.nested {
            // we hit the expression we started from
            // disable nesting + return original expression
            Some(id) if &id == expr.data() => {
                self.nested = None;
                expr
            }
            // we are currently nesting, but we haven't
            // hit the start point yet. these are the expressions
            // that we want to lift
            Some(_) => self.push_expr(expr),

            // we aren't in a nesting context, just return
            // the original expression
            None => expr,
        }
    }
}