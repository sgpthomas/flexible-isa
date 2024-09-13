use super::{ast, Visitor};

/// Rename every variable in a module so that it is unique. If two separate
/// functions both take in some parameter `input`. We don't want those to accidently
/// be interpreted as meaning the same thing. We solve that by rewriting all
/// identifiers to start with a unique prefix.
#[derive(Default)]
pub struct UniqueIdents {
    module_name: Option<ast::Id>,
    count: usize,
}

impl UniqueIdents {
    fn prefix(&self) -> Option<String> {
        self.module_name
            .as_ref()
            .map(|id| format!("{}_{}$", id.name, self.count))
    }
}

impl<T> Visitor<T> for UniqueIdents {
    type Output = T;

    fn default_u(&mut self, data: T) -> T {
        data
    }

    fn start_module(&mut self, module: &ast::Module<T>) {
        self.module_name = module.get_param("name").cloned();
    }

    fn start_func(
        &mut self,
        _metadata: &ast::Id,
        _name: &ast::Id,
        _args: &[ast::Id],
        _stmts: &ast::Block<T>,
        _data: &T,
    ) {
        self.count += 1;
    }

    fn make_func(
        &mut self,
        metadata: ast::Id,
        name: ast::Id,
        args: Vec<ast::Id>,
        stmts: ast::Block<T>,
        data: T,
    ) -> ast::Func<T> {
        ast::Func {
            metadata,
            name,
            args: args
                .into_iter()
                .map(|id| id.with_prefix(&self.prefix()))
                .collect(),
            stmts,
            data,
        }
    }

    fn make_produce_stmt(&mut self, var: ast::Id, body: ast::Block<T>, data: T) -> ast::Stmt<T> {
        ast::Stmt::Produce {
            var: var.with_prefix(&self.prefix()),
            body,
            data,
        }
    }

    fn consume_stmt(&mut self, var: ast::Id, body: ast::Block<T>, data: T) -> ast::Stmt<T> {
        ast::Stmt::Consume {
            var: var.with_prefix(&self.prefix()),
            body,
            data,
        }
    }

    fn make_access_expr(&mut self, access: ast::Access<T>, data: T) -> ast::Expr<T> {
        ast::Expr::Access(
            ast::Access {
                var: access.var.with_prefix(&self.prefix()),
                idx: access.idx,
                align: access.align,
                predicate: access.predicate,
            },
            data,
        )
    }

    fn allocate_stmt(
        &mut self,
        name: ast::Id,
        typ: ast::Id,
        extents: Vec<ast::Expr<T>>,
        loc: ast::MemoryType,
        condition: Option<ast::Expr<T>>,
        data: T,
    ) -> ast::Stmt<T> {
        ast::Stmt::Allocate {
            name: name.with_prefix(&self.prefix()),
            typ,
            extents,
            loc,
            condition,
            data,
        }
    }

    fn make_for_stmt(
        &mut self,
        var: ast::Id,
        low: ast::Expr<T>,
        high: ast::Expr<T>,
        device: ast::DeviceApi,
        body: ast::Block<T>,
        data: T,
    ) -> ast::Stmt<T> {
        ast::Stmt::For {
            var: var.with_prefix(&self.prefix()),
            low,
            high,
            device,
            body,
            data,
        }
    }

    fn free_stmt(&mut self, var: ast::Id, data: T) -> ast::Stmt<T> {
        ast::Stmt::Free {
            var: var.with_prefix(&self.prefix()),
            data,
        }
    }

    fn make_let_stmt(&mut self, var: ast::Id, expr: ast::Expr<T>, data: T) -> Vec<ast::Stmt<T>> {
        vec![ast::Stmt::Let {
            var: var.with_prefix(&self.prefix()),
            expr,
            data,
        }]
    }

    fn make_ident_expr(&mut self, id: ast::Id, data: T) -> ast::Expr<T> {
        ast::Expr::Ident(id.with_prefix(&self.prefix()), data)
    }
}
