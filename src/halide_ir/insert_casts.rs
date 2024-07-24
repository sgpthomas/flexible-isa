use super::{
    ast::{self},
    type_annotator::HalideType,
    Annotation,
};

pub struct InsertCasts;

impl InsertCasts {
    pub fn cast_module(&mut self, module: ast::Module<HalideType>) -> ast::Module<HalideType> {
        let ast::Module {
            params,
            funcs,
            data,
        } = module;

        ast::Module {
            params,
            funcs: funcs.into_iter().map(|f| self.cast_func(f)).collect(),
            data,
        }
    }

    pub fn cast_func(&mut self, func: ast::Func<HalideType>) -> ast::Func<HalideType> {
        let ast::Func {
            metadata,
            name,
            args,
            stmts,
            data,
        } = func;

        ast::Func {
            metadata,
            name,
            args,
            stmts: self.cast_block(stmts),
            data,
        }
    }

    pub fn cast_block(&mut self, block: ast::Block<HalideType>) -> ast::Block<HalideType> {
        block.into_iter().map(|s| self.cast_stmt(s)).collect()
    }

    pub fn cast_stmt(&mut self, stmt: ast::Stmt<HalideType>) -> ast::Stmt<HalideType> {
        match stmt {
            ast::Stmt::Let { var, expr, data } => ast::Stmt::Let {
                var,
                expr: self.cast_expr(expr, true),
                data,
            },
            ast::Stmt::Produce { var, body, data } => ast::Stmt::Produce {
                var,
                body: self.cast_block(body),
                data,
            },
            ast::Stmt::Consume { var, body, data } => ast::Stmt::Consume {
                var,
                body: self.cast_block(body),
                data,
            },
            ast::Stmt::Store {
                access,
                value,
                data,
            } => ast::Stmt::Store {
                access: self.cast_access(access),
                value: self.cast_expr(value, true),
                data,
            },
            ast::Stmt::Allocate {
                name,
                typ,
                extents,
                loc,
                condition,
                data,
            } => ast::Stmt::Allocate {
                name,
                typ,
                extents: extents
                    .into_iter()
                    .map(|e| self.cast_expr(e, false))
                    .collect(),
                loc,
                condition: condition.map(|e| self.cast_expr(e, true)),
                data,
            },
            x @ ast::Stmt::Free { .. } => x,
            ast::Stmt::For {
                var,
                low,
                high,
                device,
                body,
                data,
            } => ast::Stmt::For {
                var,
                low: self.cast_expr(low, true),
                high: self.cast_expr(high, true),
                device,
                body: self.cast_block(body),
                data,
            },
            ast::Stmt::If {
                cond,
                tru,
                fls,
                data,
            } => ast::Stmt::If {
                cond: self.cast_expr(cond, true),
                tru: self.cast_block(tru),
                fls: fls.map(|block| self.cast_block(block)),
                data,
            },
            ast::Stmt::Predicate { cond, stmt, data } => ast::Stmt::Predicate {
                cond: self.cast_expr(cond, true),
                stmt: Box::new(self.cast_stmt(*stmt)),
                data,
            },
            ast::Stmt::Expr(expr, data) => ast::Stmt::Expr(expr, data),
        }
    }

    pub fn cast_access(&mut self, access: ast::Access<HalideType>) -> ast::Access<HalideType> {
        let ast::Access { var, idx, align } = access;

        ast::Access {
            var,
            idx: Box::new(self.cast_expr(*idx, true)),
            align,
        }
    }

    pub fn cast_expr(
        &mut self,
        expr: ast::Expr<HalideType>,
        cast_children: bool,
    ) -> ast::Expr<HalideType> {
        match expr {
            ast::Expr::Unop(op, inner, data) => Self::wrap_cast(
                ast::Expr::Unop(op, Box::new(self.cast_expr(*inner, true)), data),
                cast_children,
            ),
            ast::Expr::ArithBinop(op, lhs, rhs, data) => Self::wrap_cast(
                ast::Expr::ArithBinop(
                    op,
                    Box::new(self.cast_expr(*lhs, true)),
                    Box::new(self.cast_expr(*rhs, true)),
                    data,
                ),
                cast_children,
            ),
            ast::Expr::CompBinop(op, lhs, rhs, data) => Self::wrap_cast(
                ast::Expr::CompBinop(
                    op,
                    Box::new(self.cast_expr(*lhs, true)),
                    Box::new(self.cast_expr(*rhs, true)),
                    data,
                ),
                cast_children,
            ),
            ast::Expr::If(cond, val, data) => Self::wrap_cast(
                ast::Expr::If(
                    Box::new(self.cast_expr(*cond, true)),
                    Box::new(self.cast_expr(*val, true)),
                    data,
                ),
                cast_children,
            ),
            ast::Expr::FunCall(name, args, data) => Self::wrap_cast(
                ast::Expr::FunCall(
                    name,
                    args.into_iter().map(|a| self.cast_expr(a, true)).collect(),
                    data,
                ),
                cast_children,
            ),
            ast::Expr::Reinterpret(typs, exprs, data) => ast::Expr::Reinterpret(
                typs,
                exprs.into_iter().map(|e| self.cast_expr(e, true)).collect(),
                data,
            ),
            ast::Expr::Cast(typ, expr, data) => {
                ast::Expr::Cast(typ, Box::new(self.cast_expr(*expr, false)), data)
            }
            ast::Expr::PtrCast(typs, expr, data) => {
                ast::Expr::PtrCast(typs, Box::new(self.cast_expr(*expr, false)), data)
            }
            ast::Expr::Access(access, data) => ast::Expr::Access(self.cast_access(access), data),
            ast::Expr::LetIn(_, _, _, _) => todo!(),
            x => Self::wrap_cast(x, cast_children),
        }
    }

    fn wrap_cast(expr: ast::Expr<HalideType>, should_cast: bool) -> ast::Expr<HalideType> {
        if should_cast {
            let typ = expr.data().clone();
            ast::Expr::Cast(typ.to_id(), Box::new(expr), typ)
        } else {
            expr
        }
    }
}
