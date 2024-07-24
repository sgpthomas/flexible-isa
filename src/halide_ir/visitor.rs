use std::collections::HashMap;

use super::ast::{
    Access, ArithBinop, Block, CompBinop, DeviceApi, Expr, Func, Id, MemoryType, Module, Number,
    Stmt, Unop,
};

/// Implements a bottom up visitor over the Halide IR.
pub trait Visitor<T, U> {
    // base cases (non-recursive cases)
    fn default_u(&mut self, _data: T) -> U;

    fn module(&mut self, params: HashMap<Id, Id>, funcs: Vec<Func<U>>, data: T) -> Module<U> {
        Module {
            params,
            funcs,
            data: self.default_u(data),
        }
    }

    fn func(&mut self, metadata: Id, name: Id, args: Vec<Id>, stmts: Block<U>, data: T) -> Func<U> {
        Func {
            metadata,
            name,
            args,
            stmts,
            data: self.default_u(data),
        }
    }

    fn let_stmt(&mut self, var: Id, expr: Expr<U>, data: T) -> Stmt<U> {
        Stmt::Let {
            var,
            expr,
            data: self.default_u(data),
        }
    }

    fn produce_stmt(&mut self, var: Id, body: Block<U>, data: T) -> Stmt<U> {
        Stmt::Produce {
            var,
            body,
            data: self.default_u(data),
        }
    }

    fn consume_stmt(&mut self, var: Id, body: Block<U>, data: T) -> Stmt<U> {
        Stmt::Consume {
            var,
            body,
            data: self.default_u(data),
        }
    }

    fn store_stmt(&mut self, access: Access<U>, value: Expr<U>, data: T) -> Stmt<U> {
        Stmt::Store {
            access,
            value,
            data: self.default_u(data),
        }
    }

    fn allocate_stmt(
        &mut self,
        name: Id,
        typ: Id,
        extents: Vec<Expr<U>>,
        loc: MemoryType,
        condition: Option<Expr<U>>,
        data: T,
    ) -> Stmt<U> {
        Stmt::Allocate {
            name,
            typ,
            extents,
            loc,
            condition,
            data: self.default_u(data),
        }
    }

    fn free_stmt(&mut self, var: Id, data: T) -> Stmt<U> {
        Stmt::Free {
            var,
            data: self.default_u(data),
        }
    }

    fn for_stmt(
        &mut self,
        var: Id,
        low: Expr<U>,
        high: Expr<U>,
        device: DeviceApi,
        body: Block<U>,
        data: T,
    ) -> Stmt<U> {
        Stmt::For {
            var,
            low,
            high,
            device,
            body,
            data: self.default_u(data),
        }
    }

    fn if_stmt(&mut self, cond: Expr<U>, tru: Block<U>, fls: Option<Block<U>>, data: T) -> Stmt<U> {
        Stmt::If {
            cond,
            tru,
            fls,
            data: self.default_u(data),
        }
    }

    fn predicate_stmt(&mut self, cond: Expr<U>, stmt: Stmt<U>, data: T) -> Stmt<U> {
        Stmt::Predicate {
            cond,
            stmt: Box::new(stmt),
            data: self.default_u(data),
        }
    }

    fn expr_stmt(&mut self, expr: Expr<U>, data: T) -> Stmt<U> {
        Stmt::Expr(expr, self.default_u(data))
    }

    fn number_expr(&mut self, number: Number, data: T) -> Expr<U> {
        Expr::Number(number, self.default_u(data))
    }

    fn ident_expr(&mut self, id: Id, data: T) -> Expr<U> {
        Expr::Ident(id, self.default_u(data))
    }

    fn unop_expr(&mut self, op: Unop, inner: Expr<U>, data: T) -> Expr<U> {
        Expr::Unop(op, Box::new(inner), self.default_u(data))
    }

    fn arith_binop_expr(&mut self, op: ArithBinop, lhs: Expr<U>, rhs: Expr<U>, data: T) -> Expr<U> {
        Expr::ArithBinop(op, Box::new(lhs), Box::new(rhs), self.default_u(data))
    }

    fn comp_binop_expr(&mut self, op: CompBinop, lhs: Expr<U>, rhs: Expr<U>, data: T) -> Expr<U> {
        Expr::CompBinop(op, Box::new(lhs), Box::new(rhs), self.default_u(data))
    }

    fn if_expr(&mut self, cond: Expr<U>, expr: Expr<U>, data: T) -> Expr<U> {
        Expr::If(Box::new(cond), Box::new(expr), self.default_u(data))
    }

    fn funcall_expr(&mut self, id: Id, args: Vec<Expr<U>>, data: T) -> Expr<U> {
        Expr::FunCall(id, args, self.default_u(data))
    }

    fn reinterpret_expr(&mut self, typs: Vec<Id>, args: Vec<Expr<U>>, data: T) -> Expr<U> {
        Expr::Reinterpret(typs, args, self.default_u(data))
    }

    fn cast_expr(&mut self, typ: Id, expr: Expr<U>, data: T) -> Expr<U> {
        Expr::Cast(typ, Box::new(expr), self.default_u(data))
    }

    fn ptr_cast_expr(&mut self, typs: Vec<Id>, expr: Expr<U>, data: T) -> Expr<U> {
        Expr::PtrCast(typs, Box::new(expr), self.default_u(data))
    }

    fn access_expr(&mut self, access: Access<U>, data: T) -> Expr<U> {
        Expr::Access(access, self.default_u(data))
    }

    fn letin_expr(&mut self, var: Id, binding: Expr<U>, body: Expr<U>, data: T) -> Expr<U> {
        Expr::LetIn(var, Box::new(binding), Box::new(body), self.default_u(data))
    }
}

pub trait Visitable<T, U> {
    type Res;
    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res;
}

impl<T, U> Visitable<T, U> for Module<T> {
    type Res = Module<U>;
    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        let Module {
            params,
            funcs,
            data,
        } = self;

        let funcs = funcs.into_iter().map(|f| f.visit(visitor)).collect();
        visitor.module(params, funcs, data)
    }
}

impl<T, U> Visitable<T, U> for Func<T> {
    type Res = Func<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        let Func {
            metadata,
            name,
            args,
            stmts,
            data,
        } = self;

        let stmts = stmts.visit(visitor);

        visitor.func(metadata, name, args, stmts, data)
    }
}

impl<T, U> Visitable<T, U> for Stmt<T> {
    type Res = Stmt<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        match self {
            Stmt::Let { var, expr, data } => {
                let expr = expr.visit(visitor);
                visitor.let_stmt(var, expr, data)
            }
            Stmt::Produce { var, body, data } => {
                let body = body.visit(visitor);
                visitor.produce_stmt(var, body, data)
            }
            Stmt::Consume { var, body, data } => {
                let body = body.visit(visitor);
                visitor.consume_stmt(var, body, data)
            }
            Stmt::Store {
                access,
                value,
                data,
            } => {
                let access = access.visit(visitor);
                let value = value.visit(visitor);
                visitor.store_stmt(access, value, data)
            }
            Stmt::Allocate {
                name,
                typ,
                extents,
                loc,
                condition,
                data,
            } => {
                let extents = extents.visit(visitor);
                let condition = condition.visit(visitor);
                visitor.allocate_stmt(name, typ, extents, loc, condition, data)
            }
            Stmt::Free { var, data } => visitor.free_stmt(var, data),
            Stmt::For {
                var,
                low,
                high,
                device,
                body,
                data,
            } => {
                let low = low.visit(visitor);
                let high = high.visit(visitor);
                let body = body.visit(visitor);
                visitor.for_stmt(var, low, high, device, body, data)
            }
            Stmt::If {
                cond,
                tru,
                fls,
                data,
            } => {
                let cond = cond.visit(visitor);
                let tru = tru.visit(visitor);
                let fls = fls.visit(visitor);
                visitor.if_stmt(cond, tru, fls, data)
            }
            Stmt::Predicate { cond, stmt, data } => {
                let cond = cond.visit(visitor);
                let stmt = stmt.visit(visitor);
                visitor.predicate_stmt(cond, stmt, data)
            }
            Stmt::Expr(expr, data) => {
                let expr = expr.visit(visitor);
                visitor.expr_stmt(expr, data)
            }
        }
    }
}

impl<T, U> Visitable<T, U> for Access<T> {
    type Res = Access<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        let Access { var, idx, align } = self;

        Access {
            var,
            idx: Box::new(idx.visit(visitor)),
            align,
        }
    }
}

impl<T, U> Visitable<T, U> for Expr<T> {
    type Res = Expr<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        match self {
            Expr::Number(number, data) => visitor.number_expr(number, data),
            Expr::Ident(id, data) => visitor.ident_expr(id, data),
            Expr::Unop(op, inner, data) => {
                let inner = inner.visit(visitor);
                visitor.unop_expr(op, inner, data)
            }
            Expr::ArithBinop(op, lhs, rhs, data) => {
                let lhs = lhs.visit(visitor);
                let rhs = rhs.visit(visitor);
                visitor.arith_binop_expr(op, lhs, rhs, data)
            }
            Expr::CompBinop(op, lhs, rhs, data) => {
                let lhs = lhs.visit(visitor);
                let rhs = rhs.visit(visitor);
                visitor.comp_binop_expr(op, lhs, rhs, data)
            }
            Expr::If(cond, expr, data) => {
                let cond = cond.visit(visitor);
                let expr = expr.visit(visitor);
                visitor.if_expr(cond, expr, data)
            }
            Expr::FunCall(name, args, data) => {
                let args = args.visit(visitor);
                visitor.funcall_expr(name, args, data)
            }
            Expr::Reinterpret(typs, args, data) => {
                let args = args.visit(visitor);
                visitor.reinterpret_expr(typs, args, data)
            }
            Expr::Cast(typ, expr, data) => {
                let expr = expr.visit(visitor);
                visitor.cast_expr(typ, expr, data)
            }
            Expr::PtrCast(typs, expr, data) => {
                let expr = expr.visit(visitor);
                visitor.ptr_cast_expr(typs, expr, data)
            }
            Expr::Access(access, data) => {
                let access = access.visit(visitor);
                visitor.access_expr(access, data)
            }
            Expr::LetIn(var, binding, body, data) => {
                let binding = binding.visit(visitor);
                let body = body.visit(visitor);
                visitor.letin_expr(var, binding, body, data)
            }
        }
    }
}

impl<X, T, U> Visitable<T, U> for Option<X>
where
    X: Visitable<T, U>,
{
    type Res = Option<X::Res>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        self.map(|e| e.visit(visitor))
    }
}

impl<X, T, U> Visitable<T, U> for Vec<X>
where
    X: Visitable<T, U>,
{
    type Res = Vec<X::Res>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        self.into_iter().map(|x| x.visit(visitor)).collect()
    }
}
