use std::collections::HashMap;

use super::ast::{
    Access, ArithBinop, Block, CompBinop, DeviceApi, Expr, Func, Id, MemoryType, Module, Number,
    Stmt, Unop,
};

/// Implements a visitor over the Halide IR.
pub trait Visitor<T, U> {
    /// The default value of `U` to be used when a method is not overridden
    /// in the trait definition.
    fn default_u(&mut self, _data: T) -> U;

    fn do_pass(&mut self, ast: Module<T>) -> Module<U>
    where
        Self: Sized,
    {
        ast.visit(self)
    }

    fn start_module(&mut self, _module: &Module<T>) {}

    fn make_module(&mut self, params: HashMap<Id, Id>, funcs: Vec<Func<U>>, data: T) -> Module<U> {
        Module {
            params,
            funcs,
            data: self.default_u(data),
        }
    }

    fn start_func(
        &mut self,
        _metadata: &Id,
        _name: &Id,
        _args: &[Id],
        _stmts: &Block<T>,
        _data: &T,
    ) {
    }

    fn make_func(
        &mut self,
        metadata: Id,
        name: Id,
        args: Vec<Id>,
        stmts: Block<U>,
        data: T,
    ) -> Func<U> {
        Func {
            metadata,
            name,
            args,
            stmts,
            data: self.default_u(data),
        }
    }

    fn stmt(&mut self, stmt: Stmt<U>) -> Stmt<U> {
        stmt
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

    fn start_for_stmt(
        &mut self,
        _var: &Id,
        _low: &mut Expr<U>,
        _high: &mut Expr<U>,
        _device: &DeviceApi,
        _data: &T,
    ) {
    }

    fn make_for_stmt(
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

    fn start_expr(&mut self, _expr: &Expr<T>) {}

    fn make_expr(&mut self, expr: Expr<U>) -> Expr<U> {
        expr
    }

    fn make_number_expr(&mut self, number: Number, data: T) -> Expr<U> {
        Expr::Number(number, self.default_u(data))
    }

    fn make_ident_expr(&mut self, id: Id, data: T) -> Expr<U> {
        Expr::Ident(id, self.default_u(data))
    }

    fn start_unop_expr(&mut self, _op: &Unop, _inner: &Expr<T>, _data: &T) {}

    fn make_unop_expr(&mut self, op: Unop, inner: Expr<U>, data: T) -> Expr<U> {
        Expr::Unop(op, Box::new(inner), self.default_u(data))
    }

    fn start_arith_binop_expr(
        &mut self,
        _op: &ArithBinop,
        _lhs: &Expr<T>,
        _rhs: &Expr<T>,
        _data: &T,
    ) {
    }

    fn make_arith_binop_expr(
        &mut self,
        op: ArithBinop,
        lhs: Expr<U>,
        rhs: Expr<U>,
        data: T,
    ) -> Expr<U> {
        Expr::ArithBinop(op, Box::new(lhs), Box::new(rhs), self.default_u(data))
    }

    fn start_comp_binop_expr(
        &mut self,
        _op: &CompBinop,
        _lhs: &Expr<T>,
        _rhs: &Expr<T>,
        _data: &T,
    ) {
    }

    fn make_comp_binop_expr(
        &mut self,
        op: CompBinop,
        lhs: Expr<U>,
        rhs: Expr<U>,
        data: T,
    ) -> Expr<U> {
        Expr::CompBinop(op, Box::new(lhs), Box::new(rhs), self.default_u(data))
    }

    fn start_if_expr(&mut self, _cond: &Expr<T>, _expr: &Expr<T>, _data: &T) {}

    fn make_if_expr(&mut self, expr: Expr<U>, cond: Expr<U>, data: T) -> Expr<U> {
        Expr::If(Box::new(expr), Box::new(cond), self.default_u(data))
    }

    fn make_struct_member_expr(&mut self, struct_id: Id, thing: Id, data: T) -> Expr<U> {
        Expr::StructMember(struct_id, thing, self.default_u(data))
    }

    fn start_funcall_expr(&mut self, _id: &Id, _args: &[Expr<T>], _data: &T) {}

    fn make_funcall_expr(&mut self, id: Id, args: Vec<Expr<U>>, data: T) -> Expr<U> {
        Expr::FunCall(id, args, self.default_u(data))
    }

    fn start_reinterpret_expr(&mut self, _typs: &[Id], _args: &[Expr<T>], _data: &T) {}

    fn make_reinterpret_expr(&mut self, typs: Vec<Id>, args: Vec<Expr<U>>, data: T) -> Expr<U> {
        Expr::Reinterpret(typs, args, self.default_u(data))
    }

    fn start_cast_expr(&mut self, _typ: &Id, _expr: &Expr<T>, _data: &T) {}

    fn make_cast_expr(&mut self, typ: Id, expr: Expr<U>, data: T) -> Expr<U> {
        Expr::Cast(typ, Box::new(expr), self.default_u(data))
    }

    fn start_ptrcast_expr(&mut self, _typs: &[Id], _expr: &Expr<T>, _data: &T) {}

    fn make_ptrcast_expr(&mut self, typs: Vec<Id>, expr: Expr<U>, data: T) -> Expr<U> {
        Expr::PtrCast(typs, Box::new(expr), self.default_u(data))
    }

    fn start_access_expr(&mut self, _access: &Access<T>, _data: &T) {}

    fn make_access_expr(&mut self, access: Access<U>, data: T) -> Expr<U> {
        Expr::Access(access, self.default_u(data))
    }

    fn start_letin_expr(&mut self, _var: &Id, _binding: &Expr<T>, _body: &Expr<T>, _data: &T) {}

    fn make_letin_expr(&mut self, var: Id, binding: Expr<U>, body: Expr<U>, data: T) -> Expr<U> {
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
        visitor.start_module(&self);

        let Module {
            params,
            funcs,
            data,
        } = self;

        let funcs = funcs.into_iter().map(|f| f.visit(visitor)).collect();
        visitor.make_module(params, funcs, data)
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

        visitor.start_func(&metadata, &name, &args, &stmts, &data);
        let stmts = stmts.visit(visitor);
        visitor.make_func(metadata, name, args, stmts, data)
    }
}

impl<T, U> Visitable<T, U> for Stmt<T> {
    type Res = Stmt<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, U>) -> Self::Res {
        let stmt = match self {
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
                let mut low = low.visit(visitor);
                let mut high = high.visit(visitor);
                visitor.start_for_stmt(&var, &mut low, &mut high, &device, &data);
                let body = body.visit(visitor);
                visitor.make_for_stmt(var, low, high, device, body, data)
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
        };
        visitor.stmt(stmt)
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
        visitor.start_expr(&self);
        let expr = match self {
            Expr::Number(number, data) => visitor.make_number_expr(number, data),
            Expr::Ident(id, data) => visitor.make_ident_expr(id, data),
            Expr::Unop(op, inner, data) => {
                visitor.start_unop_expr(&op, &*inner, &data);
                let inner = inner.visit(visitor);
                visitor.make_unop_expr(op, inner, data)
            }
            Expr::ArithBinop(op, lhs, rhs, data) => {
                visitor.start_arith_binop_expr(&op, &lhs, &rhs, &data);
                let lhs = lhs.visit(visitor);
                let rhs = rhs.visit(visitor);
                visitor.make_arith_binop_expr(op, lhs, rhs, data)
            }
            Expr::CompBinop(op, lhs, rhs, data) => {
                visitor.start_comp_binop_expr(&op, &lhs, &rhs, &data);
                let lhs = lhs.visit(visitor);
                let rhs = rhs.visit(visitor);
                visitor.make_comp_binop_expr(op, lhs, rhs, data)
            }
            Expr::If(expr, cond, data) => {
                visitor.start_if_expr(&expr, &cond, &data);
                let expr = expr.visit(visitor);
                let cond = cond.visit(visitor);
                visitor.make_if_expr(expr, cond, data)
            }
            Expr::StructMember(struct_id, thing, data) => {
                visitor.make_struct_member_expr(struct_id, thing, data)
            }
            Expr::FunCall(name, args, data) => {
                visitor.start_funcall_expr(&name, &args, &data);
                let args = args.visit(visitor);
                visitor.make_funcall_expr(name, args, data)
            }
            Expr::Reinterpret(typs, args, data) => {
                visitor.start_reinterpret_expr(&typs, &args, &data);
                let args = args.visit(visitor);
                visitor.make_reinterpret_expr(typs, args, data)
            }
            Expr::Cast(typ, expr, data) => {
                visitor.start_cast_expr(&typ, &expr, &data);
                let expr = expr.visit(visitor);
                visitor.make_cast_expr(typ, expr, data)
            }
            Expr::PtrCast(typs, expr, data) => {
                visitor.start_ptrcast_expr(&typs, &expr, &data);
                let expr = expr.visit(visitor);
                visitor.make_ptrcast_expr(typs, expr, data)
            }
            Expr::Access(access, data) => {
                visitor.start_access_expr(&access, &data);
                let access = access.visit(visitor);
                visitor.make_access_expr(access, data)
            }
            Expr::LetIn(var, binding, body, data) => {
                visitor.start_letin_expr(&var, &binding, &body, &data);
                let binding = binding.visit(visitor);
                let body = body.visit(visitor);
                visitor.make_letin_expr(var, binding, body, data)
            }
        };
        visitor.make_expr(expr)
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
