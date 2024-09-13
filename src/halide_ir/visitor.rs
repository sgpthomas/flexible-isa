use std::collections::HashMap;

use itertools::Itertools;

use super::ast::{
    Access, ArithBinop, Block, CompBinop, DeviceApi, Expr, Func, Id, MemoryType, Module, Number,
    Stmt, Unop,
};

/// Implements a visitor over the Halide IR.
pub trait Visitor<T> {
    type Output;

    /// The default value of `U` to be used when a method is not overridden
    /// in the trait definition.
    #[allow(unused_variables)]
    fn default_u(&mut self, data: T) -> Self::Output;

    fn do_pass_default(ast: Module<T>) -> Module<Self::Output>
    where
        Self: Default,
    {
        let mut visitor = Self::default();
        visitor.do_pass(ast)
    }

    fn do_pass(&mut self, ast: Module<T>) -> Module<Self::Output>
    where
        Self: Sized,
    {
        ast.visit(self)
    }

    fn do_pass_expr(&mut self, expr: Expr<T>) -> Expr<Self::Output>
    where
        Self: Sized,
    {
        expr.visit(self)
    }

    #[allow(unused_variables)]
    fn start_module(&mut self, module: &Module<T>) {}

    fn make_module(
        &mut self,
        params: HashMap<Id, Id>,
        funcs: Vec<Func<Self::Output>>,
        data: T,
    ) -> Module<Self::Output> {
        Module {
            params,
            funcs,
            data: self.default_u(data),
        }
    }

    #[allow(unused_variables)]
    fn start_func(&mut self, metadata: &Id, name: &Id, args: &[Id], stmts: &Block<T>, data: &T) {}

    fn make_func(
        &mut self,
        metadata: Id,
        name: Id,
        args: Vec<Id>,
        stmts: Block<Self::Output>,
        data: T,
    ) -> Func<Self::Output> {
        Func {
            metadata,
            name,
            args,
            stmts,
            data: self.default_u(data),
        }
    }

    #[allow(unused_variables)]
    fn start_stmt(&mut self, stmt: &Stmt<T>) {}

    fn make_stmt(&mut self, stmt: Stmt<Self::Output>) -> Vec<Stmt<Self::Output>> {
        vec![stmt]
    }

    #[allow(unused_variables)]
    fn start_let_stmt(&mut self, var: &Id, expr: &Expr<T>, data: &T) {}

    fn make_let_stmt(
        &mut self,
        var: Id,
        expr: Expr<Self::Output>,
        data: T,
    ) -> Vec<Stmt<Self::Output>> {
        vec![Stmt::Let {
            var,
            expr,
            data: self.default_u(data),
        }]
    }

    #[allow(unused_variables)]
    fn start_produce_stmt(&mut self, var: &Id, body: &Block<T>, data: &T) {}

    fn make_produce_stmt(
        &mut self,
        var: Id,
        body: Block<Self::Output>,
        data: T,
    ) -> Stmt<Self::Output> {
        Stmt::Produce {
            var,
            body,
            data: self.default_u(data),
        }
    }

    fn consume_stmt(&mut self, var: Id, body: Block<Self::Output>, data: T) -> Stmt<Self::Output> {
        Stmt::Consume {
            var,
            body,
            data: self.default_u(data),
        }
    }

    #[allow(unused_variables)]
    fn start_store_stmt(&mut self, access: &Access<T>, value: &Expr<T>, data: &T) {}

    fn make_store_stmt(
        &mut self,
        access: Access<Self::Output>,
        value: Expr<Self::Output>,
        data: T,
    ) -> Vec<Stmt<Self::Output>> {
        vec![Stmt::Store {
            access,
            value,
            data: self.default_u(data),
        }]
    }

    fn allocate_stmt(
        &mut self,
        name: Id,
        typ: Id,
        extents: Vec<Expr<Self::Output>>,
        loc: MemoryType,
        condition: Option<Expr<Self::Output>>,
        data: T,
    ) -> Stmt<Self::Output> {
        Stmt::Allocate {
            name,
            typ,
            extents,
            loc,
            condition,
            data: self.default_u(data),
        }
    }

    fn free_stmt(&mut self, var: Id, data: T) -> Stmt<Self::Output> {
        Stmt::Free {
            var,
            data: self.default_u(data),
        }
    }

    #[allow(unused_variables)]
    fn start_for_stmt(
        &mut self,
        var: &Id,
        low: &mut Expr<Self::Output>,
        high: &mut Expr<Self::Output>,
        device: &DeviceApi,
        data: &T,
    ) {
    }

    fn make_for_stmt(
        &mut self,
        var: Id,
        low: Expr<Self::Output>,
        high: Expr<Self::Output>,
        device: DeviceApi,
        body: Block<Self::Output>,
        data: T,
    ) -> Stmt<Self::Output> {
        Stmt::For {
            var,
            low,
            high,
            device,
            body,
            data: self.default_u(data),
        }
    }

    #[allow(unused_variables)]
    fn start_if_stmt(&mut self, cond: &Expr<T>, tru: &Block<T>, fls: &Option<Block<T>>, data: &T) {}

    fn make_if_stmt(
        &mut self,
        cond: Expr<Self::Output>,
        tru: Block<Self::Output>,
        fls: Option<Block<Self::Output>>,
        data: T,
    ) -> Vec<Stmt<Self::Output>> {
        vec![Stmt::If {
            cond,
            tru,
            fls,
            data: self.default_u(data),
        }]
    }

    #[allow(unused_variables)]
    fn start_predicate_stmt(&mut self, cond: &Expr<T>, stmt: &Stmt<T>, data: &T) {}

    fn make_predicate_stmt(
        &mut self,
        cond: Expr<Self::Output>,
        stmt: Stmt<Self::Output>,
        data: T,
    ) -> Stmt<Self::Output> {
        Stmt::Predicate {
            cond,
            stmt: Box::new(stmt),
            data: self.default_u(data),
        }
    }

    fn expr_stmt(&mut self, expr: Expr<Self::Output>, data: T) -> Stmt<Self::Output> {
        Stmt::Expr(expr, self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_expr(&mut self, expr: &Expr<T>) {}

    fn make_expr(&mut self, expr: Expr<Self::Output>) -> Expr<Self::Output> {
        expr
    }

    fn make_number_expr(&mut self, number: Number, data: T) -> Expr<Self::Output> {
        Expr::Number(number, self.default_u(data))
    }

    fn make_ident_expr(&mut self, id: Id, data: T) -> Expr<Self::Output> {
        Expr::Ident(id, self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_unop_expr(&mut self, op: &Unop, inner: &Expr<T>, data: &T) {}

    fn make_unop_expr(
        &mut self,
        op: Unop,
        inner: Expr<Self::Output>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::Unop(op, Box::new(inner), self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_arith_binop_expr(&mut self, op: &ArithBinop, lhs: &Expr<T>, rhs: &Expr<T>, data: &T) {}

    fn make_arith_binop_expr(
        &mut self,
        op: ArithBinop,
        lhs: Expr<Self::Output>,
        rhs: Expr<Self::Output>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::ArithBinop(op, Box::new(lhs), Box::new(rhs), self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_comp_binop_expr(&mut self, op: &CompBinop, lhs: &Expr<T>, rhs: &Expr<T>, data: &T) {}

    fn make_comp_binop_expr(
        &mut self,
        op: CompBinop,
        lhs: Expr<Self::Output>,
        rhs: Expr<Self::Output>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::CompBinop(op, Box::new(lhs), Box::new(rhs), self.default_u(data))
    }

    fn make_struct_member_expr(&mut self, struct_id: Id, thing: Id, data: T) -> Expr<Self::Output> {
        Expr::StructMember(struct_id, thing, self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_funcall_expr(&mut self, id: &Id, args: &[Expr<T>], data: &T) {}

    fn make_funcall_expr(
        &mut self,
        id: Id,
        args: Vec<Expr<Self::Output>>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::FunCall(id, args, self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_reinterpret_expr(&mut self, typs: &[Id], args: &[Expr<T>], data: &T) {}

    fn make_reinterpret_expr(
        &mut self,
        typs: Vec<Id>,
        args: Vec<Expr<Self::Output>>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::Reinterpret(typs, args, self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_cast_expr(&mut self, typ: &Id, expr: &Expr<T>, data: &T) {}

    fn make_cast_expr(&mut self, typ: Id, expr: Expr<Self::Output>, data: T) -> Expr<Self::Output> {
        Expr::Cast(typ, Box::new(expr), self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_ptrcast_expr(&mut self, typs: &[Id], expr: &Expr<T>, data: &T) {}

    fn make_ptrcast_expr(
        &mut self,
        typs: Vec<Id>,
        expr: Expr<Self::Output>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::PtrCast(typs, Box::new(expr), self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_access_expr(&mut self, access: &Access<T>, data: &T) {}

    fn make_access_expr(&mut self, access: Access<Self::Output>, data: T) -> Expr<Self::Output> {
        Expr::Access(access, self.default_u(data))
    }

    #[allow(unused_variables)]
    fn start_letin_expr(&mut self, var: &Id, binding: &Expr<T>, body: &Expr<T>, data: &T) {}

    fn make_letin_expr(
        &mut self,
        var: Id,
        binding: Expr<Self::Output>,
        body: Expr<Self::Output>,
        data: T,
    ) -> Expr<Self::Output> {
        Expr::LetIn(var, Box::new(binding), Box::new(body), self.default_u(data))
    }
}

pub trait Visitable<T, U> {
    type Res;
    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res;
}

impl<T, U> Visitable<T, U> for Module<T> {
    type Res = Module<U>;
    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
        visitor.start_module(&self);

        let Module {
            params,
            funcs,
            data,
        } = self;

        // visit external functions before internal ones
        let funcs = funcs
            .into_iter()
            .sorted_by_key(|f1| f1.metadata.name.contains("external"))
            .rev()
            .map(|f| f.visit(visitor))
            .collect();
        visitor.make_module(params, funcs, data)
    }
}

impl<T, U> Visitable<T, U> for Func<T> {
    type Res = Func<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
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
    type Res = Vec<Stmt<U>>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
        visitor.start_stmt(&self);
        let stmts: Vec<_> = match self {
            Stmt::Let { var, expr, data } => {
                visitor.start_let_stmt(&var, &expr, &data);
                let expr = expr.visit(visitor);
                visitor.make_let_stmt(var, expr, data)
            }
            Stmt::Produce { var, body, data } => {
                visitor.start_produce_stmt(&var, &body, &data);
                let body = body.visit(visitor);
                vec![visitor.make_produce_stmt(var, body, data)]
            }
            Stmt::Consume { var, body, data } => {
                let body = body.visit(visitor);
                vec![visitor.consume_stmt(var, body, data)]
            }
            Stmt::Store {
                access,
                value,
                data,
            } => {
                visitor.start_store_stmt(&access, &value, &data);
                let access = access.visit(visitor);
                let value = value.visit(visitor);
                visitor.make_store_stmt(access, value, data)
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
                vec![visitor.allocate_stmt(name, typ, extents, loc, condition, data)]
            }
            Stmt::Free { var, data } => vec![visitor.free_stmt(var, data)],
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
                vec![visitor.make_for_stmt(var, low, high, device, body, data)]
            }
            Stmt::If {
                cond,
                tru,
                fls,
                data,
            } => {
                visitor.start_if_stmt(&cond, &tru, &fls, &data);
                let cond = cond.visit(visitor);
                let tru = tru.visit(visitor);
                let fls = fls.visit(visitor);
                visitor.make_if_stmt(cond, tru, fls, data)
            }
            Stmt::Predicate { cond, stmt, data } => {
                visitor.start_predicate_stmt(&cond, &*stmt, &data);
                let cond = cond.visit(visitor);
                // stmt.visit can produce a list of new stmts
                // the last stmt will be the "original" stmt that
                // we visited. pop that off and construct the predicate
                // from that. we then push that to the end of the list
                // that we got from `stmt.visit`. if there is nothing
                // to pop, this statement was removed from the visit
                // and so we also return nothing here. we need some statement
                // to predicate
                let mut preconds = stmt.visit(visitor);
                let stmt = preconds.pop();
                if let Some(stmt) = stmt {
                    preconds.push(visitor.make_predicate_stmt(cond, stmt, data));
                    preconds
                } else {
                    vec![]
                }
            }
            Stmt::Expr(expr, data) => {
                let expr = expr.visit(visitor);
                vec![visitor.expr_stmt(expr, data)]
            }
        };
        stmts
            .into_iter()
            .flat_map(|stmt| visitor.make_stmt(stmt))
            .collect()
    }
}

impl<T, U> Visitable<T, U> for Access<T> {
    type Res = Access<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
        let Access {
            var,
            idx,
            align,
            predicate,
        } = self;

        Access {
            var,
            idx: Box::new(idx.visit(visitor)),
            align,
            predicate: predicate.map(|expr| Box::new(expr.visit(visitor))),
        }
    }
}

impl<T, U> Visitable<T, U> for Expr<T> {
    type Res = Expr<U>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
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
            Expr::Instruction { num, args, data } => Expr::Instruction {
                num,
                args: args.visit(visitor),
                data: visitor.default_u(data),
            },
        };
        visitor.make_expr(expr)
    }
}

impl<X, T, U> Visitable<T, U> for Option<X>
where
    X: Visitable<T, U>,
{
    type Res = Option<X::Res>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
        self.map(|e| e.visit(visitor))
    }
}

impl<T, U> Visitable<T, U> for Vec<Stmt<T>> {
    type Res = Vec<Stmt<U>>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
        self.into_iter().flat_map(|x| x.visit(visitor)).collect()
    }
}

impl<T, U> Visitable<T, U> for Vec<Expr<T>> {
    type Res = Vec<Expr<U>>;

    fn visit(self, visitor: &mut dyn Visitor<T, Output = U>) -> Self::Res {
        self.into_iter().map(|x| x.visit(visitor)).collect()
    }
}
