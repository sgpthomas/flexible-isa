use std::collections::HashMap;

use super::ast::{self, Annotation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HalideType {
    Unknown,
    AnyNumber,
    Unsigned(usize),
    Signed(usize),
    Bool,
    Vec(usize, Box<HalideType>),
    Ptr(Vec<ast::Id>),
}

impl HalideType {
    fn union(&self, other: &Self) -> Self {
        use HalideType::*;
        match (self, other) {
            (&Unknown, _) | (_, &Unknown) => Unknown,
            (&AnyNumber, x @ &Unsigned(_)) | (x @ &Unsigned(_), &AnyNumber) => x.clone(),
            (x, y) if x == y => x.clone(),
            _ => Unknown,
        }
    }

    fn from_id(id: &ast::Id) -> Self {
        use HalideType::*;
        match id.name.as_str() {
            "uint8" => Unsigned(8),
            "uint16" => Unsigned(16),
            "int8" => Signed(8),
            "int16" => Signed(16),
            _ => Unknown,
        }
    }
}

#[derive(Default)]
pub struct TypeAnnotator {
    context: HashMap<ast::Id, HalideType>,
}

impl TypeAnnotator {
    pub fn check_module<T>(&mut self, module: ast::Module<T>) -> ast::Module<HalideType> {
        let ast::Module {
            params,
            funcs,
            data: _,
        } = module;

        ast::Module {
            params,
            funcs: funcs.into_iter().map(|f| self.check_func(f)).collect(),
            data: HalideType::Unknown,
        }
    }

    pub fn check_func<T>(&mut self, func: ast::Func<T>) -> ast::Func<HalideType> {
        let ast::Func {
            metadata,
            name,
            args,
            stmts,
            data: _,
        } = func;

        ast::Func {
            metadata,
            name,
            args,
            stmts: self.check_block(stmts),
            data: HalideType::Unknown,
        }
    }

    pub fn check_block<T>(&mut self, block: ast::Block<T>) -> ast::Block<HalideType> {
        block.into_iter().map(|s| self.check_stmt(s)).collect()
    }

    pub fn check_stmt<T>(&mut self, stmt: ast::Stmt<T>) -> ast::Stmt<HalideType> {
        match stmt {
            ast::Stmt::Let { var, expr, .. } => {
                let expr = self.check_expr(expr);
                let typ = expr.data().clone();
                self.context.insert(var.clone(), typ.clone());
                ast::Stmt::Let {
                    var,
                    expr,
                    data: typ,
                }
            }
            ast::Stmt::Produce { var, body, data: _ } => ast::Stmt::Produce {
                var,
                body: self.check_block(body),
                data: HalideType::Unknown,
            },
            ast::Stmt::Consume { var, body, data: _ } => ast::Stmt::Consume {
                var,
                body: self.check_block(body),
                data: HalideType::Unknown,
            },
            ast::Stmt::Store {
                access: _,
                value: _,
                data: _,
            } => todo!(),
            ast::Stmt::Allocate {
                access: _,
                loc: _,
                condition: _,
                data: _,
            } => todo!(),
            ast::Stmt::Free { var: _, data: _ } => todo!(),
            ast::Stmt::For {
                var: _,
                low: _,
                high: _,
                device: _,
                body: _,
                data: _,
            } => todo!(),
            ast::Stmt::If {
                cond: _,
                tru: _,
                fls: _,
                data: _,
            } => todo!(),
            ast::Stmt::Predicate {
                cond: _,
                stmt: _,
                data: _,
            } => todo!(),
            ast::Stmt::Expr(_, _) => todo!(),
        }
    }

    /// Calculate types for `expr`
    pub fn check_expr<T>(&mut self, expr: ast::Expr<T>) -> ast::Expr<HalideType> {
        match expr {
            ast::Expr::Number(n, _) => ast::Expr::Number(n, HalideType::AnyNumber),
            ast::Expr::Ident(id, _) => {
                let typ = self
                    .context
                    .get(&id)
                    .cloned()
                    .unwrap_or(HalideType::Unknown);
                ast::Expr::Ident(id, typ)
            }
            ast::Expr::Unop(op, inner, _) => {
                let expr = self.check_expr(*inner);
                let typ = expr.data().clone();
                ast::Expr::Unop(op, Box::new(expr), typ)
            }
            ast::Expr::ArithBinop(op, lhs, rhs, _) => {
                let lhs = self.check_expr(*lhs);
                let rhs = self.check_expr(*rhs);
                let typ = lhs.data().union(rhs.data());
                ast::Expr::ArithBinop(op, Box::new(lhs), Box::new(rhs), typ)
            }
            ast::Expr::CompBinop(op, lhs, rhs, _) => {
                let lhs = self.check_expr(*lhs);
                let rhs = self.check_expr(*rhs);
                ast::Expr::CompBinop(op, Box::new(lhs), Box::new(rhs), HalideType::Bool)
            }
            ast::Expr::If(_, _, _) => todo!(),
            ast::Expr::FunCall(id, args, _) => {
                let typ = halide_intrinsic_type(&id);
                ast::Expr::FunCall(
                    id,
                    args.into_iter().map(|e| self.check_expr(e)).collect(),
                    typ,
                )
            }
            ast::Expr::Reinterpret(_, _, _) => todo!(),
            ast::Expr::Cast(typ_id, expr, _) => {
                let typ = HalideType::from_id(&typ_id);
                ast::Expr::Cast(typ_id, Box::new(self.check_expr(*expr)), typ)
            }
            ast::Expr::PtrCast(typs, expr, _) => {
                let typ = HalideType::Ptr(typs.clone());
                ast::Expr::PtrCast(typs, Box::new(self.check_expr(*expr)), typ)
            }
            ast::Expr::Access(_, _) => todo!(),
            ast::Expr::LetIn(_, _, _, _) => todo!(),
        }
    }
}

fn halide_intrinsic_type(id: &ast::Id) -> HalideType {
    match id.name.as_str() {
        "_halide_buffer_get_host" => HalideType::Ptr(vec![ast::Id::new("void")]),
        "_halide_buffer_get_min" => HalideType::Unsigned(32),
        "_halide_buffer_get_max" => HalideType::Unsigned(32),
        "_halide_buffer_get_extent" => HalideType::Unsigned(32),
        _ => HalideType::Unknown,
    }
}
