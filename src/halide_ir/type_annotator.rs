use std::collections::HashMap;

use super::{ast, Annotation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HalideType {
    Unknown,
    AnyNumber,
    Unsigned(u64),
    Signed(u64),
    Bool,
    Vec(u64, Box<HalideType>),
    Ptr(Vec<ast::Id>),
}

impl HalideType {
    fn union(&mut self, other: &mut Self) -> Self {
        use HalideType::*;
        let union = match (&*self, &*other) {
            (&Unknown, _) | (_, &Unknown) => Unknown,
            (&AnyNumber, x @ &Unsigned(_)) | (x @ &Unsigned(_), &AnyNumber) => x.clone(),
            (x, y) if x == y => x.clone(),
            _ => Unknown,
        };

        *self = union.clone();
        *other = union.clone();
        union
    }

    pub fn from_id(id: &ast::Id) -> Self {
        use HalideType::*;
        match id.name.as_str() {
            "uint8" => Unsigned(8),
            "uint16" => Unsigned(16),
            "uint32" => Unsigned(32),
            "uint64" => Unsigned(64),
            "uint128" => Unsigned(128),
            "int8" => Signed(8),
            "int16" => Signed(16),
            "int32" => Signed(32),
            "int64" => Signed(64),
            "int128" => Signed(128),
            _ => Unknown,
        }
    }

    pub fn to_id(&self) -> ast::Id {
        use HalideType::*;
        match self {
            Unknown => ast::Id::new("unknown"),
            AnyNumber => ast::Id::new("number"),
            Unsigned(n) => ast::Id::new(format!("uint{n}")),
            Signed(n) => ast::Id::new(format!("int{n}")),
            Bool => ast::Id::new("bool"),
            Vec(n, typ) => ast::Id::new(format!("{}x{n}", typ.to_id().name)),
            Ptr(typs) => ast::Id::new(format!(
                "{} *",
                typs.iter()
                    .map(|id| id.name.to_string())
                    .collect::<std::vec::Vec<_>>()
                    .join(" ")
            )),
        }
    }
}

#[derive(Default)]
pub struct TypeAnnotator {
    context: HashMap<ast::Id, HalideType>,
}

// impl<T> Visitor<T, HalideType> for TypeAnnotator {}

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
                access,
                value,
                data: _,
            } => ast::Stmt::Store {
                access: self.check_access(access),
                value: self.check_expr(value),
                data: HalideType::Unknown,
            },
            ast::Stmt::Allocate {
                name,
                typ,
                extents,
                loc,
                condition,
                data: _,
            } => {
                let data = HalideType::from_id(&typ);
                self.context.insert(name.clone(), data.clone());
                ast::Stmt::Allocate {
                    name,
                    typ: typ.clone(),
                    extents: self.check_exprs(extents),
                    loc,
                    condition: condition.map(|expr| self.check_expr(expr)),
                    data,
                }
            }
            ast::Stmt::Free { var, data: _ } => ast::Stmt::Free {
                var,
                data: HalideType::Unknown,
            },
            ast::Stmt::For {
                var,
                low,
                high,
                device,
                body,
                data: _,
            } => {
                let mut low = self.check_expr(low);
                let mut high = self.check_expr(high);
                let typ = low.data_mut().union(high.data_mut());
                self.context.insert(var.clone(), typ);
                ast::Stmt::For {
                    var,
                    low,
                    high,
                    device,
                    body: self.check_block(body),
                    data: HalideType::Unknown,
                }
            }
            ast::Stmt::If {
                cond,
                tru,
                fls,
                data: _,
            } => ast::Stmt::If {
                cond: self.check_expr(cond),
                tru: self.check_block(tru),
                fls: fls.map(|block| self.check_block(block)),
                data: HalideType::Unknown,
            },
            ast::Stmt::Predicate {
                cond,
                stmt,
                data: _,
            } => ast::Stmt::Predicate {
                cond: self.check_expr(cond),
                stmt: Box::new(self.check_stmt(*stmt)),
                data: HalideType::Unknown,
            },
            ast::Stmt::Expr(expr, _) => ast::Stmt::Expr(self.check_expr(expr), HalideType::Unknown),
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
                let mut lhs = self.check_expr(*lhs);
                let mut rhs = self.check_expr(*rhs);
                let typ = lhs.data_mut().union(rhs.data_mut());
                ast::Expr::ArithBinop(op, Box::new(lhs), Box::new(rhs), typ)
            }
            ast::Expr::CompBinop(op, lhs, rhs, _) => {
                let mut lhs = self.check_expr(*lhs);
                let mut rhs = self.check_expr(*rhs);
                lhs.data_mut().union(rhs.data_mut());
                ast::Expr::CompBinop(op, Box::new(lhs), Box::new(rhs), HalideType::Bool)
            }
            ast::Expr::If(expr, cond, _) => ast::Expr::If(
                Box::new(self.check_expr(*expr)),
                Box::new(self.check_expr(*cond)),
                HalideType::Unknown,
            ),
            ast::Expr::FunCall(id, args, _) => {
                let args = self.check_exprs(args);
                let typ = halide_intrinsic_type(&id, &args);
                ast::Expr::FunCall(id, args, typ)
            }
            ast::Expr::Reinterpret(typs, args, _) => {
                ast::Expr::Reinterpret(typs, self.check_exprs(args), HalideType::Unknown)
            }
            ast::Expr::Cast(typ_id, expr, _) => {
                let typ = HalideType::from_id(&typ_id);
                ast::Expr::Cast(typ_id, Box::new(self.check_expr(*expr)), typ)
            }
            ast::Expr::PtrCast(typs, expr, _) => {
                let typ = HalideType::Ptr(typs.clone());
                ast::Expr::PtrCast(typs, Box::new(self.check_expr(*expr)), typ)
            }
            ast::Expr::Access(access, _) => {
                let access = self.check_access(access);
                // when the var is a pointer, the type of the access is the pointer type
                let typ = if let HalideType::Ptr(typs) = &self.context[&access.var] {
                    match typs.as_slice() {
                        [t] if t.name == "void" => HalideType::Unknown,
                        [t] => HalideType::from_id(t),
                        _ => HalideType::Unknown,
                    }
                } else {
                    // otherwise we just say we don't know what the type of this is
                    HalideType::Unknown
                };
                ast::Expr::Access(access, typ)
            }
            ast::Expr::LetIn(id, binding, body, _) => {
                let binding = self.check_expr(*binding);
                self.context.insert(id.clone(), binding.data().clone());
                let body = self.check_expr(*body);
                let body_typ = body.data().clone();
                ast::Expr::LetIn(id, Box::new(binding), Box::new(body), body_typ)
            }
        }
    }

    pub fn check_access<T>(&mut self, access: ast::Access<T>) -> ast::Access<HalideType> {
        let ast::Access { var, idx, align } = access;
        ast::Access {
            var,
            idx: Box::new(self.check_expr(*idx)),
            align,
        }
    }

    pub fn check_exprs<T>(&mut self, exprs: Vec<ast::Expr<T>>) -> Vec<ast::Expr<HalideType>> {
        exprs.into_iter().map(|e| self.check_expr(e)).collect()
    }
}

fn halide_intrinsic_type(id: &ast::Id, args: &[ast::Expr<HalideType>]) -> HalideType {
    match (id.name.as_str(), args) {
        ("_halide_buffer_get_host", _) => HalideType::Ptr(vec![ast::Id::new("void")]),
        ("_halide_buffer_get_min", _) => HalideType::Unsigned(16),
        ("_halide_buffer_get_max", _) => HalideType::Unsigned(16),
        ("_halide_buffer_get_stride", _) => HalideType::Unsigned(16),
        ("_halide_buffer_get_extent", _) => HalideType::Unsigned(16),
        ("ramp", [base, _stride, lanes]) => {
            if let ast::Expr::Number(n, _) = lanes {
                HalideType::Vec(n.value, Box::new(base.data().clone()))
            } else {
                HalideType::Unknown
            }
        }
        ("int8", _) => HalideType::Signed(8),
        ("int16", _) => HalideType::Signed(16),
        ("int32", _) => HalideType::Signed(32),
        ("int64", _) => HalideType::Signed(64),
        ("int128", _) => HalideType::Signed(128),
        ("x8", [arg]) => HalideType::Vec(8, Box::new(arg.data().clone())),
        ("x16", [arg]) => HalideType::Vec(16, Box::new(arg.data().clone())),
        ("x32", [arg]) => HalideType::Vec(32, Box::new(arg.data().clone())),
        ("x64", [arg]) => HalideType::Vec(64, Box::new(arg.data().clone())),
        ("x128", [arg]) => HalideType::Vec(128, Box::new(arg.data().clone())),
        _ => HalideType::Unknown,
    }
}
