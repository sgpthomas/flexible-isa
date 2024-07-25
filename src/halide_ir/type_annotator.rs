use std::collections::HashMap;

use super::{ast, Annotation, Visitor};

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
        let mut update = false;
        let union = match (&*self, &*other) {
            (&Unknown, _) | (_, &Unknown) => Unknown,
            (&AnyNumber, x @ &Unsigned(_)) | (x @ &Unsigned(_), &AnyNumber) => {
                update = true;
                x.clone()
            }
            (&AnyNumber, x @ &Signed(_)) | (x @ &Signed(_), &AnyNumber) => {
                update = true;
                x.clone()
            }
            (x, y) if x == y => x.clone(),
            _ => Unknown,
        };

        if update {
            *self = union.clone();
            *other = union.clone();
        }

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

impl<T> Visitor<T, HalideType> for TypeAnnotator {
    fn default_u(&mut self, _data: T) -> HalideType {
        HalideType::Unknown
    }

    fn let_stmt(
        &mut self,
        var: ast::Id,
        expr: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Stmt<HalideType> {
        let typ = expr.data().clone();
        self.context.insert(var.clone(), typ.clone());
        ast::Stmt::Let {
            var,
            expr,
            data: typ,
        }
    }

    fn allocate_stmt(
        &mut self,
        name: ast::Id,
        typ: ast::Id,
        extents: Vec<ast::Expr<HalideType>>,
        loc: ast::MemoryType,
        condition: Option<ast::Expr<HalideType>>,
        _data: T,
    ) -> ast::Stmt<HalideType> {
        let data = HalideType::from_id(&typ);
        self.context.insert(name.clone(), data.clone());
        ast::Stmt::Allocate {
            name,
            typ,
            extents,
            loc,
            condition,
            data,
        }
    }

    fn start_for_stmt(
        &mut self,
        var: &ast::Id,
        low: &mut ast::Expr<HalideType>,
        high: &mut ast::Expr<HalideType>,
        _device: &ast::DeviceApi,
        _data: &T,
    ) {
        let typ = low.data_mut().union(high.data_mut());
        self.context.insert(var.clone(), typ);
    }

    fn make_number_expr(&mut self, number: ast::Number, _data: T) -> ast::Expr<HalideType> {
        ast::Expr::Number(number, HalideType::AnyNumber)
    }

    fn make_ident_expr(&mut self, id: ast::Id, _data: T) -> ast::Expr<HalideType> {
        let typ = self
            .context
            .get(&id)
            .cloned()
            .unwrap_or(HalideType::Unknown);
        ast::Expr::Ident(id, typ)
    }

    fn make_unop_expr(
        &mut self,
        op: ast::Unop,
        inner: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = inner.data().clone();
        ast::Expr::Unop(op, Box::new(inner), typ)
    }

    fn make_arith_binop_expr(
        &mut self,
        op: ast::ArithBinop,
        mut lhs: ast::Expr<HalideType>,
        mut rhs: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = lhs.data_mut().union(rhs.data_mut());
        ast::Expr::ArithBinop(op, Box::new(lhs), Box::new(rhs), typ)
    }

    fn make_comp_binop_expr(
        &mut self,
        op: ast::CompBinop,
        mut lhs: ast::Expr<HalideType>,
        mut rhs: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        lhs.data_mut().union(rhs.data_mut());
        ast::Expr::CompBinop(op, Box::new(lhs), Box::new(rhs), HalideType::Bool)
    }

    fn make_funcall_expr(
        &mut self,
        id: ast::Id,
        args: Vec<ast::Expr<HalideType>>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = halide_intrinsic_type(&id, &args);
        ast::Expr::FunCall(id, args, typ)
    }

    fn make_cast_expr(
        &mut self,
        typ_id: ast::Id,
        mut expr: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = HalideType::from_id(&typ_id);
        *expr.data_mut() = typ.clone();
        expr
        // ast::Expr::Cast(typ_id, Box::new(expr), typ)
    }

    fn make_ptrcast_expr(
        &mut self,
        typs: Vec<ast::Id>,
        mut expr: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = HalideType::Ptr(typs);
        *expr.data_mut() = typ;
        expr
    }

    fn make_access_expr(
        &mut self,
        access: ast::Access<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
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
