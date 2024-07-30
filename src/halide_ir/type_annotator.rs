use std::collections::HashMap;

use super::{ast, Annotation, HalideType, MatchWidth, Visitor};

#[derive(Default)]
pub struct TypeAnnotator {
    context: HashMap<ast::Id, HalideType>,
}

impl<T> Visitor<T, HalideType> for TypeAnnotator {
    fn default_u(&mut self, _data: T) -> HalideType {
        HalideType::Unknown
    }

    // fn start_func(
    //     &mut self,
    //     _metadata: &ast::Id,
    //     _name: &ast::Id,
    //     args: &[ast::Id],
    //     _stmts: &ast::Block<T>,
    //     _data: &T,
    // ) {
    //     for arg in args {
    //         println!("adding {arg:?} to context");
    //         self.context
    //             .insert(arg.clone(), HalideType::Ptr(vec![ast::Id::new("uint8")]));
    //     }
    // }

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

    /// Annotate the expression with the cast type
    /// and then return the underlying expression.
    /// the cast will be added back in a later pass
    fn make_cast_expr(
        &mut self,
        typ_id: ast::Id,
        mut expr: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = HalideType::from_id(&typ_id);
        *expr.data_mut() = typ.clone();
        expr
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
        // let typ = access.idx.data().clone();
        // ast::Expr::Access(access, typ)
        // when the var is a pointer, the type of the access is the pointer type

        let typ = if let HalideType::Ptr(typs) = &self.context[&access.var] {
            match typs.as_slice() {
                [t] if t.name == "void" => HalideType::Unsigned(8),
                [t] => HalideType::from_id(t),
                _ => HalideType::Unknown,
            }
        } else {
            // otherwise we just say we don't know what the type of this is
            HalideType::Unknown
        };

        // if the idx expression results in a vec type, we want the type of this
        // access to be the same width as the child vector type
        let child_typ_width = access.idx.data().width();
        if child_typ_width > 1 {
            ast::Expr::Access(access, HalideType::Vec(child_typ_width, Box::new(typ)))
        } else {
            ast::Expr::Access(access, typ)
        }
    }
}

fn halide_intrinsic_type(id: &ast::Id, args: &[ast::Expr<HalideType>]) -> HalideType {
    // defined in runtime/halide_buffer_t.cpp
    match (id.name.as_str(), args) {
        ("_halide_buffer_get_dimensions", _) => HalideType::Signed(32),
        ("_halide_buffer_get_host", _) => HalideType::Ptr(vec![ast::Id::new("uint8")]),
        ("_halide_buffer_get_device", _) => HalideType::Unsigned(32),
        ("_halide_buffer_get_device_interface", _) => HalideType::Ptr(vec![
            ast::Id::new("struct"),
            ast::Id::new("halide_device_interface_t"),
        ]),
        ("_halide_buffer_get_min", _) => HalideType::Signed(32),
        ("_halide_buffer_get_max", _) => HalideType::Signed(32),
        ("_halide_buffer_get_extent", _) => HalideType::Signed(32),
        ("_halide_buffer_get_stride", _) => HalideType::Signed(32),
        ("_halide_buffer_set_host_dirty", _) => HalideType::Signed(32),
        ("_halide_buffer_set_device_dirty", _) => HalideType::Signed(32),
        ("_halide_buffer_get_host_dirty", _) => HalideType::Bool,
        ("_halide_buffer_get_device_dirty", _) => HalideType::Bool,
        ("_halide_buffer_get_shape", _) => {
            HalideType::Ptr(vec![ast::Id::new("halide_dimension_t")])
        }
        ("_halide_buffer_is_bounds_query", _) => HalideType::Bool,
        ("_halide_buffer_get_type", _) => HalideType::Unsigned(32),
        ("_halide_buffer_retire_crop_after_extern_stage", _) => HalideType::Signed(32),
        ("_halide_buffer_retire_crops_after_extern_stage", _) => HalideType::Signed(32),
        ("_halide_buffer_set_bounds", _) => HalideType::Ptr(vec![ast::Id::new("halide_buffer_t")]),
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

        // halide intrinsic functions. comments taken from `IROperator.h`
        // compute a + widen(b)
        ("widen_right_add", [a, b]) if a.match_width(b, |aw, bw| aw == bw * 2) => b.data().widen(),
        // compute a * widen(b)
        ("widen_right_mul", [a, b]) if a.match_width(b, |aw, bw| aw == bw * 2) => b.data().widen(),
        // compute a - widen(b)
        ("widen_right_sub", [a, b]) if a.match_width(b, |aw, bw| aw == bw * 2) => b.data().widen(),

        // compute widen(a) + widen(b)
        ("widening_add", [a, _b]) => a.data().widen(),
        // compute widen(a) * widen(b). a and b may have difference signedness,
        // in which case the result is signed
        ("widening_mul", [a, b]) if a.match_sign(b) => a.data().widen(),
        ("widening_mul", [a, _b]) => HalideType::Signed(a.data().widen().width()),
        // compute widen(a) - widen(b). The result is always signed
        ("widening_sub", [a, b]) if a.data().width() == b.data().width() => {
            let x = a.data().widen();
            println!(
                "yay! widening_sub(a: {:?}, b: {:?}) -> {x:?}",
                a.data(),
                b.data()
            );
            x
        }
        ("widening_sub", [a, b]) => {
            println!("widening_sub(a: {:?}, b: {:?})", a.data(), b.data());
            HalideType::Unknown
        }
        ("widening_shift_left", [a, _b]) => a.data().widen(),
        ("widening_shift_right", [a, _b]) => a.data().widen(),

        // Compute saturating_narrow(widening_add(a, (1 >> min(b, 0)) / 2) << b).
        // When b is positive indicating a left shift, the rounding term is zero.
        ("rounding_shift_left", [a, _b]) => a.data().clone(),
        // Compute saturating_narrow(widening_add(a, (1 << max(b, 0)) / 2) >> b).
        // When b is negative indicating a left shift, the rounding term is zero.
        ("rounding_shift_right", [a, _b]) => a.data().clone(),

        // Compute saturating_narrow(widen(a) + widen(b))
        ("saturating_add", [a, _b]) => a.data().clone(),
        // Compute saturating_narrow(widen(a) - widen(b))
        ("saturating_sub", [a, _b]) => a.data().clone(),

        // Compute narrow((widen(a) + widen(b)) / 2)
        ("halving_add", [a, _b]) => a.data().clone(),
        // Compute narrow((widen(a) + widen(b) + 1) / 2)
        ("rounding_halving_add", [a, _b]) => a.data().clone(),
        // Compute narrow((widen(a) - widen(b)) / 2)
        ("halving_sub", [a, _b]) => a.data().clone(),

        // Compute saturating_narrow(shift_right(widening_mul(a, b), q))
        ("mul_shift_right", [a, _b]) => a.data().clone(),
        // Compute saturating_narrow(rounding_shift_right(widening_mul(a, b), q))
        ("rounding_mul_shift_right", [a, _b]) => a.data().clone(),

        (x, _) => {
            println!("unknown intrinsic: {x}");
            HalideType::Unknown
        }
    }
}
