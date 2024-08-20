use std::collections::HashMap;

use super::{ast, Annotation, HalideType, MatchWidth, Visitor};

#[derive(Default)]
pub struct TypeAnnotator {
    module_name: Option<ast::Id>,
    count: usize,
    func_signature: HashMap<ast::Id, HalideType>,
    context: HashMap<ast::Id, HalideType>,
}

impl TypeAnnotator {
    fn bind_var(&mut self, var: ast::Id, typ: HalideType) -> Option<HalideType> {
        self.context.insert(var, typ)
    }

    fn prefix(&self) -> Option<String> {
        self.module_name
            .as_ref()
            .map(|id| format!("{}_{}$", id.name, self.count))
    }

    fn lookup(&self, var: &ast::Id) -> Option<HalideType> {
        self.func_signature
            .get(&var.clone().strip_prefix(&self.prefix()))
            .or_else(|| self.context.get(var))
            .cloned()
    }

    fn halide_intrinsic_type(&self, id: &ast::Id, args: &[ast::Expr<HalideType>]) -> HalideType {
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
            ("_halide_buffer_set_bounds", _) => {
                HalideType::Ptr(vec![ast::Id::new("halide_buffer_t")])
            }
            ("ramp", [base, _stride, lanes]) => {
                if let ast::Expr::Number(n, _) = lanes {
                    HalideType::Vec(n.value, Box::new(base.data().clone()))
                } else {
                    HalideType::Unknown
                }
            }
            // ("int8", [_arg]) => HalideType::Signed(8),
            // ("int16", [_arg]) => HalideType::Signed(16),
            // ("int32", [_arg]) => HalideType::Signed(32),
            // ("int64", [_arg]) => HalideType::Signed(64),
            // ("int128", [_arg]) => HalideType::Signed(128),
            // (vec_constructor, [arg]) if vec_constructor.contains('x') => vec_constructor
            //     .split_once('x')
            //     .map(|(typ, lanes)| todo!())
            //     .unwrap_or(HalideType::Unknown),
            // ("x8", [arg]) => HalideType::Vec(8, Box::new(arg.data().clone())),
            // ("x16", [arg]) => HalideType::Vec(16, Box::new(arg.data().clone())),
            // ("x32", [arg]) => HalideType::Vec(32, Box::new(arg.data().clone())),
            // ("x64", [arg]) => HalideType::Vec(64, Box::new(arg.data().clone())),
            // ("x128", [arg]) => HalideType::Vec(128, Box::new(arg.data().clone())),

            // halide intrinsic functions. comments taken from `IROperator.h`
            // compute a + widen(b)
            ("widen_right_add", [a, b]) if a.match_width(b, |aw, bw| aw == bw * 2) => {
                b.data().widen()
            }
            // compute a * widen(b)
            ("widen_right_mul", [a, b]) if a.match_width(b, |aw, bw| aw == bw * 2) => {
                b.data().widen()
            }
            // compute a - widen(b)
            ("widen_right_sub", [a, b]) if a.match_width(b, |aw, bw| aw == bw * 2) => {
                b.data().widen()
            }

            // compute widen(a) + widen(b)
            ("widening_add", [a, _b]) => a.data().widen(),
            // compute widen(a) * widen(b). a and b may have difference signedness,
            // in which case the result is signed
            ("widening_mul", [a, b]) if a.match_sign(b) => a.data().widen(),
            ("widening_mul", [a, _b]) => a.data().widen().signed(),
            // compute widen(a) - widen(b). The result is always signed
            ("widening_sub", [a, b]) if a.match_width(b, |aw, bw| aw == bw) => {
                a.data().widen().signed()
            }
            ("widening_shift_left", [a, _b]) => a.data().widen(),
            ("widening_shift_right", [a, _b]) => a.data().widen(),
            ("shift_left", [a, _b]) => a.data().clone(),
            ("shift_right", [a, _b]) => a.data().clone(),

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
            ("rounding_mul_shift_right", [a, _b, _c]) => a.data().clone(),

            ("vector_reduce_add", [a]) => {
                if let HalideType::Vec(_, typ) = a.data() {
                    (**typ).clone()
                } else {
                    HalideType::Unknown
                }
            }

            // utility functions
            ("count_leading_zeros", [a]) if matches!(a.data(), HalideType::Unsigned(_)) => {
                a.data().clone()
            }
            ("count_leading_zeros", [a]) if matches!(a.data(), HalideType::Signed(_)) => {
                a.data().clone()
            }
            ("bitwise_and", [a, _b]) => a.data().clone(),
            ("saturating_cast", [a]) => a.data().clone(),
            ("min", [a, _b]) => a.data().clone(),
            ("max", [a, _b]) => a.data().clone(),
            ("halide_do_par_for", [_usr_ctx, _min, _size, _closure]) => HalideType::Signed(32),
            ("assert", _) => HalideType::Bool,
            ("prefetch", _) => HalideType::void_ptr(),

            // some random loading functions
            ("make_struct", args) => {
                HalideType::Struct(args.iter().map(|a| a.data()).cloned().collect())
            }
            ("load_typed_struct_member", [_arg, proto, idx]) => {
                if let (HalideType::Struct(typs), ast::Expr::Number(n, _)) = (proto.data(), idx) {
                    typs[n.value as usize].clone()
                } else {
                    HalideType::Unknown
                }
            }

            // try parsing the function name as a normal HalideType
            (x_lanes, [arg]) if x_lanes.starts_with('x') => x_lanes
                .strip_prefix('x')
                .and_then(|lanes| lanes.parse::<u64>().ok())
                .map(|lanes| HalideType::Vec(lanes, Box::new(arg.data().clone())))
                .unwrap_or(HalideType::Unknown),
            (typ_x_lanes, [_arg]) => HalideType::from_str(typ_x_lanes),

            (x, args) => {
                println!(
                    "unknown intrinsic in {:?}: {x}({})",
                    self.prefix(),
                    args.iter()
                        .map(|a| a.data().to_id().name.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                HalideType::Unknown
            }
        }
    }
}

impl From<HashMap<ast::Id, HalideType>> for TypeAnnotator {
    fn from(func_signature: HashMap<ast::Id, HalideType>) -> Self {
        Self {
            module_name: None,
            count: 0,
            func_signature,
            context: HashMap::default(),
        }
    }
}

impl<T> Visitor<T> for TypeAnnotator {
    type Output = HalideType;

    fn default_u(&mut self, _data: T) -> HalideType {
        HalideType::Unknown
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

    fn make_let_stmt(
        &mut self,
        var: ast::Id,
        expr: ast::Expr<HalideType>,
        _data: T,
    ) -> Vec<ast::Stmt<HalideType>> {
        let typ = expr.data().clone();
        self.bind_var(var.clone(), typ.clone());
        vec![ast::Stmt::Let {
            var,
            expr,
            data: typ,
        }]
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
        let data = HalideType::Ptr(vec![typ.clone()]);
        self.bind_var(name.clone(), data.clone());
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
        self.bind_var(var.clone(), typ);
    }

    fn make_number_expr(&mut self, number: ast::Number, _data: T) -> ast::Expr<HalideType> {
        ast::Expr::Number(number, HalideType::Signed(32))
    }

    fn make_ident_expr(&mut self, id: ast::Id, _data: T) -> ast::Expr<HalideType> {
        let typ = self.lookup(&id).unwrap_or(HalideType::Unknown);
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

    fn make_struct_member_expr(
        &mut self,
        struct_id: ast::Id,
        thing: ast::Id,
        _data: T,
    ) -> ast::Expr<HalideType> {
        ast::Expr::StructMember(struct_id, thing, HalideType::void_ptr())
    }

    fn make_funcall_expr(
        &mut self,
        id: ast::Id,
        args: Vec<ast::Expr<HalideType>>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = self.halide_intrinsic_type(&id, &args);
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
        // assert_eq!(expr.data(), &typ, "{expr:#?}");
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

        // we don't want to override struct types
        if !matches!(expr.data(), HalideType::Struct(_)) {
            *expr.data_mut() = typ.clone();
        }

        expr
    }

    fn make_reinterpret_expr(
        &mut self,
        typs: Vec<ast::Id>,
        args: Vec<ast::Expr<HalideType>>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = HalideType::Ptr(typs.clone());
        ast::Expr::Reinterpret(typs, args, typ)
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
        let child_typ_width = access.idx.data().lanes();
        if child_typ_width > 1 {
            ast::Expr::Access(access, HalideType::Vec(child_typ_width, Box::new(typ)))
        } else {
            ast::Expr::Access(access, typ)
        }
    }

    fn make_if_expr(
        &mut self,
        expr: ast::Expr<HalideType>,
        cond: ast::Expr<HalideType>,
        _data: T,
    ) -> ast::Expr<HalideType> {
        let typ = expr.data().clone();
        ast::Expr::If(Box::new(expr), Box::new(cond), typ)
    }
}
