use std::{collections::HashMap, fmt::Debug};

use pretty::{
    termcolor::{Color, ColorChoice, ColorSpec, StandardStream},
    DocAllocator, DocBuilder, Pretty, RcAllocator, RcDoc,
};

use super::ast::{self, DeviceApi};

type Doc<'a> = RcDoc<'a, ColorSpec>;

#[allow(unused)]
pub trait Printer {
    fn to_doc(&self) -> Doc;

    fn to_pretty(&self) -> String {
        let mut w = Vec::new();
        self.to_doc().render(80, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn stdout(&self) {
        let stdout = StandardStream::stdout(ColorChoice::Always);
        self.to_doc().render_colored(80, stdout).unwrap()
    }
}

trait ColorUtils {
    fn keyword(&mut self) -> &mut Self;
    fn func(&mut self) -> &mut Self;
    fn var(&mut self) -> &mut Self;
    fn typcast(&mut self) -> &mut Self;
    fn funcall(&mut self) -> &mut Self;
    fn literal(&mut self) -> &mut Self;
}

impl ColorUtils for ColorSpec {
    fn keyword(&mut self) -> &mut Self {
        self.set_fg(Some(Color::Red))
    }

    fn func(&mut self) -> &mut Self {
        self.set_fg(Some(Color::Yellow))
    }

    fn var(&mut self) -> &mut Self {
        self.set_fg(Some(Color::Blue))
    }

    fn typcast(&mut self) -> &mut Self {
        self.set_fg(Some(Color::Black))
    }

    fn funcall(&mut self) -> &mut Self {
        self.set_fg(Some(Color::Yellow)).set_italic(true)
    }

    fn literal(&mut self) -> &mut Self {
        self.set_fg(Some(Color::Green))
    }
}

trait PrinterUtils<'a, A: DocAllocator<'a, ColorSpec>> {
    fn enclose<O, C>(self, open: O, close: C) -> Self
    where
        O: Pretty<'a, A, ColorSpec>,
        C: Pretty<'a, A, ColorSpec>;

    fn space_then<S>(self, next: S) -> Self
    where
        S: Pretty<'a, RcAllocator, ColorSpec>;
    fn line_then<S>(self, next: S) -> Self
    where
        S: Pretty<'a, RcAllocator, ColorSpec>;
    fn map_append<T, F, S>(self, thing: &'a Option<T>, f: F) -> Self
    where
        F: Fn(&'a T) -> S,
        S: Pretty<'a, RcAllocator, ColorSpec>;
    fn append_if<S>(self, cond: bool, next: S) -> Self
    where
        S: Pretty<'a, RcAllocator, ColorSpec>;
    fn space(self) -> Self;
    fn line(self) -> Self;
    fn line_(self) -> Self;
    fn parens(self) -> Self;
    fn block(self) -> Self;
    fn highlight<F>(self, f: F) -> Self
    where
        F: Fn(&mut ColorSpec) -> &mut ColorSpec;
}

impl<'a> PrinterUtils<'a, RcAllocator> for Doc<'a> {
    fn enclose<O, C>(self, open: O, close: C) -> Self
    where
        O: Pretty<'a, RcAllocator, ColorSpec>,
        C: Pretty<'a, RcAllocator, ColorSpec>,
    {
        DocBuilder(&RcAllocator, self.into())
            .enclose(open, close)
            .into_doc()
    }

    fn space_then<S>(self, next: S) -> Self
    where
        S: Pretty<'a, RcAllocator, ColorSpec>,
    {
        self.space().append(next)
    }

    fn line_then<S>(self, next: S) -> Self
    where
        S: Pretty<'a, RcAllocator, ColorSpec>,
    {
        self.line().append(next)
    }

    fn map_append<T, F, S>(self, thing: &'a Option<T>, f: F) -> Self
    where
        F: Fn(&'a T) -> S,
        S: Pretty<'a, RcAllocator, ColorSpec>,
    {
        if let Some(t) = thing {
            self.append(f(t))
        } else {
            self
        }
    }

    fn append_if<S>(self, cond: bool, next: S) -> Self
    where
        S: Pretty<'a, RcAllocator, ColorSpec>,
    {
        if cond {
            self.append(next)
        } else {
            self
        }
    }

    fn space(self) -> Self {
        self.append(Doc::space())
    }

    fn line(self) -> Self {
        self.append(Doc::line())
    }

    fn line_(self) -> Self {
        self.append(Doc::line())
    }

    fn parens(self) -> Self {
        Doc::line_()
            .append(self)
            .nest(2)
            .group()
            .append(Doc::line_())
            .enclose("(", ")")
    }

    fn block(self) -> Self {
        Doc::hardline()
            .append(self)
            .nest(2)
            .group()
            .append(Doc::hardline())
            .enclose("{", "}")
    }

    fn highlight<F>(self, f: F) -> Self
    where
        F: Fn(&mut ColorSpec) -> &mut ColorSpec,
    {
        {
            let mut cs = ColorSpec::new();
            f(&mut cs);
            self.annotate(cs)
        }
    }
}

trait CollectionPrinter<'a> {
    fn intersperse<S>(&'a self, sep: S) -> Doc<'a>
    where
        S: Pretty<'a, RcAllocator, ColorSpec> + Clone;
}

impl<'a, A, B> CollectionPrinter<'a> for HashMap<A, B>
where
    A: Printer,
    B: Printer,
{
    fn intersperse<S>(&'a self, sep: S) -> Doc<'a>
    where
        S: Pretty<'a, RcAllocator, ColorSpec> + Clone,
    {
        let kv_pairs = self
            .iter()
            .map(|(k, v)| k.to_doc().append(sep.clone()).append(v.to_doc()));
        Doc::intersperse(kv_pairs, Doc::space())
    }
}

impl<'a, A> CollectionPrinter<'a> for Vec<A>
where
    A: Printer,
{
    fn intersperse<S>(&'a self, sep: S) -> Doc<'a>
    where
        S: Pretty<'a, RcAllocator, ColorSpec> + Clone,
    {
        Doc::intersperse(self.iter().map(Printer::to_doc), sep)
    }
}

impl Printer for ast::Id {
    fn to_doc(&self) -> Doc {
        Doc::as_string(&self.name)
    }
}

impl Printer for ast::Number {
    fn to_doc(&self) -> Doc {
        Doc::as_string(self.value)
    }
}

impl<T> Printer for ast::Module<T>
where
    T: Debug,
{
    fn to_doc(&self) -> Doc {
        Doc::text("module")
            .highlight(|cs| cs.set_bold(true).keyword())
            .space_then(self.params.intersperse("="))
            .line_then(
                self.funcs
                    .intersperse(Doc::hardline().append(Doc::hardline())),
            )
    }
}

impl<T> Printer for ast::Func<T>
where
    T: Debug,
{
    fn to_doc(&self) -> Doc {
        self.metadata
            .to_doc()
            .highlight(|cs| cs.set_italic(true).set_fg(Some(Color::Black)))
            .space_then(Doc::text("func").highlight(|cs| cs.keyword()))
            .space_then(self.name.to_doc().highlight(|cs| cs.func()))
            .space_then(
                self.args
                    .intersperse(Doc::text(",").line())
                    .parens()
                    .group(),
            )
            .space_then(self.stmts.intersperse(Doc::hardline()).block())
    }
}

impl<T> Printer for ast::Stmt<T>
where
    T: Debug,
{
    fn to_doc(&self) -> Doc {
        match self {
            ast::Stmt::Let { var, expr, data: _ } => Doc::text("let")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.var()))
                .space_then("=")
                .space_then(expr.to_doc()),
            ast::Stmt::For {
                var,
                low,
                high,
                body,
                device,
                data: _,
            } => Doc::text("for")
                .highlight(|cs| cs.keyword())
                .append_if(
                    !matches!(device, DeviceApi::None),
                    device.to_doc().enclose("<", ">"),
                )
                .space_then(
                    var.to_doc()
                        .highlight(|cs| cs.var())
                        .append(",")
                        .space_then(low.to_doc())
                        .append(",")
                        .space_then(high.to_doc())
                        .enclose("(", ")")
                        .space_then(body.intersperse(Doc::hardline()).block()),
                ),
            ast::Stmt::If {
                cond,
                tru,
                fls,
                data: _,
            } => {
                let tru_branch = Doc::text("if")
                    .highlight(|cs| cs.keyword())
                    .space_then(cond.to_doc().enclose("(", ")"))
                    .space_then(tru.intersperse(Doc::hardline()).block());
                if let Some(fls) = fls {
                    if matches!(fls[0], ast::Stmt::If { .. }) {
                        tru_branch
                            .space_then(Doc::text("else").highlight(|cs| cs.keyword()))
                            .space_then(fls[0].to_doc())
                    } else {
                        tru_branch
                            .space_then(Doc::text("else").highlight(|cs| cs.keyword()))
                            .space_then(fls.intersperse(Doc::hardline()).block())
                    }
                } else {
                    tru_branch
                }
            }
            ast::Stmt::Produce { var, body, data: _ } => Doc::text("produce")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.set_bold(true)))
                .space_then(body.intersperse(Doc::hardline()).block()),
            ast::Stmt::Consume { var, body, data: _ } => Doc::text("consume")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.set_bold(true)))
                .space_then(body.intersperse(Doc::hardline()).block()),
            ast::Stmt::Predicate {
                cond,
                stmt,
                data: _,
            } => Doc::text("predicate")
                .highlight(|cs| cs.keyword())
                .space_then(cond.to_doc().enclose("(", ")"))
                .line_then(stmt.to_doc().group())
                .nest(2),
            ast::Stmt::Store {
                access,
                value,
                data: _,
            } => access
                .to_doc()
                .line_then(Doc::text("=").space_then(value.to_doc()))
                .nest(2)
                .group(),
            ast::Stmt::Allocate {
                name,
                typ,
                extents,
                loc,
                condition,
                data: _,
            } => Doc::text("allocate")
                .highlight(|cs| cs.keyword())
                .space_then(name.to_doc())
                .append(
                    typ.to_doc()
                        .space_then("*")
                        .space_then(extents.intersperse("+"))
                        .enclose("[", "]")
                        .group(),
                )
                .append_if(
                    !matches!(loc, ast::MemoryType::Auto),
                    Doc::space()
                        .append(Doc::text("in").highlight(|cs| cs.keyword()))
                        .space_then(loc.to_doc()),
                )
                .map_append(condition, |e| {
                    Doc::space()
                        .append(Doc::text("if").highlight(|cs| cs.keyword()))
                        .space_then(e.to_doc())
                }),
            ast::Stmt::Free { var, data: _ } => Doc::text("free")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc()),
            ast::Stmt::Expr(e, _) => e.to_doc(),
        }
    }
}

impl<T> Printer for ast::Expr<T> {
    fn to_doc(&self) -> Doc {
        let doc = match self {
            ast::Expr::Number(n, _) => Doc::as_string(n.value).highlight(|cs| cs.literal()),
            ast::Expr::Ident(id, _) => id.to_doc(),
            ast::Expr::Unop(op, rhs, _) => {
                let op = match op {
                    ast::Unop::Neg => "-",
                };
                Doc::text(op).append(rhs.to_doc())
            }
            ast::Expr::ArithBinop(op, lhs, rhs, _) => {
                let op = match op {
                    ast::ArithBinop::Add => "+",
                    ast::ArithBinop::Sub => "-",
                    ast::ArithBinop::Mul => "*",
                    ast::ArithBinop::Div => "/",
                    ast::ArithBinop::Modulo => "%",
                };
                lhs.to_doc()
                    .space_then(op)
                    .space_then(rhs.to_doc())
                    .group()
                    .enclose("(", ")")
            }
            ast::Expr::CompBinop(op, lhs, rhs, _) => {
                let op = match op {
                    ast::CompBinop::Lt => "<",
                    ast::CompBinop::Lte => "<=",
                    ast::CompBinop::Eq => "==",
                    ast::CompBinop::Neq => "!=",
                    ast::CompBinop::Gte => ">=",
                    ast::CompBinop::Gt => ">",
                    ast::CompBinop::And => "&&",
                    ast::CompBinop::Or => "||",
                };
                lhs.to_doc()
                    .space_then(op)
                    .space_then(rhs.to_doc())
                    .group()
                    .enclose("(", ")")
            }
            ast::Expr::If(lhs, rhs, _) => lhs
                .to_doc()
                .space_then("if")
                .space_then(rhs.to_doc())
                .group()
                .enclose("(", ")"),
            ast::Expr::StructMember(struct_expr, thing, _) => struct_expr
                .to_doc()
                .enclose("(", ")")
                .append("::")
                .append(thing.to_doc())
                .group(),
            ast::Expr::FunCall(fn_name, args, _) => fn_name
                .to_doc()
                .highlight(|cs| cs.funcall())
                .append(args.intersperse(Doc::text(",").line_()).parens().group())
                .group(),
            ast::Expr::Reinterpret(cast, args, _) => Doc::text("reinterpret")
                .highlight(|cs| cs.funcall())
                .append(
                    cast.intersperse(Doc::space())
                        .space_then("*")
                        .enclose("<(", ")>")
                        .highlight(|cs| cs.typcast()),
                )
                .append(args.intersperse(Doc::text(",").line_()).parens())
                .group(),
            ast::Expr::Cast(typ, expr, _) if typ.name == "unknown" => typ
                .to_doc()
                .enclose("(", ")")
                .highlight(|cs| cs.keyword())
                .append(expr.to_doc()),
            ast::Expr::Cast(typ, expr, _) => typ
                .to_doc()
                .enclose("(", ")")
                .highlight(|cs| cs.typcast())
                .append(expr.to_doc()),
            ast::Expr::PtrCast(typs, expr, _) => typs
                .intersperse(Doc::space())
                .space_then("*")
                .enclose("(", ")")
                .highlight(|cs| cs.typcast())
                .append(expr.to_doc()),
            ast::Expr::Access(access, _) => access.to_doc(),
            ast::Expr::LetIn(var, binding, body, _) => Doc::text("let")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.var()))
                .space_then("=")
                .line_then(binding.to_doc().nest(2))
                .space_then(Doc::text("in").highlight(|cs| cs.keyword()))
                .line_then(body.to_doc().nest(2)),
            ast::Expr::Instruction { num, args, data: _ } => Doc::text("inst")
                .highlight(|cs| cs.keyword())
                .append(Doc::text(format!("{num}")).enclose("<", ">"))
                .space_then(args.intersperse(Doc::space()))
                .parens()
                .group(),
        };
        // doc.append(Doc::text(format!("{:?}", super::Annotation::data(self))).enclose("<", ">"))
        doc
    }
}

impl<T> Printer for ast::Access<T> {
    fn to_doc(&self) -> Doc {
        let idx_doc = self.idx.to_doc().map_append(&self.align, |(low, hi)| {
            Doc::space().append(
                Doc::text("aligned")
                    .highlight(|cs| cs.funcall())
                    .append(
                        Doc::as_string(low)
                            .highlight(|cs| cs.literal())
                            .append(",")
                            .space_then(Doc::as_string(hi).highlight(|cs| cs.literal()))
                            .parens(),
                    )
                    .group(),
            )
        });
        self.var
            .to_doc()
            .append(idx_doc.nest(2).group().enclose("[", "]"))
    }
}

impl Printer for ast::DeviceApi {
    fn to_doc(&self) -> Doc {
        match self {
            DeviceApi::None => Doc::text("None"),
            DeviceApi::Host => Doc::text("Host"),
            DeviceApi::DefaultGPU => Doc::text("DefaultGPU"),
            DeviceApi::Cuda => Doc::text("CUDA"),
            DeviceApi::OpenCL => Doc::text("OpenCL"),
            DeviceApi::Metal => Doc::text("Metal"),
            DeviceApi::Hexagon => Doc::text("Hexagon"),
            DeviceApi::HexagonDma => Doc::text("HexagonDma"),
            DeviceApi::D3D12Compute => Doc::text("D3D12Compute"),
            DeviceApi::Vulkan => Doc::text("Vulkan"),
            DeviceApi::WebGPU => Doc::text("WebGPU"),
        }
    }
}

impl Printer for ast::MemoryType {
    fn to_doc(&self) -> Doc {
        match self {
            ast::MemoryType::Auto => Doc::text("Auto"),
            ast::MemoryType::Heap => Doc::text("Heap"),
            ast::MemoryType::Stack => Doc::text("Stack"),
            ast::MemoryType::Register => Doc::text("Register"),
            ast::MemoryType::GPUShared => Doc::text("GPUShared"),
            ast::MemoryType::GPUTexture => Doc::text("GPUTexture"),
            ast::MemoryType::LockedCache => Doc::text("LockedCache"),
            ast::MemoryType::Vtcm => Doc::text("VTCM"),
            ast::MemoryType::AMXTile => Doc::text("AMXTile"),
        }
    }
}
