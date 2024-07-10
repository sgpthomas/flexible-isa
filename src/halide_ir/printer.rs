use std::collections::HashMap;

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
        Doc::as_string(&self.value)
    }
}

impl Printer for ast::Module {
    fn to_doc(&self) -> Doc {
        Doc::text("module")
            .highlight(|cs| cs.set_bold(true).keyword())
            .space_then(self.params.intersperse("="))
            .line_then(self.funcs.intersperse(Doc::hardline()))
    }
}

impl Printer for ast::Func {
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

impl Printer for ast::Stmt {
    fn to_doc(&self) -> Doc {
        match self {
            ast::Stmt::Let { var, expr } => Doc::text("let")
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
            ast::Stmt::If { cond, tru, fls } => {
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
            ast::Stmt::Produce { var, body } => Doc::text("produce")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.set_bold(true)))
                .space_then(body.intersperse(Doc::hardline()).block()),
            ast::Stmt::Consume { var, body } => Doc::text("consume")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.set_bold(true)))
                .space_then(body.intersperse(Doc::hardline()).block()),
            ast::Stmt::Predicate { cond, stmt } => Doc::text("predicate")
                .highlight(|cs| cs.keyword())
                .space_then(cond.to_doc().enclose("(", ")"))
                .line_then(stmt.to_doc().group())
                .nest(2),
            ast::Stmt::Store { access, value } => access
                .to_doc()
                .line_then(Doc::text("=").space_then(value.to_doc()))
                .nest(2)
                .group(),
            ast::Stmt::Allocate {
                access,
                loc,
                condition,
            } => Doc::text("allocate")
                .highlight(|cs| cs.keyword())
                .space_then(access.to_doc())
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
            ast::Stmt::Free { var } => Doc::text("free")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc()),
            ast::Stmt::Expr(e) => e.to_doc(),
        }
    }
}

impl Printer for ast::Expr {
    fn to_doc(&self) -> Doc {
        match self {
            ast::Expr::Number(n) => Doc::as_string(n.value).highlight(|cs| cs.literal()),
            ast::Expr::Ident(id) => id.to_doc(),
            ast::Expr::Neg(rhs) => Doc::text("-").append(rhs.to_doc()),
            ast::Expr::Add(lhs, rhs)
            | ast::Expr::Sub(lhs, rhs)
            | ast::Expr::Mul(lhs, rhs)
            | ast::Expr::Div(lhs, rhs)
            | ast::Expr::Modulo(lhs, rhs)
            | ast::Expr::Lt(lhs, rhs)
            | ast::Expr::Lte(lhs, rhs)
            | ast::Expr::Eq(lhs, rhs)
            | ast::Expr::Neq(lhs, rhs)
            | ast::Expr::Gte(lhs, rhs)
            | ast::Expr::Gt(lhs, rhs)
            | ast::Expr::And(lhs, rhs)
            | ast::Expr::Or(lhs, rhs)
            | ast::Expr::If(lhs, rhs) => {
                let op = match self {
                    ast::Expr::Add(_, _) => "+",
                    ast::Expr::Sub(_, _) => "-",
                    ast::Expr::Mul(_, _) => "*",
                    ast::Expr::Div(_, _) => "/",
                    ast::Expr::Modulo(_, _) => "%",
                    ast::Expr::Lt(_, _) => "<",
                    ast::Expr::Lte(_, _) => "<=",
                    ast::Expr::Eq(_, _) => "==",
                    ast::Expr::Neq(_, _) => "!=",
                    ast::Expr::Gte(_, _) => ">=",
                    ast::Expr::Gt(_, _) => ">",
                    ast::Expr::And(_, _) => "&&",
                    ast::Expr::Or(_, _) => "||",
                    ast::Expr::If(_, _) => "if",
                    _ => unreachable!(),
                };
                lhs.to_doc()
                    .space_then(op)
                    .space_then(rhs.to_doc())
                    .group()
                    .enclose("(", ")")
            }
            ast::Expr::FunCall(fn_name, args) => fn_name
                .to_doc()
                .highlight(|cs| cs.funcall())
                .append(args.intersperse(Doc::text(",").line_()).parens().group())
                .group(),
            ast::Expr::Reinterpret(cast, args) => Doc::text("reinterpret")
                .highlight(|cs| cs.funcall())
                .append(
                    cast.intersperse(Doc::space())
                        .enclose("<(", ")>")
                        .highlight(|cs| cs.typcast()),
                )
                .append(args.intersperse(Doc::text(",").line_()).parens())
                .group(),
            ast::Expr::Cast(typs, expr) => typs
                .intersperse(Doc::space())
                .enclose("(", ")")
                .highlight(|cs| cs.typcast())
                .append(expr.to_doc()),
            ast::Expr::Access(access) => access.to_doc(),
            ast::Expr::LetIn(var, binding, body) => Doc::text("let")
                .highlight(|cs| cs.keyword())
                .space_then(var.to_doc().highlight(|cs| cs.var()))
                .space_then("=")
                .line_then(binding.to_doc().nest(2))
                .space_then(Doc::text("in").highlight(|cs| cs.keyword()))
                .line_then(body.to_doc().nest(2)),
        }
    }
}

impl Printer for ast::Access {
    fn to_doc(&self) -> Doc {
        let idx_doc = self.idx.to_doc().map_append(&self.align, |(low, hi)| {
            Doc::space().append(
                Doc::text("aligned").highlight(|cs| cs.funcall()).append(
                    Doc::as_string(low)
                        .highlight(|cs| cs.literal())
                        .append(",")
                        .space_then(Doc::as_string(hi).highlight(|cs| cs.literal()))
                        .parens(),
                ),
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
            DeviceApi::CUDA => Doc::text("CUDA"),
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
            ast::MemoryType::VTCM => Doc::text("VTCM"),
            ast::MemoryType::AMXTile => Doc::text("AMXTile"),
        }
    }
}
