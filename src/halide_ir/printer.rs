use std::collections::HashMap;

use pretty::{
    termcolor::{Color, ColorChoice, ColorSpec, StandardStream},
    DocAllocator, DocBuilder, Pretty, RcAllocator, RcDoc,
};

use super::ast;

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
        let stdout = StandardStream::stdout(ColorChoice::Auto);
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
            .line_then(self.funcs.intersperse(Doc::hardline()).block())
    }
}

impl Printer for ast::Func {
    fn to_doc(&self) -> Doc {
        self.metadata
            .to_doc()
            .highlight(|cs| cs.set_italic(true))
            .space_then(Doc::text("func").highlight(|cs| cs.keyword()))
            .space_then(self.name.to_doc().highlight(|cs| cs.func()))
            .space_then(self.args.intersperse(Doc::text(",").line()).parens())
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
            } => Doc::text("for").highlight(|cs| cs.keyword()).space_then(
                var.to_doc()
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
                .space_then(var.to_doc())
                .space_then(body.intersperse(Doc::hardline()).block()),
            ast::Stmt::Predicate { cond, stmt } => Doc::text("predicate")
                .highlight(|cs| cs.keyword())
                .space_then(cond.to_doc().enclose("(", ")"))
                .line_then(stmt.to_doc().group())
                .nest(2),
            ast::Stmt::Update { array, idx, value } => array
                .to_doc()
                .append(idx.to_doc().nest(2).group().enclose("[", "]"))
                .line_then("=")
                .space_then(value.to_doc().nest(2)),
            ast::Stmt::Expr(e) => e.to_doc(),
        }
    }
}

impl Printer for ast::Expr {
    fn to_doc(&self) -> Doc {
        match self {
            ast::Expr::Number(n) => Doc::as_string(n.value).highlight(|cs| cs.literal()),
            ast::Expr::Ident(id) => id.to_doc(),
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
                .append(args.intersperse(Doc::text(",").line_()).parens()),
            ast::Expr::Cast(typs, expr) => typs
                .intersperse(Doc::space())
                .enclose("(", ")")
                .highlight(|cs| cs.typcast())
                .append(expr.to_doc()),
            ast::Expr::Access(array, idx) => array
                .to_doc()
                .append(idx.to_doc().nest(2).group().enclose("[", "]")),
        }
    }
}
