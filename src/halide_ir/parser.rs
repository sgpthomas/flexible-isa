use std::{fs, path::Path};

use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_consume::{match_nodes, Parser};

use super::ast::{Block, Expr, Func, Id, Module, Number, Stmt};

// include the grammar file so that Cargo knows to rebuild this file on grammar changes
const _GRAMMR: &str = include_str!("stmt.pest");

type ParseResult<T> = std::result::Result<T, pest_consume::Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

// Define the precedence of binary operations. We use `lazy_static` so that
// this is only ever constructed once.
lazy_static::lazy_static! {
    static ref PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left))
        .op(Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left))
        .op(Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::modulo, Assoc::Left));
}

#[derive(pest_consume::Parser)]
#[grammar = "halide_ir/stmt.pest"]
pub struct StmtParser;

impl StmtParser {
    pub fn parse_file(path: &Path) -> anyhow::Result<Module> {
        let raw_content = fs::read(path)?;
        let string_content = std::str::from_utf8(&raw_content)?.to_string();

        println!("{string_content}");

        let inputs = StmtParser::parse(Rule::file, &string_content)?;

        Ok(StmtParser::file(inputs.single().unwrap()).unwrap())
    }

    fn binop_helper(pairs: Pairs<Rule>) -> ParseResult<Expr> {
        PRATT
            .map_primary(|primary| {
                println!("{primary:#?}");
                match primary.as_rule() {
                    Rule::number => Ok(Expr::Number(Self::number(Node::new(primary))?)),
                    Rule::identifier => Ok(Expr::Ident(Self::identifier(Node::new(primary))?)),
                    Rule::expr => Self::expr(Node::new(primary)),
                    x => unreachable!("Unexpected rule {x:?} for expr"),
                }
            })
            .map_infix(|lhs, op, rhs| {
                {
                    Ok(match op.as_rule() {
                        Rule::add => Expr::Add(Box::new(lhs?), Box::new(rhs?)),
                        Rule::sub => Expr::Sub(Box::new(lhs?), Box::new(rhs?)),
                        _ => unreachable!(),
                    })
                }
            })
            .parse(pairs)
    }
}

#[pest_consume::parser]
impl StmtParser {
    fn num(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn int(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn identifier(input: Node) -> ParseResult<Id> {
        Ok(Id::new(input.as_str()))
    }

    fn number(input: Node) -> ParseResult<Number> {
        input
            .as_str()
            .parse::<u64>()
            .map(|n| Number::new(n))
            .map_err(|_| input.error("Expected valid u64"))
    }

    fn file(input: Node) -> ParseResult<Module> {
        Ok(match_nodes!(
            input.into_children();
            [module(module), EOI(_)] => module
        ))
    }

    fn module(input: Node) -> ParseResult<Module> {
        Ok(match_nodes!(
            input.into_children();
            [module_kv(pairs).., funcs(fs)] => Module {
                params: pairs.collect(),
                funcs: fs
            }
        ))
    }

    fn module_kv(input: Node) -> ParseResult<(Id, Id)> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(key), identifier(value)] => (key, value)
        ))
    }

    fn funcs(input: Node) -> ParseResult<Vec<Func>> {
        Ok(match_nodes!(
            input.into_children();
            [func(fs)..] => fs.collect()
        ))
    }

    fn func(input: Node) -> ParseResult<Func> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(metadata), identifier(fn_name), func_arg_list(args), block(block)] => Func {
                metadata,
                name: fn_name,
                args,
                stmts: block
            }
        ))
    }

    fn func_arg_list(input: Node) -> ParseResult<Vec<Id>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(ident)..] => ident.collect()
        ))
    }

    fn block(input: Node) -> ParseResult<Block> {
        Ok(match_nodes!(
            input.into_children();
            [stmt(stmt)..] => stmt.collect()
        ))
    }

    fn stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [let_stmt(s)] => s,
            [for_stmt(s)] => s,
            [if_stmt(s)] => s,
            [produce_stmt(s)] => s,
            [predicate_stmt(s)] => s
        ))
    }

    fn let_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), expr(expr)] => Stmt::Let { var, expr }
        ))
    }

    fn for_stmt(_input: Node) -> ParseResult<Stmt> {
        todo!()
    }

    fn if_stmt(_input: Node) -> ParseResult<Stmt> {
        todo!()
    }

    fn produce_stmt(_input: Node) -> ParseResult<Stmt> {
        todo!()
    }

    fn predicate_stmt(_input: Node) -> ParseResult<Stmt> {
        todo!()
    }

    fn expr(input: Node) -> ParseResult<Expr> {
        Ok(match_nodes!(
            input.into_children();
            [binop(expr)] => expr,
            [funcall(expr)] => expr,
            [cast_expr(expr)] => expr
        ))
    }

    fn binop(input: Node) -> ParseResult<Expr> {
        Self::binop_helper(input.into_pair().into_inner())
    }

    fn add(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn sub(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn mul(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn div(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn modulo(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn funcall(input: Node) -> ParseResult<Expr> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(name)] => Expr::FunCall(name, vec![]),
            [identifier(name), funcall_args(args)] => Expr::FunCall(name, args)
        ))
    }

    fn funcall_args(input: Node) -> ParseResult<Vec<Expr>> {
        Ok(match_nodes!(
            input.into_children();
            [expr(e)..] => e.collect()
        ))
    }

    fn cast_expr(input: Node) -> ParseResult<Expr> {
        Ok(match_nodes!(
            input.into_children();
            [cast_args(args), expr(expr)] => Expr::Cast(args, Box::new(expr)),
            [expr(expr)] => Expr::Cast(vec![], Box::new(expr)),
        ))
    }

    fn ptr(input: Node) -> ParseResult<Id> {
        Ok(Id::new("*"))
    }

    fn cast_args(input: Node) -> ParseResult<Vec<Id>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id)..] => id.collect(),
            [identifier(id).., ptr(ptr)] => id.chain(vec![ptr].into_iter()).collect()
        ))
    }

    fn EOI(_input: Node) -> ParseResult<()> {
        Ok(())
    }
}
