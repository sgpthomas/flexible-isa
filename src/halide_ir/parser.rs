use std::{fs, path::Path};

use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_consume::{match_nodes, Parser};

use super::ast::{Access, Block, DeviceApi, Expr, Func, Id, MemoryType, Module, Number, Stmt};

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
        .op(Op::infix(Rule::modulo, Assoc::Left))
        .op(Op::infix(Rule::lt, Assoc::Left))
        .op(Op::infix(Rule::lte, Assoc::Left))
        .op(Op::infix(Rule::eq, Assoc::Left))
        .op(Op::infix(Rule::neq, Assoc::Left))
        .op(Op::infix(Rule::gte, Assoc::Left))
        .op(Op::infix(Rule::gt, Assoc::Left))
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::if_infx, Assoc::Left))
        .op(Op::infix(Rule::struct_member, Assoc::Left))
        .op(Op::prefix(Rule::neg));
}

#[derive(pest_consume::Parser)]
#[grammar = "halide_ir/stmt.pest"]
pub struct StmtParser;

impl StmtParser {
    pub fn parse_file(path: &Path) -> anyhow::Result<Module> {
        let raw_content = fs::read(path)?;
        let string_content = std::str::from_utf8(&raw_content)?.to_string();

        let inputs = StmtParser::parse(Rule::file, &string_content)?;

        Ok(StmtParser::file(inputs.single().unwrap())?)
    }

    fn struct_member(lhs: Expr, rhs: Expr, span: pest::Span<'_>) -> ParseResult<Expr> {
        Expr::struct_member(lhs, rhs).map_err(|e| {
            pest_consume::Error::new_from_span(
                pest::error::ErrorVariant::CustomError {
                    message: e.to_string(),
                },
                span,
            )
        })
    }

    fn expr_pratt(pairs: Pairs<Rule>) -> ParseResult<Expr> {
        PRATT
            .map_primary(|primary| match primary.as_rule() {
                Rule::reinterpret => Self::reinterpret(Node::new(primary)),
                Rule::funcall => Self::funcall(Node::new(primary)),
                Rule::cast_expr => Self::cast_expr(Node::new(primary)),
                Rule::access_expr => Ok(Expr::Access(Self::access_expr(Node::new(primary))?, ())),
                Rule::number => Ok(Expr::Number(Self::number(Node::new(primary))?, ())),
                Rule::identifier => Ok(Expr::Ident(Self::identifier(Node::new(primary))?, ())),
                Rule::let_expr => Self::let_expr(Node::new(primary)),
                Rule::expr => Self::expr(Node::new(primary)),
                x => unreachable!("Unexpected rule `{x:?}` for primary {primary:#?}"),
            })
            .map_prefix(|op, rhs| {
                Ok(match op.as_rule() {
                    Rule::neg => Expr::neg(rhs?),
                    x => unreachable!("Unexpected prefix `{x:?}"),
                })
            })
            .map_infix(|lhs, op, rhs| {
                Ok(match op.as_rule() {
                    Rule::add => Expr::add(lhs?, rhs?),
                    Rule::sub => Expr::sub(lhs?, rhs?),
                    Rule::mul => Expr::mul(lhs?, rhs?),
                    Rule::div => Expr::div(lhs?, rhs?),
                    Rule::modulo => Expr::modulo(lhs?, rhs?),
                    Rule::lt => Expr::lt(lhs?, rhs?),
                    Rule::lte => Expr::lte(lhs?, rhs?),
                    Rule::eq => Expr::eq(lhs?, rhs?),
                    Rule::neq => Expr::neq(lhs?, rhs?),
                    Rule::gte => Expr::gte(lhs?, rhs?),
                    Rule::gt => Expr::gt(lhs?, rhs?),
                    Rule::and => Expr::and(lhs?, rhs?),
                    Rule::or => Expr::or(lhs?, rhs?),
                    Rule::if_infx => Expr::If(Box::new(lhs?), Box::new(rhs?), ()),
                    Rule::struct_member => Self::struct_member(lhs?, rhs?, op.as_span())?,
                    x => unreachable!("Unexpected infix operator: `{x:?}`"),
                })
            })
            .parse(pairs)
    }
}

#[pest_consume::parser]
impl StmtParser {
    fn identifier(input: Node) -> ParseResult<Id> {
        Ok(Id::new(input.as_str()))
    }

    fn number(input: Node) -> ParseResult<Number> {
        Ok(match_nodes!(
            input.into_children();
            [int(value)] => Number::new_int(value),
            [float(value)] => Number::new_float(value)
        ))
    }

    fn int(input: Node) -> ParseResult<u64> {
        input
            .as_str()
            .parse::<u64>()
            .map_err(|_| input.error("Expected valid u64"))
    }

    fn float(input: Node) -> ParseResult<String> {
        input
            .as_str()
            .strip_suffix('f')
            .map(ToString::to_string)
            .ok_or_else(|| input.error("Float literal missing `f` suffix"))
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
                funcs: fs,
                data: ()
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
                stmts: block,
                data: ()
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
            [consume_stmt(s)] => s,
            [predicate_stmt(s)] => s,
            [update_stmt(s)] => s,
            [free_stmt(s)] => s,
            [allocate_stmt(s)] => s,
            [expr(e)] => Stmt::Expr(e, ())
        ))
    }

    fn let_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), expr(expr)] => Stmt::Let { var, expr, data: () },
            [identifier(var), expr(binding), expr(body)] => Stmt::Expr(
                Expr::LetIn(var, Box::new(binding), Box::new(body), ()),
                ()
            )
        ))
    }

    fn for_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), expr(low), expr(high), block(body)] => Stmt::For {
                var,
                low,
                high,
                body,
                device: DeviceApi::None,
                data: ()
            },
            [device_api(device), identifier(var), expr(low), expr(high), block(body)] => Stmt::For {
                var,
                low,
                high,
                body,
                device,
                data: ()
            },
        ))
    }

    fn device_api(input: Node) -> ParseResult<DeviceApi> {
        Ok(match_nodes!(
            input.into_children();
            [none(_)] => DeviceApi::None,
            [host(_)] => DeviceApi::Host,
            [default_gpu(_)] => DeviceApi::DefaultGPU,
            [cuda(_)] => DeviceApi::Cuda,
            [open_cl(_)] => DeviceApi::OpenCL,
            [metal(_)] => DeviceApi::Metal,
            [hexagon(_)] => DeviceApi::Hexagon,
            [hexagon_dma(_)] => DeviceApi::HexagonDma,
            [d3d12_compute(_)] => DeviceApi::D3D12Compute,
            [vulkan(_)] => DeviceApi::Vulkan,
            [web_gpu(_)] => DeviceApi::WebGPU,
        ))
    }

    fn none(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn host(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn default_gpu(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn cuda(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn open_cl(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn metal(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn hexagon(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn hexagon_dma(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn d3d12_compute(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn vulkan(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn web_gpu(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn if_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [expr(cond), block(tru)] => Stmt::If { cond, tru, fls: None, data: () },
            [expr(cond), block(tru), block(fls)] => Stmt::If { cond, tru, fls: Some(fls), data: () },
            [expr(cond), block(tru), if_stmt(fls)] => Stmt::If { cond, tru, fls: Some(vec![fls]), data: () }
        ))
    }

    fn produce_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), block(body)] => Stmt::Produce { var, body, data: () }
        ))
    }

    fn consume_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), block(body)] => Stmt::Consume { var, body, data: () }
        ))
    }

    fn predicate_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [expr(cond), stmt(stmt)] => Stmt::Predicate { cond, stmt: Box::new(stmt), data: () }
        ))
    }

    fn update_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [access_expr(access), expr(value)] => Stmt::Store { access, value, data: () }
        ))
    }

    fn allocate_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(name), identifier(typ), expr(extents)..] => Stmt::Allocate {
                name,
                typ,
                extents: extents.collect(),
                loc: MemoryType::Auto,
                condition: None,
                data: ()
            },
            [identifier(name), identifier(typ), expr(extents).., memory_type(loc)] => Stmt::Allocate {
                name,
                typ,
                extents: extents.collect(),
                loc,
                condition: None,
                data: ()
            },
            [identifier(name), identifier(typ), expr(extents).., expr(condition)] => Stmt::Allocate {
                name,
                typ,
                extents: extents.collect(),
                loc: MemoryType::Auto,
                condition: Some(condition),
                data: ()
            },
            [identifier(name), identifier(typ), expr(extents).., memory_type(loc), expr(condition)] => Stmt::Allocate {
                name,
                typ,
                extents: extents.collect(),
                loc,
                condition: Some(condition),
                data: ()
            },
        ))
    }

    fn memory_type(input: Node) -> ParseResult<MemoryType> {
        Ok(match_nodes!(
            input.into_children();
            [auto(_)] => MemoryType::Auto,
            [heap(_)] => MemoryType::Heap,
            [stack(_)] => MemoryType::Stack,
            [register(_)] => MemoryType::Register,
            [gpu_shared(_)] => MemoryType::GPUShared,
            [gpu_texture(_)] => MemoryType::GPUTexture,
            [locked_cache(_)] => MemoryType::LockedCache,
            [vtcm(_)] => MemoryType::Vtcm,
            [amx_tile(_)] => MemoryType::AMXTile,
        ))
    }

    fn auto(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn heap(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn stack(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn register(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn gpu_shared(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn gpu_texture(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn locked_cache(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn vtcm(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn amx_tile(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn free_stmt(input: Node) -> ParseResult<Stmt> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var)] => Stmt::Free { var, data: () }
        ))
    }

    fn expr(input: Node) -> ParseResult<Expr> {
        Self::expr_pratt(input.into_pair().into_inner())
    }

    fn neg(input: Node) -> ParseResult<()> {
        Ok(())
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

    fn lt(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn lte(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn eq(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn neq(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn gte(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn gt(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn and(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn or(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn if_infx(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn reinterpret(input: Node) -> ParseResult<Expr> {
        Ok(match_nodes!(
            input.into_children();
            [cast_args(cast), funcall_args(args)] => Expr::Reinterpret(cast.0, args, ())
        ))
    }

    fn funcall(input: Node) -> ParseResult<Expr> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(name)] => Expr::FunCall(name, vec![], ()),
            [identifier(name), funcall_args(args)] => Expr::FunCall(name, args, ())
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
            [cast_args(args), expr(expr)] => {
                let (args, ptr) = args;
                if ptr {
                    Expr::PtrCast(args, Box::new(expr), ())
                } else {
                    Expr::Cast(args[0].clone(), Box::new(expr), ())
                }
            }
        ))
    }

    fn ptr(input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn cast_args(input: Node) -> ParseResult<(Vec<Id>, bool)> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id)..] => (id.collect(), false),
            [identifier(id).., ptr(_)] => (id.collect(), true)
        ))
    }

    fn access_expr(input: Node) -> ParseResult<Access> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), expr(idx)] => Access { var, idx: Box::new(idx), align: None },
            [identifier(var), expr(idx), int(low), int(hi)] => Access {
                var,
                idx: Box::new(idx),
                align: Some((low, hi))
            }
        ))
    }

    fn let_expr(input: Node) -> ParseResult<Expr> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(var), expr(binding), expr(body)] => Expr::LetIn(var, Box::new(binding), Box::new(body), ())
        ))
    }

    fn EOI(_input: Node) -> ParseResult<()> {
        Ok(())
    }
}
