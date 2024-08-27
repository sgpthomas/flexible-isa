use std::{convert::Infallible, fmt::Display, str::FromStr};

use babble::PartialExpr;
use itertools::Itertools;

use crate::halide_ir::{ast, HalideType};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BabbleOp {
    List,
    Symbol(egg::Symbol),
    /// An anonymous function
    Lambda,
    /// Apply a babble function
    Apply,
    /// A DeBrujn indexed lambda variable
    LambdaVar(babble::DeBruijnIndex),
    /// A library function binding
    Lib(babble::LibId),
    /// A reference to a lib var
    LibVar(babble::LibId),
}

pub type HalideLang = babble::AstNode<HalideExprOp>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HalideExprOp {
    // Base cases
    Number(ast::Number),
    Ident(ast::Id),

    // arithmetic operators
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Modulo,

    // comparison operators
    Lt,
    Lte,
    Eq,
    Neq,
    Gte,
    Gt,
    And,
    Or,

    // other operators
    If,
    StructMember,

    // function calls
    FunCall(ast::Id),
    Reinterpret(Vec<ast::Id>),

    // casts
    Cast(Vec<ast::Id>),

    // array access
    Access,

    // Instructions
    Instruction(u64),

    Named(u64),

    /// Babble necessary operations
    Babble(BabbleOp),
    PatternVar(egg::Var),
}

enum HalideExprOpParseError {
    Cast,
    Call,
    Reinterpret,
}

impl HalideExprOp {
    fn parse_cast(input: &str) -> Result<Self, HalideExprOpParseError> {
        input
            .strip_prefix("cast<")
            .and_then(|x| x.strip_suffix('>'))
            .ok_or(HalideExprOpParseError::Cast)
            .map(|typs| {
                typs.split(' ')
                    .map(HalideType::from_str)
                    .map(|x| HalideType::to_id(&x))
                    .collect_vec()
            })
            .map(Self::Cast)
    }

    fn parse_call(input: &str) -> Result<Self, HalideExprOpParseError> {
        input
            .strip_prefix("call[")
            .and_then(|x| x.strip_suffix(']'))
            .ok_or(HalideExprOpParseError::Call)
            .map(ast::Id::new)
            .map(Self::FunCall)
    }

    fn parse_reinterpret(input: &str) -> Result<Self, HalideExprOpParseError> {
        input
            .strip_prefix("reinterpret<")
            .and_then(|x| x.strip_suffix('>'))
            .ok_or(HalideExprOpParseError::Reinterpret)
            .map(|typs| {
                typs.split(' ')
                    .map(HalideType::from_str)
                    .map(|x| HalideType::to_id(&x))
                    .collect_vec()
            })
            .map(Self::Reinterpret)
    }

    /// Operations that we need to have instructions for.
    pub fn essential_operations() -> impl Iterator<Item = (Self, usize)> {
        [
            (HalideExprOp::Neg, 1),
            (HalideExprOp::Add, 2),
            (HalideExprOp::Sub, 2),
            (HalideExprOp::Mul, 2),
            (HalideExprOp::Div, 2),
            (HalideExprOp::Modulo, 2),
            (HalideExprOp::Lt, 2),
            (HalideExprOp::Lte, 2),
            (HalideExprOp::Eq, 2),
            (HalideExprOp::Neq, 2),
            (HalideExprOp::Gte, 2),
            (HalideExprOp::Gt, 2),
            (HalideExprOp::And, 2),
            (HalideExprOp::Or, 2),
        ]
        .into_iter()
    }

    /// Produce a partial expression of `arity` for `self` of the form:
    ///   `(op ?0 .. ?arity)`
    pub fn partial_expr(self, arity: usize) -> PartialExpr<Self, egg::Var> {
        PartialExpr::Node(babble::AstNode::new(
            self,
            (0..arity).map(|i| PartialExpr::Hole(egg::Var::from_u32(i as u32))),
        ))
    }
}

impl FromStr for HalideExprOp {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            // arithmetic operators
            "neg" => Self::Neg,
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "%" => Self::Modulo,

            // comparison operators
            "<" => Self::Lt,
            "<=" => Self::Lte,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            ">=" => Self::Gte,
            ">" => Self::Gt,
            "&&" => Self::And,
            "||" => Self::Or,

            // other operators
            "if" => Self::If,
            "::" => Self::StructMember,

            // array access
            "get" => Self::Access,

            // anything with arguments
            // constants, casts, fun_calls, reinterprets, && babble ops
            // tries parsing in order, if something fails, try the next thing
            // if nothing succeeds, default to BabbleOp::Symbol
            input => input
                .parse()
                .map(|value| Self::Number(ast::Number::new_int(value)))
                .or_else(|_| Self::parse_cast(input))
                .or_else(|_| Self::parse_call(input))
                .or_else(|_| Self::parse_reinterpret(input))
                .or_else(|_| input.parse().map(|x| Self::Babble(BabbleOp::LambdaVar(x))))
                .or_else(|_| input.parse().map(|x| Self::Babble(BabbleOp::LibVar(x))))
                .or_else(|_| {
                    input
                        .strip_prefix("lib ")
                        .ok_or(babble::ParseLibIdError::NoLeadingL)
                        .and_then(|x| x.parse().map(|x| Self::Babble(BabbleOp::Lib(x))))
                })
                .unwrap_or_else(|_| Self::Babble(BabbleOp::Symbol(input.into()))),
        })
    }
}

impl Display for BabbleOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BabbleOp::List => f.write_str("bab:list"),
            BabbleOp::Symbol(sym) => write!(f, "bab:{sym}"),
            BabbleOp::Lambda => f.write_str("bab:lambda"),
            BabbleOp::Apply => f.write_str("bab:apply"),
            BabbleOp::LambdaVar(didx) => write!(f, "bab:{didx}"),
            BabbleOp::Lib(lib_id) => write!(f, "bab:{lib_id}"),
            BabbleOp::LibVar(lib_id) => write!(f, "bab:{lib_id}"),
        }
    }
}

impl Display for HalideExprOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HalideExprOp::Number(ast::Number::Int(value)) => f.write_str(&value.to_string()),
            HalideExprOp::Number(ast::Number::Float(value)) => write!(f, "{value}f"),
            HalideExprOp::Ident(ast::Id { name }) => f.write_str(name),
            HalideExprOp::Neg => f.write_str("neg"),
            HalideExprOp::Add => f.write_str("+"),
            HalideExprOp::Sub => f.write_str("-"),
            HalideExprOp::Mul => f.write_str("*"),
            HalideExprOp::Div => f.write_str("/"),
            HalideExprOp::Modulo => f.write_str("%"),
            HalideExprOp::Lt => f.write_str("<"),
            HalideExprOp::Lte => f.write_str("<="),
            HalideExprOp::Eq => f.write_str("=="),
            HalideExprOp::Neq => f.write_str("!="),
            HalideExprOp::Gte => f.write_str(">="),
            HalideExprOp::Gt => f.write_str(">"),
            HalideExprOp::And => f.write_str("&&"),
            HalideExprOp::Or => f.write_str("||"),
            HalideExprOp::If => f.write_str("if"),
            HalideExprOp::StructMember => f.write_str("::"),
            HalideExprOp::FunCall(ast::Id { name }) => write!(f, "call[{name}]"),
            HalideExprOp::Reinterpret(ids) => {
                write!(
                    f,
                    "reinterpret<{}>",
                    ids.iter()
                        .map(|id| id.name.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            HalideExprOp::Cast(ids) => {
                write!(
                    f,
                    "cast<{}>",
                    ids.iter()
                        .map(|id| id.name.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            HalideExprOp::Access => f.write_str("get"),
            HalideExprOp::Instruction(i) => write!(f, "inst<{i}>"),
            HalideExprOp::Named(i) => write!(f, "{i}"),
            HalideExprOp::Babble(bab) => bab.fmt(f),
            HalideExprOp::PatternVar(v) => v.fmt(f),
        }
    }
}

impl babble::Arity for BabbleOp {
    fn min_arity(&self) -> usize {
        match self {
            BabbleOp::List => 0,
            BabbleOp::Symbol(_) => 0,
            BabbleOp::Lambda => 1,
            BabbleOp::Apply => 2,
            BabbleOp::LambdaVar(_) => 0,
            BabbleOp::Lib(_) => 2,
            BabbleOp::LibVar(_) => 0,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            BabbleOp::List => None,
            _ => Some(self.min_arity()),
        }
    }
}

impl babble::Arity for HalideExprOp {
    fn min_arity(&self) -> usize {
        match self {
            HalideExprOp::Number(_) => 0,
            HalideExprOp::Ident(_) => 0,
            HalideExprOp::Neg => 1,
            HalideExprOp::Add
            | HalideExprOp::Sub
            | HalideExprOp::Mul
            | HalideExprOp::Div
            | HalideExprOp::Modulo
            | HalideExprOp::Lt
            | HalideExprOp::Lte
            | HalideExprOp::Eq
            | HalideExprOp::Neq
            | HalideExprOp::Gte
            | HalideExprOp::Gt
            | HalideExprOp::And
            | HalideExprOp::Or
            | HalideExprOp::If
            | HalideExprOp::StructMember => 2,
            HalideExprOp::FunCall(_) => 1,
            HalideExprOp::Reinterpret(_) => 1,
            HalideExprOp::Cast(_) => 1,
            HalideExprOp::Access => 2,
            HalideExprOp::Instruction(_) => 0,
            HalideExprOp::Named(_) => 1,
            HalideExprOp::Babble(b) => b.min_arity(),
            HalideExprOp::PatternVar(_) => 0,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            HalideExprOp::FunCall(_)
            | HalideExprOp::Reinterpret(_)
            | HalideExprOp::Instruction(_) => None,
            HalideExprOp::Babble(b) => b.max_arity(),
            _ => Some(self.min_arity()),
        }
    }
}

/// A `babble::teachable::Teachable` trait that allows implementations to be
/// transitive. The problem with the existing implementation is that it is
/// impossible to implement `Teachable::as_binding_expr` that calls another
/// `Teachable::as_binding_expr` for some children of an enumeration because it
/// requires you to convert an `&AstNode<Op, T>` into another `&AstNode<Op2, T>`.
/// This trait solves that by defining the method on the parts of `AstNode` so that
/// you only have to convert `&Op` into `&Op2`.
trait TransitiveTeachable<T>: Sized {
    fn as_binding_expr_comm<'a>(op: &'a Self, parts: &'a [T])
        -> Option<babble::BindingExpr<&'a T>>;
}

impl<T> TransitiveTeachable<T> for BabbleOp {
    fn as_binding_expr_comm<'a>(
        op: &'a Self,
        parts: &'a [T],
    ) -> Option<babble::BindingExpr<&'a T>> {
        use babble::BindingExpr;
        match (op, parts) {
            (BabbleOp::LambdaVar(idx), []) => Some(BindingExpr::Var(*idx)),
            (BabbleOp::LibVar(lib_id), []) => Some(BindingExpr::LibVar(*lib_id)),
            (BabbleOp::Lambda, [body]) => Some(BindingExpr::Lambda(body)),
            (BabbleOp::Apply, [fun, arg]) => Some(BindingExpr::Apply(fun, arg)),
            (BabbleOp::Lib(lib_id), [bound_value, body]) => {
                Some(BindingExpr::Lib(*lib_id, bound_value, body))
            }
            _ => None,
        }
    }
}

impl<T> TransitiveTeachable<T> for HalideExprOp {
    fn as_binding_expr_comm<'a>(
        op: &'a Self,
        parts: &'a [T],
    ) -> Option<babble::BindingExpr<&'a T>> {
        match op {
            HalideExprOp::Babble(op) => BabbleOp::as_binding_expr_comm(op, parts),
            _ => None,
        }
    }
}

impl babble::Teachable for BabbleOp {
    fn from_binding_expr<T>(binding_expr: babble::BindingExpr<T>) -> babble::AstNode<Self, T> {
        use babble::BindingExpr;
        match binding_expr {
            BindingExpr::Var(idx) => babble::AstNode::leaf(BabbleOp::LambdaVar(idx)),
            BindingExpr::LibVar(lib_id) => babble::AstNode::leaf(BabbleOp::LibVar(lib_id)),
            BindingExpr::Lambda(body) => babble::AstNode::new(BabbleOp::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => babble::AstNode::new(BabbleOp::Apply, [fun, arg]),
            BindingExpr::Lib(lib_id, bound_value, body) => {
                babble::AstNode::new(BabbleOp::Lib(lib_id), [bound_value, body])
            }
        }
    }

    fn as_binding_expr<T>(node: &babble::AstNode<Self, T>) -> Option<babble::BindingExpr<&T>> {
        let (op, parts) = node.as_parts();
        Self::as_binding_expr_comm(op, parts)
    }

    fn list() -> Self {
        Self::List
    }
}

impl babble::Teachable for HalideExprOp {
    fn from_binding_expr<T>(binding_expr: babble::BindingExpr<T>) -> babble::AstNode<Self, T> {
        let (op, children) = BabbleOp::from_binding_expr(binding_expr).into_parts();
        babble::AstNode::new(Self::Babble(op), children)
    }

    fn as_binding_expr<T>(node: &babble::AstNode<Self, T>) -> Option<babble::BindingExpr<&T>> {
        let (op, parts) = node.as_parts();
        Self::as_binding_expr_comm(op, parts)
    }

    fn list() -> Self {
        HalideExprOp::Babble(BabbleOp::list())
    }
}

impl babble::Printable for BabbleOp {
    fn precedence(&self) -> babble::Precedence {
        todo!()
    }

    fn print_naked<W: std::fmt::Write>(
        _expr: &babble::Expr<Self>,
        _printer: &mut babble::Printer<W>,
    ) -> std::fmt::Result {
        todo!()
    }
}

impl babble::Printable for HalideExprOp {
    fn precedence(&self) -> babble::Precedence {
        todo!()
    }

    fn print_naked<W: std::fmt::Write>(
        _expr: &babble::Expr<Self>,
        _printer: &mut babble::Printer<W>,
    ) -> std::fmt::Result {
        todo!()
    }
}

/// Translate an `egg::Language` into and out of an `egg::Pattern`
pub trait PatternConvert: Sized + Clone + egg::Language {
    /// How to construct a pattern var from self
    fn construct_pattern_var(&self) -> egg::ENodeOrVar<Self>;

    /// How to construct Self from a pattern var
    fn deconstruct_pattern_var(var: egg::Var) -> Self;

    fn from_pattern(pat: &egg::Pattern<Self>) -> egg::RecExpr<Self> {
        pat.ast
            .as_ref()
            .iter()
            .map(|node| match node {
                egg::ENodeOrVar::ENode(node) => node.clone(),
                egg::ENodeOrVar::Var(v) => Self::deconstruct_pattern_var(*v),
            })
            .collect::<Vec<Self>>()
            .into()
    }

    fn to_pattern(pat: &egg::RecExpr<Self>) -> egg::Pattern<Self> {
        let rec_expr: egg::RecExpr<_> = pat
            .as_ref()
            .iter()
            .map(Self::construct_pattern_var)
            .collect::<Vec<_>>()
            .into();
        egg::Pattern::new(rec_expr)
    }
}

impl PatternConvert for babble::AstNode<HalideExprOp> {
    fn construct_pattern_var(&self) -> egg::ENodeOrVar<Self> {
        match self.operation() {
            HalideExprOp::PatternVar(v) => egg::ENodeOrVar::Var(*v),
            _ => egg::ENodeOrVar::ENode(self.clone()),
        }
    }

    fn deconstruct_pattern_var(var: egg::Var) -> Self {
        babble::AstNode::leaf(HalideExprOp::PatternVar(var))
    }
}

impl babble::DiscriminantEq for HalideExprOp {
    fn discriminant_eq(&self, other: &Self) -> bool {
        use HalideExprOp::*;
        match (self, other) {
            (Cast(_), Cast(_)) => true,
            (FunCall(_), FunCall(_)) => true,
            (Named(_), Named(_)) => true,
            (x, y) => x.eq(y),
        }
    }
}
