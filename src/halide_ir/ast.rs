use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Id {
    name: String,
}

#[derive(Debug)]
pub struct Number {
    value: u64,
}

impl Id {
    pub fn new<S: ToString>(name: S) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl Number {
    pub fn new(value: u64) -> Self {
        Self { value }
    }
}

#[derive(Debug)]
pub struct Module {
    pub params: HashMap<Id, Id>,
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct Func {
    pub metadata: Id,
    pub name: Id,
    pub args: Vec<Id>,
    pub stmts: Block,
}

pub type Block = Vec<Stmt>;

#[derive(Debug)]
pub enum Stmt {
    Let { var: Id, expr: Expr },
    For { var: Expr, low: Expr, high: Expr },
    If { cond: Expr, tru: Block, fals: Block },
    Produce { var: Id, block: Block },
    Predicate,
}

#[derive(Debug)]
pub enum Expr {
    // Base cases
    Number(Number),
    Ident(Id),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    // function calls
    FunCall(Id, Vec<Expr>),

    // Casts
    Cast(Vec<Id>, Box<Expr>),
    StructCast(Id, Id, Box<Expr>),

    // placeholder expr
    TODO,
}
