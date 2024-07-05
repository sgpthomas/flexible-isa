use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Id {
    pub name: String,
}

#[derive(Debug)]
pub struct Number {
    pub value: u64,
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
    Let {
        var: Id,
        expr: Expr,
    },
    For {
        var: Expr,
        low: Expr,
        high: Expr,
        body: Block,
    },
    If {
        cond: Expr,
        tru: Block,
        fls: Option<Block>,
    },
    Produce {
        var: Id,
        body: Block,
    },
    Predicate {
        cond: Expr,
        stmt: Box<Stmt>,
    },
    Update {
        array: Id,
        idx: Expr,
        value: Expr,
    },
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    // Base cases
    Number(Number),
    Ident(Id),

    // arith operators
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Modulo(Box<Expr>, Box<Expr>),

    // comparison operators
    Lt(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Gte(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>),

    // function calls
    FunCall(Id, Vec<Expr>),

    // Casts
    Cast(Vec<Id>, Box<Expr>),

    // array access
    Access(Id, Box<Expr>),
}
