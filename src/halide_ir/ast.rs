use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    // Assert {
    //     condition: Expr,
    //     body: Box<Stmt>,
    // },
    Produce {
        var: Id,
        body: Block,
    },
    Consume {
        var: Id,
        body: Block,
    },
    Store {
        access: Access,
        value: Expr,
        // TODO: in halide, predicate is stored here
    },
    // Halide Provide
    Allocate {
        access: Access,
        loc: MemoryType,
        condition: Option<Expr>,
    },
    Free {
        var: Id,
    },
    // Fork
    For {
        var: Expr,
        low: Expr,
        high: Expr,
        device: DeviceApi,
        body: Block,
    },
    If {
        cond: Expr,
        tru: Block,
        fls: Option<Block>,
    },
    Predicate {
        cond: Expr,
        stmt: Box<Stmt>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    // Base cases
    Number(Number),
    Ident(Id),

    // arith operators
    Neg(Box<Expr>),
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
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>),

    // function calls
    FunCall(Id, Vec<Expr>),
    Reinterpret(Vec<Id>, Vec<Expr>),

    // Casts
    Cast(Vec<Id>, Box<Expr>),

    // array access
    Access(Access),

    // let in exprs
    LetIn(Id, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Access {
    pub var: Id,
    pub idx: Box<Expr>,
    pub align: Option<(u64, u64)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DeviceApi {
    /// Used to denote for loops that run on the same device as the containing code.
    None,
    Host,
    DefaultGPU,
    CUDA,
    OpenCL,
    Metal,
    Hexagon,
    HexagonDma,
    D3D12Compute,
    Vulkan,
    WebGPU,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemoryType {
    Auto,
    Heap,
    Stack,
    Register,
    GPUShared,
    GPUTexture,
    LockedCache,
    VTCM,
    AMXTile,
}
