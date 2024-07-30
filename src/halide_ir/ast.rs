use derive_deftly::Deftly;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Deftly)]
#[derive_deftly(Annotation)]
pub struct Module<T = ()> {
    pub params: HashMap<Id, Id>,
    pub funcs: Vec<Func<T>>,
    #[deftly(data)]
    pub data: T,
}

#[derive(Debug, Clone, Deftly)]
#[derive_deftly(Annotation)]
pub struct Func<T = ()> {
    pub metadata: Id,
    pub name: Id,
    pub args: Vec<Id>,
    pub stmts: Block<T>,
    #[deftly(data)]
    pub data: T,
}

pub type Block<T = ()> = Vec<Stmt<T>>;

#[derive(Debug, Clone, Deftly)]
#[derive_deftly(Annotation)]
pub enum Stmt<T = ()> {
    Let {
        var: Id,
        expr: Expr<T>,
        #[deftly(data)]
        data: T,
    },
    // Assert {
    //     condition: Expr<T>,
    //     body: Box<Stmt>,
    // },
    Produce {
        var: Id,
        body: Block<T>,
        #[deftly(data)]
        data: T,
    },
    Consume {
        var: Id,
        body: Block<T>,
        #[deftly(data)]
        data: T,
    },
    Store {
        access: Access<T>,
        value: Expr<T>,
        #[deftly(data)]
        data: T, // TODO: in halide, predicate is stored here
    },
    // Halide Provide
    Allocate {
        name: Id,
        typ: Id,
        extents: Vec<Expr<T>>,
        loc: MemoryType,
        condition: Option<Expr<T>>,
        #[deftly(data)]
        data: T,
    },
    Free {
        var: Id,
        #[deftly(data)]
        data: T,
    },
    // Fork
    For {
        var: Id,
        low: Expr<T>,
        high: Expr<T>,
        device: DeviceApi,
        body: Block<T>,
        #[deftly(data)]
        data: T,
    },
    If {
        cond: Expr<T>,
        tru: Block<T>,
        fls: Option<Block<T>>,
        #[deftly(data)]
        data: T,
    },
    Predicate {
        cond: Expr<T>,
        stmt: Box<Stmt<T>>,
        #[deftly(data)]
        data: T,
    },
    Expr(Expr<T>, #[deftly(data)] T),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unop {
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ArithBinop {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CompBinop {
    Lt,
    Lte,
    Eq,
    Neq,
    Gte,
    Gt,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deftly)]
#[derive_deftly(Annotation)]
pub enum Expr<T = ()> {
    // Base cases
    Number(Number, #[deftly(data)] T),
    Ident(Id, #[deftly(data)] T),

    Unop(Unop, Box<Expr<T>>, #[deftly(data)] T),
    ArithBinop(ArithBinop, Box<Expr<T>>, Box<Expr<T>>, #[deftly(data)] T),
    CompBinop(CompBinop, Box<Expr<T>>, Box<Expr<T>>, #[deftly(data)] T),

    // Not sure what the semantics of this are
    If(Box<Expr<T>>, Box<Expr<T>>, #[deftly(data)] T),
    StructMember(Box<Expr<T>>, Box<Expr<T>>, #[deftly(data)] T),

    // function calls
    FunCall(Id, Vec<Expr<T>>, #[deftly(data)] T),
    Reinterpret(Vec<Id>, Vec<Expr<T>>, #[deftly(data)] T),

    // Casts
    Cast(Id, Box<Expr<T>>, #[deftly(data)] T),
    PtrCast(Vec<Id>, Box<Expr<T>>, #[deftly(data)] T),

    // array access
    Access(Access<T>, #[deftly(data)] T),

    // let in exprs
    LetIn(Id, Box<Expr<T>>, Box<Expr<T>>, #[deftly(data)] T),
}

impl<T> Expr<T>
where
    T: Default,
{
    pub fn neg(inner: Expr<T>) -> Expr<T> {
        Expr::Unop(Unop::Neg, Box::new(inner), T::default())
    }

    pub fn add(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::ArithBinop(ArithBinop::Add, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn sub(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::ArithBinop(ArithBinop::Sub, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn mul(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::ArithBinop(ArithBinop::Mul, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn div(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::ArithBinop(ArithBinop::Div, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn modulo(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::ArithBinop(
            ArithBinop::Modulo,
            Box::new(lhs),
            Box::new(rhs),
            T::default(),
        )
    }

    pub fn lt(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Lt, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn lte(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Lte, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn eq(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Eq, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn neq(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Neq, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn gte(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Gte, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn gt(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Gt, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn and(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::And, Box::new(lhs), Box::new(rhs), T::default())
    }

    pub fn or(lhs: Expr<T>, rhs: Expr<T>) -> Expr<T> {
        Expr::CompBinop(CompBinop::Or, Box::new(lhs), Box::new(rhs), T::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Access<T = ()> {
    pub var: Id,
    pub idx: Box<Expr<T>>,
    pub align: Option<(u64, u64)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DeviceApi {
    /// Used to denote for loops that run on the same device as the containing code.
    None,
    Host,
    DefaultGPU,
    Cuda,
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
    Vtcm,
    AMXTile,
}
