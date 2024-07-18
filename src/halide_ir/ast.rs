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

#[derive(Debug, Clone)]
pub struct Module<T = ()> {
    pub params: HashMap<Id, Id>,
    pub funcs: Vec<Func<T>>,
    pub data: T,
}

#[derive(Debug, Clone)]
pub struct Func<T = ()> {
    pub metadata: Id,
    pub name: Id,
    pub args: Vec<Id>,
    pub stmts: Block<T>,
    pub data: T,
}

pub type Block<T = ()> = Vec<Stmt<T>>;

#[derive(Debug, Clone)]
pub enum Stmt<T = ()> {
    Let {
        var: Id,
        expr: Expr<T>,
        data: T,
    },
    // Assert {
    //     condition: Expr<T>,
    //     body: Box<Stmt>,
    // },
    Produce {
        var: Id,
        body: Block<T>,
        data: T,
    },
    Consume {
        var: Id,
        body: Block<T>,
        data: T,
    },
    Store {
        access: Access<T>,
        value: Expr<T>,
        data: T, // TODO: in halide, predicate is stored here
    },
    // Halide Provide
    Allocate {
        access: Access<T>,
        loc: MemoryType,
        condition: Option<Expr<T>>,
        data: T,
    },
    Free {
        var: Id,
        data: T,
    },
    // Fork
    For {
        var: Expr<T>,
        low: Expr<T>,
        high: Expr<T>,
        device: DeviceApi,
        body: Block<T>,
        data: T,
    },
    If {
        cond: Expr<T>,
        tru: Block<T>,
        fls: Option<Block<T>>,
        data: T,
    },
    Predicate {
        cond: Expr<T>,
        stmt: Box<Stmt<T>>,
        data: T,
    },
    Expr(Expr<T>, T),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr<T = ()> {
    // Base cases
    Number(Number, T),
    Ident(Id, T),

    Unop(Unop, Box<Expr<T>>, T),
    ArithBinop(ArithBinop, Box<Expr<T>>, Box<Expr<T>>, T),
    CompBinop(CompBinop, Box<Expr<T>>, Box<Expr<T>>, T),

    // Not sure what the semnatics of this are
    If(Box<Expr<T>>, Box<Expr<T>>, T),

    // function calls
    FunCall(Id, Vec<Expr<T>>, T),
    Reinterpret(Vec<Id>, Vec<Expr<T>>, T),

    // Casts
    Cast(Id, Box<Expr<T>>, T),
    PtrCast(Vec<Id>, Box<Expr<T>>, T),

    // array access
    Access(Access, T),

    // let in exprs
    LetIn(Id, Box<Expr<T>>, Box<Expr<T>>, T),
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

pub trait Annotation<T> {
    fn data(&self) -> &T;
}

impl<T> Annotation<T> for Module<T> {
    fn data(&self) -> &T {
        &self.data
    }
}

impl<T> Annotation<T> for Func<T> {
    fn data(&self) -> &T {
        &self.data
    }
}

impl<T> Annotation<T> for Stmt<T> {
    fn data(&self) -> &T {
        match &self {
            Stmt::Let { data, .. } => data,
            Stmt::Produce { data, .. } => data,
            Stmt::Consume { data, .. } => data,
            Stmt::Store { data, .. } => data,
            Stmt::Allocate { data, .. } => data,
            Stmt::Free { data, .. } => data,
            Stmt::For { data, .. } => data,
            Stmt::If { data, .. } => data,
            Stmt::Predicate { data, .. } => data,
            Stmt::Expr(_, data) => data,
        }
    }
}

impl<T> Annotation<T> for Expr<T> {
    fn data(&self) -> &T {
        match self {
            Expr::Number(_, data) => data,
            Expr::Ident(_, data) => data,
            Expr::Unop(_, _, data) => data,
            Expr::ArithBinop(_, _, _, data) => data,
            Expr::CompBinop(_, _, _, data) => data,
            Expr::If(_, _, data) => data,
            Expr::FunCall(_, _, data) => data,
            Expr::Reinterpret(_, _, data) => data,
            Expr::Cast(_, _, data) => data,
            Expr::PtrCast(_, _, data) => data,
            Expr::Access(_, data) => data,
            Expr::LetIn(_, _, _, data) => data,
        }
    }
}
