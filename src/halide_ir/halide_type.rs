use serde::{Deserialize, Serialize};

use super::{ast, Annotation};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum HalideType {
    Unknown,
    // AnyNumber,
    Unsigned(u64),
    Signed(u64),
    Bool,
    Vec(u64, Box<HalideType>),
    Ptr(Vec<ast::Id>),
}

impl HalideType {
    pub fn union(&mut self, other: &mut Self) -> Self {
        use HalideType::*;
        match (&*self, &*other) {
            (&Unknown, _) | (_, &Unknown) => Unknown,
            (x, y) if x == y => x.clone(),
            _ => Unknown,
        }
    }

    pub fn from_str(typ: &str) -> Self {
        use HalideType::*;
        match typ {
            "uint1" => Unsigned(1),
            "uint8" => Unsigned(8),
            "uint16" => Unsigned(16),
            "uint32" => Unsigned(32),
            "uint64" => Unsigned(64),
            "uint128" => Unsigned(128),
            "int" => Signed(32),
            "int1" => Signed(1),
            "int8" => Signed(8),
            "int16" => Signed(16),
            "int32" => Signed(32),
            "int64" => Signed(64),
            "int128" => Signed(128),
            // vector types are written `int16x128`
            // so to parse them, we split at the x
            // reparse the left side, and parse the right side
            // as a u64
            typ if typ.contains('x') => typ
                .split_once('x')
                .and_then(|(typ, lanes)| {
                    lanes
                        .parse::<u64>()
                        .ok()
                        .map(|lanes| (Self::from_str(typ), lanes))
                })
                .map(|(typ, lanes)| Vec(lanes, Box::new(typ)))
                .unwrap_or(Unknown),
            _ => Unknown,
        }
    }

    pub fn from_id(id: &ast::Id) -> Self {
        Self::from_str(&id.name)
    }

    pub fn to_id(&self) -> ast::Id {
        use HalideType::*;
        match self {
            Unknown => ast::Id::new("unknown"),
            // AnyNumber => ast::Id::new("number"),
            Unsigned(n) => ast::Id::new(format!("uint{n}")),
            Signed(n) => ast::Id::new(format!("int{n}")),
            Bool => ast::Id::new("bool"),
            Vec(n, typ) => ast::Id::new(format!("{}x{n}", typ.to_id().name)),
            Ptr(typs) => ast::Id::new(format!(
                "{} *",
                typs.iter()
                    .map(|id| id.name.to_string())
                    .collect::<std::vec::Vec<_>>()
                    .join(" ")
            )),
        }
    }

    pub fn bits(&self) -> u64 {
        match self {
            HalideType::Unknown => 0,
            HalideType::Unsigned(x) => *x,
            HalideType::Signed(x) => *x,
            HalideType::Bool => 1,
            HalideType::Vec(_, typ) => typ.bits(),
            HalideType::Ptr(_) => 8,
        }
    }

    pub fn lanes(&self) -> u64 {
        match self {
            HalideType::Vec(width, _) => *width,
            _ => 1,
        }
    }

    pub fn widen(&self) -> Self {
        // XXX: technically widening a 1-bit type should produce an 8-bit type
        match self {
            HalideType::Unsigned(x) => HalideType::Unsigned(x * 2),
            HalideType::Signed(x) => HalideType::Signed(x * 2),
            HalideType::Vec(width, vec_typ) => HalideType::Vec(*width, Box::new(vec_typ.widen())),
            HalideType::Unknown | HalideType::Bool | HalideType::Ptr(_) => self.clone(),
        }
    }

    pub fn signed(self) -> Self {
        match self {
            x @ (HalideType::Unknown
            // | HalideType::AnyNumber
            | HalideType::Signed(_)
            | HalideType::Bool
            | HalideType::Ptr(_)) => x,

            HalideType::Unsigned(bits) => HalideType::Signed(bits),
            HalideType::Vec(lanes, typ) => HalideType::Vec(lanes, Box::new(typ.signed())),
        }
    }
}

pub trait MatchWidth {
    fn match_width<F>(&self, other: &Self, f: F) -> bool
    where
        F: Fn(u64, u64) -> bool;

    fn match_sign(&self, other: &Self) -> bool;
}

impl MatchWidth for ast::Expr<HalideType> {
    fn match_width<F>(&self, other: &Self, f: F) -> bool
    where
        F: Fn(u64, u64) -> bool,
    {
        f(self.data().bits(), other.data().bits())
    }

    fn match_sign(&self, other: &Self) -> bool {
        matches!(
            (self.data(), other.data()),
            (HalideType::Signed(_), HalideType::Signed(_))
                | (HalideType::Unsigned(_), HalideType::Unsigned(_))
        )
    }
}
