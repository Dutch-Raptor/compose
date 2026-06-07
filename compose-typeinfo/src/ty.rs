use crate::intern::{InterfaceName, TypeName};
use std::fmt;

/// A cheap, copyable ID for a type inference variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub u32);

/// Stable identifier reserved for future type interning.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(pub u32);

/// Public alias used by APIs that want to name inference variables explicitly.
pub type TypeVarId = TypeVar;

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?T{}", self.0)
    }
}

/// Generates fresh TypeVars with monotonically increasing IDs.
#[derive(Default)]
pub struct TyVarGen(u32);

impl TyVarGen {
    pub fn fresh(&mut self) -> TypeVar {
        let id = self.0;
        self.0 += 1;
        TypeVar(id)
    }

    pub fn fresh_ty(&mut self) -> Ty {
        Ty::Var(self.fresh())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// An unsolved inference variable.
    Var(TypeVar),

    Int,
    Bool,
    Float,
    Str,
    Unit,

    /// A named type applied to type arguments: Array<T>, Option<T>, MyStruct<A, B>.
    App(TypeName, Vec<Ty>),

    /// A fixed-arity function type: fn(A, B) -> C.
    Fn(Vec<Ty>, Box<Ty>),

    /// A variadic function type: fn(A, B, T...) -> C.
    VariadicFn {
        params: Vec<Ty>,
        variadic: Box<Ty>,
        ret: Box<Ty>,
    },

    /// A dynamically-dispatched interface object: `dyn Drawable`.
    Dyn(InterfaceName, Vec<Ty>),

    /// Propagates through the tree after an error is recorded, preventing
    /// cascading type errors from a single root cause.
    Error,
}

impl Ty {
    /// Collect all TypeVars mentioned anywhere in this type.
    pub fn free_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.collect_vars(&mut vars);
        vars
    }

    fn collect_vars(&self, out: &mut Vec<TypeVar>) {
        match self {
            Ty::Var(v) => {
                if !out.contains(v) {
                    out.push(*v);
                }
            }
            Ty::App(_, args) => args.iter().for_each(|a| a.collect_vars(out)),
            Ty::Fn(params, ret) => {
                params.iter().for_each(|p| p.collect_vars(out));
                ret.collect_vars(out);
            }
            Ty::VariadicFn {
                params,
                variadic,
                ret,
            } => {
                params.iter().for_each(|p| p.collect_vars(out));
                variadic.collect_vars(out);
                ret.collect_vars(out);
            }
            Ty::Dyn(_, args) => args.iter().for_each(|a| a.collect_vars(out)),
            _ => {}
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Ty::Error)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Var(v) => write!(f, "{v}"),
            Ty::Int => write!(f, "int"),
            Ty::Bool => write!(f, "bool"),
            Ty::Float => write!(f, "float"),
            Ty::Str => write!(f, "str"),
            Ty::Unit => write!(f, "()"),
            Ty::App(name, args) if args.is_empty() => write!(f, "{name}"),
            Ty::App(name, args) => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{name}<{args_str}>")
            }
            Ty::Fn(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({params_str}) -> {ret}")
            }
            Ty::VariadicFn {
                params,
                variadic,
                ret,
            } => {
                let mut params = params.iter().map(|p| p.to_string()).collect::<Vec<_>>();
                params.push(format!("{variadic}..."));
                write!(f, "fn({}) -> {ret}", params.join(", "))
            }
            Ty::Dyn(iface, args) if args.is_empty() => write!(f, "dyn {iface}"),
            Ty::Dyn(iface, args) => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "dyn {iface}<{args_str}>")
            }
            Ty::Error => write!(f, "{{error}}"),
        }
    }
}

/// What kind of literal is this? Used for literal constraints and defaulting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralKind {
    Integer,
    Float,
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Integer => write!(f, "integer"),
            LiteralKind::Float => write!(f, "float"),
        }
    }
}
