use crate::Type;
use crate::Reflect;
use crate::IntoValue;
use crate::CastInfo;
use crate::foundations::str::Str;
use crate::{FromValue, Func};
use compose_library::diag::StrResult;
use compose_syntax::Span;
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
    Str(Str),
    Func(Func),
    Type(Type),
}

impl Value {
    pub fn ty(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Bool(_) => "bool",
            Value::Unit => "unit",
            Value::Str(_) => "str",
            Value::Func(_) => "func",
            Value::Type(_) => "type",       
        }
    }

    pub fn cast<T: FromValue>(self) -> StrResult<T> {
        T::from_value(self)
    }

    pub fn spanned(self, span: Span) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.spanned(span)),
            _ => self,
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Unit
    }
}


/// Implements traits for primitives (Value enum variants).
macro_rules! primitive {
    (
        $ty:ty: $name:literal, $variant:ident
        $(, $other:ident$(($binding:ident))? => $out:expr)*
    ) => {
        impl Reflect for $ty {
            fn input() -> CastInfo {
                CastInfo::Type(Type::of::<Self>())
            }

            fn output() -> CastInfo {
                CastInfo::Type(Type::of::<Self>())
            }

            fn castable(value: &Value) -> bool {
                matches!(value, Value::$variant(_)
                    $(|  primitive!(@$other $(($binding))?))*)
            }
        }

        impl IntoValue for $ty {
            fn into_value(self) -> Value {
                Value::$variant(self)
            }
        }

        impl FromValue for $ty {
            fn from_value(value: Value) -> StrResult<Self> {
                match value {
                    Value::$variant(v) => Ok(v),
                    $(Value::$other$(($binding))? => Ok($out),)*
                    v => Err(<Self as Reflect>::error(&v)),
                }
            }
        }
    };

    (@$other:ident($binding:ident)) => { Value::$other(_) };
    (@$other:ident) => { Value::$other };
}

primitive!(i64: "int", Int);
primitive!(bool: "bool", Bool);
primitive!(Str: "str", Str);
primitive!(Func: "func", Func);
primitive!(Type: "type", Type);

