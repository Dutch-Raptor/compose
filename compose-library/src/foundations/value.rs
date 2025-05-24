use crate::diag::{At, SourceResult};
use crate::IntoValue;
use crate::Reflect;
use crate::{CastInfo, Str, UnitValue};
use crate::{FromValue, Func};
use crate::{Sink, Type};
use compose_library::diag::{bail, StrResult};
use compose_syntax::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit(UnitValue),
    Str(Str),
    Func(Func),
    Type(Type),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Int(_) => Type::of::<i64>(),
            Value::Bool(_) => Type::of::<bool>(),
            Value::Unit(_) => Type::of::<UnitValue>(),
            Value::Str(_) => Type::of::<Str>(),
            Value::Func(_) => Type::of::<Func>(),
            Value::Type(_) => Type::of::<Type>(),
        }
    }
    
    pub fn unit() -> Value {
        Value::Unit(UnitValue)
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

    /// Access a field on this value (with dot syntax) `a.b`
    pub fn field(&self, field: &str, access_span: Span, sink: &mut impl Sink) -> StrResult<Value> {
        let field_value = match self {
            Self::Func(func) => func.field(field, access_span, sink).cloned(),
            Self::Type(ty) => ty.field(field, access_span, sink).cloned(),
            _ => bail!("no field or method named `{}` on `{}`", field, self.ty()),
        };

        field_value.or_else(|e| {
            // Try to get the field from the type
            self.ty()
                .scope()
                .get(field)
                .map(|b| b.read_checked(access_span, sink))
                .cloned()
                // Return the original error if that failed
                .ok_or(e)
        })
    }

    /// Access an associated value of this value by path syntax `a::b`
    pub fn path(&self, path: &str, access_span: Span, sink: &mut impl Sink) -> SourceResult<Value> {
        match self {
            Self::Type(ty) => ty.path(path, access_span, sink).cloned().at(access_span),
            _ => bail!(access_span, "no associated field or method named `{}` on `{}`", path, self.ty()),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Unit(UnitValue)
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
primitive!(UnitValue: "unit", Unit);
