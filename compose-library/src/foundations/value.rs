use crate::foundations::str::Str;
use crate::{FromValue, Func, NativeFuncData};
use compose_library::diag::StrResult;
use compose_syntax::Span;
use ecow::EcoString;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
    Str(Str),
    Func(Func),
}

impl Value {
    pub fn ty(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Bool(_) => "bool",
            Value::Unit => "unit",
            Value::Str(_) => "str",
            Value::Func(_) => "func",
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

pub trait IntoValue {
    fn into_value(self) -> Value;
}

impl IntoValue for Value {
    fn into_value(self) -> Value {
        self
    }
}

macro_rules! impl_into_value {
    ($($t:ty => $i:ident,)+ $(,)?) => {
        $(
            impl IntoValue for $t {
                fn into_value(self) -> Value {
                    Value::$i(self)
                }
            }
        )*
    }
}

impl IntoValue for () {
    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl IntoValue for &'static NativeFuncData {
    fn into_value(self) -> Value {
        Value::Func(self.into())
    }
}

impl IntoValue for &'static str {
    fn into_value(self) -> Value {
        Value::Str(EcoString::from(self).into())
    }
}

impl IntoValue for String {
    fn into_value(self) -> Value {
        Value::Str(EcoString::from(self).into())
    }
}

impl IntoValue for EcoString {
    fn into_value(self) -> Value {
        Value::Str(self.into())
    }
}

impl_into_value!(
    i64 => Int,
    bool => Bool,
    Func => Func,
);
