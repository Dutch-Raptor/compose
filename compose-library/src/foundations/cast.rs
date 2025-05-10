use crate::diag::{At, SourceResult, Spanned, StrResult};
use crate::{IntoValue, Value};
use compose_library::Func;
use compose_syntax::Span;
use compose_syntax::ast::DestructuringItem::Pattern;
use std::iter::FromFn;
use ecow::EcoString;
use compose_library::diag::bail;

pub trait IntoResult {
    fn into_result(self, span: Span) -> SourceResult<Value>;
}

impl<T: IntoValue> IntoResult for T {
    fn into_result(self, _: Span) -> SourceResult<Value> {
        Ok(self.into_value())
    }
}

impl<T: IntoValue> IntoResult for StrResult<T> {
    fn into_result(self, span: Span) -> SourceResult<Value> {
        self.map(IntoValue::into_value).at(span)
    }
}

impl<T: IntoValue> IntoResult for SourceResult<T> {
    fn into_result(self, _: Span) -> SourceResult<Value> {
        self.map(IntoValue::into_value)
    }
}

impl<T: IntoValue> IntoValue for fn() -> T {
    fn into_value(self) -> Value {
        self().into_value()
    }
}

pub trait FromValue<V = Value>: Sized {
    fn from_value(value: V) -> StrResult<Self>;
}

impl FromValue for Value {
    fn from_value(value: Value) -> StrResult<Self> {
        Ok(value)
    }
}

impl<T: FromValue> FromValue<Spanned<Value>> for T {
    fn from_value(value: Spanned<Value>) -> StrResult<Self> {
        Ok(T::from_value(value.value)?)
    }
}

impl<T: FromValue> FromValue<Spanned<Value>> for Spanned<T> {
    fn from_value(value: Spanned<Value>) -> StrResult<Self> {
        Ok(Spanned::new(T::from_value(value.value)?, value.span))
    }
}

impl<T: FromValue> FromValue for Option<T> {
    fn from_value(value: Value) -> StrResult<Self> {
        match value {
            Value::Unit => Ok(None),
            _ => Ok(Some(T::from_value(value)?)),
        }
    }
}

macro_rules! impl_from_value {
    ($(($variant:ident))* => $ty:ty) => (
        impl FromValue for $ty {
            fn from_value(value: Value) -> StrResult<Self> {
                match value {
                    $(Value::$variant(i) => Ok(i),)*
                    _ => Err("Expected $variant".into()),
                }
            }
        }
    );
    ($(($variant:ident => $transform:expr))* => $ty:ty) => (
        impl FromValue for $ty {
            fn from_value(value: Value) -> StrResult<Self> {
                match value {
                    $(Value::$variant(i) => Ok($transform(i)),)*
                    _ => Err("Expected $variant".into()),
                }
            }
        }
    )
}

impl_from_value!((Int) => i64);
impl_from_value!((Bool) => bool);
impl_from_value!((Func) => Func);

impl FromValue for EcoString {
    fn from_value(value: Value) -> StrResult<Self> {
        match value {
            Value::Str(s) => Ok(s.0),
            _ => bail!("Expected string"),
        }
    }
}
