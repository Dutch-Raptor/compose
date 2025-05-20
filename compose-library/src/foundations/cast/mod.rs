mod reflect;
mod into_value;
mod into_result;

use crate::diag::{At, Spanned, StrResult};
use crate::Value;
use compose_library::Func;
use compose_macros::cast;
pub use reflect::*;
pub use into_value::*;
pub use into_result::*;


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

cast! {
    (),
    self => Value::Unit,
    v: () => v,
}