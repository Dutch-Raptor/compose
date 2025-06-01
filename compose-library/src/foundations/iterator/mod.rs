use compose_macros::func;
use crate::Value;
use compose_library::IntoValue;
use compose_library::diag::bail;
use compose_macros::{scope, ty};
use dyn_clone::DynClone;
use ecow::EcoString;
use std::fmt::Debug;

mod string_iter;
mod take_iter;

pub use string_iter::*;
pub use take_iter::*;

#[ty(scope, cast, name = "Iterator")]
#[derive(Debug, Clone)]
pub struct ValueIter {
    iter: Box<dyn ValueIterator>,
}

impl ValueIter {
    pub fn from_dyn(iter: Box<dyn ValueIterator>) -> Self {
        Self { iter }
    }
}


#[scope]
impl ValueIter {
    #[func]
    fn take(self, n: usize) -> Self {
        ValueIter::from_dyn(Box::new(TakeIter::new(self.iter, n)))
    }
}


pub trait ValueIterator: DynClone + Debug + Send + Sync {
    fn next(&mut self) -> Option<Value>;
}

dyn_clone::clone_trait_object!(ValueIterator);

impl<T, V> ValueIterator for T
where
    T: Iterator<Item = V> + DynClone,
    T: Debug + Send + Sync,
    V: IntoValue,
{
    fn next(&mut self) -> Option<Value> {
        self.next().map(IntoValue::into_value)
    }
}

impl TryFrom<Value> for ValueIter {
    type Error = EcoString;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Self {
            iter: Box::new(match value {
                Value::Iterator(v) => return Ok(v),
                Value::Str(s) => StringIterator::new(s.0),
                v @ (Value::Int(_)
                | Value::Bool(_)
                | Value::Unit(_)
                | Value::Func(_)
                | Value::Type(_)) => {
                    bail!("Cannot construct an iterator from an {}", v.ty().name())
                }
            }),
        })
    }
}
impl PartialEq for ValueIter {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Iterator for ValueIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}
