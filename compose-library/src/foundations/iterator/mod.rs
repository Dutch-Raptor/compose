use crate::Value;
use compose_library::IntoValue;
use compose_library::diag::bail;
use compose_macros::ty;
use dyn_clone::DynClone;
use ecow::EcoString;
use std::fmt::Debug;
use string_iter::StringIterator;

mod string_iter;

#[ty(cast, name = "Iterator")]
#[derive(Debug, Clone)]
pub struct ValueIter {
    iter: Box<dyn ValueIterator>,
}

pub trait ValueIterator: DynClone + Debug + Send + Sync {
    fn next(&mut self) -> Option<Value>;
}

dyn_clone::clone_trait_object!(ValueIterator);

#[derive(Debug, Clone)]
pub struct EmptyIterator;

impl ValueIterator for EmptyIterator {
    fn next(&mut self) -> Option<Value> {
        None
    }
}

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
