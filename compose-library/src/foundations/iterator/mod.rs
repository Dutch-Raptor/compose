use crate::Value;
use compose_library::diag::{SourceResult, bail};
use compose_library::{Engine, Func, IntoValue};
use compose_macros::func;
use compose_macros::{scope, ty};
use dyn_clone::DynClone;
use ecow::EcoString;
use std::fmt::Debug;

mod iter_combinators;
mod string_iter;

pub use iter_combinators::*;
pub use string_iter::*;

#[ty(scope, cast, name = "Iterator")]
#[derive(Debug, Clone)]
pub struct ValueIter {
    iter: Box<dyn ValueIterator>,
}

impl ValueIterator for ValueIter {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        self.iter.next(engine)
    }
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
        ValueIter::from_dyn(Box::new(TakeIter {
            inner: self.iter,
            take: n,
        }))
    }

    #[func]
    fn take_while(self, predicate: Func) -> Self {
        ValueIter::from_dyn(Box::new(TakeWhileIter {
            inner: self.iter,
            predicate,
        }))
    }
}

pub trait ValueIterator: DynClone + Debug + Send + Sync {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>>;
}

dyn_clone::clone_trait_object!(ValueIterator);

impl<T, V> ValueIterator for T
where
    T: Iterator<Item = V> + DynClone,
    T: Debug + Send + Sync,
    V: IntoValue,
{
    fn next(&mut self, _engine: &mut Engine) -> SourceResult<Option<Value>> {
        Ok(self.next().map(IntoValue::into_value))
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
