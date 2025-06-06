use crate::Value;
use compose_library::diag::{SourceResult, error};
use compose_library::gc::GcValue;
use compose_library::{Engine, Func, IntoValue};
use compose_macros::func;
use compose_macros::{scope, ty};
use compose_syntax::Span;
use dumpster::Trace;
use dyn_clone::DynClone;
use std::fmt::Debug;

mod iter_combinators;
mod string_iter;

use crate::diag::{SourceDiagnostic, UnSpanned};
pub use iter_combinators::*;
pub use string_iter::*;

#[ty(scope, cast, name = "Iterator")]
#[derive(Debug, Clone, Trace)]
pub struct IterValue {
    iter: GcValue<Iter>,
}

impl From<Iter> for IterValue {
    fn from(iter: Iter) -> Self {
        Self {
            iter: GcValue::new(iter),
        }
    }
}

#[derive(Clone, Debug, Trace)]
enum Iter {
    String(StringIterator),
    Take(TakeIter),
    TakeWhile(TakeWhileIter),
    Map(MapIter),
    Skip(SkipIter),
}

macro_rules! iter_from_impl {
    ($($t:ty => $variant:ident),* $(,)?) => {
        $(
            impl From<$t> for IterValue {
                fn from(value: $t) -> Self {
                    IterValue::from(Iter::$variant(value))
                }
            }

            impl IntoValue for $t {
                fn into_value(self) -> Value {
                    Value::Iterator(self.into())
                }
            }
        )*
    }
}

iter_from_impl!(
    StringIterator => String,
    TakeIter => Take,
    TakeWhileIter => TakeWhile,
    MapIter => Map,
    SkipIter => Skip,
);

impl Iter {
    pub fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        match self {
            Iter::String(s) => ValueIterator::next(s, engine),
            Iter::Take(t) => t.next(engine),
            Iter::TakeWhile(t) => t.next(engine),
            Iter::Map(m) => m.next(engine),
            Iter::Skip(s) => s.next(engine),
        }
    }
}

impl ValueIterator for IterValue {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        self.iter.get_mut().next(engine)
    }
}

#[scope]
impl IterValue {
    #[func(name = "next")]
    fn next_(&self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        self.iter.get_mut().next(engine)
    }

    #[func]
    fn take(self, n: usize) -> Self {
        TakeIter {
            inner: self,
            take: n,
        }
        .into()
    }

    #[func]
    fn take_while(self, predicate: Func) -> Self {
        TakeWhileIter {
            inner: self,
            predicate,
        }
        .into()
    }

    #[func]
    fn map(self, map: Func) -> Self {
        MapIter { inner: self, map }.into()
    }

    #[func]
    fn skip(self, n: usize) -> Self {
        SkipIter {
            inner: self,
            skip: n,
        }
        .into()
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

impl TryFrom<Value> for IterValue {
    type Error = UnSpanned<SourceDiagnostic>;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let iter = match value {
            Value::Iterator(v) => Ok(v),
            Value::Str(_) => Err(
                error!(Span::detached(), "cannot iterate over a string directly")
                    .with_hint("call `.chars()` to iterate over the characters of a string"),
            ),
            Value::Box(b) => {
                let val = b.get().map_err(|e| error!(Span::detached(), "{e}"))?;

                Ok(IterValue::try_from(val.clone())?)
            }
            v @ (Value::Int(_)
            | Value::Bool(_)
            | Value::Unit(_)
            | Value::Func(_)
            | Value::Type(_)) => Err(error!(
                Span::detached(),
                "cannot construct an iterator from an {}",
                v.ty().name()
            )),
        }?;

        Ok(iter)
    }
}

impl PartialEq for IterValue {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
