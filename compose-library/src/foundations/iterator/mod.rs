use crate::{HeapRef, Trace};
use crate::{UntypedRef, Value};
use compose_library::diag::{error, At, SourceResult};
use compose_library::foundations::iterator::array_iter::ArrayIter;
use compose_library::vm::Vm;
use compose_library::Func;
use compose_macros::{func, scope, ty};
use compose_syntax::Span;
use dyn_clone::DynClone;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

mod array_iter;
mod iter_combinators;
mod string_iter;

use crate::diag::{SourceDiagnostic, StrResult, UnSpanned};
pub use iter_combinators::*;
pub use string_iter::*;

#[ty(scope, cast, name = "Iterator")]
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct IterValue {
    iter: HeapRef<Iter>,
}

impl Trace for IterValue {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(self.iter.key())
    }
}

impl IterValue {
    pub(crate) fn new(iter: Iter, vm: &mut dyn Vm) -> Self {
        Self {
            iter: vm.heap_mut().alloc(iter),
        }
    }

    pub fn try_from_value(
        value: Value,
        mutable: bool,
        vm: &mut dyn Vm,
    ) -> Result<IterValue, UnSpanned<SourceDiagnostic>> {
        match value {
            Value::Iterator(i) if mutable => Ok(i),

            Value::Str(_) => Err(error!(
                Span::detached(), "cannot iterate over a string directly";
                hint: "try calling `.chars()` to iterate over the characters of the string"
            )
            .into()),
            Value::Box(_) => Err(error!(
                Span::detached(), "cannot iterate over a boxed value directly";
                hint: "try dereferencing the box first with `*`"
            )
            .into()),
            Value::Array(arr) => Ok(IterValue::new(
                Iter::Array(ArrayIter::new(
                    arr.try_get(vm.heap())
                        .map_err(|e| SourceDiagnostic::error(Span::detached(), e))?,
                )),
                vm,
            )),
            other @ (Value::Int(_)
            | Value::Func(_)
            | Value::Type(_)
            | Value::Bool(_)
            | Value::Unit(_)) => {
                Err(error!(Span::detached(), "cannot iterate over type {}", other.ty()).into())
            }
            immut => requires_mutable_iter(immut),
        }
    }

    pub(crate) fn shallow_clone(&self, vm: &mut dyn Vm) -> StrResult<Self> {
        let self_ = self.iter.try_get(vm.heap()).cloned()?;
        Ok(Self {
            iter: vm.heap_mut().alloc(self_),
        })
    }
}

pub fn requires_mutable_iter<T>(value: Value) -> Result<T, UnSpanned<SourceDiagnostic>> {
    Err(error!(
        Span::detached(),
        "cannot iterate over a value of type {} that is not marked as mutable",
        value.ty()
    )
    .into())
}

#[derive(Clone, Debug, PartialEq)]
pub enum Iter {
    String(StringIterator),
    Array(ArrayIter),
    Take(TakeIter),
    TakeWhile(TakeWhileIter),
    Map(MapIter),
    Skip(SkipIter),
}

impl Trace for Iter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        match self {
            Iter::String(_) => {}
            Iter::TakeWhile(iter) => iter.visit_refs(f),
            Iter::Take(iter) => iter.visit_refs(f),
            Iter::Skip(iter) => iter.visit_refs(f),
            Iter::Map(iter) => iter.visit_refs(f),
            Iter::Array(arr) => arr.visit_refs(f),
        }
    }
}

impl Iter {
    pub fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        match self {
            Iter::String(s) => ValueIterator::next(s, vm),
            Iter::Take(t) => t.next(vm),
            Iter::TakeWhile(t) => t.next(vm),
            Iter::Map(m) => m.next(vm),
            Iter::Skip(s) => s.next(vm),
            Iter::Array(a) => a.next(vm),
        }
    }
}

impl ValueIterator for IterValue {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let iter = self
            .iter
            .get(vm.heap())
            .cloned()
            .expect("expected to point to an iter value");
        iter.next(vm)
    }
}

#[scope]
impl IterValue {
    #[func(name = "next")]
    fn next_(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let iter = self
            .iter
            .get(vm.heap())
            .cloned()
            .expect("expected to point to an iter value");

        iter.next(vm)
    }

    #[func]
    fn take(self, vm: &mut dyn Vm, n: usize) -> Self {
        IterValue::new(
            Iter::Take(TakeIter {
                inner: self,
                take: Arc::new(Mutex::new(n)),
            }),
            vm,
        )
    }

    #[func]
    fn take_while(self, vm: &mut dyn Vm, predicate: Func) -> Self {
        IterValue::new(
            Iter::TakeWhile(TakeWhileIter {
                inner: self,
                predicate: Arc::new(predicate),
            }),
            vm,
        )
    }

    #[func]
    fn map(self, vm: &mut dyn Vm, map: Func) -> Self {
        IterValue::new(
            Iter::Map(MapIter {
                inner: self,
                map: Arc::new(map),
            }),
            vm,
        )
    }

    #[func]
    fn skip(self, vm: &mut dyn Vm, n: usize) -> Self {
        IterValue::new(
            Iter::Skip(SkipIter {
                inner: self,
                skip: Arc::new(Mutex::new(n)),
            }),
            vm,
        )
    }
}

pub trait ValueIterator: DynClone + Debug + Send + Sync {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>>;
}
