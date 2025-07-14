use crate::{ArrayValue, HeapRef, MapValue, Trace};
use crate::{UntypedRef, Value};
use compose_library::diag::{SourceResult, bail, error};
use compose_library::vm::Vm;
use compose_library::{Func, Str};
use compose_macros::{func, scope, ty};
use compose_syntax::Span;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

mod array_iter;
mod iter_combinators;
mod range_iter;
mod string_iter;

use crate::diag::{SourceDiagnostic, StrResult, UnSpanned};
use crate::support::eval_func;
pub use array_iter::*;
use compose_library::support::eval_predicate;
pub use iter_combinators::*;
pub use range_iter::*;
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
            Value::Range(range) => Ok(IterValue::new(
                Iter::Range(
                    RangeIter::new(range.inner())
                        .map_err(|e| SourceDiagnostic::error(Span::detached(), e))?,
                ),
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

    pub(crate) fn shallow_clone(&self, vm: &mut dyn Vm) -> Self {
        let self_ = self.iter.get_unwrap(vm.heap()).clone();

        Self {
            iter: vm.heap_mut().alloc(self_),
        }
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
    Range(RangeIter),
    StepBy(StepByIter),
    Filter(FilterIter),
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
            Iter::Range(_) => {}
            Iter::StepBy(iter) => iter.visit_refs(f),
            Iter::Filter(filter) => filter.visit_refs(f),
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
            Iter::Range(r) => r.next(vm),
            Iter::StepBy(s) => s.next(vm),
            Iter::Filter(f) => f.next(vm),
        }
    }

    pub fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        match self {
            Iter::String(s) => ValueIterator::nth(s, vm, n),
            Iter::Take(t) => t.nth(vm, n),
            Iter::TakeWhile(t) => t.nth(vm, n),
            Iter::Map(m) => m.nth(vm, n),
            Iter::Skip(s) => s.nth(vm, n),
            Iter::Array(a) => a.nth(vm, n),
            Iter::Range(r) => r.nth(vm, n),
            Iter::StepBy(s) => s.nth(vm, n),
            Iter::Filter(f) => f.nth(vm, n),
        }
    }
}

impl ValueIterator for IterValue {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let iter = self.iter.get_unwrap(vm.heap()).clone();
        iter.next(vm)
    }

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        let iter = self.iter.get_unwrap(vm.heap()).clone();
        iter.nth(vm, n)
    }
}

#[scope]
impl IterValue {
    #[func(name = "next")]
    fn next_(&mut self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let iter = self.iter.get_unwrap(vm.heap()).clone();

        iter.next(vm)
    }

    #[func(name = "nth")]
    fn nth_(&mut self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        let iter = self.iter.get_unwrap(vm.heap()).clone();

        iter.nth(vm, n)
    }

    #[func]
    fn take(self, vm: &mut dyn Vm, n: usize) -> Self {
        IterValue::new(Iter::Take(TakeIter::new(self, n)), vm)
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

    #[func]
    fn step_by(self, vm: &mut dyn Vm, step: usize) -> StrResult<Self> {
        Ok(IterValue::new(
            Iter::StepBy(StepByIter::new(self, step)?),
            vm,
        ))
    }

    #[func]
    fn filter(self, vm: &mut dyn Vm, predicate: Func) -> Self {
        IterValue::new(
            Iter::Filter(FilterIter {
                inner: self,
                predicate: Arc::new(predicate),
            }),
            vm,
        )
    }

    #[func]
    fn find(&mut self, vm: &mut dyn Vm, predicate: Func) -> SourceResult<Option<Value>> {
        while let Some(v) = self.next(vm)? {
            if eval_predicate(vm, &predicate, v.clone(), "find")? {
                return Ok(Some(v));
            }
        }

        Ok(None)
    }

    #[func]
    fn all(&mut self, vm: &mut dyn Vm, predicate: Func) -> SourceResult<bool> {
        while let Some(v) = self.next(vm)? {
            if !eval_predicate(vm, &predicate, v.clone(), "all")? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    #[func]
    fn any(&mut self, vm: &mut dyn Vm, predicate: Func) -> SourceResult<bool> {
        while let Some(v) = self.next(vm)? {
            if eval_predicate(vm, &predicate, v.clone(), "any")? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    #[func]
    fn position(&mut self, vm: &mut dyn Vm, predicate: Func) -> SourceResult<Option<usize>> {
        let mut i = 0;
        while let Some(v) = self.next(vm)? {
            if eval_predicate(vm, &predicate, v.clone(), "position")? {
                return Ok(Some(i));
            }
            i += 1;
        }
        Ok(None)
    }

    #[func]
    fn to_array(self, vm: &mut dyn Vm) -> SourceResult<ArrayValue> {
        let mut values = Vec::new();
        while let Some(v) = self.next(vm)? {
            values.push(v);
        }

        Ok(ArrayValue::from(vm.heap_mut(), values))
    }

    #[func]
    fn to_map(
        self,
        vm: &mut dyn Vm,
        key_mapper: Func,
        value_mapper: Func,
    ) -> SourceResult<MapValue> {
        let mut map = HashMap::new();
        while let Some(v) = self.next(vm)? {
            let k = eval_func(vm, &key_mapper, [v.clone()])?;

            let Value::Str(Str(key)) = k else {
                bail!(key_mapper.span, "key mapper must return a string");
            };

            let v = eval_func(vm, &value_mapper, [v.clone()])?;

            map.insert(key, v);
        }

        Ok(MapValue::from(vm.heap_mut(), map))
    }
}

pub trait ValueIterator: Debug + Send + Sync {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>>;

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        if n == 0 {
            return self.next(vm);
        }

        let iter = self;
        for _ in 0..n {
            match iter.next(vm)? {
                Some(_) => {}
                None => return Ok(None),
            }
        }

        iter.next(vm)
    }
}

macro_rules! impl_into_iter {
    (
        $($iter:ident => $ty:ty),* $(,)?
    ) => {
        $(
            impl Into<Iter> for $ty {
                fn into(self) -> Iter {
                    Iter::$iter(self)
                }
            }
        )*
    };
}

impl_into_iter!(
    String => StringIterator,
    Array => ArrayIter,
    Take => TakeIter,
    TakeWhile => TakeWhileIter,
    Map => MapIter,
    Skip => SkipIter,
    Range => RangeIter,
    StepBy => StepByIter,
    Filter => FilterIter,
);
