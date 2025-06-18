use crate::IterValue;
use compose_library::diag::{At, SourceResult};
use compose_library::vm::Vm;
use compose_library::{Args, Func, Trace, UntypedRef, Value, ValueIterator};
use std::iter;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct TakeIter {
    pub(crate) inner: IterValue,
    pub(crate) take: Arc<Mutex<usize>>,
}

impl PartialEq for TakeIter {
    fn eq(&self, other: &Self) -> bool {
        if self.inner != other.inner {
            return false;
        }

        let take_a = self.take.lock().expect("mutex poisoned");
        let take_b = self.take.lock().expect("mutex poisoned");

        if *take_a != *take_b {
            return false;
        }

        true
    }
}

impl Trace for TakeIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f);
    }
}

impl ValueIterator for TakeIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        {
            let mut take = self.take.lock().expect("take poisoned");
            if *take == 0 {
                return Ok(None);
            }
            *take -= 1;
        }

        self.inner.next(vm)
    }
}

#[derive(Debug, Clone)]
pub struct SkipIter {
    pub(crate) inner: IterValue,
    pub(crate) skip: Arc<Mutex<usize>>,
}

impl PartialEq for SkipIter {
    fn eq(&self, other: &Self) -> bool {
        if self.inner != other.inner {
            return false;
        }

        {
            let skip_a = self.skip.lock().expect("mutex poisoned");
            let skip_b = self.skip.lock().expect("mutex poisoned");

            if *skip_a != *skip_b {
                return false;
            }
        }

        true
    }
}

impl Trace for SkipIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f)
    }
}

impl ValueIterator for SkipIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        {
            let mut skip = self.skip.lock().expect("Skip lock poisoned");
            while *skip > 0 {
                *skip -= 1;
                self.inner.next(vm)?;
            }
        }
        self.inner.next(vm)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TakeWhileIter {
    pub(crate) inner: IterValue,
    pub(crate) predicate: Arc<Func>,
}

impl Trace for TakeWhileIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f);
        self.predicate.visit_refs(f);
    }
}

impl ValueIterator for TakeWhileIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let item = match self.inner.next(vm) {
            Ok(Some(item)) => item,
            Ok(None) => return Ok(None),
            Err(err) => return Err(err),
        };

        let span = self.predicate.span();
        let args = Args::new(span, iter::once(item.clone()));

        if self
            .predicate
            .call(vm, args)?
            .cast::<bool>()
            .at(span)
            .map_err(|mut err| {
                err.make_mut()[0].hint("predicate must return a boolean");
                err
            })?
        {
            Ok(Some(item))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapIter {
    pub(crate) inner: IterValue,
    pub(crate) map: Arc<Func>,
}

impl Trace for MapIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f);
        self.map.visit_refs(f);
    }
}

impl ValueIterator for MapIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        self.inner
            .next(vm)?
            .map(|item| {
                let span = self.map.span();
                let args = Args::new(span, iter::once(item.clone()));

                self.map.call(vm, args)
            })
            .transpose()
    }
}
