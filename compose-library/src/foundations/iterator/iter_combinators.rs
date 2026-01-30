use crate::diag::StrResult;
use compose_library::Value;
use compose_library::diag::{At, SourceResult, bail};
use compose_library::foundations::args::Args;
use compose_library::foundations::cast::IntoValue;
use compose_library::foundations::iterator::{IterValue, ValueIterator};
use compose_library::foundations::support::eval_predicate;
use compose_library::foundations::types::{ArrayValue, Func};
use compose_library::gc::{Trace, UntypedRef};
use compose_library::vm::Vm;
use std::iter;
use std::ops::DerefMut;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct TakeIter {
    pub(crate) inner: IterValue,
    pub(crate) take: Arc<Mutex<usize>>,
}

impl TakeIter {
    pub fn new(inner: IterValue, take: usize) -> Self {
        Self {
            inner,
            take: Arc::new(Mutex::new(take)),
        }
    }
}

impl PartialEq for TakeIter {
    fn eq(&self, other: &Self) -> bool {
        if self.inner != other.inner {
            return false;
        }

        let take_a = self.take.lock().expect("mutex poisoned").clone();
        let take_b = other.take.lock().expect("mutex poisoned").clone();

        if take_a != take_b {
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
        self.nth(vm, 0)
    }

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        let mut take = self.take.lock().expect("take poisoned");
        if *take <= n {
            *take = 0;
            return Ok(None);
        }

        *take = *take - n - 1; // minus one because n is 0 indexed. (for 0 we do yield an item, so take should be decremented)
        drop(take);

        self.inner.nth(vm, n)
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
            let skip_a = self.skip.lock().expect("mutex poisoned").clone();
            let skip_b = self.skip.lock().expect("mutex poisoned").clone();

            if skip_a != skip_b {
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
        let mut skip = self.skip.lock().expect("Skip lock poisoned");
        let n = std::mem::replace(skip.deref_mut(), 0);
        self.inner.nth(vm, n)
    }

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        let mut skip = self.skip.lock().expect("Skip lock poisoned");
        let to_skip = std::mem::replace(skip.deref_mut(), 0);
        self.inner.nth(vm, to_skip + n)
    }
}

#[derive(Debug, Clone)]
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

    // nth method cannot be optimized here, so we just fall back to the default
}

impl PartialEq for TakeWhileIter {
    fn eq(&self, other: &Self) -> bool {
        if self.inner != other.inner {
            return false;
        }

        self.predicate == other.predicate
    }
}

#[derive(Debug, Clone)]
pub struct FilterIter {
    pub(crate) inner: IterValue,
    pub(crate) predicate: Arc<Func>,
}

impl Trace for FilterIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f);
        self.predicate.visit_refs(f);
    }
}

impl ValueIterator for FilterIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        loop {
            let item = match self.inner.next(vm) {
                Ok(Some(item)) => item,
                Ok(None) => return Ok(None),
                Err(err) => return Err(err),
            };

            if eval_predicate(vm, &self.predicate, item.clone(), "filter")? {
                return Ok(Some(item));
            }
        }
    }

    // Filter is not nth optimizable because it needs to evaluate predicate for each item it examines
    // both for potential side effects and for keeping track of which values it would have yielded.
    // Fall back to the default implementation
}

impl PartialEq for FilterIter {
    fn eq(&self, other: &Self) -> bool {
        if self.inner != other.inner {
            return false;
        }

        self.predicate == other.predicate
    }
}

#[derive(Debug, Clone)]
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
        self.nth(vm, 0)
    }

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        self.inner
            .nth(vm, n)?
            .map(|item| {
                let span = self.map.span();
                let args = Args::new(span, iter::once(item.clone()));

                self.map.call(vm, args)
            })
            .transpose()
    }
}

impl PartialEq for MapIter {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && self.map == other.map
    }
}

impl StepByIter {
    pub fn new(inner: IterValue, step: usize) -> StrResult<Self> {
        if step == 0 {
            bail!("step must be greater than 0");
        }

        Ok(Self {
            inner,
            step,
            first_step: Arc::new(Mutex::new(true)),
        })
    }
}

impl PartialEq for StepByIter {
    fn eq(&self, other: &Self) -> bool {
        if self.step != other.step {
            return false;
        }

        if self.inner != other.inner {
            return false;
        }

        let self_first_step = self.first_step.lock().expect("lock poisened").clone();
        let other_first_step = other.first_step.lock().expect("lock poisened").clone();

        if self_first_step != other_first_step {
            return false;
        }

        true
    }
}

#[derive(Debug, Clone)]
pub struct StepByIter {
    inner: IterValue,
    step: usize,
    first_step: Arc<Mutex<bool>>,
}

impl ValueIterator for StepByIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        self.nth(vm, 0)
    }

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        let mut first_step = self.first_step.lock().unwrap();
        let first_step = std::mem::replace(first_step.deref_mut(), false);

        // Compute the number of elements to skip in the underlying iterator.
        //
        // If this is the first call and the first element hasn't been yielded yet,
        // we must consume it (index 0) before starting to step.
        //
        // For step = 2:
        // - first_step = true:
        //   nth(1) should yield index: 2 * 1 + 1 = 3
        // - first_step = false:
        //   nth(1) should yield index: 2 * (1 + 1) = 4
        //
        // This ensures consistent stepping after the initial yield.
        let offset = if first_step {
            self.step * n
        } else {
            self.step * (n + 1) - 1
        };

        self.inner.nth(vm, offset)
    }
}

impl Trace for StepByIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f);
    }
}

#[derive(Debug, Clone)]
pub struct EnumerateIter {
    pub(crate) inner: IterValue,
    pub(crate) index: Arc<Mutex<usize>>,
}

impl Trace for EnumerateIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.visit_refs(f)
    }
}

impl ValueIterator for EnumerateIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        self.nth(vm, 0)
    }

    fn nth(&self, vm: &mut dyn Vm, n: usize) -> SourceResult<Option<Value>> {
        let mut index = self.index.lock().expect("index poisoned");

        let index_cpy = *index;
        *index = *index + n + 1; // plus one because n is 0 indexed. (for 0 we do yield an item, so index should be incremented by 1)

        drop(index);

        let Some(inner_value) = self.inner.nth(vm, n)? else {
            return Ok(None);
        };

        Ok(Some(
            ArrayValue::from(vm.heap_mut(), vec![index_cpy.into_value(), inner_value]).into_value(),
        ))
    }
}

impl PartialEq for EnumerateIter {
    fn eq(&self, other: &Self) -> bool {
        if self.inner.iter != other.inner.iter {
            return false;
        }

        let self_index = self.index.lock().expect("index poisoned").clone();
        let other_index = other.index.lock().expect("index poisoned").clone();

        self_index == other_index
    }
}
