use crate::IterValue;
use crate::diag::StrResult;
use compose_library::diag::{At, SourceResult, bail};
use compose_library::vm::Vm;
use compose_library::{Args, Func, Trace, UntypedRef, Value, ValueIterator};
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

        let take_a = self.take.lock().expect("mutex poisoned");
        let take_b = other.take.lock().expect("mutex poisoned");

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

    // nth method cannot be optimized here, so we just fall back to the default
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

        if *self.first_step.lock().unwrap() != *other.first_step.lock().unwrap() {
            return false;
        }

        if self.inner != other.inner {
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
