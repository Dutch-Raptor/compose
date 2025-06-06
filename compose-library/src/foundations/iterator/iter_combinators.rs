use crate::{Args, IterValue, ValueIterator};
use compose_library::diag::{At, SourceResult};
use compose_library::{Engine, Func, Value};
use dumpster::Trace;
use std::iter;

#[derive(Debug, Clone, Trace)]
pub struct TakeIter {
    pub(crate) inner: IterValue,
    pub(crate) take: usize,
}

impl ValueIterator for TakeIter {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        if self.take <= 0 {
            return Ok(None);
        }
        self.take -= 1;
        self.inner.next(engine)
    }
}

#[derive(Debug, Clone, Trace)]
pub struct SkipIter {
    pub(crate) inner: IterValue,
    pub(crate) skip: usize,
}

impl ValueIterator for SkipIter {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        while self.skip > 0 {
            self.skip -= 1;
            self.inner.next(engine)?;
        }
        self.inner.next(engine)
    }
}

#[derive(Debug, Clone, Trace)]
pub struct TakeWhileIter {
    pub(crate) inner: IterValue,
    pub(crate) predicate: Func,
}

impl ValueIterator for TakeWhileIter {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        let item = match self.inner.next(engine) {
            Ok(Some(item)) => item,
            Ok(None) => return Ok(None),
            Err(err) => return Err(err),
        };

        let span = self.predicate.span();
        let args = Args::new(span, iter::once(item.clone()));

        if self.predicate.call(engine, args)?.cast::<bool>().at(span)
            .map_err(|mut err| {
                err.make_mut()[0].hint("predicate must return a boolean");
                err
            })
            ? {
            Ok(Some(item))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct MapIter {
    pub(crate) inner: IterValue,
    pub(crate) map: Func,
}

impl ValueIterator for MapIter {
    fn next(&mut self, engine: &mut Engine) -> SourceResult<Option<Value>> {
        self.inner
            .next(engine)?
            .map(|item| {
                let span = self.map.span();
                let args = Args::new(span, iter::once(item.clone()));

                self.map.call(engine, args)
            })
            .transpose()
    }
}
