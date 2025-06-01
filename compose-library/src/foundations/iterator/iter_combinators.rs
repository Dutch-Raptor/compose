use crate::{Args, ValueIterator};
use compose_library::diag::{At, SourceResult};
use compose_library::{Engine, Func, Value};
use std::iter;

#[derive(Debug, Clone)]
pub struct TakeIter {
    pub(crate) inner: Box<dyn ValueIterator>,
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

#[derive(Debug, Clone)]
pub struct SkipIter {
    inner: Box<dyn ValueIterator>,
    skip: usize,
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

#[derive(Debug, Clone)]
pub struct TakeWhileIter {
    pub(crate) inner: Box<dyn ValueIterator>,
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

        if self.predicate.call(engine, args)?.cast::<bool>().at(span)? {
            Ok(Some(item))
        } else {
            Ok(None)
        }
    }
}
