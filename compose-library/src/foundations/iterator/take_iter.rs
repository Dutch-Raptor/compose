use compose_library::{IntoValue, Value, ValueIter};
use crate::ValueIterator;

#[derive(Debug, Clone)]
pub struct TakeIter {
    inner: Box<dyn ValueIterator>,
    take: usize
}

impl Iterator for TakeIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.take <= 0 {
            return None;
        }
        self.take -= 1;
        self.inner.next()
    }
}

impl TakeIter {
    pub fn new(inner: Box<dyn ValueIterator>, take: usize) -> Self {
        Self { inner, take }
    }
}

impl IntoValue for TakeIter {
    fn into_value(self) -> Value {
        Value::Iterator(ValueIter::from_dyn(Box::new(self)))
    }
}
