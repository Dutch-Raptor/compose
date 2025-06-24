use crate::{Array, Trace};
use compose_library::{UntypedRef, Value, ValueIterator, Vm};
use ecow::EcoVec;
use std::sync::{Arc, Mutex};
use compose_library::diag::SourceResult;

#[derive(Debug, Clone)]
pub struct ArrayIter {
    arr: EcoVec<Value>,
    index: Arc<Mutex<usize>>,
}

impl PartialEq for ArrayIter {
    fn eq(&self, other: &Self) -> bool {
        if self.arr != other.arr {
            return false;
        }

        // Load the positions of each iterator.
        let pos_a = self.index.lock().expect("Poisoned");
        let pos_b = self.index.lock().expect("Poisoned");

        if *pos_a != *pos_b {
            return false;
        }

        true
    }
}

impl ArrayIter {
    pub fn new(arr: &Array) -> Self {
        Self {
            arr: EcoVec::from(arr.values.as_slice()),
            index: Arc::new(Mutex::new(0)),
        }
    }
}

impl ValueIterator for ArrayIter {
    fn next(&self, _: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let mut idx = self.index.lock().expect("Poisoned");

        if *idx >= self.arr.len() {
            return Ok(None);
        }
        
        let item = &self.arr[*idx];

        *idx += 1;

        Ok(Some(item.clone()))
    }
}

impl Trace for ArrayIter {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for item in &self.arr {
            item.visit_refs(f);
        }
    }
}
