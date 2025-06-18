use compose_library::diag::SourceResult;
use compose_library::vm::Vm;
use compose_library::{IntoValue, Value, ValueIterator};
use ecow::EcoString;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct StringIterator {
    s: EcoString,
    byte_pos: Arc<Mutex<usize>>,
}

impl PartialEq for StringIterator {
    fn eq(&self, other: &Self) -> bool {
        if self.s != other.s {
            return false;
        }

        // Load the positions of each iterator. 
        let pos_a = self.byte_pos.lock().expect("Poisoned");
        let pos_b = self.byte_pos.lock().expect("Poisoned");

        if *pos_a != *pos_b {
            return false;
        }

        true
    }
}

impl StringIterator {
    pub fn new(s: EcoString) -> Self {
        Self { s, byte_pos: Arc::new(Mutex::new(0)) }
    }
}

impl ValueIterator for StringIterator {
    fn next(&self, _: &mut dyn Vm) -> SourceResult<Option<Value>> {
        let mut idx = self.byte_pos.lock().expect("Poisoned");

        if *idx >= self.s.len() {
            return Ok(None);
        }

        let Some(c) = self.s[*idx..].chars().next() else {
            return Ok(None)
        };

        *idx += c.len_utf8();

        Ok(Some(c.into_value()))
    }
}