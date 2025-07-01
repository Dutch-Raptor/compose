use crate::diag::SourceResult;
use compose_library::diag::{bail, StrResult};
use compose_library::{ops, Value, ValueIterator, Vm};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub struct RangeIter {
    current: Arc<Mutex<Value>>,
    max_inclusive: bool,
    max: Option<Value>,
}

impl Value {
    fn next(&self) -> StrResult<Value> {
        Ok(match self {
            Value::Int(v) => Value::Int(v + 1),
            _ => bail!("Cannot increment value of type {}", self.ty().name()),
        })
    }
}

impl ValueIterator for RangeIter {
    fn next(&self, _: &mut dyn Vm<'_>) -> SourceResult<Option<Value>> {
        let mut current = self.current.lock().unwrap();
        let next = current.next().expect("Cannot increment value");
        
        let Some(max) = &self.max else {
            return Ok(Some(next));       
        };

        let eq = ops::eq(&current, &max)
            .and_then(Value::cast::<bool>)
            .expect("Cannot compare values");
        let lt = ops::lt(&current, &max)
            .and_then(Value::cast::<bool>)
            .expect("Cannot compare values");
        *current = next.clone();

        match self.max_inclusive {
            _ if lt => Ok(Some(next)),
            true if eq => Ok(Some(next)),
            _ => Ok(None),
        }
    }
}
