use crate::diag::StrResult;
use crate::{Array, ArrayValue, Value};
use compose_library::diag::bail;
use compose_library::Heap;

pub trait Comparison {
    fn equals(&self, other: &Self, heap: &crate::Heap) -> compose_library::diag::StrResult<bool>;

    fn not_equals(
        &self,
        other: &Self,
        heap: &Heap,
    ) -> StrResult<bool> {
        self.equals(other, heap).map(|b| !b)
    }
}

impl Comparison for Value {
    fn equals(&self, other: &Value, heap: &Heap) -> StrResult<bool> {
        match (self, other) {
            (Value::Int(left), Value::Int(right)) => Ok(left == right),
            (Value::Bool(left), Value::Bool(right)) => Ok(left == right),
            (Value::Str(left), Value::Str(right)) => Ok(left == right),
            (Value::Func(left), Value::Func(right)) => Ok(left == right),
            (Value::Type(left), Value::Type(right)) => Ok(left == right),
            (Value::Iterator(_left), Value::Iterator(_right)) => {
                bail!("cannot compare iterators for equality")
            }
            (Value::Array(left), Value::Array(right)) => left.equals(right, heap),

            _ => Ok(false),
        }
    }
}

impl Comparison for ArrayValue {
    fn equals(&self, other: &Self, heap: &Heap) -> StrResult<bool> {
        let a = self.heap_ref().get_unwrap(heap);
        let b = other.heap_ref().get_unwrap(heap);

        a.equals(b, heap)
    }
}

impl Comparison for Array {
    fn equals(&self, other: &Self, heap: &Heap) -> StrResult<bool> {
        if self.values.len() != other.values.len() {
            return Ok(false);
        }

        for (a, b) in self.values.iter().zip(&other.values) {
            if !a.equals(b, heap)? {
                return Ok(false);
            }
        }

        Ok(true)
    }
}
