use crate::diag::StrResult;
use compose_library::diag::bail;
use compose_library::foundations::types::{Array, ArrayValue, MapValue};
use compose_library::gc::Heap;
use compose_library::Value;

pub trait Comparison {
    fn equals(&self, other: &Self, heap: &Heap) -> compose_library::diag::StrResult<bool>;

    fn not_equals(&self, other: &Self, heap: &Heap) -> StrResult<bool> {
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
            (Value::Unit(_), Value::Unit(_)) => Ok(true),
            // Box performs a HeapRef comparison, so we can compare them directly
            (Value::Box(left), Value::Box(right)) => Ok(left == right),
            (Value::Range(left), Value::Range(right)) => Ok(left == right),
            (Value::Array(left), Value::Array(right)) => left.equals(right, heap),
            (Value::Map(left), Value::Map(right)) => left.equals(right, heap),
            (Value::Module(left), Value::Module(right)) => Ok(left == right),

            // Mismatched Value variants are never equal.
            // We spell out every variant explicitly (instead of using `_`) so that
            // adding a new Value variant causes a non-exhaustive match error.
            (Value::Range(_), _)
            | (Value::Func(_), _)
            | (Value::Type(_), _)
            | (Value::Unit(_), _)
            | (Value::Str(_), _)
            | (Value::Bool(_), _)
            | (Value::Iterator(_), _)
            | (Value::Box(_), _)
            | (Value::Array(_), _)
            | (Value::Map(_), _)
            | (Value::Module(_), _)
            | (Value::Int(_), _) => Ok(false),
        }
    }
}

impl Comparison for ArrayValue {
    fn equals(&self, other: &Self, heap: &Heap) -> StrResult<bool> {
        let left = self.heap_ref().get_unwrap(heap);
        let right = other.heap_ref().get_unwrap(heap);

        left.equals(right, heap)
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

impl Comparison for MapValue {
    fn equals(&self, other: &Self, heap: &Heap) -> StrResult<bool> {
        let left = self.heap_ref().get_unwrap(heap);
        let right = other.heap_ref().get_unwrap(heap);

        left.equals(right, heap)
    }
}
