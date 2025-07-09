use crate::repr::Repr;
use crate::{HeapRef, Iter, IterValue, Trace};
use compose_library::diag::StrResult;
use compose_library::{ArrayIter, Heap, UntypedRef, Value, Vm};
use compose_macros::{func, scope, ty};
use ecow::{eco_format, EcoString};
use std::ops::{Deref, DerefMut};

#[ty(scope, cast, name = "array")]
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayValue(HeapRef<Array>);

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub(crate) values: Vec<Value>,
}

impl From<Vec<Value>> for Array {
    fn from(value: Vec<Value>) -> Self {
        Self { values: value }
    }
}

impl Deref for Array {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl DerefMut for Array {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

impl Array {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }
}

impl Trace for Array {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for v in &self.values {
            v.visit_refs(f);
        }
    }
}

impl ArrayValue {
    pub fn new(heap: &mut Heap) -> Self {
        Self(heap.alloc(Array::new()))
    }

    pub fn from(heap: &mut Heap, values: Vec<Value>) -> Self {
        Self(heap.alloc(Array::from(values)))
    }
}

impl Trace for ArrayValue {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(self.0.key());
    }
}

impl Repr for ArrayValue {
    fn repr(&self, vm: &dyn Vm) -> EcoString {
        let self_ = self.0.try_get(vm.heap()).expect("array value");

        let items = self_.values.iter().map(|v| v.repr(vm)).collect::<Vec<_>>();

        eco_format!("[{}]", items.join(", "))
    }
}

impl ArrayValue {
    pub(crate) fn try_get<'a>(&self, heap: &'a Heap) -> StrResult<&'a Array> {
        self.0.try_get(heap)
    }
}

#[scope]
impl ArrayValue {
    #[func]
    fn len(&self, vm: &dyn Vm) -> StrResult<usize> {
        Ok(self.0.try_get(vm.heap())?.len())
    }

    #[func]
    fn is_empty(&self, vm: &dyn Vm) -> StrResult<bool> {
        Ok(self.0.try_get(vm.heap())?.is_empty())
    }

    #[func]
    fn push(&mut self, vm: &mut dyn Vm, value: Value) -> StrResult<()> {
        let arr = self.0.try_get_mut(vm.heap_mut())?;

        arr.push(value);

        Ok(())
    }

    #[func]
    fn pop(&mut self, vm: &mut dyn Vm) -> StrResult<Option<Value>> {
        let arr = self.0.try_get_mut(vm.heap_mut())?;

        Ok(arr.pop())
    }

    #[func]
    pub fn shallow_clone(&self, vm: &mut dyn Vm) -> Self {
        let clone = self.0.get_unwrap(vm.heap()).clone();

        Self(vm.heap_mut().alloc(clone))
    }

    #[func]
    pub fn iter(&self, vm: &mut dyn Vm) -> StrResult<IterValue> {
        let arr = self.0.try_get(vm.heap())?;

        let iter = Iter::Array(ArrayIter::new(arr));
        Ok(IterValue::new(iter, vm))
    }

    #[func]
    pub fn contains(&self, vm: &mut dyn Vm, value: Value) -> StrResult<bool> {
        let arr = self.0.try_get(vm.heap())?;
        Ok(arr.contains(&value))
    }
}
