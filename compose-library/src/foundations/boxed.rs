use crate::gc::HeapRef;
use crate::UntypedRef;
use compose_library::gc::Heap;
use compose_library::vm::Vm;
use compose_library::Value;
use compose_macros::func;
use compose_macros::{scope, ty};
use std::fmt::{Debug, Display, Formatter};

#[ty(scope, cast, name = "box")]
#[derive(Clone, Debug, PartialEq)]
pub struct Boxed(HeapRef<Value>);

impl Boxed {
    pub fn get<'a>(&self, heap: &'a Heap) -> Option<&'a Value> {
        heap.get(self.0)
    }

    pub fn get_mut<'a>(&self, heap: &'a mut Heap) -> Option<&'a mut Value> {
        heap.get_mut(self.0)
    }

    pub fn key(&self) -> UntypedRef {
        self.0.key()
    }
}

#[scope]
impl Boxed {
    #[func]
    pub fn new(vm: &mut dyn Vm, value: Value) -> Self {
        Self(vm.heap_mut().alloc(value))
    }
}

impl Display for Boxed {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "box({:?})", self.0)
    }
}
