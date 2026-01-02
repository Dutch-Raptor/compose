use crate::repr::Repr;
use crate::{ArrayIter, Heap, Trace};
use compose_library::{HeapRef, IntoValue, IterValue, UntypedRef, Value, Vm};
use compose_macros::func;
use compose_macros::{scope, ty};
use ecow::{EcoString, EcoVec};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use compose_library::diag::StrResult;
use compose_library::ops::Comparison;

#[ty(scope, cast, name = "Map")]
#[derive(Debug, Clone, PartialEq)]
pub struct MapValue(HeapRef<Map>);

impl Trace for MapValue {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(self.0.key())
    }
}

impl MapValue {
    pub fn new(heap: &mut Heap) -> Self {
        Self(heap.alloc(Map::new()))
    }

    pub fn from(heap: &mut Heap, values: HashMap<EcoString, Value>) -> Self {
        Self(heap.alloc(values.into()))
    }

    pub fn shallow_clone(&self, vm: &mut dyn Vm) -> Self {
        let clone = self.0.get_unwrap(vm.heap()).clone();

        Self(vm.heap_mut().alloc(clone))
    }

    pub fn heap_ref(&self) -> HeapRef<Map> {
        self.0
    }
}

#[scope]
impl MapValue {
    #[func]
    pub fn get(&self, vm: &dyn Vm, key: EcoString) -> Option<Value> {
        self.0.get_unwrap(vm.heap()).get(&key).cloned()
    }

    #[func]
    pub fn insert(&mut self, vm: &mut dyn Vm, key: EcoString, value: Value) {
        self.0.get_mut_unwrap(vm.heap_mut()).insert(key, value);
    }

    #[func]
    pub fn len(&self, vm: &dyn Vm) -> usize {
        self.0.get_unwrap(vm.heap()).len()
    }

    #[func]
    pub fn contains_key(&self, vm: &dyn Vm, key: EcoString) -> bool {
        self.0.get_unwrap(vm.heap()).contains_key(&key)
    }

    #[func]
    pub fn values(&self, vm: &mut dyn Vm) -> IterValue {
        let values: EcoVec<_> = self.0.get_unwrap(vm.heap()).values().cloned().collect();

        IterValue::new(ArrayIter::from(values).into(), vm)
    }

    #[func]
    pub fn keys(&self, vm: &mut dyn Vm) -> IterValue {
        let keys: EcoVec<_> = self
            .0
            .get_unwrap(vm.heap())
            .keys()
            .map(|s| s.as_str().into_value())
            .collect();

        IterValue::new(ArrayIter::from(keys).into(), vm)
    }
}

impl Repr for MapValue {
    fn repr(&self, vm: &dyn Vm) -> EcoString {
        let inner = self.0.get_unwrap(vm.heap());
        let mut repr = EcoString::new();
        repr.push_str("{\n");
        for (key, value) in inner.iter() {
            repr.push_str(&format!("\t{}: {},\n", key, value.repr(vm)));
        }
        repr.push('}');
        repr
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map {
    values: HashMap<EcoString, Value>,
}

impl Map {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}

impl Comparison for Map {
    fn equals(&self, other: &Self, _heap: &Heap) -> StrResult<bool> {
        Ok(self.values == other.values)
    }
}

impl Trace for Map {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for (_, value) in self.values.iter() {
            value.visit_refs(f);
        }
    }
}

impl Deref for Map {
    type Target = HashMap<EcoString, Value>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl DerefMut for Map {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

impl From<HashMap<EcoString, Value>> for Map {
    fn from(values: HashMap<EcoString, Value>) -> Self {
        Self { values }
    }
}
