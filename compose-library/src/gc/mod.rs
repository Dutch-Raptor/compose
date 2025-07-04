mod clean;
mod trigger;

use crate::gc::trigger::{GcTriggerPolicy, SimplePolicy};
use crate::{Array, Value};
use compose_library::gc::trigger::GcData;
use compose_library::Iter;
use slotmap::{new_key_type, SlotMap};
use std::fmt::Debug;
use std::ops::Deref;

#[derive(Debug)]
pub struct Heap {
    map: SlotMap<UntypedRef, HeapItem>,
    policy: Box<dyn GcTriggerPolicy>,
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HeapRef<T: HeapObject> {
    key: UntypedRef,
    _marker: std::marker::PhantomData<T>,
}

impl<T: HeapObject> Clone for HeapRef<T> {
    fn clone(&self) -> Self {
        HeapRef {
            key: self.key,
            _marker: Default::default(),
        }
    }
}
impl<T: HeapObject> Copy for HeapRef<T> {}

impl<T: HeapObject> HeapRef<T> {
    pub fn key(&self) -> UntypedRef {
        self.key
    }
}

impl<T> Deref for HeapRef<T>
where
    T: HeapObject,
{
    type Target = UntypedRef;

    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

impl<T> From<UntypedRef> for HeapRef<T>
where
    T: HeapObject,
{
    fn from(key: UntypedRef) -> Self {
        Self {
            key,
            _marker: Default::default(),
        }
    }
}

new_key_type! {
    pub struct UntypedRef;
}

impl<T> HeapRef<T>
where
    T: HeapObject,
{
    pub fn get(self, heap: &Heap) -> Option<&T> {
        heap.get(self)
    }
    
    pub fn try_get(self, heap: &Heap) -> StrResult<&T> {
        heap.get(self).ok_or_else(|| "Use after free. This is a bug.".into())
    }

    pub fn get_mut(self, heap: &mut Heap) -> Option<&mut T> {
        heap.get_mut(self)
    }
    
    pub fn try_get_mut(self, heap: &mut Heap) -> StrResult<&mut T> {
        heap.get_mut(self).ok_or_else(|| "Use after free. This is a bug.".into())
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            map: SlotMap::with_key(),
            policy: Box::new(SimplePolicy {
                heap_size_threshold: 500,
            })
        }
    }

    pub fn get_mut<T: HeapObject>(&mut self, key: HeapRef<T>) -> Option<&mut T> {
        self.map.get_mut(*key)?.as_type_mut()
    }
    pub fn alloc<T: HeapObject>(&mut self, value: T) -> HeapRef<T> {
        self.map.insert(value.to_untyped()).into()
    }

    pub fn data(&self) -> GcData {
        GcData {
            heap_size: self.map.len(),
        }
    }

    pub fn get<T: HeapObject>(&self, key: HeapRef<T>) -> Option<&T> {
        self.map.get(*key)?.as_type()
    }

    pub fn remove<T: HeapObject>(&mut self, key: HeapRef<T>) -> Option<HeapItem> {
        self.map.remove(*key)
    }
    
    pub fn get_untyped(&self, key: UntypedRef) -> Option<&HeapItem> {
        self.map.get(key)
    }
}

/// A type that can keep references to heap values
pub trait Trace: Debug {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef));
}

impl Trace for &[UntypedRef] {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for key in *self {
            f(*key)
        }
    }
}

pub trait HeapObject: Trace + Debug {
    fn from_untyped(untyped: &HeapItem) -> Option<&Self>;
    fn from_untyped_mut(untyped: &mut HeapItem) -> Option<&mut Self>;
    fn to_untyped(self) -> HeapItem;
}

macro_rules! heap_enum {
    (
        $(#[$meta:meta])*
        pub enum $enum_name:ident {
            $(
                $variant:ident($ty:ty)
            ),* $(,)?
        }
    ) => {
        $(#[$meta])*
        pub enum $enum_name {
            $(
                $variant($ty),
            )*
        }

        impl Trace for $enum_name {
            fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
                match self {
                    $(
                        $enum_name::$variant(inner) => inner.visit_refs(f),
                    )*
                }
            }
        }
    };
}

impl Trace for Value {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        match self {
            Value::Int(_) => {}
            Value::Bool(_) => {}
            Value::Unit(_) => {}
            Value::Str(_) => {}
            Value::Func(func) => func.visit_refs(f),
            Value::Type(ty) => ty.visit_refs(f),
            Value::Iterator(i) => i.visit_refs(f),
            Value::Box(b) => f(b.key()),
            Value::Array(a) => a.visit_refs(f),
            Value::Range(r) => r.visit_refs(f),
        }
    }
}

macro_rules! impl_heap_obj {
    ($ident:ident, $ty:ty) => {
        impl HeapObject for $ty {
            fn from_untyped(untyped: &HeapItem) -> Option<&Self> {
                match untyped {
                    HeapItem::$ident(v) => Some(v),
                    _ => None,
                }
            }

            fn from_untyped_mut(untyped: &mut HeapItem) -> Option<&mut Self> {
                match untyped {
                    HeapItem::$ident(v) => Some(v),
                    _ => None,
                }
            }

            fn to_untyped(self) -> HeapItem {
                HeapItem::$ident(self)
            }
        }
    };
}

pub(crate) use impl_heap_obj;
use crate::diag::StrResult;

impl_heap_obj!(Value, Value);
impl_heap_obj!(Iter, Iter);
impl_heap_obj!(Array, Array);

heap_enum! {
    #[derive(Debug, Clone, PartialEq)]
    pub enum HeapItem {
        Value(Value),
        Iter(Iter),
        Array(Array),
    }
}

impl HeapItem {
    fn as_type<T: HeapObject>(&self) -> Option<&T> {
        T::from_untyped(self)
    }

    fn as_type_mut<T: HeapObject>(&mut self) -> Option<&mut T> {
        T::from_untyped_mut(self)
    }
}
