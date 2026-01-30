mod clean;
mod trigger;

use crate::Value;
use crate::gc::trigger::{GcTriggerPolicy, SimplePolicy};
use compose_library::foundations::iterator::Iter;
use compose_library::foundations::types::{Array, Map};
use compose_library::gc::trigger::GcData;
use slotmap::{SlotMap, new_key_type};
use std::fmt::Debug;
use std::ops::Deref;

/// A managed heap containing [`HeapItem`]s accessible through [`UntypedRef`] or [`HeapRef`] keys.
///
/// The heap implements garbage collection through a simple mark and sweep algorithm. When cleaning the roots need to be provided.
/// These roots are any type implementing the [Trace] trait. Anything not accessible through the provided roots will be deallocated.
///
/// # Example: Interacting with heap values
///
/// ```
/// use compose_library::{gc::Heap, Value, gc::HeapItem};
/// let mut heap = Heap::new();
///
/// let value = Value::Int(6);
///
/// // Allocate a value on the heap and get a key to it.
/// let key = heap.alloc(value);
///
/// // Now the value is accessible through the key.
/// assert_eq!(heap.get(key), Some(&Value::Int(6)));
///
/// let value_mut = heap.get_mut(key).unwrap();
/// *value_mut = Value::Int(7);
///
/// // The value is updated and anyone with the key will see the new value
/// assert_eq!(heap.get(key), Some(&Value::Int(7)));
///
/// // Remove the value from the heap. This will give ownership of the value to the caller.
/// let removed_item = heap.remove(key);
///
/// assert_eq!(removed_item, Some(HeapItem::Value(Value::Int(7))));
/// assert_eq!(heap.get(key), None);
/// ```
///
/// # Example: Garbage Collection
///
/// ```
/// use compose_library::gc::{Heap, UntypedRef};
/// use compose_library::Value;
/// let mut heap = Heap::new();
///
/// let key = heap.alloc(Value::Int(6));
/// assert_eq!(heap.metadata().heap_size, 1);
/// assert_eq!(heap.get(key), Some(&Value::Int(6)));
///
/// // clean while passing the key to the value as a simple root
/// heap.clean(&key);
///
/// // The value is still accessible and the heap size hasn't changed
/// assert_eq!(heap.metadata().heap_size, 1);
/// assert_eq!(heap.get(key), Some(&Value::Int(6)));
///
/// // clean while passing a root that doesn't have access to the value
/// heap.clean(&None::<UntypedRef>);
///
/// assert_eq!(heap.metadata().heap_size, 0);
/// assert_eq!(heap.get(key), None);
/// ```
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

// Manually implement clone and copy for HeapRef because
// the derive macro puts a Clone requirement on T, which is not
// actually required
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
    #[track_caller]
    pub fn get_unwrap(self, heap: &Heap) -> &T {
        heap.get(self)
            .unwrap_or_else(|| panic!("Use after free. This is a bug. Key: {:?}", self.key))
    }

    pub fn try_get(self, heap: &Heap) -> StrResult<&T> {
        heap.get(self)
            .ok_or_else(|| "Use after free. This is a bug.".into())
    }

    #[track_caller]
    pub fn get_mut_unwrap(self, heap: &mut Heap) -> &mut T {
        heap.get_mut(self)
            .unwrap_or_else(|| panic!("Use after free. This is a bug. Key: {:?}", self.key))
    }

    pub fn try_get_mut(self, heap: &mut Heap) -> StrResult<&mut T> {
        heap.get_mut(self)
            .ok_or_else(|| "Use after free. This is a bug.".into())
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            map: SlotMap::with_key(),
            policy: Box::new(SimplePolicy {
                heap_size_threshold: 500,
            }),
        }
    }

    pub fn get_mut<T: HeapObject>(&mut self, key: HeapRef<T>) -> Option<&mut T> {
        self.map.get_mut(*key)?.as_type_mut()
    }
    pub fn alloc<T: HeapObject>(&mut self, value: T) -> HeapRef<T> {
        self.map.insert(value.to_untyped()).into()
    }

    pub fn metadata(&self) -> GcData {
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

impl<T: HeapObject> Trace for HeapRef<T> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(self.key())
    }
}

impl Trace for UntypedRef {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(*self)
    }
}

impl<T: Trace> Trace for Option<T> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        if let Some(v) = self {
            v.visit_refs(f)
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
            Value::Map(m) => m.visit_refs(f),
            Value::Module(m) => m.visit_refs(f),
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

use crate::diag::StrResult;

impl_heap_obj!(Value, Value);
impl_heap_obj!(Iter, Iter);
impl_heap_obj!(Array, Array);
impl_heap_obj!(Map, Map);

heap_enum! {
    #[derive(Debug, Clone, PartialEq)]
    pub enum HeapItem {
        Value(Value),
        Iter(Iter),
        Array(Array),
        Map(Map)
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
