use std::fmt::Debug;
use compose_library::{HeapObject, HeapRef, UntypedRef, Value};

/// A type that can keep references to heap values
pub trait Trace {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef));
}

impl Trace for UntypedRef {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(*self);
    }
}

impl<T: HeapObject> Trace for HeapRef<T> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        f(self.key);
    }
}

impl<T: Trace> Trace for Vec<T> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for traceable in self {
            traceable.visit_refs(f)
        }
    }
}

impl<T: Trace, const N: usize> Trace for [T; N] {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for traceable in self {
            traceable.visit_refs(f)
        }
    }   
}

impl<T: Trace> Trace for [T] {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for traceable in self {
            traceable.visit_refs(f)
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        if let Some(traceable) = self {
            traceable.visit_refs(f)
        }
    }
}

impl<T: Trace> Trace for Box<T> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        (**self).visit_refs(f)
    }
}

impl<T: Trace> Trace for &T {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        (**self).visit_refs(f)
    }
}

impl<T: Trace, E: Debug> Trace for Result<T, E> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        if let Ok(traceable) = self {
            traceable.visit_refs(f)
        }
    }
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