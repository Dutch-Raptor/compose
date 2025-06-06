use dumpster::Trace;
use dumpster::sync::Gc;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

#[derive(Debug, Clone, Trace)]
pub struct GcValue<T>(Gc<RwLock<T>>)
where
    T: Trace + Send + Sync + 'static;

impl<T> GcValue<T>
where
    T: Trace + Send + Sync + 'static,
{
    pub fn new(value: T) -> Self {
        GcValue(Gc::new(RwLock::new(value)))
    }

    pub fn get(&self) -> RwLockReadGuard<T> {
        self.0.read().unwrap()
    }

    pub fn get_mut(&self) -> RwLockWriteGuard<T> {
        self.0.write().unwrap()
    }

    pub fn try_get(&self) -> Option<RwLockReadGuard<T>> {
        self.0.try_read().ok()
    }

    pub fn try_get_mut(&self) -> Option<RwLockWriteGuard<T>> {
        self.0.try_write().ok()
    }

    pub fn as_ptr(&self) -> *const RwLock<T> {
        Gc::<RwLock<T>>::as_ptr(&self.0)
    }
}

impl<T: Trace + Send + Sync + Clone + 'static> GcValue<T> {
    pub fn clone_value(&self) -> T {
        self.get().clone()
    }
}
