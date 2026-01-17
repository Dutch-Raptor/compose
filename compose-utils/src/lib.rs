mod trace;

use std::hash::Hash;
use std::ops::{Deref, DerefMut};

pub use trace::*;

#[derive(Debug)]
pub struct Static<T: 'static>(pub &'static T);

impl<T> Deref for Static<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<T> Copy for Static<T> {}

impl<T> Clone for Static<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for Static<T> {}

impl<T> PartialEq for Static<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Hash for Static<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(std::ptr::from_ref(self.0) as _);
    }
}


/// Automatically calls a deferred function when the returned handle is dropped.
pub fn defer<T, F: FnOnce(&mut T)>(
    thing: &mut T,
    deferred: F,
) -> impl DerefMut<Target = T> {
    struct DeferHandle<'a, T, F: FnOnce(&mut T)> {
        thing: &'a mut T,
        deferred: Option<F>,
    }

    impl<'a, T, F: FnOnce(&mut T)> Drop for DeferHandle<'a, T, F> {
        fn drop(&mut self) {
            std::mem::take(&mut self.deferred).expect("deferred function")(self.thing);
        }
    }

    impl<T, F: FnOnce(&mut T)> Deref for DeferHandle<'_, T, F> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            self.thing
        }
    }

    impl<T, F: FnOnce(&mut T)> DerefMut for DeferHandle<'_, T, F> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            self.thing
        }
    }

    DeferHandle { thing, deferred: Some(deferred) }
}
