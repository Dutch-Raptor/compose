use compose_library::diag::{bail, StrResult};
use compose_library::gc::GcValue;
use compose_library::Value;
use compose_macros::func;
use compose_macros::{scope, ty};
use dumpster::Trace;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use tap::Pipe;

#[ty(scope, cast, name = "box")]
#[derive(Clone, Trace)]
pub struct Boxed(GcValue<Value>);

impl Boxed {
    pub fn get(&self) -> StrResult<RwLockReadGuard<Value>> {
        match self.0.try_get() {
            None => bail!("concurrent access"),
            Some(v) => Ok(v),
        }
    }

    pub fn get_mut(&mut self) -> StrResult<RwLockWriteGuard<Value>> {
        match self.0.try_get_mut() {
            None => bail!("concurrent access"),
            Some(v) => Ok(v),
        }
    }

    pub fn as_ptr(&self) -> *const RwLock<Value> {
        self.0.as_ptr()
    }
}

impl Debug for Boxed {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Boxed")
            .pipe(|mut s| {
                if let Ok(v) = self.get() {
                    s.field("value", &v);
                };
                s
            })
            .finish()
    }
}

#[scope]
impl Boxed {
    #[func]
    pub fn new(value: Value) -> Self {
        Self(GcValue::new(value))
    }
}

impl PartialEq for Boxed {
    fn eq(&self, other: &Self) -> bool {
        match (self.0.try_get(), other.0.try_get()) {
            (Some(a), Some(b)) => *a == *b,
            _ => false,
        }
    }
}

impl Display for Boxed {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "box({})",
            match self.0.try_get() {
                Some(v) => v.to_string(),
                None => "".to_string(),
            }
        )
    }
}
