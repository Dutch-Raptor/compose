use std::ops::{Deref, DerefMut};
use std::sync::RwLockWriteGuard;
use compose_library::diag::StrResult;
use compose_library::Value;
use crate::Boxed;

#[derive(Debug)]
pub struct ValueRef<'a>(Repr<'a>);

#[derive(Debug)]
enum Repr<'a> {
    Direct(&'a Value),
    Boxed(std::sync::RwLockReadGuard<'a, Value>)
}

impl<'a> ValueRef<'a> {
    pub fn direct(val: &'a Value) -> Self {
        ValueRef(Repr::Direct(val))
    }
    
    pub fn boxed(guard: std::sync::RwLockReadGuard<'a, Value>) -> Self {
        ValueRef(Repr::Boxed(guard))
    }
}

impl Boxed {
    pub fn as_ref(&self) -> StrResult<ValueRef> {
        self.get().map(ValueRef::boxed)
    }
    
    pub fn as_mut(&mut self) -> StrResult<ValueRefMut> {
        self.get_mut().map(ValueRefMut::boxed)
    }
}

impl Value {
    pub fn as_ref(&self) -> StrResult<ValueRef> {
        match self {
            Value::Box(b) => b.as_ref(),
            v => Ok(ValueRef::direct(v))
        }
    }
    
    pub fn as_mut(&mut self) -> StrResult<ValueRefMut> {
        match self {
            Value::Box(b) => b.as_mut(),
            v => Ok(ValueRefMut::direct(v))
        }
    }
}

impl Deref for ValueRef<'_> {
    type Target = Value;
    
    fn deref(&self) -> &Self::Target {
        match &self.0 {
            Repr::Direct(v) => v,
            Repr::Boxed(v) => v
        }
    }
}


#[derive(Debug)]
pub struct ValueRefMut<'a>(ReprMut<'a>);

#[derive(Debug)]
enum ReprMut<'a> {
    Direct(&'a mut Value),
    Boxed(RwLockWriteGuard<'a, Value>),
}

impl<'a> ValueRefMut<'a> {
    pub fn direct(val: &'a mut Value) -> Self {
        ValueRefMut(ReprMut::Direct(val))
    }

    pub fn boxed(guard: RwLockWriteGuard<'a, Value>) -> Self {
        ValueRefMut(ReprMut::Boxed(guard))
    }
    
    pub fn as_value(&self) -> &Value {
        match &self.0 {
            ReprMut::Direct(v) => v,
            ReprMut::Boxed(v) => v,
        }
    }
    
    pub fn as_value_mut(&mut self) -> &mut Value {
        match &mut self.0 {
            ReprMut::Direct(v) => v,
            ReprMut::Boxed(v) => v,
        }   
    }
}

impl Deref for ValueRefMut<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match &self.0 {
            ReprMut::Direct(v) => v,
            ReprMut::Boxed(v) => v,
        }
    }
}

impl DerefMut for ValueRefMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match &mut self.0 {
            ReprMut::Direct(v) => v,
            ReprMut::Boxed(v) => v,
        }
    }
}
