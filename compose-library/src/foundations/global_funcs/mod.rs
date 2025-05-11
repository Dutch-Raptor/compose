use crate::diag::{bail, StrResult};
use crate::Value;
use compose_macros::func;

#[func]
pub fn assert(cond: bool) -> StrResult<()> {
    if !cond {
        bail!("assertion failed")
    } else {
        Ok(())
    }
}

#[func]
pub fn assert_eq(a: Value, b: Value) -> StrResult<()> {
    if a != b {
        bail!("assertion failed: {:?} != {:?}", a, b)
    } else {
        Ok(())
    }
}

#[func]
pub fn assert_ne(a: Value, b: Value) -> StrResult<()> {
    if a == b {
        bail!("assertion failed: {:?} == {:?}", a, b)
    } else {
        Ok(())
    }
}

#[func]
pub fn panic(msg: Value) -> StrResult<()> {
    bail!("Panic: {:?}", msg)
}