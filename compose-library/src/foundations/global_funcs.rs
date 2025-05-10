use ecow::{eco_format, EcoString};
use crate::diag::{bail, StrResult};
use crate::Value;
use compose_macros::func;

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

#[func]
pub fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[func]
pub fn assert_even(a: i64) -> StrResult<i64> {
    if a % 2 != 0 {
        bail!("assertion failed: {:?} is not even", a)
    } else {
        Ok(a)
    }
}

#[func]
pub fn str_add(a: EcoString, b: EcoString) -> StrResult<EcoString> {
    Ok(eco_format!("{}{}", a, b))
}