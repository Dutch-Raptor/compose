use crate::Value;
use crate::diag::{bail, StrResult};
use compose_macros::func;

mod assertions;

pub use assertions::*;

#[func]
pub fn add_one(x: i64) -> i64 {
    x + 1
}

#[func]
pub fn panic(msg: Value) -> StrResult<()> {
    bail!("Panic: {:?}", msg)
}
