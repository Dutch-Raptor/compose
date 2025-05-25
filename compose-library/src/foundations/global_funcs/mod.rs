use crate::Value;
use crate::diag::{StrResult, bail};
use compose_macros::func;

mod assertions;

pub use assertions::*;

#[func]
pub fn panic(msg: Value) -> StrResult<()> {
    bail!("Panic: {:?}", msg)
}

#[func]
pub fn print(#[variadic] print_args: Vec<Value>) {
    let message = print_args
        .iter()
        .map(|v| format!("{v:?}"))
        .collect::<Vec<_>>()
        .join(" ");
    print!("{}", message);
}
#[func]
pub fn println(#[variadic] print_args: Vec<Value>) {
    let message = print_args
        .iter()
        .map(|v| format!("{v:?}"))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", message);
}
