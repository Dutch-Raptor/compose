use crate::diag::{StrResult, bail};
use crate::{Engine, Value};
use compose_macros::func;
use ecow::EcoString;
use std::io::Write;

mod assertions;

pub use assertions::*;

#[func]
pub fn panic(msg: Value) -> StrResult<()> {
    bail!("Panic: {:?}", msg)
}

#[func]
pub fn print(engine: &mut Engine, #[variadic] print_args: Vec<Value>) -> StrResult<()> {
    engine
        .world
        .write(&|wtr: &mut dyn Write| write!(wtr, "{}", join_args(&print_args)))
        .map_err(|e| e.to_string().into())
}
#[func]
pub fn println(engine: &mut Engine, #[variadic] print_args: Vec<Value>) -> StrResult<()> {
    engine
        .world
        .write(&|wtr: &mut dyn Write| writeln!(wtr, "{}", join_args(&print_args)))
        .map_err(|e| e.to_string().into())
}

fn join_args(args: &[Value]) -> EcoString {
    let mut joined = EcoString::new();
    for arg in args {
        if !joined.is_empty() {
            joined.push(' ');
        }
        joined.push_str(&arg.to_string());
    }
    joined
}
