pub mod diag;
mod world;
mod foundations;
mod sink;

pub use foundations::*;
pub use sink::*;
pub use world::*;

// Workaround to refer to self as compose_library instead of crate. Needed for some macros
extern crate self as compose_library;