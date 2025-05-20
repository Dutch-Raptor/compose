pub mod diag;
mod world;
mod foundations;
mod sink;
mod repr;

pub use foundations::*;
pub use sink::*;
pub use world::*;
pub use foundations::ty::*;

// Workaround to refer to self as compose_library instead of crate. Needed for some macros
extern crate self as compose_library;

#[derive(Debug, Clone)]
pub struct Library {
    /// The module containing global functions, types and values.
    pub global: Module,
}


pub fn library() -> Library {
    let mut global = Scope::new();

    global.define_func::<assert>();
    global.define_func::<assert_eq>();
    global.define_func::<assert_ne>();
    global.define_func::<panic>();
    global.define_func::<add_one>();
    
    Library {
        global: Module::new("global", global),
    }
}
