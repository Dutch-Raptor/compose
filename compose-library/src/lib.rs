pub mod diag;
mod world;
mod foundations;
mod sink;
mod repr;

use compose_library::diag::SourceResult;
pub use foundations::*;
pub use sink::*;
pub use world::*;

// Workaround to refer to self as compose_library instead of crate. Needed for some macros
extern crate self as compose_library;

#[derive(Debug, Clone)]
pub struct Library {
    /// The module containing global functions, types and values.
    pub global: Module,
}

pub struct Routines {
    pub eval_closure: fn(&Func, &Closure, &dyn World, Args) -> SourceResult<Value>
}


pub fn library() -> Library {
    let mut global = Scope::new();

    global.define_func::<assert>();
    global.define_func::<panic>();
    global.define_func::<print>();
    global.define_func::<println>();
    global.define_type::<i64>();
    global.define_type::<Type>();
    
    Library {
        global: Module::new("global", global),
    }
}
