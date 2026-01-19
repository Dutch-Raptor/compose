// Workaround to refer to self as compose_library instead of crate. Needed for some macros
extern crate self as compose_library;
pub mod diag;
mod world;
mod foundations;
mod sink;
pub mod repr;
mod engine;
mod gc;
mod vm;
pub use engine::*;
pub use foundations::*;
pub use gc::*;
pub use sink::*;
use std::fmt::Debug;
pub use vm::*;
pub use world::*;

#[derive(Clone)]
pub struct Library {
    /// The module containing global functions, types and values.
    pub global: Module,
}

impl Debug for Library {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Library")
    }
}

impl Trace for Library {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.global.visit_refs(f);
    }
}

pub struct Routines {
}


pub fn library() -> Library {
    let mut global = Scope::new_lexical();

    global.define_func::<assert>();
    global.define_func::<panic>();
    global.define_func::<print>();
    global.define_func::<println>();
    global.define_type::<i64>();
    global.define_type::<bool>();
    global.define_type::<Type>();
    global.define_type::<IterValue>();
    global.define_type::<Func>();
    global.define_type::<Boxed>();
    global.define_type::<Str>();
    global.define_type::<MapValue>();
    global.define_type::<ArrayValue>();
    global.define_type::<RangeValue>();

    global.define("std", Module::new("std", global.clone()));
    
    Library {
        global: Module::new("global", global),
    }
}
