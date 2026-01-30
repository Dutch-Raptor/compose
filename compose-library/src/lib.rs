// Workaround to refer to self as compose_library instead of crate. Needed for some macros
extern crate self as compose_library;
pub mod diag;
pub mod world;
pub mod foundations;
pub mod sink;
pub mod repr;
pub mod engine;
pub mod gc;
pub mod vm;

pub use diag::{SourceResult, SourceDiagnostic};
pub use world::{World};
pub use foundations::{Value};
pub use vm::{Vm};
use std::fmt::Debug;
use compose_library::{
    foundations::global_funcs::{assert, panic, print, println},
    foundations::iterator::IterValue,
    foundations::module::Module,
    foundations::scope::Scope,
    foundations::types::{ArrayValue, Boxed, Func, MapValue, RangeValue, Str, Type},
    gc::{Trace, UntypedRef}
};

#[derive(Clone)]
pub struct Library {
    /// The module containing global functions, types and values.
    pub global: Module,
}

impl Library {
    pub fn empty() -> Self {
        Self {
            global: Module::new("std", Scope::new_lexical())
        }
    }
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
