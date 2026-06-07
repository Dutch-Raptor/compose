// Workaround to refer to self as compose_library instead of crate. Needed for some macros
extern crate self as compose_library;
pub mod diag;
pub mod engine;
pub mod foundations;
pub mod gc;
pub mod repr;
pub mod sink;
pub mod vm;
pub mod world;

use compose_library::{
    foundations::global_funcs::{assert, panic, print, println},
    foundations::iterator::IterValue,
    foundations::module::Module,
    foundations::scope::Scope,
    foundations::type_info::standard_type_info,
    foundations::types::{ArrayValue, Boxed, Func, MapValue, RangeValue, Str, Type},
    gc::{Trace, UntypedRef},
};
use compose_typeinfo::TypeInfoRegistry;
pub use diag::{SourceDiagnostic, SourceResult};
pub use foundations::Value;
use std::fmt::Debug;
pub use vm::Vm;
pub use world::World;

#[derive(Clone)]
pub struct Library {
    /// The module containing global functions, types, and values.
    pub global: Module,
    /// Static type metadata for the standard library/runtime surface.
    pub type_info: TypeInfoRegistry,
}

impl Default for Library {
    fn default() -> Self {
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
            type_info: standard_type_info(),
        }
    }
}

impl Library {
    pub fn empty() -> Self {
        Self {
            global: Module::new("std", Scope::new_lexical()),
            type_info: TypeInfoRegistry::default(),
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
