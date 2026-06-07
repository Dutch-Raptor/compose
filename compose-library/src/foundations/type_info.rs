use compose_syntax::Span;
use compose_typeinfo::{InterfaceInfo, InterfaceName, PrimitiveTy, Ty, TypeInfoRegistry};

use crate::foundations::global_funcs::{print, println};
use crate::foundations::types::func::NativeFunc;
use compose_macros::interface;

#[interface(name = "Display")]
pub trait ComposeDisplay {}

pub trait NativeInterface {
    const NAME: &'static str;

    fn interface_name() -> InterfaceName {
        InterfaceName::std(Self::NAME)
    }

    fn interface_info() -> InterfaceInfo {
        InterfaceInfo {
            name: Self::interface_name(),
            methods: Vec::new(),
        }
    }

    fn interface_ty() -> Ty {
        Ty::Dyn(Self::interface_name(), Vec::new())
    }
}

pub fn standard_type_info() -> TypeInfoRegistry {
    let mut registry = TypeInfoRegistry::default();

    registry.register_interface(<dyn ComposeDisplay>::interface_info());
    let display = <dyn ComposeDisplay>::interface_name();

    for primitive in [
        PrimitiveTy::Int,
        PrimitiveTy::Bool,
        PrimitiveTy::Float,
        PrimitiveTy::Str,
        PrimitiveTy::Unit,
    ] {
        registry.interface_table_mut().register_primitive_impl(
            primitive,
            display.clone(),
            Span::detached(),
        );
    }

    register_native_func::<print>(&mut registry);
    register_native_func::<println>(&mut registry);

    registry
}

fn register_native_func<T: NativeFunc>(registry: &mut TypeInfoRegistry) {
    let data = T::data();
    registry.register_function(data.name, data.ty().clone());
}
