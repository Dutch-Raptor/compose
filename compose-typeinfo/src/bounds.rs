use crate::{
    intern::{InterfaceName, TypeName},
    ty::Ty,
};
use compose_syntax::Span;
use std::collections::HashMap;

/// Records which concrete types implement which interfaces.
#[derive(Debug, Default, Clone)]
pub struct InterfaceTable {
    impls: HashMap<(TypeName, InterfaceName), ImplRecord>,
    primitive_impls: HashMap<(PrimitiveTy, InterfaceName), ImplRecord>,
}

#[derive(Debug, Clone)]
pub struct ImplRecord {
    pub impl_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveTy {
    Int,
    Bool,
    Float,
    Str,
    Unit,
}

impl InterfaceTable {
    pub fn register_impl(
        &mut self,
        type_name: TypeName,
        interface: InterfaceName,
        impl_span: Span,
    ) {
        self.impls
            .insert((type_name, interface), ImplRecord { impl_span });
    }

    pub fn register_primitive_impl(
        &mut self,
        prim: PrimitiveTy,
        interface: InterfaceName,
        impl_span: Span,
    ) {
        self.primitive_impls
            .insert((prim, interface), ImplRecord { impl_span });
    }

    pub fn implements(&self, ty: &Ty, interface: &InterfaceName) -> bool {
        match ty {
            Ty::App(name, _) => self.impls.contains_key(&(name.clone(), interface.clone())),
            Ty::Int => self
                .primitive_impls
                .contains_key(&(PrimitiveTy::Int, interface.clone())),
            Ty::Bool => self
                .primitive_impls
                .contains_key(&(PrimitiveTy::Bool, interface.clone())),
            Ty::Float => self
                .primitive_impls
                .contains_key(&(PrimitiveTy::Float, interface.clone())),
            Ty::Str => self
                .primitive_impls
                .contains_key(&(PrimitiveTy::Str, interface.clone())),
            Ty::Unit => self
                .primitive_impls
                .contains_key(&(PrimitiveTy::Unit, interface.clone())),
            Ty::Dyn(dyn_interface, _) => dyn_interface == interface,
            _ => false,
        }
    }
}
