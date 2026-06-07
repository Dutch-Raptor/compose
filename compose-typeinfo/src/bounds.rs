use crate::{
    intern::{InterfaceName, TypeName},
    ty::{Ty, TypeVar},
};
use compose_syntax::Span;
use std::collections::HashMap;

/// A single interface bound declared on a type parameter.
#[derive(Debug, Clone)]
pub struct BoundEntry {
    pub interface: InterfaceName,
    pub interface_args: Vec<Ty>,
    /// Where this bound was declared in the source (for diagnostics).
    pub declared_at: Span,
}

/// What we know about in-scope type parameters while checking a generic
/// function body.
#[derive(Debug, Default)]
pub struct TypeParamEnv {
    bounds: HashMap<TypeVar, Vec<BoundEntry>>,
}

impl TypeParamEnv {
    pub fn register(&mut self, var: TypeVar, bounds: Vec<BoundEntry>) {
        self.bounds.insert(var, bounds);
    }

    pub fn is_type_param(&self, var: TypeVar) -> bool {
        self.bounds.contains_key(&var)
    }

    pub fn satisfies(&self, var: &TypeVar, interface: &InterfaceName) -> bool {
        self.bounds
            .get(var)
            .map(|bounds| bounds.iter().any(|b| &b.interface == interface))
            .unwrap_or(false)
    }

    pub fn bounds_for(&self, var: &TypeVar) -> &[BoundEntry] {
        self.bounds.get(var).map(|v| v.as_slice()).unwrap_or(&[])
    }
}

/// Records which concrete types implement which interfaces.
#[derive(Debug, Default, Clone)]
pub struct InterfaceTable {
    impls: HashMap<(TypeName, InterfaceName), ImplRecord>,
    primitive_impls: HashMap<(PrimitiveTy, InterfaceName), ImplRecord>,
    blanket_impls: Vec<BlanketImpl>,
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

#[derive(Debug, Clone)]
pub struct BlanketImpl {
    pub required_bound: InterfaceName,
    pub implemented_interface: InterfaceName,
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

    pub fn register_blanket(&mut self, required: InterfaceName, implemented: InterfaceName) {
        self.blanket_impls.push(BlanketImpl {
            required_bound: required,
            implemented_interface: implemented,
        });
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

    pub fn impl_record(&self, ty: &Ty, interface: &InterfaceName) -> Option<&ImplRecord> {
        match ty {
            Ty::App(name, _) => self.impls.get(&(name.clone(), interface.clone())),
            _ => None,
        }
    }
}
