use crate::{InterfaceName, InterfaceTable, Ty, TypeName};
use ecow::EcoString;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: EcoString,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: EcoString,
    pub receiver: Ty,
    pub ty: Ty,
    pub mutates_self: bool,
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: EcoString,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct InterfaceInfo {
    pub name: InterfaceName,
    pub methods: Vec<MethodInfo>,
}

#[derive(Debug, Clone, Default)]
pub struct TypeInfoRegistry {
    functions: HashMap<EcoString, FunctionInfo>,
    methods: HashMap<TypeName, Vec<MethodInfo>>,
    fields: HashMap<TypeName, Vec<FieldInfo>>,
    interfaces: HashMap<InterfaceName, InterfaceInfo>,
    interface_table: InterfaceTable,
}

impl TypeInfoRegistry {
    pub fn register_function(&mut self, name: impl Into<EcoString>, ty: Ty) {
        let name = name.into();
        self.functions
            .insert(name.clone(), FunctionInfo { name, ty });
    }

    pub fn function(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }

    pub fn register_method(&mut self, type_name: TypeName, method: MethodInfo) {
        self.methods.entry(type_name).or_default().push(method);
    }

    pub fn methods(&self, type_name: &TypeName) -> &[MethodInfo] {
        self.methods
            .get(type_name)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    pub fn register_field(&mut self, type_name: TypeName, field: FieldInfo) {
        self.fields.entry(type_name).or_default().push(field);
    }

    pub fn fields(&self, type_name: &TypeName) -> &[FieldInfo] {
        self.fields.get(type_name).map(Vec::as_slice).unwrap_or(&[])
    }

    pub fn register_interface(&mut self, interface: InterfaceInfo) {
        self.interfaces.insert(interface.name.clone(), interface);
    }

    pub fn interface(&self, name: &InterfaceName) -> Option<&InterfaceInfo> {
        self.interfaces.get(name)
    }

    pub fn interface_table(&self) -> &InterfaceTable {
        &self.interface_table
    }

    pub fn interface_table_mut(&mut self) -> &mut InterfaceTable {
        &mut self.interface_table
    }
}
