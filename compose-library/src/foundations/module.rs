use ecow::EcoString;
use compose_syntax::FileId;
use crate::Scope;




#[derive(Debug, Clone)]
pub struct Module {
    /// The name of the module.
    name: Option<EcoString>,
    /// The scope containing the module's definitions.
    scope: Scope,
    /// The file that defines this module. Or `None` if the module is defined in memory.
    file_id: Option<FileId>,
}

/// Creators
impl Module {
    pub fn new(name: impl Into<EcoString>, scope: Scope) -> Self {
        Self {
            name: Some(name.into()),
            scope,
            file_id: None,
        }   
    }
    
    pub fn anonymous(scope: Scope) -> Self {
        Self {
            name: None,
            scope,
            file_id: None,
        }
    }
    
    pub fn with_file_id(mut self, file_id: FileId) -> Self {
        self.file_id = Some(file_id);
        self
    }
    
    pub fn with_name(mut self, name: impl Into<EcoString>) -> Self {
        self.name = Some(name.into());
        self   
    }
    
    pub fn with_scope(mut self, scope: Scope) -> Self {
        self.scope = scope;
        self  
    }
}

/// Accessors
impl Module {
    pub fn name(&self) -> Option<&EcoString> {
        self.name.as_ref()
    }
    
    pub fn scope(&self) -> &Scope {
        &self.scope
    }
    
    pub fn scope_mut(&mut self) -> &mut Scope {
        &mut self.scope
    }
    
    pub fn file_id(&self) -> Option<FileId> {
        self.file_id
    }
}