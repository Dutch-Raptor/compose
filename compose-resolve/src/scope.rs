use crate::ExprId;
use crate::module::ModuleId;
use compose_typeinfo::SymbolId;
use compose_utils::trace_log;
use ecow::EcoString;
use fxhash::FxHashMap;
use std::error::Error;
use std::fmt::Debug;
use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(NonZeroU64);

impl From<NonZeroU64> for ScopeId {
    fn from(id: NonZeroU64) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ScopeKind {
    Flow,
    Lexical,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeSource {
    ModuleRoot { module: ModuleId },
    Expr { module: ModuleId, expr: ExprId },
}

impl ScopeSource {
    pub(crate) fn module(module: ModuleId) -> Self {
        Self::ModuleRoot { module }
    }

    pub(crate) fn expr(module: ModuleId, expr: ExprId) -> Self {
        Self::Expr { expr, module }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    scope_source: ScopeSource,
    id: ScopeId,
    /// The Id of the parent scope, if any.
    parent: Option<ScopeId>,
    map: FxHashMap<EcoString, SymbolId>,
    kind: ScopeKind,
}

impl Scope {
    pub(crate) fn new_flow(id: ScopeId, source: ScopeSource, parent: ScopeId) -> Self {
        Self {
            id,
            scope_source: source,
            parent: Some(parent),
            map: FxHashMap::default(),
            kind: ScopeKind::Flow,
        }
    }

    pub(crate) fn new_lexical(
        id: ScopeId,
        scope_source: ScopeSource,
        parent: Option<ScopeId>,
    ) -> Self {
        Self {
            id,
            scope_source,
            parent,
            map: FxHashMap::default(),
            kind: ScopeKind::Lexical,
        }
    }

    pub fn bind(&mut self, name: EcoString, symbol_id: SymbolId) {
        self.map.insert(name, symbol_id);
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<SymbolId> {
        self.map.get(name.as_ref()).copied()
    }

    pub fn source(&self) -> &ScopeSource {
        &self.scope_source
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Scopes {
    stack: Vec<Scope>,
}

#[derive(Debug)]
pub struct TopScopeNotFlowError;

impl std::fmt::Display for TopScopeNotFlowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cannot bind to a symbol in the top scope, as it is not a flow scope."
        )
    }
}

impl Error for TopScopeNotFlowError {}

impl Scopes {
    pub(crate) fn new(root_scope_id: ScopeId, scope_source: ScopeSource) -> Self {
        Self {
            stack: vec![Scope::new_lexical(root_scope_id, scope_source, None)],
        }
    }

    pub(crate) fn bind_lexical(&mut self, name: EcoString, symbol_id: SymbolId) {
        let scope = self
            .stack
            .iter_mut()
            .rev()
            .find(|s| s.kind == ScopeKind::Lexical)
            .expect("No lexical scope found, at least one lexical scope must always exist. This is a compiler bug.");

        scope.bind(name, symbol_id);
    }

    pub(crate) fn bind_flow(
        &mut self,
        name: EcoString,
        symbol_id: SymbolId,
    ) -> Result<(), TopScopeNotFlowError> {
        let scope = self
            .stack
            .last_mut()
            .expect("No scope found, this is a compiler bug.");

        if scope.kind == ScopeKind::Flow {
            scope.bind(name, symbol_id);
            Ok(())
        } else {
            Err(TopScopeNotFlowError)
        }
    }

    pub(crate) fn get(&self, name: impl AsRef<str>) -> Option<SymbolId> {
        self.stack.iter().rev().find_map(|s| s.get(name.as_ref()))
    }

    fn top_scope(&self) -> &Scope {
        self.stack
            .last()
            .expect("No scopes on the stack, this is a compiler bug.")
    }

    fn top_lexical_mut(&mut self) -> &mut Scope {
        self.stack
            .iter_mut()
            .rev()
            .find(|s| s.kind == ScopeKind::Lexical)
            .expect("No lexical scope found, this is a compiler bug.")
    }

    fn push_scope(&mut self, scope: Scope) {
        self.stack.push(scope);
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.stack.pop()
    }
}

#[derive(Debug)]
pub(crate) struct Frames {
    stack: Vec<Scopes>,
    scopes: FxHashMap<ScopeSource, Scope>,
}

impl Frames {
    pub(crate) fn to_scopes(&self) -> Vec<Scope> {
        self.stack
            .iter()
            .flat_map(|s| s.stack.iter())
            .chain(self.scopes.values())
            .cloned()
            .collect()
    }
}

impl Frames {
    pub(crate) fn new() -> Self {
        Self {
            stack: Vec::new(),
            scopes: FxHashMap::default(),
        }
    }

    pub(crate) fn current(&self) -> &Scope {
        self.top_scopes().top_scope()
    }

    pub(crate) fn enter_frame(&mut self, root_scope_id: ScopeId, scope_source: ScopeSource) {
        self.stack.push(Scopes::new(root_scope_id, scope_source));
    }

    pub(crate) fn exit_frame(&mut self) {
        let scopes = self
            .stack
            .pop()
            .expect("No scopes to exit, this is a compiler bug.");
        for scope in scopes.stack {
            self.scopes.insert(scope.scope_source, scope);
        }
    }

    fn top_scopes(&self) -> &Scopes {
        self.stack
            .last()
            .expect("No scopes on the stack, this is a compiler bug.")
    }

    fn top_scopes_mut(&mut self) -> &mut Scopes {
        self.stack
            .last_mut()
            .expect("No scopes on the stack, this is a compiler bug.")
    }

    pub(crate) fn in_flow(&self) -> bool {
        self.top_scopes().top_scope().kind == ScopeKind::Flow
    }

    pub(crate) fn enter_flow(&mut self, scope_source: ScopeSource, scope_id: ScopeId) {
        let parent = self.top_scopes_mut().top_scope().id;
        trace_log!(
            "Entering flow scope: {:?} (parent: {:?})",
            scope_source,
            parent
        );
        self.top_scopes_mut()
            .push_scope(Scope::new_flow(scope_id, scope_source, parent));
    }

    pub(crate) fn exit_flow(&mut self) {
        trace_log!("Exiting flow scope");
        let scope = self
            .top_scopes_mut()
            .pop_scope()
            .expect("No flow scope to exit, this is a compiler bug.");
        assert_eq!(
            scope.kind,
            ScopeKind::Flow,
            "Exiting flow scope, but scope kind is not flow."
        );
        self.scopes.insert(scope.scope_source, scope);
    }

    pub(crate) fn enter_lexical(&mut self, scope_source: ScopeSource, scope_id: ScopeId) {
        let parent = self.top_scopes_mut().top_scope().id;
        trace_log!(
            "Entering lexical scope: {:?} (parent: {:?})",
            scope_source,
            parent
        );
        self.top_scopes_mut()
            .push_scope(Scope::new_lexical(scope_id, scope_source, Some(parent)));
    }

    pub(crate) fn exit_lexical(&mut self) {
        trace_log!("Exiting lexical scope");
        let scope = self
            .top_scopes_mut()
            .pop_scope()
            .expect("No lexical scope to exit, this is a compiler bug.");

        assert_eq!(
            scope.kind,
            ScopeKind::Lexical,
            "Exiting lexical scope, but scope kind is not lexical."
        );
        assert!(
            !self.top_scopes_mut().stack.is_empty(),
            "At least one scope should always exist on the stack, this is a compiler bug."
        );

        self.scopes.insert(scope.scope_source, scope);
    }

    pub(crate) fn get(&self, name: &str) -> Option<SymbolId> {
        self.top_scopes().get(name)
    }

    pub(crate) fn bind_flow(
        &mut self,
        name: EcoString,
        symbol_id: SymbolId,
    ) -> Result<(), TopScopeNotFlowError> {
        self.top_scopes_mut().bind_flow(name, symbol_id)
    }

    pub(crate) fn bind_lexical(&mut self, name: EcoString, symbol_id: SymbolId) {
        self.top_scopes_mut().bind_lexical(name, symbol_id);
    }
}
