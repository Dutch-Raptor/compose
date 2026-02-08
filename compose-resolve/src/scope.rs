use crate::{ExprId, SymbolId};
use ecow::EcoString;
use fxhash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ScopeKind {
    Flow,
    Lexical,
}

pub(crate) struct Scope {
    /// The Id of the expression that introduced this scope
    id: ExprId,
    /// The Id of the parent scope, if any.
    parent: Option<ExprId>,
    map: FxHashMap<EcoString, SymbolId>,
    kind: ScopeKind,
}

impl Scope {
    pub(crate) fn new_flow(id: ExprId, parent: ExprId) -> Self {
        Self {
            id,
            parent: Some(parent),
            map: FxHashMap::default(),
            kind: ScopeKind::Flow,
        }
    }

    pub(crate) fn new_lexical(id: ExprId, parent: Option<ExprId>) -> Self {
        Self {
            id,
            parent,
            map: FxHashMap::default(),
            kind: ScopeKind::Lexical,
        }
    }

    pub(crate) fn bind(&mut self, name: EcoString, symbol_id: SymbolId) {
        self.map.insert(name, symbol_id);
    }

    pub(crate) fn get(&self, name: impl AsRef<str>) -> Option<SymbolId> {
        self.map.get(name.as_ref()).copied()
    }
}

pub(crate) struct Scopes {
    stack: Vec<Scope>,
}

impl Scopes {
    pub(crate) fn new(scope_expr_id: ExprId) -> Self {
        Self {
            stack: vec![Scope::new_lexical(scope_expr_id, None)],
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

    pub(crate) fn bind_flow(&mut self, name: EcoString, symbol_id: SymbolId) {
        let scope = self
            .stack
            .last_mut()
            .expect("No flow scope found, this is a compiler bug.");

        scope.bind(name, symbol_id);
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

    fn top_flow_mut(&mut self) -> Option<&mut Scope> {
        self.stack
            .iter_mut()
            .rev()
            .find(|s| s.kind == ScopeKind::Flow)
    }

    fn push_scope(&mut self, scope: Scope) {
        self.stack.push(scope);
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.stack.pop()
    }
}

pub(crate) struct Frames {
    stack: Vec<Scopes>,
    scopes: FxHashMap<ExprId, Scope>,
}

impl Frames {
    pub(crate) fn new() -> Self {
        let id = ScopeId::new(0);
        Self {
            stack: vec![Scopes::new(id)],
            scopes: FxHashMap::default(),
            next_scope_id: id.next(),
        }
    }

    fn top_scopes(&mut self) -> &mut Scopes {
        self.stack
            .last_mut()
            .expect("No scopes on the stack, this is a compiler bug.")
    }

    fn advance_id(&mut self) -> ScopeId {
        let current = self.next_scope_id;
        self.next_scope_id = self.next_scope_id.next();
        current
    }

    pub(crate) fn enter_flow(&mut self) {
        let parent = self.top_scopes().top_scope().id;
        let id = self.advance_id();
        self.top_scopes().push_scope(Scope::new_flow(id, parent));
    }

    pub(crate) fn exit_flow(&mut self) {
        let scope = self
            .top_scopes()
            .pop_scope()
            .expect("No flow scope to exit, this is a compiler bug.");
        assert_eq!(
            scope.kind,
            ScopeKind::Flow,
            "Exiting flow scope, but scope kind is not flow."
        );
        self.scopes.insert(scope.id, scope);
    }

    pub(crate) fn enter_lexical(&mut self) {
        let parent = self.top_scopes().top_scope().id;
        let id = self.advance_id();
        self.top_scopes()
            .push_scope(Scope::new_lexical(id, Some(parent)));
    }

    pub(crate) fn exit_lexical(&mut self) {
        let scope = self
            .top_scopes()
            .pop_scope()
            .expect("No lexical scope to exit, this is a compiler bug.");

        assert_eq!(
            scope.kind,
            ScopeKind::Lexical,
            "Exiting lexical scope, but scope kind is not lexical."
        );
        assert!(
            !self.top_scopes().stack.is_empty(),
            "At least one scope should always exist on the stack, this is a compiler bug."
        );

        self.scopes.insert(scope.id, scope);
    }
}
