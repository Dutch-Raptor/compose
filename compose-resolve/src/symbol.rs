use crate::ExprId;
use crate::module::ModuleId;
use crate::scope::ScopeSource;
use compose_typeinfo::SymbolId;
use ecow::EcoString;
use fxhash::FxHashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: FxHashMap<SymbolId, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: FxHashMap::default(),
        }
    }

    pub fn get(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(&symbol_id)
    }

    pub fn insert(&mut self, symbol: Symbol) -> &mut Symbol {
        self.symbols.entry(symbol.symbol_id).or_insert(symbol)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &Symbol)> {
        self.symbols.iter()
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: EcoString,
    pub symbol_id: SymbolId,
    pub module_id: ModuleId,
    /// The expression Id of the expression that declared this symbol
    pub symbol_origin: SymbolOrigin,
    pub alias_of: Option<SymbolId>,
    pub symbol_kind: SymbolKind,
}

impl Symbol {
    pub fn new(
        name: EcoString,
        symbol_id: SymbolId,
        module_id: ModuleId,
        symbol_origin: SymbolOrigin,
    ) -> Self {
        Self {
            name,
            symbol_id,
            module_id,
            symbol_origin,
            alias_of: None,
            symbol_kind: SymbolKind::Local,
        }
    }

    pub fn alias_of(&mut self, alias_of: SymbolId) -> &mut Self {
        self.alias_of = Some(alias_of);
        self
    }

    pub fn with_origin(&mut self, symbol_origin: SymbolOrigin) -> &mut Self {
        self.symbol_origin = symbol_origin;
        self
    }

    pub fn with_kind(&mut self, symbol_kind: SymbolKind) -> &mut Self {
        self.symbol_kind = symbol_kind;
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolOrigin {
    Local(ExprId),
    Static(ExprId),
    Capture {
        capture_decl: ExprId,
        source: SymbolId,
    },
    Param(ExprId),
    Flow(ExprId),
    Import(ExprId),
    Glob(ExprId),
    Implicit,
}

impl SymbolOrigin {
    pub fn should_capture(&self) -> bool {
        matches!(
            self,
            SymbolOrigin::Local(_)
                | SymbolOrigin::Capture { .. }
                | SymbolOrigin::Param(_)
                | SymbolOrigin::Flow(_)
        )
    }
    pub(crate) fn article(&self) -> &'static str {
        match self {
            SymbolOrigin::Local(_) => "a",
            SymbolOrigin::Static(_) => "a",
            SymbolOrigin::Capture { .. } => "a",
            SymbolOrigin::Param(_) => "a",
            SymbolOrigin::Flow(_) => "a",
            SymbolOrigin::Import(_) => "an",
            SymbolOrigin::Glob(_) => "a",
            SymbolOrigin::Implicit => "an",
        }
    }

    pub(crate) fn noun(&self) -> &'static str {
        match self {
            SymbolOrigin::Local(_) => "local",
            SymbolOrigin::Static(_) => "static",
            SymbolOrigin::Capture { .. } => "captured variable",
            SymbolOrigin::Param(_) => "parameter",
            SymbolOrigin::Flow(_) => "flow variable",
            SymbolOrigin::Import(_) => "import",
            SymbolOrigin::Glob(_) => "glob import",
            SymbolOrigin::Implicit => "implicit import",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Local,
    Param,
    Function,
    Type,
    Module,
    Static,
}

impl Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolKind::Local => write!(f, "local variable"),
            SymbolKind::Param => write!(f, "parameter"),
            SymbolKind::Function => write!(f, "function"),
            SymbolKind::Type => write!(f, "type"),
            SymbolKind::Module => write!(f, "module"),
            SymbolKind::Static => write!(f, "static variable"),
        }
    }
}

impl SymbolOrigin {
    pub fn expr_id(&self) -> Option<ExprId> {
        match self {
            SymbolOrigin::Capture { capture_decl, .. } => Some(*capture_decl),
            SymbolOrigin::Param(expr_id) => Some(*expr_id),
            SymbolOrigin::Local(expr_id) => Some(*expr_id),
            SymbolOrigin::Static(expr_id) => Some(*expr_id),
            SymbolOrigin::Flow(expr_id) => Some(*expr_id),
            SymbolOrigin::Import(expr_id) => Some(*expr_id),
            SymbolOrigin::Glob(expr_id) => Some(*expr_id),
            SymbolOrigin::Implicit => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolUsage {
    pub symbol_id: SymbolId,
    pub expr_id: ExprId,
}

#[derive(Debug, Clone)]
pub struct UnresolvedSymbol {
    pub name: EcoString,
    pub expr_id: ExprId,
    pub scope: ScopeSource,
    pub expected_kind: Option<SymbolKind>,
}

impl UnresolvedSymbol {
    pub fn new(name: EcoString, expr_id: ExprId, scope_source: ScopeSource) -> Self {
        Self {
            name,
            expr_id,
            expected_kind: None,
            scope: scope_source,
        }
    }

    pub fn with_expected_kind(&mut self, expected_kind: SymbolKind) -> &mut Self {
        self.expected_kind = Some(expected_kind);
        self
    }
}
