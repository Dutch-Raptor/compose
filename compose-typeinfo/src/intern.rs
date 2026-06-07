use crate::SymbolId;
use ecow::EcoString;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// A name introduced by the resolver for user/source code.
    Source(SymbolId),
    /// A stable name owned by the standard library/runtime surface.
    Std(EcoString),
}

impl Name {
    pub fn source(symbol: SymbolId) -> Self {
        Self::Source(symbol)
    }

    pub fn std(name: impl Into<EcoString>) -> Self {
        Self::Std(name.into())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Source(symbol) => write!(f, "{symbol:?}"),
            Name::Std(name) => write!(f, "{name}"),
        }
    }
}

/// A resolver-agnostic name for a concrete type: Array, Option, MyStruct, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName(Name);

impl TypeName {
    /// Construct a standard-library/runtime type name.
    pub fn new(name: impl Into<EcoString>) -> Self {
        Self::std(name)
    }

    pub fn source(symbol: SymbolId) -> Self {
        Self(Name::source(symbol))
    }

    pub fn std(name: impl Into<EcoString>) -> Self {
        Self(Name::std(name))
    }

    pub fn name(&self) -> &Name {
        &self.0
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A resolver-agnostic name for an interface: Add, Eq, Ord, Iterator, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceName(Name);

impl InterfaceName {
    /// Construct a standard-library/runtime interface name.
    pub fn new(name: impl Into<EcoString>) -> Self {
        Self::std(name)
    }

    pub fn source(symbol: SymbolId) -> Self {
        Self(Name::source(symbol))
    }

    pub fn std(name: impl Into<EcoString>) -> Self {
        Self(Name::std(name))
    }

    pub fn name(&self) -> &Name {
        &self.0
    }
}

impl fmt::Display for InterfaceName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
