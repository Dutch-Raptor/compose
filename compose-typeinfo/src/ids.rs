use std::num::NonZeroU64;

/// A unique identifier for a resolved source symbol.
///
/// This lives below both `compose-resolve` and `compose-library` so type
/// metadata can distinguish source-resolved names from std/runtime names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct SymbolId(NonZeroU64);

impl From<NonZeroU64> for SymbolId {
    fn from(value: NonZeroU64) -> Self {
        Self(value)
    }
}

impl SymbolId {
    pub fn new(id: u64) -> Option<Self> {
        NonZeroU64::new(id).map(Self)
    }

    pub fn next(&self) -> Self {
        Self(self.0.saturating_add(1))
    }
}
