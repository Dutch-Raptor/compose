use ecow::EcoString;
use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(NonZeroU64);

impl From<NonZeroU64> for ModuleId {
    fn from(id: NonZeroU64) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: EcoString,
    pub id: ModuleId,
}
