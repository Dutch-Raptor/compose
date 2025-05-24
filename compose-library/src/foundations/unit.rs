use compose_macros::ty;

#[ty(cast, name = "()")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnitValue;