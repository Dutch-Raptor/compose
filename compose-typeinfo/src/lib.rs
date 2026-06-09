pub mod bounds;
pub mod ids;
pub mod intern;
pub mod registry;
pub mod ty;

pub use bounds::{InterfaceTable, PrimitiveTy};
pub use ids::SymbolId;
pub use intern::{InterfaceName, Name, TypeName};
pub use registry::{FieldInfo, FunctionInfo, InterfaceInfo, MethodInfo, TypeInfoRegistry};
pub use ty::{LiteralKind, Ty, TyVarGen, TypeId, TypeVar, TypeVarId};
