pub mod bounds;
pub mod collect;
pub mod constraint;
pub mod env;
pub mod error;
pub mod graph;
pub mod intern;
pub mod namer;
pub mod subst;
pub mod ty;
pub mod unify;
pub mod zonk;

// Re-export the most commonly used types at the module level.
pub use collect::{InferenceEngine, TypeCheckResult, TypeInfo, type_check};
pub use constraint::{Constraint, ConstraintSource};
pub use error::{Diagnostic, TypeError};
pub use graph::InferenceGraph;
pub use ty::{Ty, TyVarGen, TypeId, TypeVar, TypeVarId};
