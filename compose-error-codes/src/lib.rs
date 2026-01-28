use std::fmt::Debug;

include!(concat!(env!("OUT_DIR"), "/Error_Codes"));

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ErrorCode {
    /// The error code (e.g. E0001)
    pub code: &'static str,
    /// The error name (e.g. `InvalidCharacter`)
    pub name: &'static str,
    /// The error description
    /// 
    /// A markdown string that explains the error and how it can be avoided.
    pub description: &'static str,
}

impl Debug for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.code, self.name)
    }   
}
