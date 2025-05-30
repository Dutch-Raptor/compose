use std::fmt;
use compose_library::Value;
use compose_macros::{cast, ty};
use ecow::EcoString;
use compose_library::repr::Repr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[ty(cast, title = "String")]
pub struct Str(pub EcoString);

impl Repr for Str {
    fn repr(&self) -> EcoString {
        self.0.clone()
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<EcoString> for Str {
    fn from(s: EcoString) -> Self {
        Str(s)
    }
}

impl From<String> for Str {
    fn from(value: String) -> Self {
        Str(value.into())
    }
}


impl From<&str> for Str {
    fn from(s: &str) -> Self {
        Str(s.into())
    }
}

impl From<Str> for String {
    fn from(s: Str) -> Self {
        s.0.into()
    }
}

impl From<Str> for EcoString {
    fn from(s: Str) -> Self {
        s.0
    }
}

cast! {
    &str,
    self => Value::Str(self.into()),
}

cast! {
    EcoString,
    self => Value::Str(self.into()),
    v: Str => v.into()
}