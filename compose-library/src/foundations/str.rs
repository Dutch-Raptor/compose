use compose_library::Value;
use compose_macros::{cast, ty};
use ecow::EcoString;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[ty(cast, title = "String")]
pub struct Str(pub EcoString);

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