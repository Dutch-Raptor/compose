use compose_macros::func;
use std::fmt;
use std::ops::Add;
use compose_library::{StringIterator, Value};
use compose_macros::{cast, scope, ty};
use ecow::EcoString;
use compose_library::repr::Repr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[ty(scope, cast, name = "string")]
pub struct Str(pub EcoString);

impl Add for &Str {
    type Output = Str;

    fn add(self, rhs: Self) -> Self::Output {
        let mut l = self.0.clone();
        l.push_str(&rhs.0);
        Str(l)
    }
}

#[scope]
impl Str {
    #[func]
    pub fn chars(self) -> StringIterator {
        StringIterator::new(self.0)
    }
}

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

cast! {
    char,
    self => Value::Str(EcoString::from(self).into()),
}
