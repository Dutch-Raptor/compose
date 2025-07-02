use crate::Iter;
use compose_library::diag::bail;
use compose_library::repr::Repr;
use compose_library::vm::Vm;
use compose_library::{IterValue, StringIterator, Value};
use compose_macros::func;
use compose_macros::{cast, scope, ty};
use ecow::EcoString;
use std::fmt;
use std::ops::Add;

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
    pub fn chars(self, vm: &mut dyn Vm) -> IterValue {
        IterValue::new(Iter::String(StringIterator::new(self.0)), vm)
    }

    #[func]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[func]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Repr for Str {
    fn repr(&self, _vm: &dyn Vm) -> EcoString {
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
    v: Str => {
        let mut chars = v.0.chars();
        match (chars.next(), chars.next()) {
            (Some(_), Some(_)) => bail!("cannot convert a string with more than one character to char: {}", v.0),
            (Some(c), None) => c,
            (None, _) => bail!("cannot convert empty string to char"),
        }
    }
}
