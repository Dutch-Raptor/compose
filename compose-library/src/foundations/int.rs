use crate::diag::bail;
use compose_library::diag::StrResult;
use compose_library::Value;
use compose_macros::{cast, func};
use compose_macros::{scope, ty};

#[ty(scope, cast, name = "Int")]
type i64;

#[scope]
impl i64 {
    #[func]
    pub fn increment(value: i64) -> i64 {
        value + 1
    }

    #[func]
    pub fn incremented(self) -> i64 {
        self + 1
    }

    #[func]
    pub fn add(self, other: i64) -> i64 {
        self + other
    }

    #[func(name = "pow")]
    pub fn power(self, exponent: i64) -> StrResult<i64> {
        match exponent.try_into() {
            Ok(as_u32) => Ok(self.pow(as_u32)),
            Err(e) => bail!("{} is too big for an exponent", e.to_string(),),
        }
    }
}

cast! {
    usize,
    self => Value::Int(self as i64),
    v: i64 => v as usize,
}