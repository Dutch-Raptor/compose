use ecow::eco_format;
use crate::diag::StrResult;
use crate::Value;

macro_rules! type_error {
    ($fmt:expr, $($value:expr),* $(,)?) => {
        return Err(eco_format!($fmt, $($value.ty()),*).into())
    };
}

pub fn add(lhs: Value, rhs: Value) -> StrResult<Value> {
    use Value::*;
    match (lhs, rhs) {
        (Int(left), Int(right)) => Ok(Int(left + right)),
        (left, right) => type_error!("cannot add {} to {}", left, right),
    }
}

pub fn mul(lhs: Value, rhs: Value) -> StrResult<Value> {
    use Value::*;
    match (lhs, rhs) {
        (Int(left), Int(right)) => Ok(Int(left * right)),
        (left, right) => type_error!("cannot multiply {} by {}", left, right),   
    }
}

pub fn lt(lhs: Value, rhs: Value) -> StrResult<Value> {
    use Value::*;
    match (lhs, rhs) {
        (Int(left), Int(right)) => Ok(Bool(left < right)),
        (left, right) => type_error!("cannot compare {} to {}", left, right),
    }
}

pub fn unary_plus(value: Value) -> StrResult<Value> {
    use Value::*;
    match value {
        Int(value) => Ok(Int(value)),
        left => type_error!("cannot take the unary plus of {}", left),
    }
}

pub fn unary_minus(value: Value) -> StrResult<Value> {
    use Value::*;
    match value {
        Int(value) => Ok(Int(-value)),
        left => type_error!("cannot take the unary minus of {}", left),
    }
}

pub fn unary_not(value: Value) -> StrResult<Value> {
    use Value::*;
    match value {
        Bool(value) => Ok(Bool(!value)),
        left => type_error!("cannot take the unary not of {}", left),
    }
}

pub fn unary_bitwise_not(value: Value) -> StrResult<Value> {
    use Value::*;
    match value {
        Int(value) => Ok(Int(!value)),
        left => type_error!("cannot take the unary bitwise not of {}", left),
    }
}