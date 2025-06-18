use crate::diag::StrResult;
use crate::Value;
use ecow::eco_format;

macro_rules! type_error {
    ($fmt:expr, $($value:expr),* $(,)?) => {
        return Err(eco_format!($fmt, $($value.ty()),*).into())
    };
}

pub fn add(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
        (Value::Str(left), Value::Str(right)) => Ok(Value::Str(left + right)),
        (left, right) => type_error!("cannot add {} to {}", left, right),
    }
}

pub fn sub(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
        (left, right) => type_error!("cannot subtract {} from {}", left, right),
    }
}

pub fn mul(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
        (left, right) => type_error!("cannot multiply {} by {}", left, right),
    }
}

pub fn lt(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left < right)),
        (left, right) => type_error!("cannot compare {} to {}", left, right),
    }
}

pub fn gt(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left > right)),
        (left, right) => type_error!("cannot compare {} to {}", left, right),
    }
}

pub fn gte(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left >= right)),
        (left, right) => type_error!("cannot compare {} to {}", left, right),
    }
}

pub fn neq(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    Ok(Value::Bool(lhs != rhs))
}

pub fn eq(lhs: &Value, rhs: &Value) -> StrResult<Value> {
    Ok(Value::Bool(lhs == rhs))
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
