use crate::diag::StrResult;
use crate::{Value, ValueRef};
use ecow::eco_format;
use std::ops::Deref;

macro_rules! type_error {
    ($fmt:expr, $($value:expr),* $(,)?) => {
        return Err(eco_format!($fmt, $($value.ty()),*).into())
    };
}

pub fn add(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    match (lhs.deref(), rhs.deref()) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
        (left, right) => type_error!("cannot add {} to {}", left, right),
    }
}

pub fn sub(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    match (lhs.deref(), rhs.deref()) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
        (left, right) => type_error!("cannot subtract {} from {}", left, right),
    }
}

pub fn mul(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    match (lhs.deref(), rhs.deref()) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
        (left, right) => type_error!("cannot multiply {} by {}", left, right),
    }
}

pub fn lt(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    match (lhs.deref(), rhs.deref()) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left < right)),
        (left, right) => type_error!("cannot compare {} to {}", left, right),
    }
}

pub fn gt(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    match (lhs.deref(), rhs.deref()) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left > right)),
        (left, right) => type_error!("cannot compare {} to {}", left, right),
    }
}

pub fn neq(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    Ok(Value::Bool(lhs.deref() != rhs.deref()))
}

pub fn eq(lhs: ValueRef, rhs: ValueRef) -> StrResult<Value> {
    Ok(Value::Bool(lhs.deref() == rhs.deref()))
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
