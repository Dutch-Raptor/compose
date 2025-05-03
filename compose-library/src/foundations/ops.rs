use crate::Value;
use crate::diag::StrResult;

pub fn add(lhs: Value, rhs: Value) -> StrResult<Value> {
    use Value::*;
    match (lhs, rhs) {
        (Int(left), Int(right)) => Ok(Int(left + right)),
        (left, right) => unimplemented!("{:?} + {:?}", left, right),
    }
}

pub fn mul(lhs: Value, rhs: Value) -> StrResult<Value> {
    use Value::*;
    match (lhs, rhs) {
        (Int(left), Int(right)) => Ok(Int(left * right)),
        (left, right) => unimplemented!("{:?} * {:?}", left, right),
    }
}
