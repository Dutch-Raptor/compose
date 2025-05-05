use crate::foundations::str::Str;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
    Str(Str),
}

impl Default for Value {
    fn default() -> Self {
        Value::Unit
    }   
}


pub trait IntoValue {
    fn into_value(self) -> Value;
}

impl IntoValue for Value {
    fn into_value(self) -> Value {
        self
    }
}

macro_rules! impl_into_value {
    ($($t:ty => $i:ident,)+ $(,)?) => {
        $(
            impl IntoValue for $t {
                fn into_value(self) -> Value {
                    Value::$i(self)
                }
            }
        )*
    }
}

impl_into_value!(
    i64 => Int,
    bool => Bool,
);
