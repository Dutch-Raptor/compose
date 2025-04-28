#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
}


pub trait IntoValue {
    fn into_value(self) -> Value;
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