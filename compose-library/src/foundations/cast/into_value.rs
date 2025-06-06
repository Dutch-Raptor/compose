use compose_library::Value;

pub trait IntoValue {
    fn into_value(self) -> Value;
}

impl IntoValue for Value {
    fn into_value(self) -> Value {
        self
    }
}

impl<T: IntoValue> IntoValue for fn() -> T {
    fn into_value(self) -> Value {
        self().into_value()
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(self) -> Value {
        self.map(IntoValue::into_value).unwrap_or(Value::unit())
    }
}
