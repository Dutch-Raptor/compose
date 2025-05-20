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
