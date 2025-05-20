use compose_library::diag::{At, SourceResult, StrResult};
use compose_library::foundations::cast::into_value::IntoValue;
use compose_library::Value;
use compose_syntax::Span;

pub trait IntoResult {
    fn into_result(self, span: Span) -> SourceResult<Value>;
}

impl<T: IntoValue> IntoResult for T {
    fn into_result(self, _: Span) -> SourceResult<Value> {
        Ok(self.into_value())
    }
}

impl<T: IntoValue> IntoResult for StrResult<T> {
    fn into_result(self, span: Span) -> SourceResult<Value> {
        self.map(IntoValue::into_value).at(span)
    }
}

impl<T: IntoValue> IntoResult for SourceResult<T> {
    fn into_result(self, _: Span) -> SourceResult<Value> {
        self.map(IntoValue::into_value)
    }
}