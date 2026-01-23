use compose_library::Value;
use compose_syntax::Span;
use crate::Machine;
use crate::vm::Tracked;

#[derive(Debug, Clone, PartialEq)]
pub struct Evaluated {
    pub value: Value,
    /// Whether the value is allowed to be mutated.
    ///
    /// True for any expression except for reading or dereferencing immutable values.
    pub mutable: bool,
    /// The span of the binding this value is related to
    pub origin: Option<Span>,
}

impl Evaluated {
    pub fn new(value: Value, mutable: bool) -> Self {
        Self { value, mutable, origin: None }
    }

    pub fn mutable(value: Value) -> Self {
        Self::new(value, true)
    }

    pub fn immutable(value: Value) -> Self {
        Self::new(value, false)
    }

    pub fn unit() -> Self {
        Self::new(Value::unit(), true)
    }

    pub fn spanned(self, span: Span) -> Self {
        Self {
            value: self.value.spanned(span),
            ..self
        }
    }

    pub fn with_origin(self, origin: Span) -> Self {
        Self { origin: Some(origin), ..self }
    }

    pub fn with_value(self, value: Value) -> Self {
        Self { value, ..self }
    }

    pub fn make_mutable(self) -> Self {
        Self { mutable: true, ..self }
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn into_value(self) -> Value {
        self.value
    }
}

impl Tracked for Evaluated {
    fn track_tmp_root(self, vm: &mut Machine) -> Self {
        Self {
            value: self.value.track_tmp_root(vm),
            ..self
        }
    }
}

pub trait ValueEvaluatedExtensions {
    fn mutable(self) -> Evaluated;
    #[expect(unused)]
    fn immutable(self) -> Evaluated;
}

impl ValueEvaluatedExtensions for Value {
    fn mutable(self) -> Evaluated {
        Evaluated::new(self, true)
    }

    fn immutable(self) -> Evaluated {
        Evaluated::new(self, false)
    }
}