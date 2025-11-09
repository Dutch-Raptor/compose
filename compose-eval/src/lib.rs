mod access;
mod expression;
mod statement;
pub mod test;
mod vm;

pub use crate::vm::Machine;
use crate::vm::Tracked;
use compose_library::Value;
use compose_library::diag::{SourceDiagnostic, SourceResult, Warned, error};
use compose_syntax::ast::Statement;
use compose_syntax::{Source, Span};
use ecow::{EcoVec, eco_vec};
use std::cmp::min;
use std::ops::Range;

pub trait Eval {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated>;
}

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


#[derive(Default)]
pub struct EvalConfig {
    /// Whether to include syntax warnings in the returned result.
    pub include_syntax_warnings: bool,
}

pub fn eval(
    source: &Source,
    vm: &mut Machine,
    eval_config: &EvalConfig,
) -> Warned<SourceResult<Value>> {
    eval_range(source, 0..usize::MAX, vm, eval_config)
}

/// Eval a source file.
///
/// eval_range: eval these nodes
pub fn eval_range(
    source: &Source,
    eval_range: Range<usize>,
    vm: &mut Machine,
    config: &EvalConfig,
) -> Warned<SourceResult<Value>> {
    let mut result = Value::unit();

    let range_start = min(eval_range.start, source.nodes().len());
    let range_end = min(eval_range.end, source.nodes().len());

    let nodes = source.nodes().get(range_start..range_end).unwrap();
    let errors = nodes
        .iter()
        .flat_map(|n| n.errors())
        .map(|e| e.into())
        .collect::<EcoVec<SourceDiagnostic>>();

    let syntax_warnings = if config.include_syntax_warnings {
        nodes
            .iter()
            .flat_map(|n| n.warnings())
            .map(|e| e.into())
            .collect::<EcoVec<SourceDiagnostic>>()
    } else {
        eco_vec![]
    };

    if !errors.is_empty() {
        return Warned::new(Err(errors)).with_warnings(syntax_warnings);
    }

    for node in nodes {
        let statement: Statement = match node.cast() {
            Some(expr) => expr,
            None => {
                let span = node.span();
                let err = error!(span, "expected a statement, found {:?}", node);

                return build_err(&syntax_warnings, vm, eco_vec![err], config);
            }
        };
        result = match statement.eval(vm) {
            Ok(value) => value.value,
            Err(err) => return build_err(&syntax_warnings, vm, err, config),
        }
    }

    let mut warnings = vm.sink_mut().take_warnings();
    if config.include_syntax_warnings {
        warnings.extend_from_slice(&syntax_warnings);
    }

    Warned::new(Ok(result)).with_warnings(warnings)
}

pub fn build_err(
    syntax_warnings: &[SourceDiagnostic],
    vm: &mut Machine,
    errs: EcoVec<SourceDiagnostic>,
    config: &EvalConfig,
) -> Warned<SourceResult<Value>> {
    let mut warnings = vm.sink_mut().take_warnings();
    if config.include_syntax_warnings {
        warnings.extend_from_slice(syntax_warnings);
    }

    Warned::new(Err(errs)).with_warnings(warnings)
}
