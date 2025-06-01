mod access;
mod expression;
mod statement;
mod vm;

pub use crate::vm::Vm;
use compose_library::diag::{error, SourceDiagnostic, SourceResult, Warned};
use compose_library::{IntoResult, Value};
use compose_syntax::ast::Statement;
use compose_syntax::Source;
use ecow::{eco_vec, EcoVec};
use std::cmp::min;
use std::ops::Range;

pub trait Eval {
    type Output;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output>;
}

pub struct EvalConfig {
    /// Whether to include syntax warnings in the returned result.
    include_syntax_warnings: bool,
}

impl Default for EvalConfig {
    fn default() -> Self {
        Self {
            include_syntax_warnings: false,
        }
    }
}

pub fn eval(source: &Source, vm: &mut Vm, eval_config: &EvalConfig) -> Warned<SourceResult<Value>> {
    eval_range(source, 0..usize::MAX, vm, eval_config)
}

/// Eval a source file.
///
/// eval_range: eval these nodes
pub fn eval_range(
    source: &Source,
    eval_range: Range<usize>,
    vm: &mut Vm,
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

    let syntax_warnings = nodes
        .iter()
        .flat_map(|n| n.warnings())
        .map(|e| e.into())
        .collect::<EcoVec<SourceDiagnostic>>();

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
            Ok(value) => value,
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
    vm: &mut Vm,
    errs: EcoVec<SourceDiagnostic>,
    config: &EvalConfig,
) -> Warned<SourceResult<Value>> {
    let mut warnings = vm.sink_mut().take_warnings();
    if config.include_syntax_warnings {
        warnings.extend_from_slice(syntax_warnings);
    }

    Warned::new(Err(errs)).with_warnings(warnings)
}

#[cfg(test)]
mod test_utils {
    use compose_library::{library, TestWorld};
    use compose_syntax::Source;

    pub(crate) fn test_world(source: impl ToString) -> TestWorld {
        TestWorld {
            main: Source::from_string("mail.comp", source.to_string()),
            files: Default::default(),
            library: library(),
        }
    }

    pub(crate) fn empty_world() -> TestWorld {
        test_world("")
    }
}
