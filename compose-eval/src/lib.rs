mod vm;
mod expression;
mod access;
mod statement;

use std::cmp::min;
use std::ops::Range;
use ecow::{eco_vec, EcoVec};
pub use crate::vm::Vm;
use compose_library::diag::{error, SourceDiagnostic, SourceResult, Warned};
use compose_library::Value;
use compose_syntax::ast::Statement;
use compose_syntax::Source;

pub trait Eval {
    type Output;
    
    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output>;
}


pub fn eval(source: &Source, vm: &mut Vm) -> Warned<SourceResult<Value>> {
    eval_range(source, 0..usize::MAX, vm)
}

/// Eval a source file.
///
/// eval_range: eval these nodes
pub fn eval_range(
    source: &Source,
    eval_range: Range<usize>,
    vm: &mut Vm,
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
    if !errors.is_empty() {
        return Warned::new(Err(errors));
    }
    for node in nodes {
        let statement: Statement = match node.cast() {
            Some(expr) => expr,
            None => {
                let span = node.span();
                let err = error!(span, "expected a statement, found {:?}", node);
                return Warned::new(Err(eco_vec![err]))
                    .with_warnings(std::mem::take(&mut vm.sink.warnings));
            }
        };
        result = match statement.eval(vm) {
            Ok(value) => value,
            Err(err) => {
                return Warned::new(Err(err)).with_warnings(std::mem::take(&mut vm.sink.warnings));
            }
        }
    }

    Warned::new(Ok(result)).with_warnings(std::mem::take(&mut vm.sink.warnings))
}



#[cfg(test)]
mod test_utils {
    use compose_library::{library, TestWorld};
    use compose_syntax::Source;

    pub(crate) fn test_world(source: impl ToString) -> TestWorld {
        TestWorld {
            main: Source::from_string("mail.comp", source.to_string()),
            files: Default::default(),
            library: library()
        }
    }
    
    pub(crate) fn empty_world() -> TestWorld {
        test_world("")
    }
}