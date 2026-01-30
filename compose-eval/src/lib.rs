/*!
The Compose Interpreter


*/

mod access;
mod evaluated;
mod expression;
mod statement;
pub mod test;
mod vm;

pub use crate::vm::Machine;
pub use evaluated::Evaluated;
use compose_library::diag::{error, SourceDiagnostic, SourceResult, Warned};
use compose_library::Value;
use compose_syntax::ast::Statement;
use compose_syntax::Source;
use ecow::{eco_vec, EcoVec};
use std::cmp::min;
use std::ops::Range;

/**
Represents a language construct that can be evaluated. Examples are expressions and statements.

Evaluation may:
  - Have side effects like writing to stdout or writing to a file.
  - Allocate on the VM-managed heap.
  - Trigger flow control (break, continue, return).
  - Introduce lexical or flow bindings.

# Contract

## Scope management
Implementors that enter **lexical** or **flow** scopes must leave these scopes before returning from [`Eval::eval`].

To make this less error-prone, [`Machine`] provides several helpers:
- [`Machine::in_lexical_scope`]
- [`Machine::new_lexical_scope_guard`]
- [`Machine::in_flow_scope_guard`]
- [`Machine::new_flow_scope_guard`]

To read more about the rationale behind scope usage see [`compose_library::foundations::scope::Scopes`].

*Example: Evaluating a code block*

```rust,ignore
use compose_eval::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_syntax::ast::CodeBlock;
use crate::Evaluated;

impl Eval for CodeBlock<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let flow = vm.flow.take();
        let mut result = Evaluated::unit();

        let statements = self.statements();

        // in_lexical_scope enters a scope for the duration of the closure
        vm.in_lexical_scope(|vm| {
            for statement in statements {
                result = statement.eval(vm)?;
                if vm.flow.is_some() {
                    break;
                }
            }
            SourceResult::Ok(())
        })?;

        if let Some(flow) = flow {
            vm.flow = Some(flow);
        }

        Ok(result)
    }
}
```


## Any interaction with the outside world should go through the [`compose_library::World`] trait object in the [`Machine::engine`].

This is required to uphold the sandbox in which compose is evaluated and allows implementations to work in
any situation is used in.

*Example: writing to stdout*

```rust,ignore
# use compose_eval::{Eval, Machine, Evaluated};
# use compose_syntax::{Span};
# use compose_library::diag::{At, SourceResult};
struct WriteToStdout { span: Span }

impl Eval for WriteToStdout {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        vm
            .engine
            .world
            .write(&mut |write| write!(write, "Hello, world from Compose!"))
            .map_err(|e| format!("An error occurred while writing to stdout: {}", e))
            .at(self.span)?;

        Ok(Evaluated::unit())
    }
}
```

## Temporary values need to be rooted while performing GC

A value returned from an evaluation may not yet be rooted yet. Any values on the stack within the [`Eval::eval`]
function must be rooted while performing GC.

Use [`Machine::temp_root_guard`] or [`Machine::temp_root_scope`] to open a temporary root scope. Then track
any temporary values with [`Machine::track_tmp_root`].

*Example: GC after evaluating a statement*

```rust,ignore
use compose_eval::{Eval, Machine, Evaluated};
use compose_library::{Binding, IntoValue, Module, diag::SourceResult};
use compose_syntax::{ast};

impl Eval for ast::Statement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        // This might return a value with a heap allocation.
        // The VM might not yet know about it if it is not bound in any scope.
        // But the value might still be used after this statement.
        let result = match self {
            ast::Statement::Expr(e) => e.eval(vm),
            // ...
            _ => unimplemented!()
        };

        // Temporarily root the value to ensure it is not GCed before we use it.
        vm.temp_root_scope(|vm| {
            if let Ok(result) = &result {
                vm.track_tmp_root(result.value());
            }

            vm.maybe_gc();
            Ok(())
        })?;

        result
    }
}
```
*/


pub trait Eval {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated>;
}

#[derive(Default)]
pub struct EvalConfig {
    /// Whether to include syntax warnings in the returned result.
    pub include_syntax_warnings: bool,
}

pub fn eval_source(
    source: &Source,
    vm: &mut Machine,
    eval_config: &EvalConfig,
) -> Warned<SourceResult<Value>> {
    eval_source_range(source, 0..usize::MAX, vm, eval_config)
}

/// Eval a source file.
///
/// eval_range: eval these nodes
pub fn eval_source_range(
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
