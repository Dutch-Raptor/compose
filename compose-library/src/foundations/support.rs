use compose_error_codes::E0012_PREDICATE_MUST_RETURN_BOOLEAN;
use compose_library::diag::{SourceResult, bail, error};
use compose_library::{Args, Func, Value, Vm};
use std::iter;

pub fn eval_predicate(
    vm: &mut dyn Vm,
    predicate: &Func,
    value: Value,
    callee: &str,
) -> SourceResult<bool> {
    let span = predicate.span;
    let args = Args::new(span, iter::once(value));
    match predicate.call(vm, args)? {
        Value::Bool(b) => Ok(b),
        other => {
            let mut err = error!(
                span, "predicate must return a boolean";
                label_message: "this function should return a boolean, but returned type `{}` instead", other.ty();
                note: "a predicate function passed to `{callee}` must return either `true` or `false`";
                code: &E0012_PREDICATE_MUST_RETURN_BOOLEAN;
            );

            if let Some(hint) = predicate_hint(&other) {
                err.hint(hint);
            }

            bail!(err);
        }
    }
}

fn predicate_hint(value: &Value) -> Option<&'static str> {
    match value {
        Value::Int(_) => Some("did you mean to write a comparison like `x == 1` or `x > 1`?"),
        Value::Str(_) => Some("did you mean to compare it, like `x == \"hello\"`?"),
        Value::Array(_) => {
            Some("did you mean to check the contents, e.g., `x.contains(...)` or `x.len() > 0`?")
        }
        _ => None,
    }
}
