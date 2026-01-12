use crate::expression::pattern;
use crate::{Eval, Evaluated, Machine};
use compose_library::diag::{SourceResult, bail};
use compose_library::repr::Repr;
use compose_library::{BindingKind, Value, Visibility};
use compose_syntax::ast::{AstNode, MatchExpression};
use crate::expression::pattern::PatternContext;

impl Eval for MatchExpression<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let value = self.expr().eval(vm)?.value;

        'arms: for arm in self.match_arms() {
            vm.frames.top.scopes.enter();

            let mut patterns = arm.patterns();
            'patterns: loop {
                let Some(pattern) = patterns.next() else {
                    // If none of the patterns matched, continue to the next arm.
                    continue 'arms;
                };

                let match_result = pattern::destructure_pattern(
                    vm,
                    pattern,
                    value.clone(),
                    PatternContext::Match,
                    BindingKind::Mutable,
                    Visibility::Public,
                );
                if match_result.is_err() {
                    // reset the top scope, technically should not be needed in between patterns,
                    // as they should all bind the same variables. But this seems more robust.
                    vm.frames.top.scopes.exit();
                    vm.frames.top.scopes.enter();
                    continue 'patterns;
                } else {
                    break;
                }
            }

            if let Some(guard) = arm.guard() {
                match guard.eval(vm)?.value {
                    Value::Bool(true) => {}
                    Value::Bool(false) => {
                        // Continue matching arms if the guard evaluates to false.
                        // Reset the scope before continuing to the next arm.
                        vm.frames.top.scopes.exit();
                        vm.frames.top.scopes.enter();
                        continue 'arms;
                    }
                    other => {
                        // leave the entered scope before exiting
                        vm.frames.top.scopes.exit();
                        bail!(
                            guard.span(), "match guard must evaluate to a boolean";
                            note: "expected `Bool`, found `{}`", other.ty()
                        )
                    }
                };
            }

            let result = arm.expr().eval(vm)?;

            // exit scope
            vm.frames.top.scopes.exit();
            return Ok(result);
        }

        // no matches
        bail!(self.span(), "no match arm applies to this value";
            note: "the value: `{}` is not covered by any match arm", value.repr(vm);
            hint: "consider adding a default `_ => ...` arm to handle all remaining cases"
        )
    }
}
