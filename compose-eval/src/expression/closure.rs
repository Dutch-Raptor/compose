use crate::expression::captures_visitor::CapturesVisitor;
use crate::expression::pattern::{destructure_pattern, PatternContext, PatternMatchResult};
use crate::vm::{FlowEvent, TrackedContainer};
use crate::{Eval, Machine};
use compose_library::diag::{bail, error, IntoSourceDiagnostic, SourceResult, Spanned};
use compose_syntax::ast::{AstNode, Expr, Ident, Param, ParamKind, Pattern};
use compose_syntax::{ast, Label};
use ecow::EcoVec;
use compose_library::foundations::args::Args;
use compose_library::foundations::cast::IntoValue;
use compose_library::foundations::scope::{Binding, BindingKind, Scope, VariableAccessError, Visibility};
use compose_library::foundations::types::Func;
use compose_library::foundations::types::func::Closure;
use compose_library::Value;
use crate::evaluated::{Evaluated, ValueEvaluatedExtensions};

impl Eval for ast::Lambda<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let vm = &mut vm.temp_root_guard();

        let mut defaults = Vec::new();
        for param in self.params().children() {
            if let ParamKind::Named(named) = param.kind() {
                defaults.push(named.expr().eval(vm)?.value);
            }
        }

        let captured = {
            let mut errors = EcoVec::new();
            let mut scope = Scope::new_lexical();
            for capture in self.captures().children() {
                let span = capture.binding().span();
                let name = capture.binding().get();
                let binding = match vm.get(&capture.binding()).cloned() {
                    Ok(v) => v,
                    Err(e) => {
                        let VariableAccessError::Unbound(unbound) = e else {
                            errors.push(e.into_source_diagnostic(span));
                            continue;
                        };

                        let mut err = error!(
                            span, "unknown variable `{name}` in closure capture list";
                            label_message: "variable `{name}` is not defined in the outer scope and cannot be captured";
                        );
                        if let Some(msg) = unbound.misspellings_hint_message() {
                            err.hint(msg);
                        }
                        errors.push(err);

                        continue;
                    }
                };

                let value = match validate_capture(capture, &binding, vm) {
                    Ok(v) => v,
                    Err(e) => {
                        errors.extend(e.into_iter());
                        continue;
                    }
                };

                scope.bind(
                    name.clone(),
                    Binding::new(value.clone(), span).with_kind(match capture.is_mut() {
                        true => BindingKind::Mutable,
                        false => BindingKind::Immutable { first_assign: None },
                    }),
                );
            }

            if !errors.is_empty() {
                return Err(errors);
            }

            scope
        };

        let unresolved_captures = {
            let mut visitor = CapturesVisitor::new(
                &vm.frames.top.scopes,
                Some(vm.engine.world.library()),
                &captured,
            );
            visitor.visit_lambda(self);
            visitor.finish()
        };

        let closure = Closure {
            name: None,
            node: self.to_untyped().clone(),
            defaults,
            num_pos_params: self
                .params()
                .children()
                .filter(|p| matches!(p.kind(), ast::ParamKind::Pos(_)))
                .count(),
            captured,
            unresolved_captures,
        };

        if !vm.context.closure_capture.should_defer() {
            closure.resolve_captures()?
        }

        Ok(Func::from(closure)
            .into_value()
            .spanned(self.span())
            .mutable())
    }
}

fn validate_capture<'a>(
    capture: ast::Capture,
    binding: &'a Binding,
    vm: &mut Machine,
) -> SourceResult<&'a Value> {
    let span = capture.binding().span();
    let name = capture.binding().get();
    if capture.is_mut() && capture.is_ref() && !binding.kind().is_mut() {
        bail!(
            capture.span(), "cannot capture variable `{name}` as `ref mut` because it is not declared as mutable";
            label_message: "capture is declared as a mutable reference";
            label: Label::secondary(binding.span(), "was defined as immutable here");
            note: "captured mutable references must match the mutability of the original declaration";
            hint: "declare the variable as mutable: `let mut {name} = ...`";
            hint: "or remove `mut` from the capture: `|ref {name}, ...|";
        );
    }

    let value = binding.read_checked(span, vm.sink_mut());

    if capture.is_ref() && !value.is_box() {
        bail!(
            span, "cannot capture non reference type by reference";
            label_message: "this captures by reference";
            note: "only boxed values can be captured by reference"
        );
    }

    Ok(value)
}

fn define(
    vm: &mut Machine,
    ident: Ident,
    Spanned { value, span }: Spanned<Value>,
    param: Param,
) -> SourceResult<()> {
    if param.is_ref() && !value.is_box() {
        bail!(
            ident.span(),
            "cannot take a reference to a value type. consider boxing it first with `box::new(value)`"
        );
    }
    if !param.is_ref() && value.is_box() {
        bail!(span, "Cannot bind a box value to a non-reference parameter";
            label_message: "this parameter is a boxed value";
            label: Label::primary(ident.span(), "this parameter is declared as an owned value");
            note: "passing a reference type (like `box`) into an owned parameter is not allowed";
            hint: "if the parameter intends to modify the original value, mark the parameter as `ref mut`";
            hint: "if you want to pass the value by ownership, use `.clone_inner()` to create a new copy";
        );
    }

    let kind = if param.is_mut() {
        BindingKind::ParamMut
    } else {
        BindingKind::Param
    };
    vm.define(ident, value, kind, Visibility::Private)?;

    Ok(())
}

//noinspection RsUnnecessaryQualifications - False Positive
pub fn eval_lambda(closure: &Closure, vm: &mut Machine, args: Args) -> SourceResult<Value> {
    let vm = &mut vm.temp_root_guard();
    let ast_closure = closure
        .node
        .cast::<ast::Lambda>()
        .expect("closure is not an ast closure");
    let params = ast_closure.params();
    let statements = ast_closure.statements();

    // Make sure a gc round is aware that the args are reachable
    vm.track_tmp_root(&args);

    let result = vm
        .with_frame(move |vm| {
            let mut args = args;
            if let Some(Spanned { value, span }) = &closure.name {
                vm.try_bind(
                    value.clone(),
                    Binding::new(Func::from(closure.clone()), *span),
                )?;
            }

            for (k, v) in closure.captured.bindings() {
                vm.bind(k.clone(), v.clone());
            }

            let mut defaults = closure.defaults.iter();
            for p in params.children() {
                match p.kind() {
                    ast::ParamKind::Pos(pattern) => match pattern {
                        Pattern::Single(Expr::Ident(ident)) => {
                            define(vm, ident, args.expect(&ident)?, p)?;
                        }
                        pattern => {
                            let Some(v) = args.eat()? else {
                                bail!(pattern.span(), "missing argument for this parameter");
                            };
                            let binding_kind = match p.is_mut() {
                                false => BindingKind::Param,
                                true => BindingKind::ParamMut,
                            };

                            match destructure_pattern(
                                vm,
                                pattern,
                                v,
                                PatternContext::Parameter,
                                binding_kind,
                                Visibility::Private,
                            )? {
                                PatternMatchResult::NotMatched(err) => bail!(err),
                                PatternMatchResult::Matched => {}
                            }
                        }
                    },
                    ast::ParamKind::Named(named) => {
                        let name = named.name();
                        let default = defaults.next().unwrap();
                        let value = args
                            .named(&name)?
                            .unwrap_or_else(|| Spanned::new(default.clone(), named.expr().span()));
                        define(vm, name, value, p)?;
                    }
                }
            }

            // Ensure all args have been used
            args.finish()?;

            let mut output = Value::unit();
            for statement in statements {
                output = statement.eval(vm)?.value;
                match &vm.flow {
                    None => {}
                    Some(FlowEvent::Return(_, Some(explicit))) => {
                        let explicit = explicit.clone();
                        vm.flow = None;
                        return Ok(explicit);
                    }
                    Some(FlowEvent::Return(_, None)) => {
                        vm.flow = None;
                        return Ok(Value::unit());
                    }
                    Some(other) => bail!(other.forbidden()),
                }
            }

            SourceResult::Ok(output)
        })
        .track_tmp_root(vm);

    vm.maybe_gc();

    result
}

#[cfg(test)]
mod tests {
    use crate::test::*;

    #[test]
    fn capturing() {
        assert_eval(
            r#"
            let a = 10;
            let f = { |a| => a + 1 };
            assert::eq(f(), 11);
        "#,
        );

        assert_eval(
            r#"
            let mut a = box::new(5);
            let f = { |ref mut a| =>
                *a += 1;
                *a;
            };
            assert::eq(f(), 6);
        "#,
        );

        assert_eval(
            r#"
            let a = 2;
            let b = 3;
            let f = { |a, b| => a * b };
            assert::eq(f(), 6);
        "#,
        );

        assert_eval(
            r#"
            let mut name = box::new("Alice");
            let age = 30;
            let show = { |ref name, age| =>
                "Name: " + *name + ", Age: " + age.to_string();
            };
            assert::eq(show(), "Name: Alice, Age: 30");
            *name = "Bob";
            assert::eq(show(), "Name: Bob, Age: 30");
        "#,
        );
    }

}
