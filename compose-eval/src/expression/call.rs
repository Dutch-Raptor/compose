use crate::vm::ErrorMode;
use crate::{Eval, Evaluated, Machine};
use compose_library::diag::{At, SourceResult, Spanned, Trace, TracePoint, bail};
use compose_library::{Arg, Args, Func, NativeScope, Type, UnboundItem, Value};
use compose_syntax::ast::AstNode;
use compose_syntax::{Label, Span, ast};
use ecow::{EcoString, EcoVec, eco_format};
use extension_traits::extension;

impl Eval for ast::FuncCall<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let callee_span = self.callee().span();

        let (callee, args) = self.eval_callee_and_args(vm)?;

        let func = callee.value.cast::<Func>().at(callee_span)?;

        func.call(vm, args).map(Evaluated::mutable).trace(
            || TracePoint::Call(func.name().map(EcoString::from)),
            self.span(),
        )
    }
}

#[extension(trait FuncCallExt)]
impl ast::FuncCall<'_> {
    fn eval_callee_and_args(&self, vm: &mut Machine) -> SourceResult<(Evaluated, Args)> {
        if let ast::Expr::FieldAccess(field_access) = self.callee() {
            self.eval_method_call(&field_access, vm)
        } else {
            self.eval_regular_call(vm)
        }
    }

    fn eval_method_call(
        &self,
        field_access: &ast::FieldAccess,
        vm: &mut Machine,
    ) -> SourceResult<(Evaluated, Args)> {
        let target_expr = field_access.target();
        let field = field_access.field();

        let target = target_expr.eval(vm)?;
        let mut args = self.args().eval(vm)?;

        let callee_binding = {
            let res = target.value.ty().scope().try_get(&field);

            match res {
                Ok(binding) => binding,
                Err(err) => {
                    let value_scope = Value::scope();
                    match value_scope.try_get(&field) {
                        Ok(binding) => binding,
                        Err(mut err2) => {
                            err2.possible_misspellings.extend(err.possible_misspellings);
                            return Err(err2.with_item(UnboundItem::FieldOrMethod(Some(
                                target.value.ty().name().into(),
                            ))))
                            .at(field.span());
                        }
                    }
                }
            }
        };
        let callee = callee_binding
            .read_checked(target_expr.span(), vm.sink_mut())
            .clone();

        let target_ty = target.value.ty();
        if let Value::Func(func) = &callee {
            if func.requires_mut_self() && !target.mutable {
                let target_text = target_expr.to_untyped().to_text();
                bail!(field.span(), "cannot call method `{}` on an immutable {}", field.as_str(), target_ty.name();
                    label_message: "cannot call `{}` on `{target_text}` because it is not mutable", field.as_str();
                    label: Label::secondary(target.origin.unwrap_or(Span::detached()), eco_format!("help: try making `{target_text}` mutable with `mut"));
                    note: "only mutable variables can call methods that modify their contents";
                    hint: "if mutation is not intended, you can use `.clone()` to create a new copy";
                )
            }

            if func.is_associated_function() {
                return err_call_associated_function_as_method(
                    &target_ty,
                    field.as_str(),
                    field.span(),
                );
            }
        }

        args.insert(0, target_expr.span(), target.value);

        Ok((Evaluated::new(callee, target.mutable), args))
    }

    fn eval_regular_call(&self, vm: &mut Machine) -> SourceResult<(Evaluated, Args)> {
        let callee = self.callee().eval(vm)?;
        let args = self.args().eval(vm)?.spanned(self.span());
        Ok((callee, args))
    }
}

fn err_call_associated_function_as_method<T>(
    target_ty: &Type,
    field: &str,
    span: Span,
) -> SourceResult<T> {
    bail!(
        span,
        "cannot call associated function `{}::{}` as a method", target_ty.name(), field;
        label_message: "not a method on `{}`", target_ty.name();
        note: "`{}` is an associated function of `{}`, not a method", field, target_ty.name();
        hint: "use path syntax instead: `{}::{}(args)`", target_ty.name(), field;
    )
}

#[extension(trait EvalArgs)]
impl ast::Args<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Args> {
        let mut items = EcoVec::with_capacity(self.items().count());

        // If we are within a let binding, function args cannot refer to the let binding,
        // So there is no need to defer these errors
        vm.with_closure_capture_errors_mode(ErrorMode::Immediate, |vm| {
            for arg in self.items() {
                let span = arg.span();
                match arg {
                    ast::Arg::Pos(expr) => items.push(Arg {
                        span,
                        name: None,
                        value: Spanned::new(expr.eval(vm)?.value, expr.span()),
                    }),
                    ast::Arg::Named(named) => items.push(Arg {
                        span,
                        name: Some(named.name().get().into()),
                        value: Spanned::new(named.expr().eval(vm)?.value, named.expr().span()),
                    }),
                }
            }

            Ok(())
        })?;

        // Do not assign a span here, we want to assign the span at the callsite (the whole call)
        Ok(Args {
            span: Span::detached(),
            items,
        })
    }
}
