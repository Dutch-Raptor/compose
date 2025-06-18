use crate::vm::ErrorMode;
use crate::{Eval, Machine};
use compose_library::diag::{bail, At, SourceResult, Spanned};
use compose_library::{Arg, Args, Func, NativeScope, Type, UnboundItem, Value};
use compose_syntax::ast::AstNode;
use compose_syntax::{ast, Span};
use ecow::EcoVec;
use extension_traits::extension;

impl Eval for ast::FuncCall<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let callee_span = self.callee().span();

        let (callee, args) = self.eval_callee_and_args(vm)?;

        let func = callee.cast::<Func>().at(callee_span)?;

        func.call(vm, args)
    }
}

#[extension(trait FuncCallExt)]
impl ast::FuncCall<'_> {
    fn eval_callee_and_args(&self, vm: &mut Machine) -> SourceResult<(Value, Args)> {
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
    ) -> SourceResult<(Value, Args)> {
        let target_expr = field_access.target();
        let field = field_access.field();

        let target = target_expr.eval(vm)?;
        let mut args = self.args().eval(vm)?;

        let callee_binding = {
            let res = target.ty().scope().try_get(&field);

            match res {
                Ok(binding) => binding,
                Err(err) => {
                    let value_scope = Value::scope();
                    match value_scope.try_get(&field) {
                        Ok(binding) => binding,
                        Err(mut err2) => {
                            err2.possible_misspellings.extend(err.possible_misspellings);
                            return Err(err2.with_item(UnboundItem::FieldOrMethod(Some(
                                target.ty().name().into(),
                            ))))
                                .at(field.span());
                        }
                    }
                }
            }
        };

        let target_ty = target.ty();

        args.insert(0, target_expr.span(), target);

        let callee = callee_binding
            .read_checked(target_expr.span(), vm.sink_mut())
            .clone();

        if let Value::Func(func) = &callee {
            if func.is_associated_function() {
                return err_call_associated_function_as_method(
                    &target_ty,
                    field.as_str(),
                    field.span(),
                );
            }
        }

        Ok((callee, args))
    }

    fn eval_regular_call(&self, vm: &mut Machine) -> SourceResult<(Value, Args)> {
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

impl Eval for ast::Args<'_> {
    type Output = Args;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
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
                        value: Spanned::new(expr.eval(vm)?, expr.span()),
                    }),
                    ast::Arg::Named(named) => items.push(Arg {
                        span,
                        name: Some(named.name().get().into()),
                        value: Spanned::new(named.expr().eval(vm)?, named.expr().span()),
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
