use crate::vm::FlowEvent;
use crate::{Eval, Vm};
use compose_library::diag::{bail, error, IntoSourceDiagnostic, SourceResult, Spanned};
use compose_library::{
    Args, Binding, BindingKind, Closure, Func, Library, Scope, Scopes, Value, VariableAccessError,
    World,
};
use compose_syntax::ast::{AstNode, Expr, Ident, Param};
use compose_syntax::{ast, Label, Span, SyntaxNode};
use ecow::{EcoString, EcoVec};
use std::collections::HashMap;

impl Eval for ast::Closure<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let mut defaults = Vec::new();
        for param in self.params().children() {
            if let ast::ParamKind::Named(named) = param.kind() {
                defaults.push(named.expr().eval(vm)?);
            }
        }

        let captured = {
            let mut errors = EcoVec::new();
            let mut scope = Scope::new();
            for capture in self.captures().children() {
                let span = capture.binding().span();
                let name = capture.binding().get();
                let binding = match vm.scopes.get(&capture.binding()).cloned() {
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
                        unbound.apply_hint(&mut err);
                        errors.push(err);

                        continue;
                    }
                };

                if capture.is_mut() && capture.is_ref() && !binding.kind().is_mut() {
                    errors.push(error!(
                        capture.span(), "cannot capture variable `{name}` as `ref mut` because it is not declared as mutable";
                        label_message: "capture is declared as a mutable reference";
                        label: Label::secondary(binding.span(), "was defined as immutable here");
                        note: "captured mutable references must match the mutability of the original declaration";
                        hint: "declare the variable as mutable: `let mut {name} = ...`";
                        hint: "or remove `mut` from the capture: `|ref {name}, ...|"
                    ));
                    continue;
                }

                let value = binding.read_checked(span, vm.sink_mut());

                if capture.is_ref() && !value.is_box() {
                    errors.push(error!(
                        span, "cannot capture non reference type by reference";
                        label_message: "this captures by reference";
                        note: "only boxed values can be captured by reference"
                    ));
                }

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
            let mut visitor =
                CapturesVisitor::new(&vm.scopes, Some(vm.engine.world.library()), &captured);
            visitor.visit(self.body().to_untyped());
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
            closure.resolve()?
        }

        let param_span = self.params().span();
        Ok(Value::Func(Func::from(closure)).spanned(param_span))
    }
}

fn define(
    vm: &mut Vm,
    ident: ast::Ident,
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
    vm.define(ident, value, kind)?;

    Ok(())
}

pub fn eval_closure(
    func: &Func,
    closure: &Closure,
    world: &dyn World,
    mut args: Args,
) -> SourceResult<Value> {
    let ast_closure = closure
        .node
        .cast::<ast::Closure>()
        .expect("closure is not an ast closure");
    let params = ast_closure.params();
    let body = ast_closure.body();

    // Don't use the scope from the call site
    let mut inner_vm = Vm::new(world);

    if let Some(Spanned { value, span }) = &closure.name {
        inner_vm.try_bind(value.clone(), Binding::new(func.clone(), *span))?;
    }

    for (k, v) in closure.captured.bindings() {
        inner_vm.scopes.top.bind(k.clone(), v.clone());
    }

    let mut defaults = closure.defaults.iter();
    for p in params.children() {
        match p.kind() {
            ast::ParamKind::Pos(pattern) => match pattern {
                ast::Pattern::Single(ast::Expr::Ident(ident)) => {
                    define(&mut inner_vm, ident, args.expect(&ident)?, p)?;
                }
                pattern => bail!(pattern.span(), "Patterns not supported in closures yet"),
            },
            ast::ParamKind::Named(named) => {
                let name = named.name();
                let default = defaults.next().unwrap();
                let value = args
                    .named(&name)?
                    .unwrap_or_else(|| Spanned::new(default.clone(), named.expr().span()));
                define(&mut inner_vm, name, value, p)?;
            }
        }
    }

    // Ensure all args have been used
    args.finish()?;

    let output = body.eval(&mut inner_vm)?;
    match inner_vm.flow {
        None => {}
        Some(FlowEvent::Return(_, Some(explicit))) => return Ok(explicit),
        Some(FlowEvent::Return(_, None)) => {}
        Some(other) => bail!(other.forbidden()),
    }
    Ok(output)
}

/// Visits a closure and determines which variables are captured implicitly.
pub struct CapturesVisitor<'a> {
    /// The external scope that variables might be captured from.
    external: &'a Scopes<'a>,
    /// The internal scope of variables defined within the closure.
    internal: Scopes<'a>,
    /// The variables that are captured.
    captures: HashMap<EcoString, Span>,
}

impl<'a> CapturesVisitor<'a> {
    pub fn new(external: &'a Scopes<'a>, library: Option<&'a Library>, existing: &Scope) -> Self {
        let mut inst = Self {
            external,
            internal: Scopes::new(library),
            captures: HashMap::new(),
        };

        for (k, v) in existing.bindings() {
            inst.internal.top.bind(k.clone(), v.clone());
        }

        inst
    }

    pub fn visit(&mut self, node: &'a SyntaxNode) {
        if let Some(ast::Statement::Let(expr)) = node.cast() {
            if let Some(init) = expr.initial_value() {
                self.visit(init.to_untyped())
            }

            for ident in expr.pattern().bindings() {
                self.bind(ident);
            }
            return;
        }

        let expr = match node.cast::<Expr>() {
            Some(expr) => expr,
            None => {
                if let Some(named) = node.cast::<ast::Named>() {
                    // Don't capture the name of a named parameter.
                    self.visit(named.expr().to_untyped());
                    return;
                }

                Expr::default()
            }
        };

        match expr {
            Expr::Ident(ident) => self.capture(ident),
            Expr::CodeBlock(_) => {
                self.internal.enter();
                for child in node.children() {
                    self.visit(child);
                }
                self.internal.exit();
            }
            Expr::FieldAccess(access) => {
                self.visit(access.target().to_untyped());
            }
            Expr::Closure(closure) => {
                for param in closure.params().children() {
                    if let ast::ParamKind::Named(named) = param.kind() {
                        self.visit(named.expr().to_untyped());
                    }
                }

                self.internal.enter();
                for param in closure.params().children() {
                    match param.kind() {
                        ast::ParamKind::Named(named) => {
                            self.bind(named.name());
                        }
                        ast::ParamKind::Pos(pat) => {
                            for ident in pat.bindings() {
                                self.bind(ident);
                            }
                        }
                    }
                }

                self.visit(closure.body().to_untyped());
                self.internal.exit();
            }

            Expr::ForLoop(for_loop) => {
                // Created in outer scope
                self.visit(for_loop.iterable().to_untyped());

                self.internal.enter();
                let pattern = for_loop.binding();
                for ident in pattern.bindings() {
                    self.bind(ident);
                }

                self.visit(for_loop.body().to_untyped());
                self.internal.exit();
            }

            _ => {}
        }

        // If not an expression or named, just go over all the children
        for child in node.children() {
            self.visit(child);
        }
    }

    fn bind(&mut self, ident: Ident) {
        self.internal.top.bind(
            ident.get().clone(),
            Binding::new(Value::unit(), ident.span()),
        );
    }

    fn capture(&mut self, ident: Ident<'a>) {
        if self.internal.get(&ident).is_ok() {
            // Was defined internally, no need to capture
            return;
        }

        // If the value does not exist in the external scope, it is not captured.
        if self.external.get(&ident).is_ok() {
            self.captures
                .entry(ident.get().clone())
                .or_insert(ident.span());
        }
    }

    fn finish(self) -> HashMap<EcoString, Span> {
        self.captures
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn capturing() {
        assert_eval(r#"
            let a = 10
            let f = |a| () => a + 1
            assert::eq(f(), 11)
        "#);

        assert_eval(r#"
            let mut a = box::new(5)
            let f = |ref mut a| () => {
                a += 1
                a
            }
            assert::eq(f(), 6)
        "#);

        assert_eval(r#"
            let a = 2
            let b = 3
            let f = |a, b| () => a * b
            assert::eq(f(), 6)
        "#);
        
        assert_eval(r#"
            let mut name = box::new("Alice")
            let age = 30
            let show = |ref name, age| () => {
                name + age.repr()
            }
            assert::eq(show(), "Name: Alice, Age: 30")
            name = "bob";
        "#);
    }

}