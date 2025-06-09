use crate::vm::FlowEvent;
use crate::{Eval, Vm};
use compose_library::diag::{SourceResult, Spanned, bail};
use compose_library::{Args, Binding, BindingKind, Closure, Func, Scopes, Value, World};
use compose_syntax::ast::{AstNode, Expr, Ident, Param};
use compose_syntax::{Label, SyntaxNode, ast};
use std::collections::HashSet;

impl Eval for ast::Closure<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let mut defaults = Vec::new();
        for param in self.params().children() {
            if let ast::ParamKind::Named(named) = param.kind() {
                defaults.push(named.expr().eval(vm)?);
            }
        }

        // let captured = {
        //     let mut visitor = CapturesVisitor::new(&vm.scopes, None);
        //     visitor.visit(self.body().to_untyped());
        //
        //     if !visitor.captures.is_empty() {
        //         let mut iter = visitor.captures.iter();
        //         let first = iter.next().expect("at least one capture");
        //
        //         // How to deal with recursion and the name of the closure itself?????
        //     }
        // };

        let closure = Closure {
            name: None,
            node: self.to_untyped().clone(),
            defaults,
            num_pos_params: self
                .params()
                .children()
                .filter(|p| matches!(p.kind(), ast::ParamKind::Pos(_)))
                .count(),
        };

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
    _func: &Func,
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
        inner_vm.try_bind(value.clone(), Binding::new(_func.clone(), *span))?;
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
    captures: HashSet<Ident<'a>>,
}

impl<'a> CapturesVisitor<'a> {
    pub fn new(external: &'a Scopes<'a>, self_name: Option<Ident<'a>>) -> Self {
        let mut inst = Self {
            external,
            internal: Scopes::new(None),
            captures: HashSet::new(),
        };

        if let Some(name) = self_name {
            inst.bind(name);
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
            self.captures.insert(ident);
        }
    }
}
