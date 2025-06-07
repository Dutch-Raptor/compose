use crate::{Eval, Vm};
use compose_library::diag::{bail, SourceResult};
use compose_library::{Args, Closure, Func, Value, World};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::vm::FlowEvent;

impl Eval for ast::Closure<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let mut defaults = Vec::new();
        for param in self.params().children() {
            if let ast::ParamKind::Named(named) = param.kind() {
                defaults.push(named.expr().eval(vm)?);
            }
        }

        let closure = Closure {
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


pub fn eval_closure(
    _func: &Func,
    closure: &Closure,
    world: &dyn World,
    mut args: Args,
) -> SourceResult<Value> {
    let ast_closure = closure.node.cast::<ast::Closure>().expect("closure is not an ast closure");
    let params = ast_closure.params();
    let body = ast_closure.body();

    // Don't use the scope from the call site
    let mut inner_vm = Vm::new(world);

    let mut defaults = closure.defaults.iter();
    for p in params.children() {
        match p.kind() {
            ast::ParamKind::Pos(pattern) => match pattern {
                ast::Pattern::Single(ast::Expr::Ident(ident)) => {
                    inner_vm.define(ident, args.expect::<Value>(&ident)?)?;
                }
                pattern => bail!(pattern.span(), "Patterns not supported in closures yet")
            }
            ast::ParamKind::Named(named) => {
                let name = named.name();
                let default = defaults.next().unwrap();
                let value =
                    args.named::<Value>(&name)?.unwrap_or_else(|| default.clone());
                inner_vm.define(name, value)?;
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
        Some(other) => bail!(other.forbidden())
    }
    Ok(output)
}
