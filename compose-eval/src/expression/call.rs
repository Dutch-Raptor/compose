use ecow::EcoVec;
use compose_library::diag::{At, SourceResult, Spanned};
use compose_library::{Arg, Args, Func, Value};
use compose_syntax::{ast, Span};
use compose_syntax::ast::{AstNode};
use crate::{Eval, Vm};

impl Eval for ast::FuncCall<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let span = self.span();
        
        let callee = self.callee();
        let callee_span = callee.span();
        
        let args = self.args();
        
        // Handle field access later
        
        let callee = callee.eval(vm)?;
        let args = args.eval(vm)?.spanned(span);
        
        let func = callee.cast::<Func>().at(callee_span)?;
        
        return func.call(args)
    }
}

impl Eval for ast::Args<'_> {
    type Output = Args;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let mut items = EcoVec::with_capacity(self.items().count());
        
        for arg in self.items() {
            let span = arg.span();
            match arg {
                ast::Arg::Pos(expr) => {
                    items.push(Arg {
                        span,
                        value: Spanned::new(expr.eval(vm)?, expr.span()),
                    })
                    
                }
            }
        }
        
        // Do not assign a span here, we want to assign the span at the callsite (the whole call)
        Ok(Args {
            span: Span::detached(),
            items,
        })
        
    }
}
