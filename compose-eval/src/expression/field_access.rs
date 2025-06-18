use crate::{Eval, Machine};
use compose_library::diag::{At, SourceResult};
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

impl Eval for ast::FieldAccess<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let target = self.target().eval(vm)?;
        let field = self.field();
        let field_span = field.span();
        
        target.field(&field, field_span, vm.sink_mut()).at(field_span)
    }
}
