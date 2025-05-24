use compose_library::diag::{At, SourceResult};
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::{Eval, Vm};

impl Eval for ast::FieldAccess<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let target = self.target().eval(vm)?;
        let field = self.field();
        let field_span = field.span();
        
        target.field(&field, field_span, &mut vm.sink).at(field_span)
    }
}
