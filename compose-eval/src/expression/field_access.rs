use crate::{Eval, Machine};
use compose_library::diag::{At, SourceResult};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::evaluated::Evaluated;

impl Eval for ast::FieldAccess<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let target = self.target().eval(vm)?;
        let field = self.field();
        let field_span = field.span();

        target.value
            .field(&field, field_span, vm.sink_mut())
            .at(field_span)
            .map(|v| target.with_value(v))
    }
}
