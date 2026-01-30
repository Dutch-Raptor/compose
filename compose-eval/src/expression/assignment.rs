use crate::access::Access;
use crate::{Eval, Machine};
use compose_library::diag::{bail, At, SourceResult};
use compose_library::{Value};
use compose_library::foundations::ops;
use compose_syntax::ast;
use compose_syntax::ast::{AssignOp, AstNode};
use crate::evaluated::Evaluated;

impl Eval for ast::Assignment<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let op = match self.op() {
            AssignOp::Assign => |_init: &Value, rhs: &Value| Ok(rhs.clone()),
            AssignOp::AddAssign => ops::add,
            AssignOp::SubAssign => ops::sub,
            AssignOp::MulAssign => ops::mul,
            AssignOp::DivAssign => ops::div,
            other => bail!(self.span(), "unsupported assignment operator: {:?}", other),
        };

        // assignments are right associative
        let rhs = self.rhs().eval(vm)?;

        let lhs = self.lhs().access(vm)?;
        let value = op(&lhs, &rhs.value).at(self.span())?;
        *lhs = value;

        Ok(Evaluated::unit())
    }
}