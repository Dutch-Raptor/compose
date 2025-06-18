use crate::vm::Machine;
use compose_library::diag::{At, SourceResult};
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

pub(crate) trait Access {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value>;
}

impl Access for ast::Expr<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        match self {
            Self::Ident(i) => i.access(vm),
            Self::Unary(u) => u.access(vm),
            _ => unimplemented!(),
        }
    }
}

impl Access for ast::Ident<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        let span = self.span();

        vm.get_mut(&self).at(span)?.write(span)
    }
}
