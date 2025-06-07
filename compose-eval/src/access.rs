use compose_library::diag::{At, SourceResult};
use compose_library::{Value, ValueRefMut};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::vm::Vm;

pub(crate) trait Access {
    fn access<'a>(self, vm: &'a mut Vm) -> SourceResult<ValueRefMut<'a>>;
}

impl Access for ast::Expr<'_> {
    fn access<'a>(self, vm: &'a mut Vm) -> SourceResult<ValueRefMut<'a>> {
        match self {
            Self::Ident(i) => i.access(vm),
            _ => unimplemented!(),
        }
    }
}

impl Access for ast::Ident<'_> {
    fn access<'a>(self, vm: &'a mut Vm) -> SourceResult<ValueRefMut<'a>> {
        let span = self.span();
        
        vm.scopes.get_mut(&self)
            .at(span)?
            .write(span)?
            .as_mut()
            .at(span)
    }
}