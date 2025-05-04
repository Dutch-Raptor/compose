mod vm;
mod expression;
mod access;

pub use crate::vm::Vm;
use compose_library::diag::{SourceResult, Warned};
use compose_library::Value;

pub trait Eval {
    type Output;
    
    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output>;
}



#[cfg(test)]
mod test_utils {
    use compose_library::TestWorld;
    use compose_syntax::Source;

    pub(crate) fn test_world(source: impl ToString) -> TestWorld {
        TestWorld {
            main: Source::from_string("mail.comp", source.to_string()),
            files: Default::default(),
        }
    }
    
    pub(crate) fn empty_world() -> TestWorld {
        test_world("")
    }
}