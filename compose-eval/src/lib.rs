mod vm;
mod expression;
mod access;
mod statement;

pub use crate::vm::Vm;
use compose_library::diag::SourceResult;

pub trait Eval {
    type Output;
    
    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output>;
}



#[cfg(test)]
mod test_utils {
    use compose_library::{library, TestWorld};
    use compose_syntax::Source;

    pub(crate) fn test_world(source: impl ToString) -> TestWorld {
        TestWorld {
            main: Source::from_string("mail.comp", source.to_string()),
            files: Default::default(),
            library: library()
        }
    }
    
    pub(crate) fn empty_world() -> TestWorld {
        test_world("")
    }
}