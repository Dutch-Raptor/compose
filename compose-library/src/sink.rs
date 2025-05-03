use crate::diag::SourceDiagnostic;

pub trait Sink {
    fn warn(&mut self, warning: SourceDiagnostic);
}