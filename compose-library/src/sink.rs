use std::fmt::Debug;
use ecow::EcoVec;
use crate::diag::SourceDiagnostic;

#[derive(Default, Debug)]
pub struct Sink {
    pub warnings: EcoVec<SourceDiagnostic>,
}

impl Sink {
    pub(crate) fn warn(&mut self, warning: SourceDiagnostic) {
        self.warnings.push(warning);
    }
    
    pub fn take_warnings(&mut self) -> EcoVec<SourceDiagnostic> {
        std::mem::take(&mut self.warnings)
    }
}
