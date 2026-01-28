use crate::world::DocWorld;
use compose_eval::{EvalConfig, Machine};
use compose_library::diag::{SourceDiagnostic, Warned};


pub(crate) fn eval_code(code: &str) -> EvalResult {
    let world = DocWorld::from_str(code);
    let mut vm = Machine::new(&world);

    let Warned { value, warnings } = compose_eval::eval(
        &world.source,
        &mut vm,
        &EvalConfig {
            include_syntax_warnings: true,
        },
    );

    let stdout = world.stdout.lock().expect("failed to lock stdout").clone();

    let errors = match value {
        Ok(_) => vec![],
        Err(err) => err.to_vec(),
    };

    let warnings = warnings.to_vec();

    EvalResult {
        world,
        stdout,
        warnings,
        errors,
    }
}

#[derive(Debug, Clone)]
pub(crate) struct EvalResult {
    pub world: DocWorld,
    pub stdout: String,
    pub warnings: Vec<SourceDiagnostic>,
    pub errors: Vec<SourceDiagnostic>,
}
