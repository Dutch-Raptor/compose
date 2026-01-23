use crate::Config;
use crate::block::BlockHeader;
use crate::diag::{Error, diagnostics_to_string};
use crate::world::DocWorld;
use compose_error_codes::{ErrorCode, lookup};
use compose_eval::{EvalConfig, Machine};
use compose_library::diag::{SourceDiagnostic, Warned};

pub(crate) fn execute_code_block(
    code: &str,
    meta: &BlockHeader,
    code_block_line_start: usize,
) -> Result<EvalResult, Error> {
    let (expected_errors, expected_warnings) =
        parse_expected_errors_and_warnings(meta, code_block_line_start)?;

    let eval_result = eval_code(code);

    if !eval_result
        .errors
        .iter()
        .map(|e| e.code)
        .zip(&expected_errors)
        .all(|(a, b)| a == Some(b))
        || eval_result.errors.len() != expected_errors.len()
    {
        let errors_str = diagnostics_to_string(
            &eval_result.world,
            &eval_result.errors,
            &[],
            &Config::no_color(),
        );
        return Err(Error {
            message: format!("expected errors: {expected_errors:?}, got:\n{errors_str}",),
            line: code_block_line_start,
        });
    }

    if !eval_result
        .warnings
        .iter()
        .map(|e| e.code)
        .zip(&expected_warnings)
        .all(|(a, b)| a == Some(b))
        || eval_result.warnings.len() != expected_warnings.len()
    {
        let warnings_str = diagnostics_to_string(
            &eval_result.world,
            &[],
            &eval_result.warnings,
            &Config::no_color(),
        );
        return Err(Error {
            message: format!("expected warnings: {expected_warnings:?}, got:\n{warnings_str}",),
            line: code_block_line_start,
        });
    }

    Ok(eval_result)
}

/// returns (expected_errors, expected_warnings)
pub(crate) fn parse_expected_errors_and_warnings(
    meta: &BlockHeader,
    code_block_line_start: usize,
) -> Result<(Vec<&ErrorCode>, Vec<&ErrorCode>), Error> {
    let expected_errors = match meta
        .expected_errors
        .iter()
        .map(|code| lookup(code).ok_or_else(|| format!("{code} is not a valid error code")))
        .collect::<Result<Vec<_>, _>>()
    {
        Ok(errors) => errors,
        Err(err) => {
            return Err(Error {
                message: err,
                line: code_block_line_start,
            });
        }
    };

    let expected_warnings = match meta
        .expected_warnings
        .iter()
        .map(|code| lookup(code).ok_or(format!("{code} is not a valid warning code")))
        .collect::<Result<Vec<_>, _>>()
    {
        Ok(warnings) => warnings,
        Err(err) => {
            return Err(Error {
                message: err,
                line: code_block_line_start,
            });
        }
    };
    Ok((expected_errors, expected_warnings))
}

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

pub(crate) struct EvalResult {
    pub world: DocWorld,
    pub stdout: String,
    pub warnings: Vec<SourceDiagnostic>,
    pub errors: Vec<SourceDiagnostic>,
}
