use compose_doc::{Config, ErrorHandlingMode};
use compose_error_codes::ERROR_CODES;
use std::fmt::Write;
use std::fs;
use std::path::Path;

fn main() {
    let mut doctests = String::new();
    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR is always set during builds");
    for code in ERROR_CODES {
        let converted_md = compose_doc::transform_markdown(
            code.description,
            &Config::new()
                .with_no_color()
                .with_code_block_error_mode(ErrorHandlingMode::EmitAsTests)
                .with_output_block_error_mode(ErrorHandlingMode::EmitAsTests),
        )
            .unwrap_or_else(|e| panic!("failed to convert markdown for {} at {}: {}", code.name, e.line, e.message));

        let ty_name = format!(
            "DOC_{}_{}",
            code.code,
            code.name.replace(' ', "_").to_uppercase()
        );

        write!(
            doctests,
            r#"
/**
{converted_md}
*/
#[allow(non_snake_case, dead_code)]
pub mod {ty_name} {{}}


        "#
        )
        .expect("writing to a string is infallible");
    }
    fs::write(Path::new(&out_dir).join("Error_Codes"), doctests)
        .expect("failed to write Error_Codes");
}
