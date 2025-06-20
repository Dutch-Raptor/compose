use codespan_reporting::term::Config;
use codespan_reporting::term::termcolor::{Ansi, NoColor, WriteColor};
use compose_library::diag::{write_diagnostics, SourceDiagnostic};
use compose_library::World;
use crate::DiagMode;

pub(crate) trait At<T> {
    fn at(self, line_nr: usize) -> Result<T, Error>;
}

impl<T, E> At<T> for Result<T, E> where E: std::fmt::Display {
    fn at(self, line_nr: usize) -> Result<T, Error> {
        self.map_err(|e| Error {
            message: e.to_string(),
            line: line_nr,
        })
    }   
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub line: usize,
}

pub(crate) fn line_starts(source: &str, offset: usize) -> impl '_ + Iterator<Item = usize> {
    core::iter::once(0).chain(source.match_indices('\n').map(move |(i, _)| i + 1 + offset))
}

pub(crate) fn offset_to_line(line_starts: &[usize], offset: usize) -> usize {
    line_starts.binary_search(&offset).unwrap_or_else(|i| i - 1)
}

pub(crate) fn diagnostics_to_string(
    world: &dyn World,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
    config: &crate::Config,
) -> String {
    let mut diags_buffer = vec![];
    let color_writer = match config.diag_mode {
        DiagMode::Ansi => &mut Ansi::new(&mut diags_buffer) as &mut dyn WriteColor,
        DiagMode::NoColor => &mut NoColor::new(&mut diags_buffer),
    };
    let config = Config::default();

    let mut output = String::new();

    write_diagnostics(world, warnings, errors, color_writer, &config)
        .expect("failed to write diagnostics");

    output.push_str(
        &String::from_utf8(diags_buffer).expect("failed to convert diagnostics to string"),
    );
    output.trim_end().to_string()
}
