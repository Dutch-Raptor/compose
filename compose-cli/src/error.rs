use compose_library::diag::{EcoString, FileError};

#[derive(Debug)]
#[allow(dead_code)]
pub enum CliError {
    Editor(EcoString),
    Io(std::io::Error),
    File(FileError),
    Execution,
}

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> Self {
        CliError::Io(err)
    }
}

impl From<FileError> for CliError {
    fn from(value: FileError) -> Self {
        CliError::File(value)
    }
}
