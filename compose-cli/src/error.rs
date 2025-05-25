use compose_library::diag::{EcoString, FileError};

#[derive(Debug)]
#[allow(dead_code)]
pub enum CliError {
    EditorError(EcoString),
    IoError(std::io::Error),
    FileError(FileError),
    ExecutionError,
}

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> Self {
        CliError::IoError(err)
    }
}

impl From<FileError> for CliError {
    fn from(value: FileError) -> Self {
        CliError::FileError(value)
    }
}
