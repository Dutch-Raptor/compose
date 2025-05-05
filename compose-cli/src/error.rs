use compose_library::diag::EcoString;

#[derive(Debug)]
#[allow(dead_code)]
pub enum CliError {
    EditorError(EcoString),
    IoError(std::io::Error),
}

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> Self {
        CliError::IoError(err)
    }
}