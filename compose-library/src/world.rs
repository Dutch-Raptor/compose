use crate::diag::{FileError, FileResult};
use compose_syntax::{FileId, Source};
use std::collections::HashMap;

pub trait World {
    /// The entrypoint file of the program to execute
    fn entry_point(&self) -> FileId;

    fn source(&self, file_id: FileId) -> FileResult<Source>;
}

pub struct TestWorld {
    pub main: Source,
    pub files: HashMap<FileId, Source>,
}

impl World for TestWorld {
    fn entry_point(&self) -> FileId {
        self.main.id()
    }

    fn source(&self, file_id: FileId) -> FileResult<Source> {
        self.files
            .get(&file_id)
            .ok_or(FileError::NotFound(file_id.path().0.clone()))
            .cloned()
    }
}
