use crate::diag::{FileError, FileResult};
use compose_syntax::{FileId, Source};
use std::collections::HashMap;
use std::io::{stdin, stdout, Read, Write};
use crate::Library;

pub trait World {
    /// The entrypoint file of the program to execute
    fn entry_point(&self) -> FileId;

    fn source(&self, file_id: FileId) -> FileResult<Source>;
    
    fn library(&self) -> &Library;
    
    fn write(&self, f: &dyn Fn(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()>;
    fn read(&self, f: &dyn Fn(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()>;
}


pub struct TestWorld {
    pub main: Source,
    pub files: HashMap<FileId, Source>,
    pub library: Library,
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

    fn library(&self) -> &Library {
        &self.library
    }

    fn write(&self, f: &dyn Fn(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut stdout())
    }

    fn read(&self, f: &dyn Fn(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut stdin())   
    }
}
