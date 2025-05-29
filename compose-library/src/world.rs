use crate::diag::{FileError, FileResult};
use compose_syntax::{FileId, Source};
use std::collections::HashMap;
use std::io::{stdin, stdout, Read, Write};
use std::ops::Range;
use codespan_reporting::files::Files;
use crate::Library;

pub trait World {
    /// The entrypoint file of the program to execute
    fn entry_point(&self) -> FileId;

    fn source(&self, file_id: FileId) -> FileResult<Source>;
    
    fn library(&self) -> &Library;
    
    fn write(&self, f: &dyn Fn(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()>;
    fn read(&self, f: &dyn Fn(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()>;
}


impl<'a> Files<'a> for &dyn World {
    type FileId = FileId;
    type Name = String;
    type Source = Source;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok(id.path().0.display().to_string())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, codespan_reporting::files::Error> {
        World::source(*self, id).map_err(|_| codespan_reporting::files::Error::FileMissing)
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, codespan_reporting::files::Error> {
        let source = Files::source(self, id)?;
        Ok(source
            .line_starts()
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, codespan_reporting::files::Error> {
        let source = Files::source(self, id)?;
        let start = source
            .line_starts()
            .get(line_index)
            .copied()
            .ok_or(codespan_reporting::files::Error::LineTooLarge {
                given: line_index,
                max: source.line_starts().len(),
            })?;

        let end = source
            .line_starts()
            .get(line_index + 1)
            .copied()
            .unwrap_or(source.text().len());

        Ok(start..end)
    }
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
