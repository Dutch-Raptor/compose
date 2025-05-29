use codespan_reporting::files::{Error, Files};
use compose_library::diag::FileResult;
use compose_library::{library, Library, World};
use compose_syntax::{FileId, Source};
use std::fmt::Debug;
use std::io::{Read, Write};
use std::ops::Range;
use std::sync::Mutex;

#[derive(Debug)]
pub struct ExplainWorld {
    pub source: Source,
    library: Library,
    pub stdout: Mutex<String>
}

impl ExplainWorld {
    pub fn empty_entrypoint() -> Self {
        Self::from_str("")
    }

    pub fn from_str(text: &str) -> Self {
        let entrypoint = FileId::new("main.comp");
        let source = Source::new(entrypoint, text.to_string());

        Self {
            source,
            library: library(),
            stdout: Mutex::new(String::new())
        }
    }
}

impl World for ExplainWorld {
    fn entry_point(&self) -> FileId {
        self.source.id()
    }

    fn source(&self, _file_id: FileId) -> FileResult<Source> {
        // Just return the source, no other files are needed.
        Ok(self.source.clone())
    }

    fn library(&self) -> &Library {
        &self.library
    }

    fn write(&self, f: &dyn Fn(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
        let mut buffer: Vec<u8> = Vec::new();
        f(&mut buffer)?;
        let output = String::from_utf8(buffer).unwrap();
        self.stdout.lock().unwrap().push_str(&output);
        Ok(())
    }

    fn read(&self, f: &dyn Fn(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdin())
    }
}

impl<'a> Files<'a> for &ExplainWorld {
    type FileId = FileId;
    type Name = String;
    type Source = Source;

    fn name(&'a self, _id: Self::FileId) -> Result<Self::Name, Error> {
        Ok("main.cmps".to_string())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, Error> {
        World::source(*self, id).map_err(|_| Error::FileMissing)
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, Error> {
        let source = Files::source(self, id)?;
        Ok(source
            .line_starts()
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, Error> {
        let source = Files::source(self, id)?;
        let start = source
            .line_starts()
            .get(line_index)
            .copied()
            .ok_or(Error::LineTooLarge {
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
