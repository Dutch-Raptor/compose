use compose_library::diag::FileResult;
use compose_library::{library, Library, World};
use compose_syntax::{FileId, Source};
use std::fmt::Debug;
use std::io::{Read, Write};
use std::sync::Mutex;

#[derive(Debug)]
pub struct ExplainWorld {
    pub source: Source,
    library: Library,
    pub stdout: Mutex<String>
}

impl ExplainWorld {
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