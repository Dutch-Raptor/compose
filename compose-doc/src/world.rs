use std::io::{Read, Write};
use std::sync::Mutex;
use compose_library::diag::FileResult;
use compose_library::{Library, World};
use compose_syntax::{FileId, Source};

#[derive(Debug)]
pub(crate) struct DocWorld {
    pub source: Source,
    library: Library,
    pub stdout: Mutex<String>
}

impl Clone for DocWorld {
    fn clone(&self) -> Self {
        Self {
            source: self.source.clone(),
            library: self.library.clone(),
            stdout: Mutex::new(self.stdout.lock().expect("failed to lock stdout").clone())
        }
    }   
}

impl DocWorld {
    pub(crate) fn from_str(text: &str) -> Self {
        let entrypoint = FileId::new("main.comp");
        let source = Source::new(entrypoint, text.to_string());

        Self {
            source,
            library: Library::default(),
            stdout: Mutex::new(String::new())
        }
    }
}

impl World for DocWorld {
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

    fn write(&self, f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
        let mut buffer: Vec<u8> = Vec::new();
        f(&mut buffer)?;
        let output = String::from_utf8(buffer).expect("Invalid UTF-8");
        self.stdout.lock().expect("failed to lock stdout").push_str(&output);
        Ok(())
    }

    fn read(&self, f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdin())
    }
}
