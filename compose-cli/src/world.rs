use compose_library::diag::{FileError, FileResult};
use compose_library::{library, Library, World};
use compose_syntax::{FileId, Source};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use tap::Pipe;

pub struct SystemWorld {
    sources: Mutex<HashMap<FileId, Source>>,
    entrypoint: FileId,
    root: PathBuf,
    library: Library,
}

impl Debug for SystemWorld {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // try to get the mutex lock

        f.debug_struct("SystemWorld")
            .field("entrypoint", &self.entrypoint)
            .field("root", &self.root)
            .pipe(|d| {
                if let Ok(sources) = self.sources.try_lock() {
                    d.field("sources", &sources)
                } else {
                    d.field("sources", &"<locked>")
                }
            })
            .finish()
    }
}

impl SystemWorld {
    /// assumes the folder containing the file is the root
    pub fn from_file(path: impl AsRef<Path>) -> FileResult<Self> {
        let path = path.as_ref();
        let root = path.parent().unwrap().to_path_buf()
            .canonicalize()
            .map_err(|e| FileError::from_io(e, path))?;

        // create a source from the file
        let entrypoint = FileId::new(path);
        let text = std::fs::read_to_string(path).map_err(|e| FileError::from_io(e, path))?;
        let source = Source::new(entrypoint, text);

        let mut sources = HashMap::new();
        sources.insert(source.id(), source);

        Ok(Self {
            sources: Mutex::new(sources),
            entrypoint,
            root,
            library: library(),
        })
    }

    pub fn from_str(text: &str) -> Self {
        let entrypoint = FileId::new("main.comp");
        let source = Source::new(entrypoint, text.to_string());
        let mut sources = HashMap::new();
        sources.insert(source.id(), source);

        Self {
            sources: Mutex::new(sources),
            entrypoint,
            root: PathBuf::new(),
            library: library(),
        }
    }
    

    pub fn edit_source(&self, file_id: FileId, editor: impl FnOnce(&mut Source)) {
        let mut sources = self.sources.lock().unwrap();
        let source = sources.get_mut(&file_id).unwrap();
        editor(source);
    }

    pub fn entry_point_source(&self) -> FileResult<Source> {
        self.source(self.entrypoint)
    }

    pub fn get_or_read_from_disk(&self, file_id: FileId) -> FileResult<Source> {
        let mut sources = self.sources.lock().unwrap();
        if let Some(source) = sources.get(&file_id) {
            return Ok(source.clone());
        }

        // try to read from disk
        let path = file_id.path().0.clone();
        let text = std::fs::read_to_string(&path).map_err(|e| FileError::from_io(e, &path))?;
        let source = Source::new(file_id, text);
        sources.insert(source.id(), source.clone());
        Ok(source)
    }
}

impl World for SystemWorld {
    fn entry_point(&self) -> FileId {
        self.entrypoint
    }

    fn source(&self, file_id: FileId) -> FileResult<Source> {
        self.get_or_read_from_disk(file_id)
    }

    fn library(&self) -> &Library {
        &self.library
    }

    fn write(&self, f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdout())
    }

    fn read(&self, f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdin())
    }

    fn name(&self, id: FileId) -> String {
        id.path()
            .0
            .strip_prefix(&self.root)
            .unwrap_or(&id.path().0)
            .display()
            .to_string()
    }
}
