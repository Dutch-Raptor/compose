use codespan_reporting::files::{Error, Files};
use compose_library::diag::{eco_format, FileError, FileResult};
use compose_library::World;
use compose_syntax::{FileId, Source};
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

pub struct SystemWorld {
    sources: Mutex<HashMap<FileId, Source>>,
    entrypoint: FileId,
    root: PathBuf,
}

impl SystemWorld {
    /// assumes the folder containing the file is the root
    pub fn from_file(path: impl AsRef<Path>) -> FileResult<Self> {
        let path = path.as_ref();
        let root = path.parent().unwrap().to_path_buf();

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
        }
    }

    pub fn edit_source(&self, file_id: FileId, editor: impl FnOnce(&mut Source)) {
        let mut sources = self.sources.lock().unwrap();
        let source = sources.get_mut(&file_id).unwrap();
        editor(source);
    }
}

impl World for SystemWorld {
    fn entry_point(&self) -> FileId {
        self.entrypoint
    }

    fn source(&self, file_id: FileId) -> FileResult<Source> {
        let sources = self
            .sources
            .lock()
            .map_err(|e| FileError::Other(Some(eco_format!("{e}"))))?;
        match sources.get(&file_id) {
            Some(s) => Ok(s.clone()),
            None => Err(FileError::NotFound(file_id.path().0.clone())),
        }
    }
}

impl<'a> Files<'a> for &SystemWorld {
    type FileId = FileId;
    type Name = String;
    type Source = Source;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, Error> {
        // get path relative to root
        let path = match id.path().0.strip_prefix(&self.root) {
            Ok(p) => p,
            Err(_) => &id.path().0,
        };

        Ok(path.display().to_string())
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
