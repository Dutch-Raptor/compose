use crate::Library;
use crate::diag::FileResult;
use compose_codespan_reporting::files::Files;
use compose_syntax::{FileId, Source, Span};
use std::io::{Read, Write};
use std::ops::Range;

/**
Defines how Compose interacts with the outside world.

[`World`] is an abstraction layer between the Compose runtime and its external environment.

It is responsible for:

- Loading source files and defining the program entrypoint.
- Providing access to a standard library.
- Provide I/O primitives for stdout.

This trait allows Compose to be embedded in different environments
(e.g. CLI tools, editors, tests, or sandboxes) without hard-coding
filesystem or I/O behaviour.
*/
pub trait World {
    /// Returns the [`FileId`] of the entrypoint of the program.
    fn entry_point(&self) -> FileId;

    /// Returns the Source identified by the given [`FileId`]
    ///
    /// # Errors
    ///
    /// Returns an error if the source cannot be located or loaded.
    fn source(&self, file_id: FileId) -> FileResult<Source>;

    /// Returns a reference to the standard library available to the program.
    fn library(&self) -> &Library;

    /// Provides write access to the program's output stream.
    ///
    /// The provided closure is given exclusive access to a writer
    /// for the duration of the call.
    fn write(
        &self,
        f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>,
    ) -> std::io::Result<()>;

    /// Provides read access to the program's input stream.
    ///
    /// The provided closure is given exclusive access to the reader
    /// for the duration of the call.
    fn read(&self, f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()>;

    /// Provides read and write access to the programs input and output stream.
    ///
    /// The provided closure is given exclusive access to the reader and writer
    /// for the duration of the call.
    fn with_io(
        &self,
        f: &mut dyn FnMut(&mut dyn Read, &mut dyn Write) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        self.read(&mut |r| self.write(&mut |w| f(r, w)))
    }

    /// Returns a human-readable name for the given file identifier.
    ///
    /// By default, this is derived from the file's path.
    fn name(&self, id: FileId) -> String {
        id.path().0.display().to_string()
    }

    /// Attempts to retrieve the source associated with the given span.
    ///
    /// This is primarily used for diagnostics and error reporting.
    fn related_source(&self, span: Span) -> Option<Source> {
        self.source(span.id()?).ok()
    }
}

pub struct SyntaxContext<'a> {
    pub world: &'a dyn World,
}

impl<'a> Files<'a> for &dyn World {
    type FileId = FileId;
    type Name = String;
    type Source = Source;

    fn name(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Name, compose_codespan_reporting::files::Error> {
        Ok(World::name(*self, id))
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, compose_codespan_reporting::files::Error> {
        World::source(*self, id).map_err(|_| compose_codespan_reporting::files::Error::FileMissing)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, compose_codespan_reporting::files::Error> {
        let source = Files::source(self, id)?;
        Ok(source
            .line_starts()
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, compose_codespan_reporting::files::Error> {
        let source = Files::source(self, id)?;
        let start = source.line_starts().get(line_index).copied().ok_or(
            compose_codespan_reporting::files::Error::LineTooLarge {
                given: line_index,
                max: source.line_starts().len(),
            },
        )?;

        let end = source
            .line_starts()
            .get(line_index + 1)
            .copied()
            .unwrap_or(source.text().len());

        Ok(start..end)
    }
}
