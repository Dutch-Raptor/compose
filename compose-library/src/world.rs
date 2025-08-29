use crate::Library;
use crate::diag::FileResult;
use compose_codespan_reporting::files::Files;
use compose_syntax::{FileId, Source, Span};
use std::io::{Read, Write};
use std::ops::Range;

pub trait World {
    /// The entrypoint file of the program to execute
    fn entry_point(&self) -> FileId;

    fn source(&self, file_id: FileId) -> FileResult<Source>;

    fn library(&self) -> &Library;

    fn write(&self, f: &dyn Fn(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()>;
    fn read(&self, f: &dyn Fn(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()>;

    fn name(&self, id: FileId) -> String {
        id.path().0.display().to_string()
    }

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

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, compose_codespan_reporting::files::Error> {
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
