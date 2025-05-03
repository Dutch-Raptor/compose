use std::num::NonZeroU16;
use crate::file::FileId;
use crate::SyntaxNode;

/// Creates a non-interned file id for testing.
pub(crate) const fn test_file_id() -> FileId {
    FileId::from_raw(NonZeroU16::new(1).unwrap())
}

pub fn test_parse(code: &str) -> Vec<SyntaxNode> {
    let file_id = FileId::new("main.comp");
    let nodes = crate::parse(code, file_id);
    dbg!(&nodes);
    nodes
}
