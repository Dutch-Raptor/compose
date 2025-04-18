use std::num::NonZeroU16;
use crate::file::FileId;

/// Creates a non-interned file id for testing.
pub(crate) const fn test_file_id() -> FileId {
    FileId::from_raw(NonZeroU16::new(1).unwrap())
}