use crate::file::FileId;
use std::fmt::{Debug, Formatter};
use std::num::{NonZeroU16, NonZeroU64};
use std::ops::Range;

/// Defines a range in a source file.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Span(NonZeroU64);

impl Span {
    /// A Span that does not point anywhere
    const DETACHED: u64 = 1;

    /// Data Layout:
    ///
    /// | 16 bits FileId | 48 bits Range |
    ///
    /// Range =
    /// - 1 means detached
    /// - rest is range
    const RANGE_BITS: usize = 48;

    const ATTACHED_RANGE: Range<u64> = 2..(1 << 48);
    const FILE_ID_SHIFT: usize = Self::RANGE_BITS;
    const ATTACHED_RANGE_START: u64 = Self::ATTACHED_RANGE.start;

    /// The number of bits in each part of the lower or higher bound of the range
    const RANGE_PART_BITS: usize = 24;

    const RANGE_PART_MASK: u64 = (1 << Self::RANGE_PART_BITS) - 1;
    const RANGE_MASK: u64 = (1 << Self::RANGE_BITS) - 1;

    /// Create a span that does not point to any file.
    pub const fn detached() -> Self {
        match NonZeroU64::new(Self::DETACHED) {
            Some(v) => Self(v),
            None => unreachable!(),
        }
    }

    /// Returns a new span with the given FileId and range
    pub(crate) const fn new(id: FileId, range: Range<usize>) -> Self {
        let range_as_number = Self::range_as_number(range);

        Self::pack(id, range_as_number)
    }

    /// Returns the range in the following format;
    /// | start (24 bits) | end (24 bits) |
    const fn range_as_number(range: Range<usize>) -> u64 {
        let max = (1 << Self::RANGE_PART_BITS) - Self::ATTACHED_RANGE_START as usize;

        debug_assert!(range.start <= max, "range.start too large");
        debug_assert!(range.end <= max, "range.end too large");

        let range_start = Self::min(range.start, max) as u64;
        let range_end = Self::min(range.end, max) as u64;
        
        (range_start << Self::RANGE_PART_BITS) | range_end
    }

    /// Packs a file id and a range
    const fn pack(id: FileId, range_as_number: u64) -> Self {
        let bits = ((id.into_raw().get() as u64) << Self::FILE_ID_SHIFT)
            | (range_as_number + Self::ATTACHED_RANGE_START);
        match NonZeroU64::new(bits) {
            Some(v) => Self(v),
            None => unreachable!(),
        }
    }

    pub const fn is_detached(self) -> bool {
        self.0.get() == Self::DETACHED
    }

    pub const fn id(self) -> Option<FileId> {
        match NonZeroU16::new((self.0.get() >> Self::FILE_ID_SHIFT) as u16) {
            None => None,
            Some(v) => Some(FileId::from_raw(v)),
        }
    }

    pub fn range(self) -> Option<Range<usize>> {
        if self.is_detached() {
            return None;
        }

        let number = (self.0.get() & Self::RANGE_MASK) - Self::ATTACHED_RANGE_START;

        let start = (number >> Self::RANGE_PART_BITS) as usize;
        let end = (number & Self::RANGE_PART_MASK) as usize;

        Some(start..end)
    }
    
    pub fn join(left: Span, right: Span) -> Span {
        if left.id() != right.id() {
            return left.or(right);
        }
        
        let id = left.or(right).id().unwrap();
        
        let (start, end) =  match (left.range(), right.range()) {
            (None, _) => return right,
            (_, None) => return left,
            (Some(l), Some(r)) => {
                (l.start.min(r.start), r.end.max(l.end))
            }
        };
        
        Span::new(id, start..end)
    }
    
    pub fn or(self, other: Span) -> Span {
        if self.is_detached() {
            return other;
        }
        self
    }
    
    pub fn after(self) -> Self {
        if self.is_detached() {
            return self;
        }
        
        let range = self.range().unwrap();
        let end = range.end;
        let at = end;
        
        Span::new(self.id().unwrap(), at..at + 1)
    }
}

impl Span {
    #[inline]
    const fn min(a: usize, b: usize) -> usize {
        if a <= b { a } else { b }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let id = match self.id() {
            Some(id) => id
                .try_path()
                .map(|p| p.display())
                .unwrap_or_else(|| "Unknown".to_string()),
            None => "Detached".to_string(),
        };

        let range = match self.range() {
            Some(range) => format!(":{:?}", range),
            None => "".to_string(),
        };

        write!(f, "{}{}", id, range)
    }
}

#[cfg(test)]
mod tests {
    use crate::file::FileId;
    use crate::span::Span;
    use crate::test_utils::test_file_id;
    use std::num::NonZeroU16;
    use std::ops::Range;

    #[test]
    fn test_range_as_number() {
        assert_eq!(
            Span::range_as_number(2..4),
            0b0000_0000_0000_0000_0000_0010_0000_0000_0000_0000_0000_0100u64
        );
    }

    #[test]
    fn pack_and_unpack_file_id() {
        let packed = Span::pack(test_file_id(), 0);
        assert_eq!(packed.id(), Some(test_file_id()));
        assert_eq!(packed.range(), Some(0..0));
    }

    #[test]
    fn test_detached() {
        let span = Span::detached();
        assert!(span.is_detached());
        assert_eq!(span.id(), None);
        assert_eq!(span.range(), None);
    }

    #[test]
    fn test_from_range() {
        let id = FileId::from_raw(NonZeroU16::new(1).unwrap());

        let round_trip = |range: Range<usize>| {
            let span = Span::new(id, range.clone());
            assert_eq!(span.id(), Some(id));
            assert_eq!(span.range(), Some(range));
        };

        round_trip(0..0);
        round_trip(0..1);
        round_trip(2..4);
        round_trip(100..100);
        round_trip(12_101..12_211);
    }
}
