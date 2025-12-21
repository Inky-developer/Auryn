use std::{fmt::Debug, num::NonZeroU64, ops::Range};

use crate::auryn::file_id::FileId;

/// Consists of a 16 bit file id and an 48 bit id that uniquely represents a [`SyntaxItem`].
/// Tries to be somewhat stable after file modifications to increase the amount of cached data
/// that can be reused.
/// Heavily inspired by typst's `Span` type.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct SyntaxId(u64);

impl SyntaxId {
    pub const NUMBER_BITS: u64 = 48;
    pub const MAX_NUMBER: NonZeroU64 = NonZeroU64::new((1 << Self::NUMBER_BITS) - 1).unwrap();
    pub const NUMBER_RANGE: Range<NonZeroU64> = NonZeroU64::new(1).unwrap()..Self::MAX_NUMBER;

    pub fn new(file_id: FileId, number: NonZeroU64) -> Self {
        assert_eq!(
            number.get() >> Self::NUMBER_BITS,
            0,
            "number must use at most {} bits",
            Self::NUMBER_BITS
        );
        const _: () =
            const { assert!(std::mem::size_of::<FileId>() == 2, "FileID must be 16 bits") };
        Self((u64::from(file_id.0) << Self::NUMBER_BITS) | number.get())
    }

    /// Creates a new span which has no number assigned to it yet.
    pub fn new_unset(file_id: FileId) -> Self {
        Self(u64::from(file_id.0) << Self::NUMBER_BITS)
    }

    pub fn file_id(self) -> FileId {
        let value = self.0 >> Self::NUMBER_BITS;
        FileId(value.try_into().unwrap())
    }

    pub fn number(self) -> Option<NonZeroU64> {
        NonZeroU64::new(self.0 & Self::MAX_NUMBER.get())
    }

    pub fn set_number(&mut self, number: NonZeroU64) {
        *self = Self::new(self.file_id(), number)
    }
}

impl Debug for SyntaxId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SyntaxId")
            .field("file_id", &self.file_id())
            .field("number", &self.number())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::auryn::{file_id::FileId, syntax_id::SyntaxId};

    #[test]
    fn test_roundtrip() {
        let syntax_id = SyntaxId::new(FileId(42), 999.try_into().unwrap());
        assert_eq!(syntax_id.file_id(), FileId(42));
        assert_eq!(syntax_id.number().unwrap().get(), 999);

        let syntax_id = SyntaxId::new(FileId(0), ((1 << 48) - 1).try_into().unwrap());
        assert_eq!(syntax_id.file_id(), FileId(0));
        assert_eq!(syntax_id.number().unwrap().get(), (1 << 48) - 1);
    }

    #[test]
    fn test_unset() {
        let id = SyntaxId::new_unset(FileId(0));
        assert_eq!(id.file_id(), FileId(0));
        assert_eq!(id.number(), None)
    }

    #[test]
    #[should_panic]
    fn test_too_big_number() {
        SyntaxId::new(FileId(42), (1 << 48).try_into().unwrap());
    }
}
