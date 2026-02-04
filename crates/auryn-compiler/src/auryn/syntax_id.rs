use std::{
    fmt::Debug,
    num::NonZeroU64,
    ops::{Deref, DerefMut, Range},
};

use crate::auryn::file_id::FileId;

/// Consists of a 16 bit file id and an 48 bit id that uniquely represents a [`super::syntax_tree::SyntaxItem`].
/// Tries to be somewhat stable after file modifications to increase the amount of cached data
/// that can be reused.
/// Heavily inspired by typst's `Span` type.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct SyntaxId(u64);

impl SyntaxId {
    pub const NUMBER_BITS: u64 = 48;
    pub const MAX_NUMBER: NonZeroU64 = NonZeroU64::new((1 << Self::NUMBER_BITS) - 1).unwrap();
    pub const NUMBER_RANGE: Range<NonZeroU64> = NonZeroU64::new(1).unwrap()..Self::MAX_NUMBER;

    // TODO: Make file_id required
    pub fn new(file_id: Option<FileId>, number: NonZeroU64) -> Self {
        Self::new_internal(file_id, number.get())
    }

    fn new_internal(file_id: Option<FileId>, number: u64) -> Self {
        assert_eq!(
            number >> Self::NUMBER_BITS,
            0,
            "number must use at most {} bits",
            Self::NUMBER_BITS
        );
        const _: () =
            const { assert!(std::mem::size_of::<FileId>() == 2, "FileID must be 16 bits") };
        let file_number: u16 = file_id.map_or(0, |it| it.0.get());
        Self((u64::from(file_number) << Self::NUMBER_BITS) | number)
    }

    /// Creates a new span which has no number assigned to it yet.
    pub fn new_unset(file_id: Option<FileId>) -> Self {
        Self::new_internal(file_id, 0)
    }

    pub fn file_id(self) -> Option<FileId> {
        let value = self.0 >> Self::NUMBER_BITS;
        let value: u16 = value.try_into().unwrap();
        value.try_into().ok().map(FileId)
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

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub syntax_id: SyntaxId,
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

#[cfg(test)]
mod tests {
    use crate::auryn::{file_id::FileId, syntax_id::SyntaxId};

    #[test]
    fn test_roundtrip() {
        let syntax_id = SyntaxId::new(
            Some(FileId(42.try_into().unwrap())),
            999.try_into().unwrap(),
        );
        assert_eq!(syntax_id.file_id(), Some(FileId(42.try_into().unwrap())));
        assert_eq!(syntax_id.number().unwrap().get(), 999);

        let syntax_id = SyntaxId::new(
            Some(FileId(1.try_into().unwrap())),
            ((1 << 48) - 1).try_into().unwrap(),
        );
        assert_eq!(syntax_id.file_id(), Some(FileId(1.try_into().unwrap())));
        assert_eq!(syntax_id.number().unwrap().get(), (1 << 48) - 1);
    }

    #[test]
    fn test_unset() {
        let id = SyntaxId::new_unset(None);
        assert_eq!(id.file_id(), None);
        assert_eq!(id.number(), None)
    }

    #[test]
    #[should_panic]
    fn test_too_big_number() {
        SyntaxId::new(
            Some(FileId(42.try_into().unwrap())),
            (1 << 48).try_into().unwrap(),
        );
    }
}
