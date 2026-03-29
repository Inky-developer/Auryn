use std::{
    borrow::Borrow,
    fmt::Debug,
    hash::Hash,
    num::{NonZeroU16, NonZeroU64},
    ops::{Deref, DerefMut, Range},
};

use stdx::SmallString;

/// Consists of a 16 bit file id and an 48 bit id that uniquely represents a [`super::syntax_tree::SyntaxItem`].
/// Tries to be somewhat stable after file modifications to increase the amount of cached data
/// that can be reused.
/// Heavily inspired by typst's `Span` type.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct SyntaxId(NonZeroU64);

impl SyntaxId {
    pub const NUMBER_BITS: u64 = 48;
    pub const MAX_NUMBER: u64 = (1 << Self::NUMBER_BITS) - 1;
    pub const NUMBER_RANGE: Range<u64> = 0..Self::MAX_NUMBER;

    pub fn new(file_id: Option<FileId>, number: u64) -> Self {
        Self::new_internal(file_id, number)
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
        let file_number = file_id.map_or(FileId::NO_FILE_VALUE, |id| id.0.get());
        let number = (u64::from(file_number) << Self::NUMBER_BITS) | number;
        // Because `FileId` itself is NonZero, this can never panic
        Self(NonZeroU64::new(number).unwrap())
    }

    /// Creates a new span which has no number assigned to it yet.
    pub fn new_unset(file_id: Option<FileId>) -> Self {
        Self::new_internal(file_id, 0)
    }

    pub fn file_id(self) -> Option<FileId> {
        let value = self.0.get() >> Self::NUMBER_BITS;
        let value: u16 = value.try_into().unwrap();
        // value is nonzero because file id itself is nonzero and the none case is mapped to a nonzero value
        FileId::new(NonZeroU16::new(value).unwrap())
    }

    pub fn number(self) -> u64 {
        self.0.get() & Self::MAX_NUMBER
    }

    pub fn set_number(&mut self, number: u64) {
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

/// Identifies a single source file
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FileId(NonZeroU16);

impl FileId {
    /// The file id of the main input file
    pub const MAIN_FILE: Self = Self::new(NonZeroU16::new(1).unwrap()).unwrap();
    /// Used by [`SyntaxId`] to represent unset files
    pub(super) const NO_FILE_VALUE: u16 = u16::MAX;

    pub(super) const fn new(id: NonZeroU16) -> Option<Self> {
        if id.get() == Self::NO_FILE_VALUE {
            return None;
        }
        Some(Self(id))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T: ?Sized> {
    pub syntax_id: SyntaxId,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: map(self.value),
            syntax_id: self.syntax_id,
        }
    }

    pub fn map_ref<U>(&self, map: impl FnOnce(&T) -> U) -> Spanned<U> {
        Spanned {
            value: map(&self.value),
            syntax_id: self.syntax_id,
        }
    }
}

impl<T, E> Spanned<Result<T, E>> {
    pub fn transpose(self) -> Result<Spanned<T>, E> {
        match self.value {
            Ok(value) => Ok(Spanned {
                value,
                syntax_id: self.syntax_id,
            }),
            Err(err) => Err(err),
        }
    }
}

impl<T> Spanned<Option<T>> {
    pub fn transpose(self) -> Option<Spanned<T>> {
        let value = self.value?;
        Some(Spanned {
            value,
            syntax_id: self.syntax_id,
        })
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T {
        &self.value
    }
}

// Unfortunate hack so that indexing into a map of `Spanned<SmallStrng>` keys works with `&str`
impl Borrow<str> for Spanned<SmallString> {
    fn borrow(&self) -> &str {
        self.value.borrow()
    }
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

pub trait SpanExt {
    fn with_span(self, syntax_id: SyntaxId) -> Spanned<Self>;
}

impl<T> SpanExt for T {
    fn with_span(self, syntax_id: SyntaxId) -> Spanned<Self> {
        Spanned {
            value: self,
            syntax_id,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::auryn::syntax_id::{FileId, SyntaxId};

    #[test]
    fn test_roundtrip() {
        let syntax_id = SyntaxId::new(
            Some(FileId(42.try_into().unwrap())),
            999.try_into().unwrap(),
        );
        assert_eq!(syntax_id.file_id(), Some(FileId(42.try_into().unwrap())));
        assert_eq!(syntax_id.number(), 999);

        let syntax_id = SyntaxId::new(Some(FileId(1.try_into().unwrap())), (1 << 48) - 1);
        assert_eq!(syntax_id.file_id(), Some(FileId(1.try_into().unwrap())));
        assert_eq!(syntax_id.number(), (1 << 48) - 1);
    }

    #[test]
    fn test_unset() {
        let id = SyntaxId::new_unset(None);
        assert_eq!(id.file_id(), None);
        assert_eq!(id.number(), 0)
    }

    #[test]
    #[should_panic]
    fn test_too_big_number() {
        SyntaxId::new(Some(FileId(42.try_into().unwrap())), 1 << 48);
    }
}
