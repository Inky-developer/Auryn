use std::{fmt, fmt::Formatter, hash::Hash, num::NonZeroU8};

/// A very simple immutable string type that implements the small string optimization.
/// Should probably be replaced at some point with a more sophisticated implementation,
/// but I am actually quite surprised how much stack space can be used with a simple enum
/// the size of a [`String`].
pub enum SmallString {
    Heap(Box<str>),
    Stack {
        data: [u8; SMALL_STRING_INLINE_SIZE],
        len: NonZeroU8,
    },
}

const SMALL_STRING_INLINE_SIZE: usize = 23;

const _: () = const { assert!(size_of::<SmallString>() == 24) };

impl AsRef<str> for SmallString {
    fn as_ref(&self) -> &str {
        match self {
            Self::Heap(s) => s.as_ref(),
            Self::Stack { data, len } => std::str::from_utf8(&data[..len.get() as usize]).unwrap(),
        }
    }
}

impl From<&str> for SmallString {
    fn from(s: &str) -> Self {
        if s.len() <= SMALL_STRING_INLINE_SIZE
            && let Some(non_zero_len) = NonZeroU8::new(s.len().try_into().unwrap())
        {
            let mut data = [0; SMALL_STRING_INLINE_SIZE];
            data[..s.len()].copy_from_slice(s.as_bytes());
            SmallString::Stack {
                data,
                len: non_zero_len,
            }
        } else {
            SmallString::Heap(s.into())
        }
    }
}

impl From<String> for SmallString {
    fn from(s: String) -> Self {
        SmallString::Heap(s.into())
    }
}

impl PartialEq for SmallString {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for SmallString {}

impl Clone for SmallString {
    fn clone(&self) -> Self {
        SmallString::from(self.as_ref())
    }
}

impl Hash for SmallString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl fmt::Debug for SmallString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl fmt::Display for SmallString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}
