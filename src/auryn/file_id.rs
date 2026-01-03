use std::num::NonZeroU16;

/// Identifies a single source file
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FileId(pub(super) NonZeroU16);

impl FileId {
    pub const MAIN_FILE: Self = Self(NonZeroU16::new(1).unwrap());
}
