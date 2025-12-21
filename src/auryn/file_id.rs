/// Identifies a single source file
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FileId(pub(super) u16);

impl FileId {
    pub const MAIN_FILE: Self = Self(0);
}
