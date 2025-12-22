#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Top,
    Number,
    String,
    Null,
    Error,
}
impl Type {
    pub fn is_subtype(&self, other: &Type) -> bool {
        self == other || matches!(other, Type::Top)
    }
}
