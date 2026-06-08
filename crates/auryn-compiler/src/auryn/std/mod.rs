const STD_LIBRARY_SOURCE: &str = include_str!("std.au");

/// Loads the standard library source code to be injected into the compilation
pub fn load_std() -> Box<str> {
    STD_LIBRARY_SOURCE.into()
}
