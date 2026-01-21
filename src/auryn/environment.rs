/// An abstraction of the environment the compiler can interact with.
/// All interactions with the outside world, like reading files, go through this trait.
pub trait Environment {
    /// Loads the file that corresponds to the given module path
    fn load_module(&self, name: &str) -> Option<Box<str>>;
}

pub struct FilesystemEnvironment;
impl Environment for FilesystemEnvironment {
    fn load_module(&self, name: &str) -> Option<Box<str>> {
        let path = name.replace(".", "/");
        let contents = std::fs::read_to_string(path).ok()?;
        Some(contents.into())
    }
}
