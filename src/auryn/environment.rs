use std::path::PathBuf;

/// An abstraction of the environment the compiler can interact with.
/// All interactions with the outside world, like reading files, go through this trait.
pub trait Environment {
    /// Loads the file that corresponds to the given module path
    fn load_module(&self, name: &str) -> Option<Box<str>>;
}

pub struct FilesystemEnvironment {
    cwd: PathBuf,
}

impl FilesystemEnvironment {
    pub fn new(working_directory: PathBuf) -> Self {
        FilesystemEnvironment {
            cwd: working_directory,
        }
    }
}

impl Environment for FilesystemEnvironment {
    fn load_module(&self, name: &str) -> Option<Box<str>> {
        let relative_path = name.replace(".", "/");
        let path = self.cwd.join(relative_path + ".au");
        let contents = std::fs::read_to_string(path).ok()?;
        Some(contents.into())
    }
}
