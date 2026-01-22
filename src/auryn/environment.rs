use std::path::PathBuf;

use crate::utils::{fast_map::FastMap, small_string::SmallString};

#[derive(Debug, Default)]
pub struct ProjectTree {
    pub source_files: FastMap<SmallString, Box<str>>,
}

/// An abstraction of the environment the compiler can interact with.
/// All interactions with the outside world, like reading files, go through this trait.
pub trait Environment {
    /// Loads the project file
    fn load_project(&self) -> ProjectTree;
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
    fn load_project(&self) -> ProjectTree {
        let mut source_files = FastMap::default();
        for entry in std::fs::read_dir(&self.cwd).unwrap() {
            let entry = entry.unwrap();
            if entry.file_type().unwrap().is_file() {
                let path = entry.path();
                let file_name = path.file_name().unwrap().to_string_lossy();
                if let Some(module_name) = file_name.strip_suffix(".au") {
                    let contents = std::fs::read_to_string(&path).unwrap();
                    source_files.insert(module_name.into(), contents.into());
                }
            }
        }

        ProjectTree { source_files }
    }
}
