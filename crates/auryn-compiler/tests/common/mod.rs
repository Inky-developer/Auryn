use std::{
    fmt::Display,
    io::{StdoutLock, Write},
};

use auryn_compiler::ProjectTree;
use yansi::Paint;

struct TestFinishedLogger(StdoutLock<'static>);

impl Drop for TestFinishedLogger {
    fn drop(&mut self) {
        if std::thread::panicking() {
            let _ = writeln!(self.0, "{}", "FAILED".red());
        } else {
            let _ = writeln!(self.0, "{}", "ok".green());
        }
    }
}

#[must_use]
pub fn log_test(name: impl Display) -> impl Drop {
    let mut stdout = std::io::stdout().lock();
    let _ = write!(stdout, "test {name} ... ");
    TestFinishedLogger(stdout)
}

/// Parses `// file: <name>` markers from test source to build a multi-file project.
/// If no markers are found, the entire source is treated as the "main" file.
pub fn parse_project_tree(source: &str) -> ProjectTree {
    let mut tree = ProjectTree::default();

    let files: Vec<&str> = source.split("\n// file: ").collect();
    if files.len() == 1 {
        tree.source_files.insert("main".into(), source.into());
    } else {
        let preamble = files[0];
        assert!(
            preamble
                .lines()
                .all(|line| line.trim().is_empty() || line.starts_with("//")),
            "Only whitespace and comments are allowed before the first `// file:` directive, \
             but found:\n{preamble}"
        );
        for file in &files[1..] {
            let (name, code) = file.split_once("\n").unwrap();
            tree.source_files.insert(name.into(), code.into());
        }
    }

    tree
}
