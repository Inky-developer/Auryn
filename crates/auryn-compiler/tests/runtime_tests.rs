use std::{path::PathBuf, process::Stdio};

use auryn_compiler::{compile_str, run};
use insta::glob;

use crate::common::log_test;

mod common;

struct TempDir(PathBuf);

impl TempDir {
    pub fn new() -> Self {
        let mut path = std::env::temp_dir();
        path.push("auryn-test-output/");
        std::fs::create_dir_all(&path).unwrap();
        Self(path)
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).unwrap();
    }
}

fn parse_expected_output(source: &str) -> String {
    let mut lines = source.lines();
    assert_eq!(lines.next(), Some("// expect-output:"));

    let mut expected_output = String::new();
    for line in lines {
        if !line.starts_with("// ") {
            break;
        }

        let line = line.trim_start_matches("// ");
        expected_output.push_str(line);
        expected_output.push('\n');
    }

    expected_output
}

#[test]
fn runtime_tests() {
    let cwd = std::env::current_dir().unwrap();
    glob!("inputs/runtime-tests/*.au", |path| {
        let _logger = log_test(path.strip_prefix(&cwd).unwrap().display());
        let content = std::fs::read_to_string(path).unwrap();
        let expected_output = parse_expected_output(&content);

        let output = match compile_str(&content) {
            Ok(output) => output,
            Err(error) => {
                eprintln!("{error}");
                panic!("Could not compile {path:?}");
            }
        };

        let dir = TempDir::new();
        let stdout = run(output, &dir.0, Stdio::piped());
        assert_eq!(stdout, expected_output);
    });
}
