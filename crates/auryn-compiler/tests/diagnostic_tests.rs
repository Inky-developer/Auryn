use std::fs;

use crate::common::{log_test, parse_project_tree};
use auryn_compiler::{AurynError, compile_in_memory, diagnostics::DisplayOptions};
use insta::{assert_snapshot, glob};

mod common;

fn compile_diagnostics(input: &str) -> String {
    let project_tree = parse_project_tree(input);
    let Err(AurynError::CompilerError(diagnostics)) = compile_in_memory(project_tree) else {
        panic!("Input compiled successfully, but expected diagnostics!");
    };

    diagnostics
        .to_display_with_opts(DisplayOptions::FOR_TESTING)
        .to_string()
}

#[test]
fn diagnostic_tests() {
    let cwd = std::env::current_dir().unwrap();
    glob!("inputs/diagnostics/*.au", |path| {
        let _logger = log_test(path.strip_prefix(&cwd).unwrap().display());
        let input = fs::read_to_string(path).unwrap();
        assert_snapshot!("ui", compile_diagnostics(&input), &input);
    });
}
