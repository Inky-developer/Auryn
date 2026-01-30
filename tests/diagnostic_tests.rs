use std::fs;

use auryn::auryn::{
    api::{AurynError, compile_str},
    diagnostic_display::DisplayOptions,
};
use insta::{assert_snapshot, glob};

use crate::common::log_test;

mod common;

fn compile_diagnostics(input: &str) -> String {
    let Err(AurynError::CompilerError(diagnostics)) = compile_str(input) else {
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
