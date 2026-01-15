use std::fs;

use auryn::auryn::{api::compile, diagnostic_display::DisplayOptions};
use insta::{assert_snapshot, glob};

fn compile_diagnostics(input: &str) -> String {
    let Err(diagnostics) = compile(input) else {
        panic!("Input compiled successfully, but expected diagnostics!");
    };

    diagnostics
        .to_display_with_opts(DisplayOptions::FOR_TESTING)
        .to_string()
}

#[test]
fn diagnostic_tests() {
    glob!("inputs/diagnostics/*.au", |path| {
        let input = fs::read_to_string(path).unwrap();
        assert_snapshot!("ui", compile_diagnostics(&input), &input);
    });
}
