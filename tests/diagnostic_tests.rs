use std::fs;

use auryn::auryn::{api::compile_str, diagnostic_display::DisplayOptions};
use insta::{assert_snapshot, glob};

fn compile_diagnostics(input: &str) -> String {
    let Err(diagnostics) = compile_str(input) else {
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
        print!("test {} ... ", path.strip_prefix(&cwd).unwrap().display());
        let input = fs::read_to_string(path).unwrap();
        assert_snapshot!("ui", compile_diagnostics(&input), &input);
        println!("ok");
    });
}
