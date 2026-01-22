use std::{fs, path::Path};

use auryn::auryn::{api::compile_file, diagnostic_display::DisplayOptions};
use insta::{assert_snapshot, glob};

fn compile_diagnostics(path: &Path) -> String {
    let Err(diagnostics) = compile_file(path) else {
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
        assert_snapshot!("ui", compile_diagnostics(path), &input);
        println!("ok");
    });
}
