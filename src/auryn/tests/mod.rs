#![cfg(test)]

use std::fs;

use insta::{assert_snapshot, glob};

use crate::auryn::{
    air::query_air,
    ast::query_ast,
    diagnostic::{Diagnostics, InputFile, InputFiles},
    file_id::FileId,
    parser::Parser,
};

/// Compiles the input and outputs the diagnostics
fn compile(input: &str) -> String {
    let mut input_files = InputFiles::default();

    let parse_result = Parser::new(FileId::MAIN_FILE, input).parse();
    let mut diagnostics = parse_result
        .syntax_tree
        .as_ref()
        .map(|it| it.collect_diagnostics())
        .unwrap_or_default();

    let syntax_tree = parse_result.syntax_tree.unwrap();
    let ast = query_ast(&syntax_tree).unwrap();
    let air = query_air(ast);
    diagnostics.extend(air.diagnostics.take());

    input_files.add(
        FileId::MAIN_FILE,
        InputFile {
            name: "main".into(),
            source: input.into(),
            syntax_tree,
        },
    );
    let diagnostics: Diagnostics = diagnostics.into_iter().collect();
    let display = diagnostics.display(&input_files);
    let mut output = Vec::new();
    display.write(&mut output, false).unwrap();
    String::from_utf8(output).unwrap()
}

#[test]
fn ui_tests() {
    glob!("inputs/ui/*.au", |path| {
        let input = fs::read_to_string(path).unwrap();
        assert_snapshot!("ui", compile(&input), &input);
    });
}
