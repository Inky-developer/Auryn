use std::{fs::OpenOptions, path::Path};

use crate::{
    auryn::{
        air::query_air,
        ast::query_ast,
        codegen_java::codegen::{CodegenOutput, codegen},
        diagnostic::{DiagnosticKind, Diagnostics, InputFile, InputFiles},
        diagnostic_display::{DiagnosticCollectionDisplay, DisplayOptions},
        file_id::FileId,
        parser::Parser,
    },
    utils::default,
};

pub struct OwnedDiagnostics {
    pub input_files: InputFiles,
    pub diagnostics: Diagnostics,
}

impl OwnedDiagnostics {
    pub fn to_display(&self) -> DiagnosticCollectionDisplay<'_> {
        self.to_display_with_opts(default())
    }

    pub fn to_display_with_opts(&self, opts: DisplayOptions) -> DiagnosticCollectionDisplay<'_> {
        self.diagnostics.to_display(&self.input_files, opts)
    }
}

pub fn compile(input: &str) -> Result<CodegenOutput, OwnedDiagnostics> {
    let result = Parser::new(FileId::MAIN_FILE, input).parse();
    let mut diagnostics = result
        .syntax_tree
        .as_ref()
        .map(|it| it.collect_diagnostics())
        .unwrap_or_default();

    let syntax_tree = result.syntax_tree.unwrap();
    // println!("{}", syntax_tree.display(input));

    let ast = query_ast(&syntax_tree).unwrap();
    let air = query_air(ast);
    // dbg!(&air);
    diagnostics.extend(air.diagnostics.take());
    if !diagnostics.is_empty() {
        let should_abort = diagnostics
            .iter()
            .any(|it| matches!(it.kind, DiagnosticKind::Error(_)));
        let mut input_files = InputFiles::default();
        input_files.add(
            FileId::MAIN_FILE,
            InputFile {
                name: "main".into(),
                source: input.into(),
                syntax_tree,
            },
        );
        let diagnostics: Diagnostics = diagnostics.into_iter().collect();

        if should_abort {
            return Err(OwnedDiagnostics {
                input_files,
                diagnostics,
            });
        }
    }
    Ok(codegen(&air.air))
}

pub fn run(codegen_output: CodegenOutput, dir: impl AsRef<Path>) -> String {
    let path = dir.as_ref();
    assert!(
        codegen_output.files.contains_key("Main"),
        "Could not find main file"
    );
    std::fs::create_dir_all(path).unwrap();

    for (file, data) in codegen_output.files {
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(path.join(format!("{file}.class")))
            .unwrap();
        data.serialize(&mut f).unwrap();
    }

    let output = std::process::Command::new("java")
        .current_dir(path)
        .arg("Main")
        .output()
        .expect("Java needs to be installed on the system to run a class");
    let mut result: String = output.stdout.try_into().unwrap();

    result.push_str(std::str::from_utf8(&output.stderr).unwrap());
    result
}
