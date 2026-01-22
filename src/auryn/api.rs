use std::{fs::OpenOptions, path::Path};

use crate::{
    auryn::{
        codegen_java::codegen::{CodegenOutput, codegen},
        diagnostic::{DiagnosticKind, Diagnostics},
        diagnostic_display::{DiagnosticCollectionDisplay, DisplayOptions},
        environment::{Environment, FilesystemEnvironment},
        input_files::InputFiles,
        world::World,
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

pub fn compile_file(main_file_path: &Path) -> Result<CodegenOutput, OwnedDiagnostics> {
    let dir = main_file_path.parent().expect("Should be a valid path");
    let main = main_file_path.file_name().expect("Should be a valid path");
    compile(
        main.to_str()
            .expect("Should be a valid file name")
            .strip_suffix(".au")
            .expect("Should be a .au file"),
        Box::new(FilesystemEnvironment::new(dir.to_path_buf())),
    )
}

pub fn compile_str(input: &str) -> Result<CodegenOutput, OwnedDiagnostics> {
    struct SingleFileEnvironment {
        file: Box<str>,
    }

    impl Environment for SingleFileEnvironment {
        fn load_module(&self, name: &str) -> Option<Box<str>> {
            if name == "main" {
                Some(self.file.clone())
            } else {
                None
            }
        }
    }

    compile(
        "main",
        Box::new(SingleFileEnvironment { file: input.into() }),
    )
}

fn compile(
    main_file: &str,
    environment: Box<dyn Environment>,
) -> Result<CodegenOutput, OwnedDiagnostics> {
    let mut world = World::new(environment);
    let file_id = world
        .file_id_for_module(main_file)
        .expect("Should be able to read input file");

    let (air, diagnostics) = world.query_air(file_id);
    if !diagnostics.is_empty() {
        let should_abort = diagnostics
            .iter()
            .any(|it| matches!(it.kind, DiagnosticKind::Error(_)));

        if should_abort {
            return Err(OwnedDiagnostics {
                input_files: world.input_files(),
                diagnostics,
            });
        }
    }
    Ok(codegen(&air))
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
