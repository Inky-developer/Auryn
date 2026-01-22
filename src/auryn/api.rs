use std::{fs::OpenOptions, path::Path};

use crate::{
    auryn::{
        codegen_java::codegen::{CodegenOutput, codegen},
        diagnostic::{DiagnosticKind, Diagnostics},
        diagnostic_display::{DiagnosticCollectionDisplay, DisplayOptions},
        environment::{Environment, FilesystemEnvironment, ProjectTree},
        file_id::FileId,
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
        &mut FilesystemEnvironment::new(dir.to_path_buf()),
    )
}

pub fn compile_str(input: &str) -> Result<CodegenOutput, OwnedDiagnostics> {
    let mut tree = ProjectTree::default();
    tree.source_files.insert("main".into(), input.into());
    compile_in_memory(tree)
}

pub fn compile_in_memory(mut project_tree: ProjectTree) -> Result<CodegenOutput, OwnedDiagnostics> {
    struct ProjectTreeEnvironment<'a> {
        project_tree: &'a mut ProjectTree,
    }

    impl Environment for ProjectTreeEnvironment<'_> {
        fn load_project(&mut self) -> ProjectTree {
            std::mem::take(self.project_tree)
        }
    }

    compile(
        "main",
        &mut ProjectTreeEnvironment {
            project_tree: &mut project_tree,
        },
    )
}

fn compile(
    main_file: &str,
    environment: &mut impl Environment,
) -> Result<CodegenOutput, OwnedDiagnostics> {
    let mut world = World::new(environment, main_file);

    let (air, diagnostics) = world.query_air(FileId::MAIN_FILE);
    if !diagnostics.is_empty() {
        let should_abort = diagnostics
            .iter()
            .any(|it| matches!(it.kind, DiagnosticKind::Error(_)));

        if should_abort {
            return Err(OwnedDiagnostics {
                input_files: world.into_input_files(),
                diagnostics,
            });
        }
    }
    Ok(codegen(world.input_files(), &air))
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
        .arg("-cp")
        .args(path)
        .arg("Main")
        .output()
        .expect("Java needs to be installed on the system to run a class");
    let mut result: String = output.stdout.try_into().unwrap();

    result.push_str(std::str::from_utf8(&output.stderr).unwrap());
    result
}
