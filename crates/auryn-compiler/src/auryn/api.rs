use std::{
    fmt::{Debug, Display},
    fs::OpenOptions,
    path::{Path, PathBuf},
    process::Stdio,
};

use stdx::default;

use crate::auryn::{
    air::typecheck::type_context::TypeContext,
    codegen_java::codegen::{CodegenOutput, codegen},
    diagnostics::{
        diagnostic::Diagnostics,
        diagnostic_display::{DiagnosticCollectionDisplay, DiagnosticLevel},
    },
    environment::{Environment, FilesystemEnvironment, ProjectTree},
    file_id::FileId,
    input_files::InputFiles,
    monomorphization::monomorphize,
    world::World,
};

pub use crate::auryn::diagnostics::diagnostic_display::DisplayOptions;

#[derive(Debug)]
pub enum AurynError {
    CompilerError(Box<OwnedDiagnostics>),
    MainFileDoesNotExist(String),
    InvalidInputDir(PathBuf),
    InvalidInputFile(PathBuf),
}

impl From<OwnedDiagnostics> for AurynError {
    fn from(value: OwnedDiagnostics) -> Self {
        Self::CompilerError(Box::new(value))
    }
}

impl Display for AurynError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AurynError::*;
        match self {
            CompilerError(owned_diagnostics) => Display::fmt(owned_diagnostics, f),
            MainFileDoesNotExist(main) => write!(f, "Could not find main file {main}.au"),
            InvalidInputDir(path_buf) => {
                write!(f, "Input is not a valid directory: {}", path_buf.display())
            }
            InvalidInputFile(path_buf) => {
                write!(f, "Input is not a valid file: {}", path_buf.display())
            }
        }
    }
}

impl std::error::Error for AurynError {}

pub struct OwnedDiagnostics {
    pub input_files: InputFiles,
    pub ty_ctx: TypeContext,
    pub diagnostics: Diagnostics,
}

impl OwnedDiagnostics {
    pub fn to_display(&self) -> DiagnosticCollectionDisplay<'_> {
        self.to_display_with_opts(default())
    }

    pub fn to_display_with_opts(&self, opts: DisplayOptions) -> DiagnosticCollectionDisplay<'_> {
        self.diagnostics
            .to_display(&self.input_files, &self.ty_ctx, opts)
    }
}

impl Debug for OwnedDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.to_display_with_opts(DisplayOptions {
                use_color: false,
                ..default()
            })
        )
    }
}

impl Display for OwnedDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

pub fn compile_file(main_file_path: &Path) -> Result<CodegenOutput, AurynError> {
    let dir = main_file_path
        .parent()
        .ok_or_else(|| AurynError::InvalidInputDir(main_file_path.to_path_buf()))?;
    let main = main_file_path
        .file_name()
        .and_then(|it| it.to_str())
        .and_then(|it| it.strip_suffix(".au"))
        .ok_or_else(|| AurynError::InvalidInputFile(main_file_path.to_path_buf()))?;
    compile(main, &mut FilesystemEnvironment::new(dir.to_path_buf()))
}

pub fn compile_str(input: &str) -> Result<CodegenOutput, AurynError> {
    let mut tree = ProjectTree::default();
    tree.source_files.insert("main".into(), input.into());
    compile_in_memory(tree)
}

pub fn compile_in_memory(mut project_tree: ProjectTree) -> Result<CodegenOutput, AurynError> {
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
) -> Result<CodegenOutput, AurynError> {
    let mut world = World::new(environment, main_file)?;

    let (mut air, diagnostics) = world.query_air(FileId::MAIN_FILE);
    if !diagnostics.is_empty() {
        let should_abort = diagnostics
            .iter()
            .any(|it| matches!(it.level(), DiagnosticLevel::Error));

        if should_abort {
            return Err(OwnedDiagnostics {
                input_files: world.into_input_files(),
                ty_ctx: air.ty_ctx,
                diagnostics,
            }
            .into());
        }
    }
    let monomorphizations = monomorphize(&mut air);
    Ok(codegen(world.input_files(), &air, &monomorphizations))
}

/// Runs a program and returns its output.
/// The output is only collected when it is not redirected.
pub fn run(
    codegen_output: CodegenOutput,
    dir: impl AsRef<Path>,
    output: impl Into<Stdio>,
) -> String {
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
        .arg(path)
        .arg("Main")
        .stdout(output)
        .output()
        .expect("Java needs to be installed on the system to run a class");
    let mut result: String = output.stdout.try_into().unwrap();

    result.push_str(std::str::from_utf8(&output.stderr).unwrap());
    result
}
