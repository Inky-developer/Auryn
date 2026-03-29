use std::path::PathBuf;

use auryn_compiler::{
    Air, Environment, FileId, FilesystemEnvironment, SyntaxId, World,
    diagnostics::{Diagnostics, DisplayOptions},
    types::{Type, TypeId, TypeView},
};
use stdx::default;
use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, Location, Range, SymbolInformation, SymbolKind, Uri,
};

use crate::cursor::Cursor;

#[derive(Debug)]
struct Outputs {
    pub air: Air,
    pub diagnostics: Diagnostics,
}

/// The analyzer does the main work.
/// A single [`Analyzer`] owns a single workspace.
#[derive(Debug)]
pub struct Analyzer {
    pub path: PathBuf,
    world: World,
    current_data: Option<Outputs>,
}

impl Analyzer {
    pub fn new(workspace_path: PathBuf, fallback_main: &str) -> Self {
        let world = {
            let mut environment = FilesystemEnvironment::new(workspace_path.clone());
            let project_tree = environment.load_project();
            let preferred_main = "main";
            let main_file = if project_tree.source_files.contains_key(preferred_main) {
                preferred_main
            } else {
                fallback_main
            };
            World::new(project_tree, main_file).unwrap()
        };
        Self {
            world,
            current_data: None,
            path: workspace_path,
        }
    }

    pub fn update(&mut self, file: &str, content: String) {
        let Some(id) = self.get_file_id(file) else {
            return;
        };
        self.world.input_files.update(id, content.into());
        let (air, diagnostics) = self.world.query_air();
        self.current_data = Some(Outputs { air, diagnostics });
    }

    pub fn get_diagnostics(&mut self) -> Vec<Diagnostic> {
        let Some(data) = &self.current_data else {
            return Vec::new();
        };

        data.diagnostics
            .to_display(
                &self.world.input_files,
                &data.air.ty_ctx,
                DisplayOptions {
                    use_color: false,
                    write_debug_info: false,
                },
            )
            .displays
            .into_iter()
            .map(|it| Diagnostic {
                file: self.get_uri(it.main_label.id.file_id().unwrap()),
                span: self.map_span(it.main_label.id),
                message: it.main_label.message,
            })
            .collect()
    }

    pub fn get_completions(&mut self, file: &str) -> Vec<CompletionItem> {
        let Some(data) = &self.current_data else {
            return Vec::new();
        };
        let Some(file_id) = self.get_file_id(file) else {
            return Vec::new();
        };
        let ty_ctx = &data.air.ty_ctx;
        let file_module = ty_ctx.get(TypeId::from(file_id));
        file_module
            .members
            .iter()
            .map(|(name, ty)| {
                let kind = TypeCategory::from(ty.as_view(ty_ctx)).into();
                CompletionItem {
                    label: name.value.as_ref().into(),
                    kind: Some(kind),
                    ..default()
                }
            })
            .collect()
    }

    fn get_symbols(
        &self,
        file_id: FileId,
        data: &Outputs,
        filter: Option<&str>,
    ) -> impl Iterator<Item = SymbolInformation> {
        let ty_ctx = &data.air.ty_ctx;
        let file_module = ty_ctx.get(TypeId::from(file_id));
        file_module
            .members
            .iter()
            .filter(|(name, _)| name.syntax_id.number().is_some())
            .filter(move |(name, _)| filter.is_none_or(|filter| name.contains(filter)))
            .map(move |(name, ty)| {
                let kind = TypeCategory::from(ty.as_view(ty_ctx)).into();
                let range = self.map_span(name.syntax_id);
                let location = Location {
                    uri: self.get_uri(file_id),
                    range,
                };
                SymbolInformation {
                    name: name.value.to_string(),
                    kind,
                    tags: None,
                    #[expect(deprecated)]
                    deprecated: None,
                    location,
                    container_name: None,
                }
            })
    }

    pub fn get_document_symbols(&self, file: &str) -> Vec<SymbolInformation> {
        let Some(data) = &self.current_data else {
            return Vec::new();
        };
        let Some(file_id) = self.get_file_id(file) else {
            return Vec::new();
        };
        self.get_symbols(file_id, data, None).collect()
    }

    pub fn get_workspace_symbols(&self, query: &str) -> Vec<SymbolInformation> {
        let Some(data) = &self.current_data else {
            return Vec::new();
        };
        self.world
            .input_files
            .iter()
            .flat_map(|(file_id, _)| self.get_symbols(file_id, data, Some(query)))
            .collect()
    }

    /// Converts a [`SyntaxId`] into a line-column range.
    /// Should probably cache this at some point.
    fn map_span(&self, id: SyntaxId) -> Range {
        let computed = self.world.input_files.compute_span(id);
        let file = self.world.input_files.get(computed.file_id);
        let mut cursor = Cursor::from(file.source.as_ref());
        cursor.advance_bytes(computed.offset as usize);
        let start = cursor.pos;
        cursor.advance_bytes(computed.len as usize);
        let end = cursor.pos;
        Range { start, end }
    }

    fn get_file_id(&self, name: &str) -> Option<FileId> {
        self.world.input_files.get_id(name)
    }

    pub fn get_uri(&self, file_id: FileId) -> Uri {
        let name = &self.world.input_files.get(file_id).name;
        self.get_uri_for_name(name)
    }

    pub fn get_uri_for_name(&self, name: &str) -> Uri {
        let file_name = format!("{name}.au");
        let path = self.path.join(file_name);
        Uri::from_file_path(path).expect("Should be a valid uri")
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub span: Range,
    pub file: Uri,
    pub message: String,
}

#[derive(Debug)]
enum TypeCategory {
    Struct,
    Module,
    Function,
}

impl From<TypeView<'_>> for TypeCategory {
    fn from(value: TypeView) -> Self {
        match value {
            TypeView::FunctionItem(_) => Self::Function,
            TypeView::Meta(meta) if matches!(meta.inner, Type::Module(_)) => Self::Module,
            _ => Self::Struct,
        }
    }
}

impl From<TypeCategory> for SymbolKind {
    fn from(value: TypeCategory) -> Self {
        match value {
            TypeCategory::Struct => Self::STRUCT,
            TypeCategory::Module => Self::MODULE,
            TypeCategory::Function => Self::FUNCTION,
        }
    }
}

impl From<TypeCategory> for CompletionItemKind {
    fn from(value: TypeCategory) -> Self {
        match value {
            TypeCategory::Struct => Self::STRUCT,
            TypeCategory::Module => Self::MODULE,
            TypeCategory::Function => Self::FUNCTION,
        }
    }
}
