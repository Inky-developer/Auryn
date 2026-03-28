use auryn_compiler::{
    Air, FileId, SyntaxId, World,
    diagnostics::{Diagnostics, DisplayOptions},
    types::{Type, TypeId},
};
use stdx::default;
use tower_lsp_server::ls_types::{CompletionItem, CompletionItemKind, Range, Uri};

use crate::cursor::Cursor;

#[derive(Debug)]
struct Outputs {
    pub air: Air,
    pub diagnostics: Diagnostics,
}

#[derive(Debug)]
pub struct Analyzer {
    world: World,
    current_data: Option<Outputs>,
    current_file: Uri,
}

impl Analyzer {
    pub fn set_file(&mut self, uri: Uri, content: String) {
        self.current_file = uri.clone();
        self.update(uri, content);
    }

    pub fn update(&mut self, uri: Uri, content: String) {
        if self.current_file == uri {
            self.world
                .input_files
                .update(FileId::MAIN_FILE, content.into());
        }
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
                span: self.map_span(it.main_label.id),
                message: it.main_label.message,
            })
            .collect()
    }

    pub fn get_completions(&mut self) -> Vec<CompletionItem> {
        let Some(data) = &self.current_data else {
            return Vec::new();
        };
        let ty_ctx = &data.air.ty_ctx;
        let main_module = ty_ctx.get(TypeId::from(FileId::MAIN_FILE));
        main_module
            .members
            .iter()
            .map(|(name, ty)| {
                let kind = match *ty {
                    Type::FunctionItem(_) => CompletionItemKind::FUNCTION,
                    Type::Module(_) => CompletionItemKind::MODULE,
                    _ => CompletionItemKind::CLASS,
                };
                CompletionItem {
                    label: name.as_ref().into(),
                    kind: Some(kind),
                    ..default()
                }
            })
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
}

impl Default for Analyzer {
    fn default() -> Self {
        let world = {
            let mut world = World::default();
            world.input_files.add("main".into(), "".into());
            world
        };
        let current_file = Uri::from_file_path(".").unwrap();
        Self {
            world,
            current_data: None,
            current_file,
        }
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub span: Range,
    pub message: String,
}
