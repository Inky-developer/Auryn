mod analyzer;
pub mod cursor;

use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex, MutexGuard},
};

use dashmap::DashMap;
use stdx::{FastMap, FastSet, default};
use tower_lsp_server::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::{Error, Result},
    ls_types::*,
};

use crate::analyzer::Analyzer;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    workspaces: Workspaces,
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["custom.notification".to_string()],
                    ..default()
                }),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    // Resolve is used to get additonal details about an item.
                    // We can just render everthing by default
                    resolve_provider: Some(false),
                    trigger_characters: None,
                    ..default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                ..default()
            },
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<LSPAny>> {
        if params.command == "custom.notification" {
            self.client
                .log_message(MessageType::INFO, "custom.notification executed")
                .await;
            self.client
                .show_message(MessageType::INFO, "Hello world!")
                .await;
            Ok(None)
        } else {
            Err(Error::invalid_request())
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let data = self
            .workspaces
            .get(&params.text_document_position.text_document.uri)?;
        let mut workspace = data.lock();
        let mut completions = workspace.analyzer.get_completions(&data.file);
        completions.extend([CompletionItem {
            label: "for".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            insert_text: Some("let i: I32 = 0\nwhile i < $1 {\n\t$0\n\ti +=1\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..default()
        }]);
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let data = self.workspaces.get(&params.text_document.uri)?;
        let mut workspace = data.lock();
        let symbols = workspace.analyzer.get_document_symbols(&data.file);
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let handle = async || -> Result<()> {
            let data = self.workspaces.get(&params.text_document.uri)?;
            data.workspace
                .lock()
                .unwrap()
                .open_file(data.file, params.text_document.text);
            self.send_diagnostics(&data.workspace).await;
            Ok(())
        };
        if let Err(e) = handle().await {
            self.client
                .log_message(MessageType::WARNING, format!("Error in did_open: {e}"))
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let handle = async || -> Result<()> {
            let data = self.workspaces.get(&params.text_document.uri)?;
            let should_close = data.lock().close_file(&data.file);
            if should_close {
                let path = data.lock().analyzer.path.clone();
                self.workspaces.close(&path);
                self.client
                    .log_message(
                        MessageType::LOG,
                        format!("Closed workspace at {}", path.display()),
                    )
                    .await;
            }
            Ok(())
        };
        if let Err(e) = handle().await {
            self.client
                .log_message(MessageType::WARNING, format!("Error in did_close: {e}"))
                .await;
        };
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        assert_eq!(params.content_changes.len(), 1);
        let Ok(data) = self.workspaces.get(&params.text_document.uri) else {
            eprintln!("Error on did change");
            return;
        };

        data.lock()
            .analyzer
            .update(&data.file, params.content_changes.remove(0).text);

        self.send_diagnostics(&data.workspace).await;
    }
}

impl Backend {
    async fn send_diagnostics(&self, workspace: &Mutex<Workspace>) {
        let mut grouped_diagnostics = {
            let workspace = workspace.lock().unwrap();
            workspace
                .open_files
                .iter()
                .map(|file| (workspace.analyzer.get_uri_for_name(file), Vec::new()))
                .collect::<FastMap<Uri, Vec<Diagnostic>>>()
        };
        let diagnostics = workspace
            .lock()
            .unwrap()
            .analyzer
            .get_diagnostics()
            .into_iter()
            .map(|it| {
                (
                    it.file,
                    Diagnostic {
                        range: it.span,
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: it.message,
                        ..default()
                    },
                )
            });
        for (file, diag) in diagnostics {
            let Some(group) = grouped_diagnostics.get_mut(&file) else {
                continue;
            };
            group.push(diag);
        }
        for (uri, diagnostics) in grouped_diagnostics {
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }
}

#[derive(Debug, Default)]
pub struct Workspaces {
    data: DashMap<PathBuf, Arc<Mutex<Workspace>>>,
}

impl Workspaces {
    fn get(&self, uri: &Uri) -> Result<WorkspaceFile> {
        let (workspace, file) = get_workspace_and_file_name(uri).ok_or_else(|| {
            Error::invalid_params(format!(
                "uri does not resolve to a valid auryn file: {}",
                uri.as_str()
            ))
        })?;
        let workspace = self
            .data
            .entry(workspace.clone())
            .or_insert_with(|| Arc::new(Mutex::new(Workspace::new(workspace, &file))))
            .value()
            .clone();
        Ok(WorkspaceFile { workspace, file })
    }

    fn close(&self, path: &Path) {
        self.data.remove(path);
    }
}

#[derive(Debug)]
pub struct Workspace {
    analyzer: Analyzer,
    open_files: FastSet<String>,
}

impl Workspace {
    pub fn new(path: PathBuf, fallback_main: &str) -> Self {
        Self {
            analyzer: Analyzer::new(path, fallback_main),
            open_files: default(),
        }
    }

    pub fn open_file(&mut self, file: String, content: String) {
        self.analyzer.update(&file, content);
        self.open_files.insert(file);
    }

    /// Closes the file and returns whether this workspace can be closed
    pub fn close_file(&mut self, file: &str) -> bool {
        self.open_files.remove(file);
        self.open_files.is_empty()
    }
}

pub struct WorkspaceFile {
    pub workspace: Arc<Mutex<Workspace>>,
    pub file: String,
}

impl WorkspaceFile {
    pub fn lock(&self) -> MutexGuard<'_, Workspace> {
        self.workspace.lock().unwrap()
    }
}

fn get_workspace_and_file_name(uri: &Uri) -> Option<(PathBuf, String)> {
    let path = get_canonical_path(uri)?;
    let workspace = path.parent()?;
    let file = path.file_name()?;
    let name = file.to_str()?.strip_suffix(".au")?.to_string();
    Some((workspace.to_owned(), name))
}

fn get_canonical_path(uri: &Uri) -> Option<PathBuf> {
    if uri.scheme().as_str() != "file" {
        return None;
    }
    Some(uri.to_file_path()?.canonicalize().unwrap())
}

fn main() {
    let stdin = blocking::Unblock::new(std::io::stdin());
    let stdout = blocking::Unblock::new(std::io::stdout());

    let (service, socket) = LspService::new(|client| Backend {
        client,
        workspaces: default(),
    });
    futures_lite::future::block_on(Server::new(stdin, stdout, socket).serve(service));
}
