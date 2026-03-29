mod analyzer;
pub mod cursor;

use std::sync::{Mutex, MutexGuard};

use stdx::default;
use tower_lsp_server::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::{Error, Result},
    ls_types::*,
};

use crate::analyzer::Analyzer;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    analyzer: Mutex<Analyzer>,
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

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut completions = self.analyzer().get_completions();
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
        _params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        // TODO: Respect the document in params
        let symbols = self.analyzer().get_document_symbols();
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.analyzer()
            .set_file(params.text_document.uri.clone(), params.text_document.text);
        self.send_diagnostics(params.text_document.uri).await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        assert_eq!(params.content_changes.len(), 1);
        self.analyzer().update(
            params.text_document.uri.clone(),
            params.content_changes.remove(0).text,
        );
        self.send_diagnostics(params.text_document.uri).await;
    }
}

impl Backend {
    fn analyzer(&self) -> MutexGuard<'_, Analyzer> {
        self.analyzer.lock().unwrap()
    }

    async fn send_diagnostics(&self, uri: Uri) {
        let diagnostics = self
            .analyzer()
            .get_diagnostics()
            .into_iter()
            .map(|it| Diagnostic {
                range: it.span,
                severity: Some(DiagnosticSeverity::ERROR),
                message: it.message,
                ..default()
            })
            .collect();
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await
    }
}

fn main() {
    let stdin = blocking::Unblock::new(std::io::stdin());
    let stdout = blocking::Unblock::new(std::io::stdout());
    let analyzer = Mutex::new(Analyzer::default());

    let (service, socket) = LspService::new(|client| Backend { client, analyzer });
    futures_lite::future::block_on(Server::new(stdin, stdout, socket).serve(service));
}
