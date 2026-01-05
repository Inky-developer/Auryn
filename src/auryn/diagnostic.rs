use crate::{
    auryn::{
        diagnostic_display::{
            ComputedSpan, DiagnosticCollectionDisplay, DiagnosticDisplay, DiagnosticLevel, Label,
        },
        file_id::FileId,
        syntax_id::SyntaxId,
        syntax_tree::SyntaxTree,
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug, Clone)]
pub enum DiagnosticError {
    // Generate by the parser
    ExpectedExpression {
        got: SmallString,
    },
    ExpectedNumber {
        got: SmallString,
    },
    ExpectedType {
        got: SmallString,
    },
    ExpectedItem {
        got: SmallString,
    },
    ExpectedExternItem {
        got: SmallString,
    },
    ExpectedExternTypeBodyItem {
        got: SmallString,
    },
    UnexpectedToken {
        expected: &'static str,
        got: SmallString,
    },
    ExpectedBinaryOperator {
        got: SmallString,
    },
    InvalidNumber,
    ExpectedValue {
        got: SmallString,
    },
    ExpectedNewline,
    // Generated during air
    UnexpectedExternTarget,
    ExternTypeRequiresMetadata,
    RedefinedVariable {
        ident: SmallString,
    },
    BreakOutsideLoop,
    UndefinedVariable {
        ident: SmallString,
    },
    // Generated during typechecking
    UndefinedProperty {
        value_id: SyntaxId,
        r#type: String,
        ident: SmallString,
    },
    TypeMismatch {
        expected: String,
        got: String,
    },
    MismatchedArgumentCount {
        expected: usize,
        got: usize,
        parameter_def: Option<SyntaxId>,
    },
}

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    Error(DiagnosticError),
}
impl DiagnosticKind {
    pub fn is_error(&self) -> bool {
        match self {
            DiagnosticKind::Error(_) => true,
        }
    }
}

impl From<DiagnosticError> for DiagnosticKind {
    fn from(value: DiagnosticError) -> Self {
        Self::Error(value)
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub syntax_id: SyntaxId,
}

impl Diagnostic {
    pub fn new(syntax_id: SyntaxId, kind: impl Into<DiagnosticKind>) -> Self {
        Diagnostic {
            kind: kind.into(),
            syntax_id,
        }
    }
}

#[derive(Debug, Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn add(&mut self, syntax_id: SyntaxId, kind: impl Into<DiagnosticKind>) {
        self.diagnostics.push(Diagnostic::new(syntax_id, kind))
    }

    pub fn take(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn display(self, input_files: &InputFiles) -> DiagnosticCollectionDisplay<'_> {
        let mut display = DiagnosticCollectionDisplay::new(input_files);
        display.extend(self.diagnostics.into_iter().map(|it| it.display()));
        display
    }
}

impl FromIterator<Diagnostic> for Diagnostics {
    fn from_iter<T: IntoIterator<Item = Diagnostic>>(iter: T) -> Self {
        Self {
            diagnostics: iter.into_iter().collect(),
        }
    }
}

#[derive(Debug)]
pub struct InputFile {
    pub name: SmallString,
    pub source: SmallString,
    pub syntax_tree: SyntaxTree,
}

impl InputFile {
    pub fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
        self.syntax_tree.get_span(syntax_id)
    }
}

#[derive(Debug, Default)]
pub struct InputFiles {
    data: FastMap<FileId, InputFile>,
}

impl InputFiles {
    pub fn add(&mut self, file_id: FileId, file: InputFile) {
        self.data.insert(file_id, file);
    }

    pub fn get(&self, file_id: FileId) -> &InputFile {
        &self.data[&file_id]
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &InputFile)> {
        self.data.iter().map(|(file_id, file)| (*file_id, file))
    }

    pub fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
        let file_id = syntax_id.file_id().unwrap();
        self.data[&file_id].compute_span(syntax_id)
    }
}

impl Diagnostic {
    fn display(&self) -> DiagnosticDisplay {
        let mut builder = DiagnosticDisplay::new(self.level(), self.syntax_id);
        self.build_report(&mut builder);
        builder
    }

    fn level(&self) -> DiagnosticLevel {
        match self.kind {
            DiagnosticKind::Error(_) => DiagnosticLevel::Error,
        }
    }

    fn build_report(&self, builder: &mut DiagnosticDisplay) {
        match &self.kind {
            DiagnosticKind::Error(error) => match error {
                DiagnosticError::ExpectedExpression { got } => builder
                    .with_code("Expected expression")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::ExpectedNumber { got } => builder
                    .with_code("Expected number")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::ExpectedType { got } => builder
                    .with_code("Expected type")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::ExpectedItem { got } => builder
                    .with_code("Expected item")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::ExpectedExternItem { got } => builder
                    .with_code("Expected extern item")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::ExpectedExternTypeBodyItem { got } => builder
                    .with_code("Expected extern type body item")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::UnexpectedToken { expected, got } => builder
                    .with_code("Unexpected token")
                    .with_message(format!("got {got:?}"))
                    .with_info(format!("Expected `{expected}`")),
                DiagnosticError::ExpectedBinaryOperator { got } => builder
                    .with_code("Expected binary operator")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::InvalidNumber => builder.with_code("Not a valid number"),
                DiagnosticError::ExpectedValue { got } => builder
                    .with_code("Expected value")
                    .with_message(format!("got {got:?}")),
                DiagnosticError::ExpectedNewline => builder.with_code("Expected newline"),
                DiagnosticError::UnexpectedExternTarget => {
                    builder.with_code("Unknown extern target")
                }
                DiagnosticError::ExternTypeRequiresMetadata => builder
                    .with_code("Extern type requires metadata")
                    .with_help(
                        "Add metadata before to declare its extern name: `[\"extern_name\"]`",
                    ),
                DiagnosticError::RedefinedVariable { ident } => builder
                    .with_code("Redefined variable")
                    .with_message(format!("Variable `{ident}` was redefined"))
                    .with_info("A variable may only be defined once"),
                DiagnosticError::BreakOutsideLoop => builder.with_code("Break outside loop"),
                DiagnosticError::UndefinedVariable { ident } => builder
                    .with_code("Undefined variable")
                    .with_message(format!("Undefined variable `{ident}`")),
                DiagnosticError::UndefinedProperty {
                    r#type,
                    ident,
                    value_id,
                } => builder
                    .with_code("Undefined property")
                    .with_message(format!("Unknown property `{ident}`"))
                    .with_label(
                        Label::new(*value_id).with_message(format!("Value has type `{type}`")),
                    ),
                DiagnosticError::TypeMismatch { expected, got } => builder
                    .with_code("Type mismatch")
                    .with_message(format!("Expected `{expected}`, but got `{got}`")),
                DiagnosticError::MismatchedArgumentCount {
                    expected,
                    got,
                    parameter_def,
                } => {
                    if let Some(def) = parameter_def {
                        builder.with_label(
                            Label::new(*def).with_message("Due to the definition here"),
                        );
                    }

                    let argument_str = if *expected == 1 {
                        "argument"
                    } else {
                        "arguments"
                    };
                    builder
                        .with_code("Mismatched argument count")
                        .with_message(format!("Expected {expected} {argument_str}, but got {got}"))
                }
            },
        };
    }
}
