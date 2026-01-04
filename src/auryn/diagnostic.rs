use crate::{
    auryn::{
        air::{data::AirType, typecheck::types::Type},
        syntax_id::SyntaxId,
        tokenizer::TokenKind,
    },
    utils::small_string::SmallString,
};

#[derive(Debug, Clone)]
pub enum DiagnosticError {
    // Generate by the parser
    ExpectedExpression { got: TokenKind },
    ExpectedNumber { got: TokenKind },
    ExpectedType { got: TokenKind },
    ExpectedItem { got: TokenKind },
    ExpectedExternItem { got: TokenKind },
    ExpectedExternTypeBodyItem { got: TokenKind },
    UnexpectedToken { expected: TokenKind, got: TokenKind },
    ExpectedBinaryOperator { got: TokenKind },
    InvalidNumber,
    ExpectedValue { got: TokenKind },
    ExpectedNewline,
    // Generated during air
    UnexpectedExternTarget,
    ExternTypeRequiresMetadata,
    RedefinedVariable { ident: SmallString },
    BreakOutsideLoop,
    UndefinedVariable { ident: SmallString },
    UnknownIntrinsic { ident: SmallString },
    // Generated during typechecking
    UndefinedProperty { r#type: Type, ident: SmallString },
    TypeMismatch { expected: Type, got: AirType },
    ExpectedArray { got: AirType },
    ExpectedFunctionType { got: AirType },
    MismatchedParameterCount { expected: usize, got: usize },
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

#[derive(Debug, Clone, Copy)]
pub struct ComputedSpan {
    pub offset: u32,
    pub len: u32,
}

#[derive(Debug)]
pub struct ComputedDiagnostic {
    pub inner: Diagnostic,
    pub span: ComputedSpan,
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
}
