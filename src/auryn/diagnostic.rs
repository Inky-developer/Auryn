use std::{
    fmt::{Debug, Display, Write},
    ops::RangeInclusive,
    panic::Location,
};

use crate::{
    auryn::{
        diagnostic_display::{
            DiagnosticCollectionDisplay, DiagnosticDisplay, DiagnosticLevel, DisplayOptions, Label,
        },
        input_files::InputFiles,
        syntax_id::SyntaxId,
        tokenizer::TokenSet,
    },
    utils::small_string::SmallString,
};

#[derive(Debug, Clone)]
pub enum DiagnosticError {
    // Generate by the parser
    ExpectedExpression {
        got: SmallString,
        valid_tokens: TokenSet,
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
        expected: TokenSet,
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
    ImmutableVariableUpdate,
    InvalidPlace,
    BreakOutsideLoop,
    ContinueOutsideLoop,
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
    UnsupportedOperationWithType {
        operation: SmallString,
        r#type: String,
    },
    ValueOutsideRange {
        range: RangeInclusive<i128>,
        got: i128,
        r#type: String,
    },
    MismatchedArgumentCount {
        expected: usize,
        got: usize,
        parameter_def: Option<SyntaxId>,
    },
    // TODO: This and Missing fields could just be a note on TypeMismatch
    UnexpectedFields {
        expected_fields: Vec<SmallString>,
        received_fields: Vec<SmallString>,
        unexpected_fields: Vec<SmallString>,
    },
    MissingFields {
        expected_fields: Vec<SmallString>,
        received_fields: Vec<SmallString>,
        missing_fields: Vec<SmallString>,
    },
    InferenceFailed,
    InvalidCast {
        from: String,
        to: String,
    },
    CircularTypeAlias {
        circular_type_alias: SyntaxId,
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

#[derive(Clone)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub syntax_id: SyntaxId,
    pub location: &'static Location<'static>,
}

impl Diagnostic {
    #[track_caller]
    pub fn new(syntax_id: SyntaxId, kind: impl Into<DiagnosticKind>) -> Self {
        Diagnostic {
            kind: kind.into(),
            syntax_id,
            location: Location::caller(),
        }
    }
}

impl Debug for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Diagnostic")
            .field("syntax_id", &self.syntax_id)
            .field("kind", &self.kind)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    #[track_caller]
    pub fn add(&mut self, syntax_id: SyntaxId, kind: impl Into<DiagnosticKind>) {
        self.diagnostics.push(Diagnostic::new(syntax_id, kind))
    }

    pub fn take(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn to_display<'a>(
        &'a self,
        input_files: &'a InputFiles,
        opts: DisplayOptions,
    ) -> DiagnosticCollectionDisplay<'a> {
        let mut display = DiagnosticCollectionDisplay::new(input_files, opts.clone());
        display.extend(self.diagnostics.iter().map(|it| it.display(&opts)));
        display
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }
}

impl FromIterator<Diagnostic> for Diagnostics {
    fn from_iter<T: IntoIterator<Item = Diagnostic>>(iter: T) -> Self {
        Self {
            diagnostics: iter.into_iter().collect(),
        }
    }
}

impl Diagnostic {
    fn display(&self, opts: &DisplayOptions) -> DiagnosticDisplay {
        let mut builder = DiagnosticDisplay::new(self.level(), self.syntax_id);
        self.build_report(&mut builder);

        if opts.write_debug_info {
            builder.with_info(format!("This diagnostic was emmited at {}", self.location));
        }

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
                DiagnosticError::ExpectedExpression { got, valid_tokens } => builder
                    .with_code("Expected expression")
                    .with_message(format!("got {got:?}"))
                    .with_info(format!("Valid tokens: {}", fmt_items(*valid_tokens, "or"))),
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
                    .with_info(format!("Expected {}", fmt_items(*expected, "or"))),
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
                DiagnosticError::ImmutableVariableUpdate => builder
                    .with_code("Immutable variable")
                    .with_message("Cannot write to an immutable variable"),
                // TODO: improve replace `expression` with the actual expression in help text
                DiagnosticError::InvalidPlace => builder
                    .with_code("Invalid place")
                    .with_message("Cannot write to this expression")
                    .with_help("Add an identifier you want to access: `expression.identifier`"),
                DiagnosticError::BreakOutsideLoop => builder.with_code("Break outside loop"),
                DiagnosticError::ContinueOutsideLoop => builder.with_code("Continue outside loop"),
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
                DiagnosticError::UnsupportedOperationWithType { operation, r#type } => builder
                    .with_code("Unsupported operation")
                    .with_message(format!(
                        "Operation `{operation}` is not supported on type `{type}`"
                    )),
                DiagnosticError::ValueOutsideRange { range, got, r#type } => builder
                    .with_code("Value outside of valid range")
                    .with_message(format!(
                        "Value {got} is outside the valid range {range:?} for type `{type}`"
                    )),
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

                    let argument_str = pluralize(*expected, "argument", "arguments");
                    builder
                        .with_code("Mismatched argument count")
                        .with_message(format!("Expected {expected} {argument_str}, but got {got}"))
                }
                DiagnosticError::InferenceFailed => builder
                    .with_code("Could not infer type")
                    .with_message("Could not infer the type of this expresssion")
                    .with_help("Try annotating the expression: `let val: ExpectedType = ...`"),
                DiagnosticError::InvalidCast { from, to } => builder
                    .with_code("Invalid cast")
                    .with_message(format!("Can not cast from `{from}` to `{to}`")),
                DiagnosticError::UnexpectedFields {
                    expected_fields: _,
                    received_fields: _,
                    unexpected_fields,
                } => builder.with_code("Unexpected fields").with_message(format!(
                    "Unexpected {}: {}",
                    pluralize(unexpected_fields.len(), "field", "fields"),
                    fmt_items(unexpected_fields, "and"),
                )),
                DiagnosticError::MissingFields {
                    expected_fields: _,
                    received_fields: _,
                    missing_fields,
                } => builder.with_code("Missing fields").with_message(format!(
                    "Missing {}: {}",
                    pluralize(missing_fields.len(), "field", "fields"),
                    fmt_items(missing_fields, "and"),
                )),
                DiagnosticError::CircularTypeAlias {
                    circular_type_alias,
                } => builder
                    .with_code("Circular type alias")
                    .with_message("A type alias must not refer to itself")
                    .with_label(Label::new(*circular_type_alias).with_message(
                        "Because it includes this type, which causes the circular reference",
                    )),
            },
        };
    }
}

fn fmt_items<
    I: IntoIterator<Item = T, IntoIter = E>,
    E: ExactSizeIterator<Item = T>,
    T: Display,
>(
    values: I,
    joiner: &str,
) -> String {
    let mut values = values.into_iter();
    match values.len() {
        0 => "nothing".to_string(),
        1 => format!("`{}`", values.next().unwrap()),
        len => {
            let mut result = String::new();

            for (index, option) in values.enumerate() {
                if index != 0 && index + 1 < len {
                    result.push_str(", ");
                } else if index + 1 == len {
                    write!(result, " {joiner} ").unwrap();
                }

                result.push('`');
                write!(result, "{option}").unwrap();
                result.push('`');
            }

            result
        }
    }
}

fn pluralize<'a>(count: usize, singular: &'a str, plural: &'a str) -> &'a str {
    if count == 1 { singular } else { plural }
}
