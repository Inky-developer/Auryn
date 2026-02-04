use std::ops::RangeInclusive;

use stdx::SmallString;

use crate::{
    auryn::{
        air::typecheck::{bounds::MaybeBounded, types::Type},
        diagnostics::{
            diagnostic_display::{DiagnosticLevel, Label},
            text_utils::{fmt_items, pluralize},
        },
        syntax_id::SyntaxId,
        tokenizer::TokenSet,
    },
    diag,
};

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected expression")]
    #[message("got {got:?}")]
    #[info("Valid tokens: {}", fmt_items(*valid_tokens, "or"))]
    pub struct ExpectedExpression {
        got: SmallString,
        #[no_display]
        valid_tokens: TokenSet,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected type")]
    #[message("got {got:?}")]
    pub struct ExpectedType {
        got: SmallString,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected item")]
    #[message("got {got:?}")]
    pub struct ExpectedItem {
        got: SmallString,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected extern item")]
    #[message("got {got:?}")]
    pub struct ExpectedExternItem {
        got: SmallString,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected extern type body item")]
    #[message("got {got:?}")]
    pub struct ExpectedExternTypeBodyItem {
        got: SmallString,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Unexpected token")]
    #[message("got {got:?}")]
    #[info("Expected {}", fmt_items(*expected, "or"))]
    pub struct UnexpectedToken {
        #[no_display]
        expected: TokenSet,
        got: SmallString,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Not a valid number")]
    pub struct InvalidNumber;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected value")]
    #[message("got {got:?}")]
    pub struct ExpectedValue {
        got: SmallString
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected newline")]
    pub struct ExpectedNewline;
}

// Generated during air

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Unknown extern target")]
    pub struct UnexpectedExternTarget;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Extern type requires metadata")]
    #[help("Add metadata before to declare its extern name `[\"extern_name\"]`")]
    pub struct ExternTypeRequiresMetadata;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Immutable variable")]
    #[message("Cannot write to an immutable variable")]
    pub struct ImmutableVariableUpdate;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Invalid place")]
    #[message("Cannot write to this expression")]
    #[help("Add an identifier you want to access: `expression.identifier`")]
    pub struct InvalidPlace;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Break outside loop")]
    pub struct BreakOutsideLoop;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Continue outside loop")]
    pub struct ContinueOutsideLoop;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Undefined variable")]
    #[message("Undefined variable `{ident}`")]
    pub struct UndefinedVariable {
        ident: SmallString,
    }
}

// Generated during typechecking

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Undefined property")]
    #[message("Unknown property `{ident}`")]
    #[label(Label::new(value_id).with_message(format!("Value has type `{type}`")))]
    pub struct UndefinedProperty {
        value_id: SyntaxId,
        #[validate]
        r#type: Type,
        ident: SmallString,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Type mismatch")]
    #[message("Expected `{expected}`, but got `{got}`")]
    pub struct TypeMismatch {
        #[validate]
        expected: MaybeBounded,
        #[validate]
        got: Type,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected struct")]
    #[message("Expected this to be a struct, but it is of type `{got}`")]
    pub struct ExpectedStruct {
        #[validate]
        got: Type,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Expected function")]
    #[message("Can only call functions, but got `{got}`")]
    pub struct ExpectedFunction {
        #[validate]
        got: Type
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Unsupported operation")]
    #[message("Operation `{operation}` is not supported on type `{type}`")]
    pub struct UnsupportedOperationWithType {
        operation: SmallString,
        #[validate]
        r#type: Type,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Value outside of valid range")]
    #[message("Value {got} is outside the valid range {range:?} for type `{type}`")]
    pub struct ValueOutsideRange {
        #[no_display]
        range: RangeInclusive<i128>,
        got: i128,
        #[validate]
        r#type: Type,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Mismatched argument count")]
    #[message(
        "Expected {expected} {}, but got {got}",
        pluralize(*expected, "argument", "arguments")
    )]
    #[custom_diagnostic(|builder|
        if let Some(def) = *parameter_def {
            builder.with_label(Label::new(def).with_message("Due to the definition here"));
        }
    )]
    pub struct MismatchedArgumentCount {
        expected: usize,
        got: usize,
        #[no_display]
        parameter_def: Option<SyntaxId>,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Unexpected fields")]
    #[message(
        "Unexpected {}: {}",
        pluralize(unexpected_fields.len(), "field", "fields"),
        fmt_items(unexpected_fields, "and")
    )]
    #[custom_diagnostic(|builder|
        if let Some(def_id) = *def_id {
            builder.with_label(Label::new(def_id).with_message("Type is defined here"));
        }
     )]
    pub struct UnexpectedFields {
        #[no_display]
        def_id: Option<SyntaxId>,
        #[no_display]
        unexpected_fields: Vec<SmallString>,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Missing fields")]
    #[message(
        "Missing {}: {}",
        pluralize(missing_fields.len(), "field", "fields"),
        fmt_items(missing_fields, "and")
    )]
    #[custom_diagnostic(|builder|
        if let Some(def_id) = *def_id {
            builder.with_label(Label::new(def_id).with_message("Type is defined here"));
        }
     )]
    pub struct MissingFields {
        #[no_display]
        def_id: Option<SyntaxId>,
        #[no_display]
        missing_fields: Vec<SmallString>,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Could not infer type")]
    #[message("Could not infer the type of this expression")]
    #[help("Try annotating the expression: `let val: ExpectedType = ...`")]
    pub struct InferenceFailed;
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Invalid cast")]
    #[message("Can not cast from `{from}` to `{to}`")]
    pub struct InvalidCast {
        #[validate]
        from: Type,
        #[validate]
        to: Type,
    }
}

diag! {
    #[level(DiagnosticLevel::Error)]
    #[code("Circular type alias")]
    #[message("A type alias must not refer to itself")]
    #[label(
        Label::new(circular_type_alias)
            .with_message("Because it includes this type, which causes the circular reference")
    )]
    pub struct CircularTypeAlias {
        circular_type_alias: SyntaxId,
    }
}
