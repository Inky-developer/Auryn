use std::{
    fmt::{Debug, Display},
    panic::Location,
    rc::Rc,
};

use crate::auryn::{
    air::typecheck::{
        bounds::{MaybeBounded, MaybeBoundedView},
        type_context::TypeContext,
        types::{Type, TypeView},
    },
    diagnostics::diagnostic_display::{
        DiagnosticCollectionDisplay, DiagnosticDisplay, DiagnosticLevel, DisplayOptions,
    },
    input_files::InputFiles,
    syntax_id::SyntaxId,
};

#[derive(Debug, Clone)]
pub struct DiagnosticContext<'a> {
    pub input_files: &'a InputFiles,
    pub ty_ctx: &'a TypeContext,
    pub options: DisplayOptions,
}

pub trait DiagnosticKind {
    fn code(&self) -> &'static str;

    fn level(&self) -> DiagnosticLevel;

    fn build(&self, builder: &mut DiagnosticDisplay, ctx: &DiagnosticContext);

    fn is_valid(&self, _ctx: &DiagnosticContext) -> bool {
        true
    }
}

#[derive(Clone)]
pub struct Diagnostic {
    pub kind: Rc<dyn DiagnosticKind>,
    pub syntax_id: SyntaxId,
    pub location: &'static Location<'static>,
}

impl Diagnostic {
    #[track_caller]
    pub fn new(syntax_id: SyntaxId, kind: impl DiagnosticKind + 'static) -> Self {
        Diagnostic {
            kind: Rc::new(kind),
            syntax_id,
            location: Location::caller(),
        }
    }
}

impl Debug for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Diagnostic")
            .field("syntax_id", &self.syntax_id)
            .field("kind", &self.kind.code())
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    #[track_caller]
    pub fn add(&mut self, syntax_id: SyntaxId, kind: impl DiagnosticKind + 'static) {
        self.diagnostics.push(Diagnostic::new(syntax_id, kind))
    }

    pub fn take(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn to_display<'a>(
        &'a self,
        input_files: &'a InputFiles,
        ty_ctx: &'a TypeContext,
        options: DisplayOptions,
    ) -> DiagnosticCollectionDisplay<'a> {
        let ctx = DiagnosticContext {
            input_files,
            ty_ctx,
            options,
        };
        let mut display = DiagnosticCollectionDisplay::new(&ctx);
        display.extend(
            self.diagnostics
                .iter()
                .filter(|it| it.kind.is_valid(&ctx))
                .map(|it| it.display(&ctx)),
        );
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
    pub fn display(&self, ctx: &DiagnosticContext) -> DiagnosticDisplay {
        let mut builder = DiagnosticDisplay::new(self.level(), self.syntax_id, self.kind.code());
        self.kind.build(&mut builder, ctx);

        if ctx.options.write_debug_info {
            builder.with_info(format!("This diagnostic was emmited at {}", self.location));
        }

        builder
    }

    pub fn level(&self) -> DiagnosticLevel {
        self.kind.level()
    }
}

/// Used for filtering out diagnostics which have invalid fields.
/// The main use case is filtering out type error diagnostics which include the Error type
pub trait DiagnosticValidator {
    fn is_valid(&self, ctx: &DiagnosticContext) -> bool;
}

impl DiagnosticValidator for Type {
    fn is_valid(&self, ctx: &DiagnosticContext) -> bool {
        !self.as_view(ctx.ty_ctx).is_erroneous()
    }
}

impl DiagnosticValidator for MaybeBounded {
    fn is_valid(&self, ctx: &DiagnosticContext) -> bool {
        self.as_type()
            .map(|ty| !ty.as_view(ctx.ty_ctx).is_erroneous())
            .unwrap_or(true)
    }
}

/// Used for converting into a representation more suited towards printing,
/// e.g. turning [`Type`] into [`TypeView`].
pub trait AsDiagnosticDisplay<'a> {
    type Output: 'a;

    fn as_display(&'a self, ctx: &'a DiagnosticContext) -> Self::Output;
}

impl<'a, T: Display + 'a> AsDiagnosticDisplay<'a> for T {
    type Output = &'a Self;

    fn as_display(&'a self, _: &'a DiagnosticContext) -> Self::Output {
        self
    }
}

impl<'a> AsDiagnosticDisplay<'a> for Type {
    type Output = TypeView<'a>;

    fn as_display(&'a self, ctx: &'a DiagnosticContext) -> Self::Output {
        self.as_view(ctx.ty_ctx)
    }
}

impl<'a> AsDiagnosticDisplay<'a> for MaybeBounded {
    type Output = MaybeBoundedView<'a>;

    fn as_display(&'a self, ctx: &'a DiagnosticContext) -> Self::Output {
        self.as_view(ctx.ty_ctx)
    }
}

impl<'a> AsDiagnosticDisplay<'a> for SyntaxId {
    type Output = SyntaxId;

    fn as_display(&'a self, _: &'a DiagnosticContext) -> Self::Output {
        *self
    }
}

/// Simple macro for creating diagnostics.
/// Non-trivial logic is not supported and needs to be implemented manually.
///
/// Supported attributes on the struct:
/// - `level(DiagnosticLevel)` (required) - specify the diagnostic level
/// - `code(str)` (required) - Specify a code for the diagnostic
/// - `message(format_str)` - Specify the main message for the diagnostic
/// - `info(format_str)` - Specify some info for the diagnostic
/// - `help(format_str)` - Specify some help for the diagnostic
/// - `label(Label)` - Add a spanned label to the diagnostic
/// - `custom_diagnostic(expr)` - Supply a custom closure which gets mutable access to the builder
///
/// Supported attributes on the struct fields:
/// - `validate` - Require this field to be valid or filter out this diagnostic.
///   A field is valid according to its implementation of `DiagnosticValidator`.
/// - `no_display` - Mark that this field should not be converted using `AsDiagnosticDisplay`.
///   Useful if some custom display logic is implement for the field.
///
/// # Examples
/// Find examples at `crate::auryn::diagnostics::errors`
#[macro_export]
macro_rules! diag {
    (
        $(#[$($meta:tt)*])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_attr:tt])?
                $field_name:ident: $field_type:ty
            ),* $(,)?
        }
    ) => {
        $vis struct $name {
            $(
                pub $field_name: $field_type
            ),*
        }

        diag! {
            impl $name with $($(#[$field_attr])? $field_name: $field_type),* {
                $(#[$($meta)*])*
            }
        }
    };
    (
        $(#[$($meta:tt)*])*
        $vis:vis struct $name:ident;
    ) => {
        $vis struct $name;

        diag! {
            impl $name with {
                $(#[$($meta)*])*
            }
        }
    };
    (
        impl $name:ident with $($(#[$field_attr:tt])? $field_name:ident: $field_type:ty),* {
            #[level($level:expr)]
            #[code($code:literal)]
            $(#[message($($message:tt)+)])?
            $(#[info($($info:tt)+)])?
            $(#[help($($help:tt)+)])?
            $(#[label($label:expr)])?
            $(#[custom_diagnostic($func:expr)])?
        }
    ) => {
        impl $crate::auryn::diagnostics::diagnostic::DiagnosticKind for $name {
            fn code(&self) -> &'static str {
                $code
            }

            fn level(&self) -> DiagnosticLevel {
                $level
            }

            fn build(
                &self,
                #[allow(unused)]
                builder: &mut $crate::auryn::diagnostics::diagnostic_display::DiagnosticDisplay,
                #[allow(unused)]
                ctx: &$crate::auryn::diagnostics::diagnostic::DiagnosticContext
            ) {
                let Self {$($field_name),*} = self;
                $(
                    let $field_name = diag!(as_display ctx $(#[$field_attr])? $field_name);
                )*

                $(builder.with_message(format!($($message)*));)?
                $(builder.with_info(format!($($info)*));)?
                $(builder.with_help(format!($($help)*));)?
                $(builder.with_label($label);)?
                $({
                    // Small hack so that rust can infer the type of the passed closure
                    fn get_handler<F, R>(handler: F) -> F
                        where F: FnOnce(&mut $crate::auryn::diagnostics::diagnostic_display::DiagnosticDisplay) -> R
                    {
                        handler
                    }
                    (get_handler($func))(builder);
                })?
            }

            #[allow(unused)]
            fn is_valid(&self, ctx: &$crate::auryn::diagnostics::diagnostic::DiagnosticContext) -> bool {
                $(
                  diag!(validate self ctx $(#[$field_attr])? $field_name);
                )*
                true
            }
        }
    };

    (validate $self:ident $ctx:ident #[validate] $field_name:ident) => {
        if !$crate::auryn::diagnostics::diagnostic::DiagnosticValidator::is_valid(&$self.$field_name, $ctx) {
            return false;
        }
    };
    (validate $self:ident $ctx:ident $(#[$other:tt])? $field_name:ident) => {};

    (as_display $ctx:ident #[no_display] $field_name:ident) => {
        $field_name
    };
    (as_display $ctx:ident $(#[$other:tt])? $field_name:ident) => {
        $crate::auryn::diagnostics::diagnostic::AsDiagnosticDisplay::as_display($field_name, $ctx)
    };
}
