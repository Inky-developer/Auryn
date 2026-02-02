use std::rc::Rc;
use std::{fmt::Debug, panic::Location};

use crate::auryn::air::typecheck::type_context::TypeContext;
use crate::auryn::{
    diagnostics::diagnostic_display::{
        DiagnosticCollectionDisplay, DiagnosticDisplay, DiagnosticLevel, DisplayOptions,
    },
    input_files::InputFiles,
    syntax_id::SyntaxId,
};

/// Simple macro for creating diagnostics.
/// Non-trivial logic is not supported and needs to be implemented manually.
#[macro_export]
macro_rules! diag {
    (
        $(#[$($meta:tt)*])*
        $vis:vis struct $name:ident {
            $(
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
            impl $name with $($field_name: $field_type),* {
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
        impl $name:ident with $($field_name:ident: $field_type:ty),* {
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
                _ctx: &$crate::auryn::diagnostics::diagnostic::DiagnosticContext
            ) {
                let Self {$($field_name),*} = self;

                $(builder.with_message(format!($($message)*));)?
                $(builder.with_info(format!($($info)*));)?
                $(builder.with_help(format!($($help)*));)?
                $(builder.with_label($label);)?
                $(
                    {
                        // Small hack so that rust can infer the type of the passed closure
                        fn get_handler<F, R>(handler: F) -> F
                            where F: FnOnce(&mut $crate::auryn::diagnostics::diagnostic_display::DiagnosticDisplay) -> R
                        {
                            handler
                        }
                        (get_handler($func))(builder);
                    }
                )?
            }
        }
    };
}

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

    fn is_valid(&self, _ctx: DiagnosticContext) -> bool {
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
        display.extend(self.diagnostics.iter().map(|it| it.display(&ctx)));
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
