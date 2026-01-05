use std::io::Write;

use crate::{
    auryn::{
        diagnostic::InputFiles, diagnostic_display::implementation::InputFilesCache,
        file_id::FileId, syntax_id::SyntaxId,
    },
    utils::default,
};

#[derive(Debug, Clone, Copy)]
pub struct ComputedSpan {
    pub file_id: FileId,
    pub offset: u32,
    pub len: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticLevel {
    Error,
}

#[derive(Debug, Clone, Copy)]
pub enum LabelKind {
    Informative,
    ErrorSource,
}

#[derive(Debug)]
pub struct Label {
    id: SyntaxId,
    message: String,
    kind: LabelKind,
}

impl Label {
    pub fn new(id: SyntaxId) -> Self {
        Label {
            id,
            message: default(),
            kind: LabelKind::Informative,
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    pub fn with_kind(mut self, kind: LabelKind) -> Self {
        self.kind = kind;
        self
    }
}

#[derive(Debug)]
pub struct DiagnosticDisplay {
    code: &'static str,
    main_label: Label,
    labels: Vec<Label>,
    infos: Vec<String>,
    helps: Vec<String>,
    level: DiagnosticLevel,
}

impl DiagnosticDisplay {
    pub fn new(level: DiagnosticLevel, id: SyntaxId) -> Self {
        Self {
            main_label: Label {
                message: default(),
                id,
                kind: match level {
                    DiagnosticLevel::Error => LabelKind::ErrorSource,
                },
            },
            code: "",
            level,
            labels: default(),
            infos: default(),
            helps: default(),
        }
    }

    pub fn with_code(&mut self, code: &'static str) -> &mut Self {
        self.code = code;
        self
    }

    pub fn with_label(&mut self, label: Label) -> &mut Self {
        self.labels.push(label);
        self
    }

    pub fn with_message(&mut self, message: impl Into<String>) -> &mut Self {
        self.main_label.message = message.into();
        self
    }

    pub fn with_info(&mut self, info: impl Into<String>) -> &mut Self {
        self.infos.push(info.into());
        self
    }

    pub fn with_help(&mut self, help: impl Into<String>) -> &mut Self {
        self.helps.push(help.into());
        self
    }
}

#[derive(Debug)]
pub struct DiagnosticCollectionDisplay<'a> {
    displays: Vec<DiagnosticDisplay>,
    cache: implementation::InputFilesCache<'a>,
}

impl<'a> DiagnosticCollectionDisplay<'a> {
    pub fn new(input_files: &'a InputFiles) -> Self {
        Self {
            cache: InputFilesCache::new(input_files),
            displays: default(),
        }
    }

    pub fn add(&mut self, display: DiagnosticDisplay) {
        self.displays.push(display);
    }

    pub fn eprint(&self) {
        self.write(std::io::stderr()).unwrap();
    }

    pub fn write(&self, writer: impl Write) -> std::io::Result<()> {
        implementation::write(self, writer)
    }

    fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
        self.cache.compute_span(syntax_id)
    }
}

impl Extend<DiagnosticDisplay> for DiagnosticCollectionDisplay<'_> {
    fn extend<T: IntoIterator<Item = DiagnosticDisplay>>(&mut self, iter: T) {
        self.displays.extend(iter)
    }
}

mod implementation {
    use std::{fmt::Debug, io::Write};

    use ariadne::{Cache, Color, Report, ReportKind, Source};

    use crate::{
        auryn::{
            diagnostic::InputFiles,
            diagnostic_display::{
                ComputedSpan, DiagnosticCollectionDisplay, DiagnosticLevel, LabelKind,
            },
            file_id::FileId,
            syntax_id::SyntaxId,
        },
        utils::{fast_map::FastMap, small_string::SmallString},
    };

    pub fn write(ctx: &DiagnosticCollectionDisplay, mut w: impl Write) -> std::io::Result<()> {
        for (index, display) in ctx.displays.iter().enumerate() {
            let report = Report::build(
                get_kind(display.level),
                ctx.compute_span(display.main_label.id),
            );
            assert!(!display.code.is_empty(), "{display:?} should have a code");
            let report = report.with_message(display.code);

            let main_label = if display.main_label.message.is_empty() {
                get_label(
                    ctx,
                    &super::Label {
                        id: display.main_label.id,
                        message: "Here".to_string(),
                        kind: display.main_label.kind,
                    },
                )
            } else {
                get_label(ctx, &display.main_label)
            };
            let report = report.with_label(main_label);

            let mut report =
                report.with_labels(display.labels.iter().map(|label| get_label(ctx, label)));

            report.with_notes(&display.infos);
            report.with_helps(&display.helps);

            report.finish().write(&ctx.cache, &mut w)?;
            if index + 1 < ctx.displays.len() {
                writeln!(w)?;
            }
        }

        Ok(())
    }

    fn get_label(
        ctx: &DiagnosticCollectionDisplay,
        label: &super::Label,
    ) -> ariadne::Label<ComputedSpan> {
        let span = ctx.compute_span(label.id);
        let prio = span.offset;
        ariadne::Label::new(span)
            .with_message(&label.message)
            .with_color(get_color(label.kind))
            .with_order(-prio.try_into().unwrap_or(i32::MAX))
    }

    fn get_color(kind: LabelKind) -> Color {
        match kind {
            LabelKind::Informative => Color::BrightCyan,
            LabelKind::ErrorSource => Color::Red,
        }
    }

    fn get_kind(level: DiagnosticLevel) -> ReportKind<'static> {
        match level {
            DiagnosticLevel::Error => ReportKind::Error,
        }
    }

    #[derive(Debug)]
    pub(super) struct InputFilesCache<'a> {
        input_files: &'a InputFiles,
        data: FastMap<FileId, Source<&'a SmallString>>,
    }

    impl<'a> InputFilesCache<'a> {
        pub fn new(input_files: &'a InputFiles) -> Self {
            Self {
                input_files,
                data: input_files
                    .iter()
                    .map(|(file_id, file)| (file_id, Source::from(&file.source)))
                    .collect(),
            }
        }

        pub fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
            self.input_files.compute_span(syntax_id)
        }
    }

    impl<'a> Cache<FileId> for &InputFilesCache<'a> {
        type Storage = &'a SmallString;

        fn fetch(&mut self, id: &FileId) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
            Result::<_, &str>::Ok(&self.data[id])
        }

        fn display<'b>(&self, id: &'b FileId) -> Option<impl std::fmt::Display + 'b> {
            Some(self.input_files.get(*id).name.clone())
        }
    }

    impl ariadne::Span for ComputedSpan {
        type SourceId = FileId;

        fn source(&self) -> &Self::SourceId {
            &self.file_id
        }

        fn start(&self) -> usize {
            self.offset.try_into().unwrap()
        }

        fn end(&self) -> usize {
            (self.offset + self.len).try_into().unwrap()
        }
    }
}
