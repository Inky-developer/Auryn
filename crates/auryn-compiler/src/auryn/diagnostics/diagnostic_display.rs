use std::{
    fmt::{Debug, Display},
    io::Write,
};

use stdx::default;

use crate::auryn::{
    diagnostics::diagnostic_display::implementation::InputFilesCache, file_id::FileId,
    input_files::InputFiles, syntax_id::SyntaxId,
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

#[derive(Debug, Clone)]
pub struct DisplayOptions {
    pub use_color: bool,
    pub write_debug_info: bool,
}

impl DisplayOptions {
    pub const FOR_TESTING: Self = Self {
        use_color: false,
        write_debug_info: false,
    };
    pub const DEFAULT: Self = Self {
        use_color: true,
        write_debug_info: cfg!(debug_assertions),
    };
}

impl Default for DisplayOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

pub struct DiagnosticCollectionDisplay<'a> {
    displays: Vec<DiagnosticDisplay>,
    opts: DisplayOptions,
    cache: implementation::InputFilesCache<'a>,
}

impl<'a> DiagnosticCollectionDisplay<'a> {
    pub fn new(input_files: &'a InputFiles, opts: DisplayOptions) -> Self {
        Self {
            cache: InputFilesCache::new(input_files),
            opts,
            displays: default(),
        }
    }

    pub fn add(&mut self, display: DiagnosticDisplay) {
        self.displays.push(display);
    }

    pub fn eprint(&self) {
        let stderr = std::io::stdout();
        let mut lock = stderr.lock();
        self.write(&mut lock).unwrap();
        lock.flush().unwrap();
    }

    pub fn write(&self, writer: impl Write) -> std::io::Result<()> {
        implementation::write(self, writer, self.opts.use_color)
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

impl Display for DiagnosticCollectionDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct IoWriteAdapter<'a, 'b>(&'a mut std::fmt::Formatter<'b>);

        impl std::io::Write for IoWriteAdapter<'_, '_> {
            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                let as_str = std::str::from_utf8(buf).expect("Should only print valid utf-8");
                self.0.write_str(as_str).map_err(std::io::Error::other)?;
                Ok(buf.len())
            }

            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        self.write(&mut IoWriteAdapter(f)).unwrap();
        Ok(())
    }
}

impl Debug for DiagnosticCollectionDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

mod implementation {
    use std::{fmt::Debug, io::Write};

    use ariadne::{Cache, Color, IndexType, Report, ReportKind, Source};
    use stdx::FastMap;

    use crate::auryn::{
        diagnostics::diagnostic_display::{
            ComputedSpan, DiagnosticCollectionDisplay, DiagnosticLevel, LabelKind,
        },
        file_id::FileId,
        input_files::InputFiles,
        syntax_id::SyntaxId,
    };

    // TODO: some syntax highlighting would be nice.
    // A quick & dirty solution would be to just run the tokenizer on the relevant lines and
    // assign a syntax highlighting color to each token, which we can tell ariadne by creating a label without message
    pub fn write(
        ctx: &DiagnosticCollectionDisplay,
        mut w: impl Write,
        enable_color: bool,
    ) -> std::io::Result<()> {
        let config = ariadne::Config::new()
            .with_color(enable_color)
            .with_index_type(IndexType::Byte);

        for (index, display) in ctx.displays.iter().enumerate() {
            let report = Report::build(
                get_kind(display.level),
                ctx.compute_span(display.main_label.id),
            )
            .with_config(config);
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
            .with_order(prio.try_into().unwrap_or(i32::MAX))
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
        data: FastMap<FileId, Source<&'a str>>,
    }

    impl<'a> InputFilesCache<'a> {
        pub fn new(input_files: &'a InputFiles) -> Self {
            Self {
                input_files,
                data: input_files
                    .iter()
                    .map(|(file_id, file)| (file_id, Source::from(file.source.as_ref())))
                    .collect(),
            }
        }

        pub fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
            self.input_files.compute_span(syntax_id)
        }
    }

    impl<'a> Cache<FileId> for &InputFilesCache<'a> {
        type Storage = &'a str;

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
