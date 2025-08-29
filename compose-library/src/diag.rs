pub use compose_codespan_reporting;
use compose_codespan_reporting::term::termcolor::WriteColor;
use compose_codespan_reporting::{diagnostic, term};
use compose_syntax::{
    FileId, Fix, FixDisplay, Label, LabelType, PatchEngine, Span, SyntaxError, SyntaxErrorSeverity,
};
use ecow::{EcoVec, eco_vec};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::str::Utf8Error;
use std::string::FromUtf8Error;
use std::{fmt, io};

pub fn write_diagnostics(
    world: &dyn World,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
    writer: &mut dyn WriteColor,
    config: &term::Config,
) -> Result<(), compose_codespan_reporting::files::Error> {
    for diag in warnings.iter().chain(errors) {
        let mut diagnostic = match diag.severity {
            Severity::Error => diagnostic::Diagnostic::error(),
            Severity::Warning => diagnostic::Diagnostic::warning(),
        }
        .with_message(diag.message.clone())
        .with_labels_iter(
            diag_label(diag)
                .into_iter()
                .chain(diag.labels.iter().flat_map(create_label)),
        );

        for fix in &diag.fixes {
            let Some(file_id) = fix.span.id() else {
                continue;
            };
            let mut engine = PatchEngine::new();
            let Ok(source) = world.source(file_id) else {
                continue;
            };
            let Some(snippet) = source.span_text(fix.span) else {
                continue;
            };
            let Some(offset) = fix.span.range().map(|r| r.start) else {
                continue;
            };

            engine.add_patches(fix.patches.iter().cloned());
            let Ok(patched) = engine.apply_all_with_offset(snippet, offset) else {
                continue;
            };

            let message = format!(
                "suggested fix: {}:\n`{}`",
                fix.message.trim_end_matches(':'),
                patched
            );
            match fix.display {
                FixDisplay::Inline { span } => diagnostic
                    .labels
                    .extend(create_label(&Label::secondary(span, message))),
                FixDisplay::Footer => {
                    diagnostic.notes.push(message);
                }
            }
        }

        diagnostic
            .notes
            .extend(diag.hints.iter().map(|h| format!("help: {h}")));
        diagnostic
            .notes
            .extend(diag.notes.iter().map(|n| format!("note: {n}")));

        if let Some(code) = &diag.code {
            diagnostic = diagnostic.with_code(code.code).with_note(eco_format!(
                "help: for more information about this error, try `compose explain {}`",
                code.code
            ))
        }

        term::emit(writer, config, &world, &diagnostic)?;

        for point in &diag.trace {
            let message = point.value.to_string();
            let help = diagnostic::Diagnostic::help()
                .with_message(message)
                .with_label(diagnostic::Label::primary(
                    point.span.id().unwrap(),
                    point.span.range().unwrap(),
                ));

            term::emit(writer, &config, &world, &help)?;
        }
    }

    Ok(())
}

fn diag_label(diag: &SourceDiagnostic) -> Option<diagnostic::Label<FileId>> {
    let id = diag.span.id()?;
    let range = diag.span.range()?;

    let mut label = diagnostic::Label::primary(id, range);
    if let Some(message) = diag.label_message.as_ref() {
        label = label.with_message(message);
    }

    Some(label)
}

fn create_label(label: &Label) -> Option<diagnostic::Label<FileId>> {
    let id = label.span.id()?;
    let range = label.span.range()?;
    let style = match label.ty {
        LabelType::Primary => diagnostic::LabelStyle::Primary,
        LabelType::Secondary => diagnostic::LabelStyle::Secondary,
    };

    Some(diagnostic::Label::new(style, id, range.clone()).with_message(&label.message))
}

/// Early-return with a [`StrResult`] or [`SourceResult`].
///
/// If called with just a string and format args, returns with a
/// `StrResult`. If called with a span, a string and format args, returns
/// a `SourceResult`.
///
/// You can also emit hints with the `; hint: "..."` syntax.
///
/// ```ignore
/// bail!("bailing with a {}", "string result");
/// bail!(span, "bailing with a {}", "source result");
/// bail!(
///     span, "bailing with a {}", "source result";
///     hint: "hint 1"
/// );
/// bail!(
///     span, "bailing with a {}", "source result";
///     hint: "hint 1";
///     hint: "hint 2";
/// );
/// ```
#[macro_export]
#[doc(hidden)]
macro_rules! __bail {
    // For bail!("just a {}", "string")
    (
        $fmt:literal $(, $arg:expr)*
        $(; label_message: $label_message:literal $(, $label_message_arg:expr)*)?
        $(; label: $label:expr)*
        $(; note: $note:literal $(, $note_arg:expr)*)*
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(; code: $code:expr)?
        $(,)?
    ) => {
        return Err($crate::diag::error!(
            $fmt $(, $arg)*
            $(; label_message: $label_message:literal $(, $label_message_arg:expr)*)?
            $(; label: $label:expr)*
            $(; note: $note:literal $(, $note_arg:expr)*)*
            $(; hint: $hint:literal $(, $hint_arg:expr)*)*
            $(; code: $code:expr)?
        ))
    };

    // For bail!(error!(..))
    ($error:expr) => {
        return Err(::ecow::eco_vec![$error])
    };

    // For bail(span, ...)
    ($($tts:tt)*) => {
        return Err(::ecow::eco_vec![$crate::diag::error!($($tts)*)])
    };
}

/// Construct an [`EcoString`], [`HintedString`] or [`SourceDiagnostic`] with
/// severity `Error`.
#[macro_export]
#[doc(hidden)]
macro_rules! __error {
    // For bail!("just a {}", "string").
    ($fmt:literal $(, $arg:expr)* $(,)?) => {
        $crate::diag::eco_format!($fmt, $($arg),*).into()
    };

    // For bail!("a hinted {}", "string"; hint: "some hint"; hint: "...")
    (
        $fmt:literal $(, $arg:expr)*
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(,)?
    ) => {
        $crate::diag::HintedString::new(
            $crate::diag::eco_format!($fmt, $($arg),*)
        ) $(.with_hint($crate::diag::eco_format!($hint, $($hint_arg),*)))*
    };

    // For bail!(span, ...)
    (
        $span:expr, $fmt:literal $(, $arg:expr)*
        $(; label_message: $label_message:literal $(, $label_message_arg:expr)*)?
        $(; label: $label:expr)*
        $(; fix: $fix:expr)*
        $(; note: $note:literal $(, $note_arg:expr)*)*
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(; code: $code:expr)?
        $(,)?
        $(;)?
    ) => {
        $crate::diag::SourceDiagnostic::error(
            $span,
            $crate::diag::eco_format!($fmt, $($arg),*),
        )  $(.with_hint($crate::diag::eco_format!($hint, $($hint_arg),*)))*
        $(.with_label_message($crate::diag::eco_format!($label_message, $($label_message_arg),*)))*
        $(.with_note($crate::diag::eco_format!($note, $($note_arg),*)))*
        $(.with_label($label))*
        $(.with_fix($fix))*
        $(.with_code($code))*

    };
}

/// Construct a [`SourceDiagnostic`] with severity `Warning`.
///
/// You can also emit hints with the `; hint: "..."` syntax.
///
/// ```ignore
/// warning!(span, "warning with a {}", "source result");
/// warning!(
///     span, "warning with a {}", "source result";
///     hint: "hint 1"
/// );
/// warning!(
///     span, "warning with a {}", "source result";
///     hint: "hint 1";
///     hint: "hint 2";
/// );
/// ```
#[macro_export]
#[doc(hidden)]
macro_rules! __warning {
    (
        $span:expr,
        $fmt:literal $(, $arg:expr)*
        $(; label_message: $label_message:literal $(, $label_message_arg:expr)*)?
        $(; label: $label:expr)*
        $(; note: $note:literal $(, $note_arg:expr)*)*
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(; code: $code:expr)?
        $(,)?
        $(;)?
    ) => {
        $crate::diag::SourceDiagnostic::warning(
            $span,
            $crate::diag::eco_format!($fmt, $($arg),*),
        ) $(.with_hint($crate::diag::eco_format!($hint, $($hint_arg),*)))*
        $(.with_label_message($crate::diag::eco_format!($label_message, $($label_message_arg),*)))*
        $(.with_note($crate::diag::eco_format!($note, $($note_arg),*)))*
        $(.with_label($label))*
        $(.with_code($code))*
    };
}

#[rustfmt::skip]
#[doc(inline)]
pub use {
    crate::__bail as bail,
    crate::__error as error,
    crate::__warning as warning,
    ecow::{eco_format, EcoString},
};
use compose_error_codes::ErrorCode;
use compose_library::World;

pub type SourceResult<T> = Result<T, EcoVec<SourceDiagnostic>>;
pub type StrResult<T> = Result<T, EcoString>;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceDiagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: EcoString,
    pub label_message: Option<EcoString>,
    pub trace: EcoVec<Spanned<TracePoint>>,
    pub hints: EcoVec<EcoString>,
    pub labels: EcoVec<Label>,
    pub notes: EcoVec<EcoString>,
    pub code: Option<&'static ErrorCode>,
    pub fixes: EcoVec<Fix>,
}

impl SourceDiagnostic {
    pub fn error<S>(span: Span, message: S) -> Self
    where
        S: Into<EcoString>,
    {
        Self {
            severity: Severity::Error,
            span,
            message: message.into(),
            label_message: None,
            trace: eco_vec!(),
            hints: eco_vec!(),
            labels: eco_vec!(),
            notes: eco_vec!(),
            code: None,
            fixes: eco_vec!(),
        }
    }

    pub fn warning(span: Span, message: impl Into<EcoString>) -> Self {
        Self {
            severity: Severity::Warning,
            span,
            message: message.into(),
            label_message: None,
            trace: eco_vec!(),
            hints: eco_vec!(),
            labels: eco_vec!(),
            notes: eco_vec!(),
            code: None,
            fixes: eco_vec!(),
        }
    }

    pub fn with_label_message(mut self, message: impl Into<EcoString>) -> Self {
        self.label_message = Some(message.into());
        self
    }

    pub fn code(&mut self, code: &'static ErrorCode) {
        self.code = Some(code);
    }

    pub fn with_code(mut self, code: &'static ErrorCode) -> Self {
        self.code(code);
        self
    }

    pub fn hint(&mut self, hint: impl Into<EcoString>) {
        self.hints.push(hint.into());
    }

    pub fn with_hint(mut self, hint: impl Into<EcoString>) -> Self {
        self.hint(hint);
        self
    }

    pub fn note(&mut self, note: impl Into<EcoString>) {
        self.notes.push(note.into());
    }

    pub fn with_note(mut self, note: impl Into<EcoString>) -> Self {
        self.note(note);
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn add_fix(&mut self, fix: Fix) {
        self.fixes.push(fix);
    }

    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.add_fix(fix);
        self
    }
}

impl From<SyntaxError> for SourceDiagnostic {
    fn from(error: SyntaxError) -> Self {
        Self {
            severity: Severity::from(error.severity),
            span: error.span,
            message: error.message,
            label_message: error.label_message,
            trace: eco_vec![],
            hints: error.hints,
            labels: error.labels,
            notes: error.notes,
            code: error.code,
            fixes: error.fixes,
        }
    }
}

pub trait IntoSourceDiagnostic {
    fn into_source_diagnostic(self, span: Span) -> SourceDiagnostic;
}

impl<S> IntoSourceDiagnostic for S
where
    S: Into<EcoString>,
{
    fn into_source_diagnostic(self, span: Span) -> SourceDiagnostic {
        SourceDiagnostic::error(span, self.into())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Severity {
    Error,
    Warning,
}

impl From<SyntaxErrorSeverity> for Severity {
    fn from(value: SyntaxErrorSeverity) -> Self {
        match value {
            SyntaxErrorSeverity::Error => Severity::Error,
            SyntaxErrorSeverity::Warning => Severity::Warning,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Warned<T> {
    pub value: T,
    pub warnings: EcoVec<SourceDiagnostic>,
}

impl<T> Warned<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            warnings: eco_vec!(),
        }
    }

    pub fn with_warning(mut self, warning: SourceDiagnostic) -> Self {
        self.warnings.push(warning);
        self
    }

    pub fn extend_warnings(&mut self, warnings: EcoVec<SourceDiagnostic>) {
        self.warnings.extend(warnings);
    }
    pub fn with_warnings(mut self, warnings: EcoVec<SourceDiagnostic>) -> Warned<T> {
        self.extend_warnings(warnings);
        self
    }
}

impl<T> Deref for Warned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Warned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub trait Trace<T> {
    fn trace<F>(self, make_point: F, span: Span) -> Self
    where
        F: Fn() -> TracePoint;
}

impl<T> Trace<T> for SourceResult<T> {
    fn trace<F>(self, make_point: F, span: Span) -> Self
    where
        F: Fn() -> TracePoint,
    {
        self.map_err(|mut errors| {
            let Some(trace_range) = span.range() else {
                return errors;
            };

            // Skip traces that are fully contained within the given span.
            for error in errors.make_mut().iter_mut() {
                match error.span.range() {
                    Some(error_range)
                        if error.span.id() == span.id()
                            && trace_range.start <= error_range.start
                            && error_range.end >= trace_range.end => {}
                    _ => error.trace.push(Spanned::new(make_point(), span)),
                }
            }
            errors
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TracePoint {
    Call(Option<EcoString>),
    Import,
}

impl Display for TracePoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TracePoint::Call(Some(name)) => {
                write!(f, "error occurred in this call to `{}`", name)
            }
            TracePoint::Call(None) => {
                write!(f, "error occurred in this call")
            }
            TracePoint::Import => {
                write!(f, "error occurred during this import")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span,
        }
    }
}

pub struct UnSpanned<T> {
    pub value: T,
}

impl<T> UnSpanned<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }
}

impl<T> From<T> for UnSpanned<T> {
    fn from(value: T) -> Self {
        Self { value }
    }
}

impl<T> At<T> for Result<T, UnSpanned<SourceDiagnostic>> {
    fn at(self, span: Span) -> SourceResult<T> {
        self.map_err(|mut err| {
            err.value.span = span;
            eco_vec![err.value]
        })
    }
}

pub trait At<T> {
    fn at(self, span: Span) -> SourceResult<T>;
}

impl<T, E> At<T> for Result<T, E>
where
    E: IntoSourceDiagnostic,
{
    fn at(self, span: Span) -> SourceResult<T> {
        self.map_err(|err| eco_vec![err.into_source_diagnostic(span)])
    }
}

/// A result type with a file-related error.
pub type FileResult<T> = Result<T, FileError>;

/// An error that occurred while trying to load of a file.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FileError {
    /// A file was not found at this path.
    NotFound(PathBuf),
    /// A file could not be accessed.
    AccessDenied,
    /// A directory was found, but a file was expected.
    IsDirectory,
    /// The file is not a Compose source file, but should have been.
    NotSource,
    /// The file was not valid UTF-8, but should have been.
    InvalidUtf8,
    /// Another error.
    ///
    /// The optional string can give more details, if available.
    Other(Option<EcoString>),
}

impl FileError {
    /// Create a file error from an I/O error.
    pub fn from_io(err: io::Error, path: &Path) -> Self {
        match err.kind() {
            io::ErrorKind::NotFound => Self::NotFound(path.into()),
            io::ErrorKind::PermissionDenied => Self::AccessDenied,
            io::ErrorKind::InvalidData
                if err
                    .to_string()
                    .contains("stream did not contain valid UTF-8") =>
            {
                Self::InvalidUtf8
            }
            _ => Self::Other(Some(eco_format!("{err}"))),
        }
    }
}

impl std::error::Error for FileError {}

impl Display for FileError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::NotFound(path) => {
                write!(f, "file not found (searched at {})", path.display())
            }
            Self::AccessDenied => f.pad("failed to load file (access denied)"),
            Self::IsDirectory => f.pad("failed to load file (is a directory)"),
            Self::NotSource => f.pad("not a compose source file"),
            Self::InvalidUtf8 => f.pad("file is not valid utf-8"),
            Self::Other(Some(err)) => write!(f, "failed to load file ({err})"),
            Self::Other(None) => f.pad("failed to load file"),
        }
    }
}

impl From<Utf8Error> for FileError {
    fn from(_: Utf8Error) -> Self {
        Self::InvalidUtf8
    }
}

impl From<FromUtf8Error> for FileError {
    fn from(_: FromUtf8Error) -> Self {
        Self::InvalidUtf8
    }
}

impl From<FileError> for EcoString {
    fn from(err: FileError) -> Self {
        eco_format!("{err}")
    }
}
