use compose_syntax::Span;
use ecow::{eco_vec, EcoVec};
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::str::Utf8Error;
use std::string::FromUtf8Error;
use std::{fmt, io};

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
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(,)?
    ) => {
        return Err($crate::diag::error!(
            $fmt $(, $arg)*
            $(; hint: $hint $(, $hint_arg)*)*
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
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(,)?
    ) => {
        $crate::diag::SourceDiagnostic::error(
            $span,
            $crate::diag::eco_format!($fmt, $($arg),*),
        )  $(.with_hint($crate::diag::eco_format!($hint, $($hint_arg),*)))*
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
        $(; hint: $hint:literal $(, $hint_arg:expr)*)*
        $(,)? $(;)?
    ) => {
        $crate::diag::SourceDiagnostic::warning(
            $span,
            $crate::diag::eco_format!($fmt, $($arg),*),
        ) $(.with_hint($crate::diag::eco_format!($hint, $($hint_arg),*)))*
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

pub type SourceResult<T> = Result<T, EcoVec<SourceDiagnostic>>;
pub type StrResult<T> = Result<T, EcoString>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SourceDiagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: EcoString,
    pub label_message: Option<EcoString>,
    pub trace: EcoVec<Spanned<TracePoint>>,
    pub hints: EcoVec<EcoString>,
    pub labels: EcoVec<Label>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Label {
    pub span: Span,
    pub message: EcoString,
}

impl Label {
    pub fn new(span: Span, message: impl Into<EcoString>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
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
        }
    }
    
    pub fn with_label_message(mut self, message: impl Into<EcoString>) -> Self {
        self.label_message = Some(message.into());
        self
    }
    
    pub fn hint(&mut self, hint: impl Into<EcoString>)
    {
        self.hints.push(hint.into());
    }
    
    pub fn with_hint(mut self, hint: impl Into<EcoString>) -> Self {
        self.hint(hint);
        self
    }
    
    pub fn with_label(mut self, span: Span, message: impl Into<EcoString>) -> Self {
        self.labels.push(Label::new(span, message));
        self
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Severity {
    Error,
    Warning,
}

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
                            && error_range.end >= trace_range.end =>
                    {
                    }
                    _ => error.trace.push(Spanned::new(make_point(), error.span)),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TracePoint::Call(Some(name)) => {
                write!(f, "error occurred in this call to `{}`", name)
            }
            TracePoint::Call(None) => {
                write!(f, "error occurred in this call")
            }
            TracePoint::Import => {
                write!(f, "error occurred while trying to import")
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

pub trait At<T> {
    fn at(self, span: Span) -> SourceResult<T>;
}

impl<T, S> At<T> for Result<T, S>
where S: Into<EcoString>,
{
    fn at(self, span: Span) -> SourceResult<T> {
        self.map_err(|msg| {
            let err = SourceDiagnostic::error(span, msg);
            eco_vec!(err)
        })
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
            if err.to_string().contains("stream did not contain valid UTF-8") =>
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
            Self::NotSource => f.pad("not a typst source file"),
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
