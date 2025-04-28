use crate::world::World;
use compose_syntax::Span;
use ecow::{eco_vec, EcoString, EcoVec};
use std::fmt::{Display, Formatter};

pub type SourceResult<T> = Result<T, EcoVec<SourceDiagnostic>>;
pub type StrResult<T> = Result<T, EcoString>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SourceDiagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: EcoString,
    pub trace: EcoVec<Spanned<TracePoint>>,
    pub hints: EcoVec<EcoString>,
    pub notes: EcoVec<EcoString>,
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
            trace: eco_vec!(),
            hints: eco_vec!(),
            notes: eco_vec!(),
        }
    }
    
    pub fn hint(&mut self, hint: impl Into<EcoString>)
    {
        self.hints.push(hint.into());
    }
    
    pub fn with_hint(mut self, hint: impl Into<EcoString>) -> Self {
        self.hint(hint);
        self
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Severity {
    Error,
    Warning,
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