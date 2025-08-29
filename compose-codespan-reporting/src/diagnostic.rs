//! Diagnostic data structures.

#[cfg(feature = "serialization")]
use serde::{Deserialize, Serialize};
use std::ops::Range;
use std::string::ToString;

/// A severity level for diagnostic messages.
///
/// These are ordered in the following way:
///
/// ```rust
/// use compose_codespan_reporting::diagnostic::Severity;
///
/// assert!(Severity::Bug > Severity::Error);
/// assert!(Severity::Error > Severity::Warning);
/// assert!(Severity::Warning > Severity::Note);
/// assert!(Severity::Note > Severity::Help);
/// ```
#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub enum Severity {
    /// A help message.
    Help,
    /// A note.
    Note,
    /// A warning.
    Warning,
    /// An error.
    Error,
    /// An unexpected bug.
    Bug,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub enum LabelStyle {
    /// Labels that describe the primary cause of a diagnostic.
    Primary,
    /// Labels that provide additional context for a diagnostic.
    Secondary,
}

/// A label describing an underlined region of code associated with a diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Label<FileId> {
    /// The style of the label.
    pub style: LabelStyle,
    /// The file that we are labelling.
    pub file_id: FileId,
    /// The range in bytes we are going to include in the final snippet.
    pub range: Range<usize>,
    /// An optional message to provide some additional information for the
    /// underlined code. These should not include line breaks.
    pub message: String,
}

impl<FileId> Label<FileId> {
    /// Create a new label.
    pub fn new(
        style: LabelStyle,
        file_id: FileId,
        range: impl Into<Range<usize>>,
    ) -> Label<FileId> {
        Label {
            style,
            file_id,
            range: range.into(),
            message: String::new(),
        }
    }

    /// Create a new label with a style of [`LabelStyle::Primary`].
    ///
    /// [`LabelStyle::Primary`]: LabelStyle::Primary
    pub fn primary(file_id: FileId, range: impl Into<Range<usize>>) -> Label<FileId> {
        Label::new(LabelStyle::Primary, file_id, range)
    }

    /// Create a new label with a style of [`LabelStyle::Secondary`].
    ///
    /// [`LabelStyle::Secondary`]: LabelStyle::Secondary
    pub fn secondary(file_id: FileId, range: impl Into<Range<usize>>) -> Label<FileId> {
        Label::new(LabelStyle::Secondary, file_id, range)
    }

    /// Add a message to the diagnostic.
    pub fn with_message(mut self, message: impl ToString) -> Label<FileId> {
        self.message = message.to_string();
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Suggestion<FileId> {
    pub file_id: FileId,
    pub message: String,
    pub parts: Vec<SuggestionPart>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct SpannedNote<FileId> {
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<Label<FileId>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub enum Subdiagnostic<FileId> {
    Suggestion(Suggestion<FileId>),
    SpannedNote(SpannedNote<FileId>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct SuggestionPart {
    pub range: Range<usize>,
    pub replacement: String,
}

/// Represents a diagnostic message that can provide information like errors and
/// warnings to the user.
///
/// The position of a Diagnostic is considered to be the position of the [`Label`] that has the earliest starting position and has the highest style which appears in all the labels of the diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Diagnostic<FileId> {
    /// The overall severity of the diagnostic
    pub severity: Severity,
    /// An optional code that identifies this diagnostic.
    pub code: Option<String>,
    /// The main message associated with this diagnostic.
    ///
    /// These should not include line breaks, and in order support the 'short'
    /// diagnostic display mod, the message should be specific enough to make
    /// sense on its own, without additional context provided by labels and notes.
    pub message: String,
    /// Source labels that describe the cause of the diagnostic.
    /// The order of the labels inside the vector does not have any meaning.
    /// The labels are always arranged in the order they appear in the source code.
    pub labels: Vec<Label<FileId>>,
    /// Notes that are associated with the primary cause of the diagnostic.
    /// These can include line breaks for improved formatting.
    // FIXME: Notes could be replaced with a Subdiagnostic::Note without a Label, but that would
    // change lots of API. Probably deprecate for one version before that.
    pub notes: Vec<String>,
    /// A list of diagnostics that are "attached" to the main diagnostic. They are rendered below
    /// the main diagnostic but still as one "unit". For example, a note with one or more labels
    /// can be attached to a warning to give additional information that doesn't make sense to
    /// group with the labels.
    pub subdiagnostics: Vec<Subdiagnostic<FileId>>,
}

impl<FileId> Diagnostic<FileId> {
    /// Create a new diagnostic.
    pub fn new(severity: Severity) -> Self {
        Diagnostic {
            severity,
            code: None,
            message: String::new(),
            labels: Vec::new(),
            notes: Vec::new(),
            subdiagnostics: Vec::new(),
        }
    }

    /// Create a new diagnostic with a severity of [`Severity::Bug`].
    ///
    /// [`Severity::Bug`]: Severity::Bug
    pub fn bug() -> Self {
        Diagnostic::new(Severity::Bug)
    }

    /// Create a new diagnostic with a severity of [`Severity::Error`].
    ///
    /// [`Severity::Error`]: Severity::Error
    pub fn error() -> Self {
        Diagnostic::new(Severity::Error)
    }

    /// Create a new diagnostic with a severity of [`Severity::Warning`].
    ///
    /// [`Severity::Warning`]: Severity::Warning
    pub fn warning() -> Self {
        Diagnostic::new(Severity::Warning)
    }

    /// Create a new diagnostic with a severity of [`Severity::Note`].
    ///
    /// [`Severity::Note`]: Severity::Note
    pub fn note() -> Self {
        Diagnostic::new(Severity::Note)
    }

    /// Create a new diagnostic with a severity of [`Severity::Help`].
    ///
    /// [`Severity::Help`]: Severity::Help
    pub fn help() -> Self {
        Diagnostic::new(Severity::Help)
    }

    /// Set the error code of the diagnostic.
    pub fn with_code(mut self, code: impl ToString) -> Self {
        self.code = Some(code.to_string());
        self
    }

    /// Set the message of the diagnostic.
    pub fn with_message(mut self, message: impl ToString) -> Self {
        self.message = message.to_string();
        self
    }

    /// Add a label to the diagnostic.
    pub fn with_label(mut self, label: Label<FileId>) -> Self {
        self.labels.push(label);
        self
    }

    /// Add some labels to the diagnostic.
    pub fn with_labels(mut self, mut labels: Vec<Label<FileId>>) -> Self {
        self.labels.append(&mut labels);
        self
    }

    /// Add some labels from an iterator to the diagnostic.
    pub fn with_labels_iter<I>(mut self, labels: I) -> Self
    where
        I: IntoIterator<Item = Label<FileId>>,
    {
        self.labels.extend(labels);
        self
    }

    /// Add some notes to the diagnostic.
    pub fn with_notes(mut self, mut notes: Vec<String>) -> Self {
        self.notes.append(&mut notes);
        self
    }

    /// Add a note to the diagnostic.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Add notes from an iterator to the diagnostic.
    pub fn with_notes_iter<I, T>(mut self, notes: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        self.notes.extend(notes.into_iter().map(Into::into));
        self
    }

    /// Add some suggestions to the diagnostic.
    ///
    /// Convenience method for [Self::with_subdiagnostics]. Use that if you want to interleave
    /// suggestions with other [Subdiagnostic]s.
    pub fn with_suggestions(mut self, suggestions: Vec<Suggestion<FileId>>) -> Self {
        self.subdiagnostics
            .extend(suggestions.into_iter().map(Subdiagnostic::Suggestion));
        self
    }

    /// Add some spanned notes to the diagnostic.
    ///
    /// Convenience method for [Self::with_subdiagnostics]. Use that if you want to interleave
    /// spanned notes with other [Subdiagnostic]s.
    pub fn with_spanned_notes(mut self, spanned_notes: Vec<SpannedNote<FileId>>) -> Self {
        self.subdiagnostics
            .extend(spanned_notes.into_iter().map(Subdiagnostic::SpannedNote));
        self
    }

    /// Add some subdiagnostics to the diagnostic.
    pub fn with_subdiagnostics(
        mut self,
        mut subdiagnostics: Vec<Subdiagnostic<FileId>>,
    ) -> Diagnostic<FileId> {
        self.subdiagnostics.append(&mut subdiagnostics);
        self
    }
}
