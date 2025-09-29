use std::io::{self, Write};
use std::iter::Peekable;
use std::ops::{Range, RangeBounds};
use std::slice::SliceIndex;
use termcolor::{ColorSpec, WriteColor};

use crate::diagnostic::{LabelStyle, Severity, Suggestion};
use crate::files::{Error, Location};
use crate::term::{Chars, Config, Styles};

/// The 'location focus' of a source code snippet.
pub struct Locus {
    /// The user-facing name of the file.
    pub name: String,
    /// The location.
    pub location: Location,
}

/// Single-line label, with an optional message.
///
/// ```text
/// ^^^^^^^^^ blah blah
/// ```
pub type SingleLabel<'diagnostic> = (LabelStyle, Range<usize>, &'diagnostic str);

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum Priority {
    Low,
    Medium,
    High,
    Critical,
}

impl LabeledLine for SingleLabel<'_> {
    fn range(&self) -> &Range<usize> {
        &self.1
    }

    fn message(&self) -> Option<&str> {
        Some(self.2)
    }

    fn label_color<'s>(
        &self,
        styles: &'s Styles,
        severity: Severity,
    ) -> Option<(&'s ColorSpec, Priority)> {
        Some((
            styles.label(severity, self.0),
            match self.0 {
                LabelStyle::Primary => Priority::High,
                LabelStyle::Secondary => Priority::Low,
            },
        ))
    }

    fn source_color<'s>(&self, styles: &'s Styles, severity: Severity) -> Option<(&'s ColorSpec, Priority)> {
        if self.0 == LabelStyle::Primary {
            self.label_color(styles, severity)
        } else {
            None
        }
    }

    fn caret<'s>(&self, chars: &Chars) -> (char, Priority) {
        match self.0 {
            LabelStyle::Primary => (chars.single_primary_caret, Priority::High),
            LabelStyle::Secondary => (chars.single_secondary_caret, Priority::Low),
        }
    }

    fn replacement(&self) -> Option<&str> {
        None
    }
}

pub trait LabeledLine {
    fn range(&self) -> &Range<usize>;
    fn message(&self) -> Option<&str>;
    fn label_color<'s>(
        &self,
        styles: &'s Styles,
        severity: Severity,
    ) -> Option<(&'s ColorSpec, Priority)>;
    fn caret<'s>(&self, chars: &Chars) -> (char, Priority);
    /// A replacement for the source text
    fn replacement(&self) -> Option<&str>;

    fn source_color<'s>(&self, styles: &'s Styles, severity: Severity) -> Option<(&'s ColorSpec, Priority)>;
}

pub struct SuggestionLine<'d> {
    /// The range within the line
    pub(crate) range: Range<usize>,
    /// The text to replace the range with
    pub(crate) replacement: &'d str,
    /// an optional message to show
    pub(crate) message: Option<&'d str>,
}

impl LabeledLine for SuggestionLine<'_> {
    fn range(&self) -> &Range<usize> {
        &self.range
    }

    fn message(&self) -> Option<&str> {
        self.message
    }

    fn label_color<'s>(
        &self,
        styles: &'s Styles,
        _severity: Severity,
    ) -> Option<(&'s ColorSpec, Priority)> {
        let has_addition = !self.replacement.is_empty();
        let has_removal = self.range.end > self.range.start;

        let color = match (has_removal, has_addition) {
            (true, true) => &styles.suggest_replace,
            (false, true) => &styles.suggest_add,
            (true, false) => &styles.suggest_remove,
            (false, false) => return None,
        };

        Some((color, Priority::Critical))
    }

    fn caret<'s>(&self, chars: &Chars) -> (char, Priority) {
        let has_addition = !self.replacement.is_empty();
        let has_removal = self.range.end > self.range.start;

        let caret = match (has_removal, has_addition) {
            (true, true) => chars.suggest_replace,
            (false, true) => chars.suggest_add,
            (true, false) => chars.suggest_remove,
            (false, false) => return (' ', Priority::Low),
        };

        (caret, Priority::Critical)
    }

    fn replacement(&self) -> Option<&str> {
        Some(self.replacement)
    }

    fn source_color<'s>(&self, styles: &'s Styles, severity: Severity) -> Option<(&'s ColorSpec, Priority)> {
        self.label_color(styles, severity)
    }
}

pub enum LabeledLineEnum<'s> {
    Suggestion(SuggestionLine<'s>),
    SingleLabel(SingleLabel<'s>),
}

impl LabeledLine for LabeledLineEnum<'_> {
    fn range(&self) -> &Range<usize> {
        match self {
            LabeledLineEnum::Suggestion(line) => line.range(),
            LabeledLineEnum::SingleLabel(line) => line.range(),
        }
    }

    fn message(&self) -> Option<&str> {
        match self {
            LabeledLineEnum::Suggestion(line) => line.message(),
            LabeledLineEnum::SingleLabel(line) => line.message(),
        }
    }

    fn label_color<'s>(
        &self,
        styles: &'s Styles,
        severity: Severity,
    ) -> Option<(&'s ColorSpec, Priority)> {
        match self {
            LabeledLineEnum::Suggestion(line) => line.label_color(styles, severity),
            LabeledLineEnum::SingleLabel(line) => line.label_color(styles, severity),
        }
    }

    fn caret<'s>(&self, chars: &Chars) -> (char, Priority) {
        match self {
            LabeledLineEnum::Suggestion(line) => line.caret(chars),
            LabeledLineEnum::SingleLabel(line) => line.caret(chars),
        }
    }

    fn replacement(&self) -> Option<&str> {
        match self {
            LabeledLineEnum::Suggestion(line) => line.replacement(),
            LabeledLineEnum::SingleLabel(line) => line.replacement(),
        }
    }

    fn source_color<'s>(&self, styles: &'s Styles, severity: Severity) -> Option<(&'s ColorSpec, Priority)> {
        match self {
            LabeledLineEnum::Suggestion(line) => line.source_color(styles, severity),
            LabeledLineEnum::SingleLabel(line) => line.source_color(styles, severity),
        }
    }
}

/// A multi-line label to render.
///
/// Locations are relative to the start of where the source code is rendered.
#[derive(Clone)]
pub enum MultiLabel<'diagnostic> {
    /// Multi-line label top.
    /// The contained value indicates where the label starts.
    ///
    /// ```text
    /// ╭────────────^
    /// ```
    ///
    /// Can also be rendered at the beginning of the line
    /// if there is only whitespace before the label starts.
    ///
    /// /// ```text
    /// ╭
    /// ```
    Top(usize),
    /// Left vertical labels for multi-line labels.
    ///
    /// ```text
    /// │
    /// ```
    Left,
    /// Multi-line label bottom, with an optional message.
    /// The first value indicates where the label ends.
    ///
    /// ```text
    /// ╰────────────^ blah blah
    /// ```
    Bottom(usize, &'diagnostic str),
}

#[derive(Copy, Clone)]
enum VerticalBound {
    Top,
    Bottom,
}

type Underline = (LabelStyle, VerticalBound);

/// A renderer of display list entries.
///
/// The following diagram gives an overview of each of the parts of the renderer's output:
///
/// ```text
///                     ┌ outer gutter
///                     │ ┌ left border
///                     │ │ ┌ inner gutter
///                     │ │ │   ┌─────────────────────────── source ─────────────────────────────┐
///                     │ │ │   │                                                                │
///                  ┌────────────────────────────────────────────────────────────────────────────
///        header ── │ error[0001]: oh noes, a cupcake has occurred!
/// snippet start ── │    ┌─ test:9:0
/// snippet empty ── │    │
///  snippet line ── │  9 │   ╭ Cupcake ipsum dolor. Sit amet marshmallow topping cheesecake
///  snippet line ── │ 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
///                  │    │ ╭─│─────────^
/// snippet break ── │    · │ │
///  snippet line ── │ 33 │ │ │ Muffin danish chocolate soufflé pastry icing bonbon oat cake.
///  snippet line ── │ 34 │ │ │ Powder cake jujubes oat cake. Lemon drops tootsie roll marshmallow
///                  │    │ │ ╰─────────────────────────────^ blah blah
/// snippet break ── │    · │
///  snippet line ── │ 38 │ │   Brownie lemon drops chocolate jelly-o candy canes. Danish marzipan
///  snippet line ── │ 39 │ │   jujubes soufflé carrot cake marshmallow tiramisu caramels candy canes.
///                  │    │ │           ^^^^^^^^^^^^^^^^^^^ -------------------- blah blah
///                  │    │ │           │
///                  │    │ │           blah blah
///                  │    │ │           note: this is a note
///  snippet line ── │ 40 │ │   Fruitcake jelly-o danish toffee. Tootsie roll pastry cheesecake
///  snippet line ── │ 41 │ │   soufflé marzipan. Chocolate bar oat cake jujubes lollipop pastry
///  snippet line ── │ 42 │ │   cupcake. Candy canes cupcake toffee gingerbread candy canes muffin
///                  │    │ │                                ^^^^^^^^^^^^^^^^^^ blah blah
///                  │    │ ╰──────────^ blah blah
/// snippet break ── │    ·
///  snippet line ── │ 82 │     gingerbread toffee chupa chups chupa chups jelly-o cotton candy.
///                  │    │                 ^^^^^^                         ------- blah blah
/// snippet empty ── │    │
///  snippet note ── │    = blah blah
///  snippet note ── │    = blah blah blah
///                  │      blah blah
///  snippet note ── │    = blah blah blah
///                  │      blah blah
///         empty ── │
/// ```
///
/// Filler text from http://www.cupcakeipsum.com
pub struct Renderer<'writer, 'config> {
    writer: &'writer mut dyn WriteColor,
    config: &'config Config,
}

impl<'writer, 'config> Renderer<'writer, 'config> {
    /// Construct a renderer from the given writer and config.
    pub fn new(
        writer: &'writer mut dyn WriteColor,
        config: &'config Config,
    ) -> Renderer<'writer, 'config> {
        Renderer { writer, config }
    }

    fn chars(&self) -> &'config Chars {
        &self.config.chars
    }

    fn styles(&self) -> &'config Styles {
        &self.config.styles
    }

    /// Diagnostic header, with severity, code, and message.
    ///
    /// ```text
    /// error[E0001]: unexpected type in `+` application
    /// ```
    pub fn render_header(
        &mut self,
        locus: Option<&Locus>,
        severity: Severity,
        code: Option<&str>,
        message: &str,
    ) -> Result<(), Error> {
        // Write locus
        //
        // ```text
        // test:2:9:
        // ```
        if let Some(locus) = locus {
            self.snippet_locus(locus)?;
            write!(self, ": ")?;
        }

        // Write severity name
        //
        // ```text
        // error
        // ```
        self.set_color(self.styles().header(severity))?;
        match severity {
            Severity::Bug => write!(self, "bug")?,
            Severity::Error => write!(self, "error")?,
            Severity::Warning => write!(self, "warning")?,
            Severity::Help => write!(self, "help")?,
            Severity::Note => write!(self, "note")?,
        }

        // Write error code
        //
        // ```text
        // [E0001]
        // ```
        if let Some(code) = &code.filter(|code| !code.is_empty()) {
            write!(self, "[{}]", code)?;
        }

        // Write diagnostic message
        //
        // ```text
        // : unexpected type in `+` application
        // ```
        self.set_color(&self.styles().header_message)?;
        write!(self, ": {}", message)?;
        self.reset()?;

        writeln!(self)?;

        Ok(())
    }

    /// Empty line.
    pub fn render_empty(&mut self) -> Result<(), Error> {
        writeln!(self)?;
        Ok(())
    }

    /// Top left border and locus.
    ///
    /// ```text
    /// ┌─ test:2:9
    /// ```
    pub fn render_snippet_start(
        &mut self,
        outer_padding: usize,
        locus: &Locus,
    ) -> Result<(), Error> {
        self.outer_gutter(outer_padding)?;

        self.set_color(&self.styles().source_border)?;
        write!(self, "{}", self.chars().snippet_start)?;
        self.reset()?;

        write!(self, " ")?;
        self.snippet_locus(locus)?;

        writeln!(self)?;

        Ok(())
    }

    /// A line of source code.
    ///
    /// ```text
    /// 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
    ///    │ ╭─│─────────^
    /// ```
    pub fn render_snippet_source<Label: LabeledLine>(
        &mut self,
        outer_padding: usize,
        line_number: usize,
        source: &str,
        severity: Severity,
        single_labels: &[Label],
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), Error> {
        // Trim trailing newlines, linefeeds, and null chars from source, if they exist.
        // FIXME: Use the number of trimmed placeholders when rendering single line carets
        let source = source.trim_end_matches(['\n', '\r', '\0'].as_ref());

        // Write source line
        //
        // ```text
        // 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
        // ```
        {
            // Write outer gutter (with line number) and border
            self.outer_gutter_number(line_number, outer_padding)?;
            self.border_left()?;

            // Write inner gutter (with multi-line continuations on the left if necessary)
            let mut multi_labels_iter = multi_labels.iter().peekable();
            for label_column in 0..num_multi_labels {
                match multi_labels_iter.peek() {
                    Some((label_index, label_style, label)) if *label_index == label_column => {
                        match label {
                            MultiLabel::Top(start)
                                if *start <= source.len() - source.trim_start().len() =>
                            {
                                self.label_multi_top_left(severity, *label_style)?;
                            }
                            MultiLabel::Top(..) => self.inner_gutter_space()?,
                            MultiLabel::Left | MultiLabel::Bottom(..) => {
                                self.label_multi_left(severity, *label_style, None)?;
                            }
                        }
                        multi_labels_iter.next();
                    }
                    Some((_, _, _)) | None => self.inner_gutter_space()?,
                }
            }

            // Write source text
            write!(self, " ")?;
            let mut used_color = None;

            let mut chars =
                Self::char_metrics(self.config.tab_width, source.char_indices()).peekable();
            while let Some((metrics, ch)) = chars.peek().cloned() {
                let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());

                let color = single_labels
                    .iter()
                    .filter(|l| is_overlapping(l.range(), &column_range))
                    .flat_map(|l| l.source_color(&self.styles(), severity))
                    .chain(
                        multi_labels
                            .iter()
                            .filter(|(_, ls, label)| {
                                let overlaps = match label {
                                    MultiLabel::Top(start) => column_range.start >= *start,
                                    MultiLabel::Left => true,
                                    MultiLabel::Bottom(start, _) => column_range.end <= *start,
                                };
                                let is_primary = *ls == LabelStyle::Primary;
                                overlaps && is_primary
                            })
                            .map(|_| {
                                (
                                    self.styles().label(severity, LabelStyle::Primary),
                                    Priority::High,
                                )
                            }),
                    )
                    .max_by_key(|(_, prio)| *prio);

                if let Some((color, _)) = color
                    && used_color != Some(color)
                {
                    self.set_color(color)?;
                    used_color = Some(color);
                }
                if color.is_none() && used_color.is_some() {
                    self.reset()?;
                    used_color = None;
                }

                self.write_char_or_replacement(
                    ch,
                    metrics,
                    &mut chars,
                    single_labels,
                    |self_, ch, metrics| {
                        match ch {
                            '\t' => {
                                (0..metrics.unicode_width).try_for_each(|_| write!(self_, " "))?
                            }
                            _ => write!(self_, "{}", ch)?,
                        }
                        Ok(())
                    },
                    |self_, _label, replacement| {
                        self_.render_using_metrics(replacement)?;
                        Ok(())
                    },
                )?;
            }
            if used_color.is_some() {
                self.reset()?;
            }
            writeln!(self)?;
        }

        // Write single labels underneath source
        //
        // ```text
        //   │     - ---- ^^^ second mutable borrow occurs here
        //   │     │ │
        //   │     │ first mutable borrow occurs here
        //   │     first borrow later used by call
        //   │     help: some help here
        // ```
        if !single_labels.is_empty() {
            // Our plan is as follows:
            //
            // 1. Do an initial scan to find:
            //    - The number of non-empty messages.
            //    - The right-most start and end positions of labels.
            //    - A candidate for a trailing label (where the label's message
            //      is printed to the left of the caret).
            // 2. Check if the trailing label candidate overlaps another label -
            //    if so we print it underneath the carets with the other labels.
            // 3. Print a line of carets, and (possibly) the trailing message
            //    to the left.
            // 4. Print vertical lines pointing to the carets, and the messages
            //    for those carets.
            //
            // We try our best avoid introducing new dynamic allocations,
            // instead preferring to iterate over the labels multiple times. It
            // is unclear what the performance tradeoffs are however, so further
            // investigation may be required.

            // The number of non-empty messages to print.
            let mut num_messages = 0;
            // The right-most start position, eg:
            //
            // ```text
            // -^^^^---- ^^^^^^^
            //           │
            //           right-most start position
            // ```
            let mut max_label_start = 0;
            // The right-most end position, eg:
            //
            // ```text
            // -^^^^---- ^^^^^^^
            //                 │
            //                 right-most end position
            // ```
            let mut max_label_end = 0;
            // A trailing message, eg:
            //
            // ```text
            // ^^^ second mutable borrow occurs here
            // ```
            let mut trailing_label: Option<(usize, &Label)> = None;

            for (label_index, label) in single_labels.iter().enumerate() {
                let range = label.range();
                let message = label.message().unwrap_or_default();
                if !message.is_empty() {
                    num_messages += 1;
                }
                max_label_start = std::cmp::max(max_label_start, range.start);
                max_label_end = std::cmp::max(max_label_end, range.end);
                // This is a candidate for the trailing label, so let's record it.
                if range.end == max_label_end {
                    if message.is_empty() {
                        trailing_label = None;
                    } else {
                        trailing_label = Some((label_index, label));
                    }
                }
            }

            // any multi-line labels that started on this line do need the vertical left line
            // to be printed for any caret/labels below that line. Convert any multi-line top labels
            // to left labels here
            let multi_labels = multi_labels
                .iter()
                .map(|(idx, style, multilabel)| match multilabel {
                    // change any top labels to left labels if they start at the start of the line
                    MultiLabel::Top(start)
                        if *start <= source.len() - source.trim_start().len() =>
                    {
                        (*idx, *style, MultiLabel::Left)
                    }
                    other => (*idx, *style, other.clone()),
                })
                .collect::<Vec<_>>();

            if let Some((trailing_label_index, label)) = trailing_label {
                // Check to see if the trailing label candidate overlaps any of
                // the other labels on the current line.
                if single_labels
                    .iter()
                    .enumerate()
                    .filter(|(label_idx, _)| *label_idx != trailing_label_index)
                    .any(|(_, l)| is_overlapping(l.range(), label.range()))
                {
                    // If it does, we'll instead want to render it below the
                    // carets along with the other hanging labels.
                    trailing_label = None;
                }
            }

            // Write a line of carets
            //
            // ```text
            //   │ ^^^^^^  -------^^^^^^^^^-------^^^^^----- ^^^^ trailing label message
            // ```
            self.outer_gutter(outer_padding)?;
            self.border_left()?;
            self.inner_gutter(severity, num_multi_labels, &multi_labels)?;
            write!(self, " ")?;

            let mut previous_color = None;
            let placeholder_metrics = Metrics {
                byte_index: source.len(),
                unicode_width: 1,
            };

            let mut chars = Self::char_metrics(self.config.tab_width, source.char_indices())
                // Add a placeholder source column at the end to allow for
                // printing carets at the end of lines, eg:
                //
                // ```text
                // 1 │ Hello world!
                //   │             ^
                // ```
                .chain(std::iter::once((placeholder_metrics, '\0')))
                .peekable();

            while let Some((metrics, ch)) = chars.peek().cloned() {
                // Find the current label style at this column
                let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());

                let caret = single_labels
                    .iter()
                    .filter(|l|
                        is_overlapping(l.range(), &column_range)
                    )
                    .map(|l| l.caret(self.chars()))
                    .max_by_key(|(_, prio)| *prio)
                    .map(|(caret, _)| caret)
                    .unwrap_or(' ');

                let color = single_labels
                    .iter()
                    .filter(|l| is_overlapping(l.range(), &column_range))
                    .filter_map(|l| l.label_color(self.styles(), severity))
                    .max_by_key(|(_, prio)| *prio)
                    .map(|(caret, _)| caret);

                // Update writer style if necessary
                if previous_color != color {
                    if let Some(color) = color {
                        self.set_color(color)?;
                    } else {
                        self.reset()?;
                    }
                    previous_color = color;
                }

                if color.is_none() && column_range.end > max_label_end {
                    break;
                }

                self.write_char_or_replacement(
                    ch,
                    metrics.clone(),
                    &mut chars,
                    single_labels,
                    |self_, _ch, metrics| {
                        (0..metrics.unicode_width).try_for_each(|_| write!(self_, "{}", caret))?;
                        Ok(())
                    },
                    |self_, _label, replacement| {
                        self_.render_part_as_replacement_using_metrics(replacement, .., caret)?;
                        Ok(())
                    },
                )?;
            }
            // Reset style if it was previously set
            if previous_color.is_some() {
                self.reset()?;
            }

            // Write first trailing label message
            if let Some((_, label)) = trailing_label {
                for (line_no, line) in label.message().unwrap_or_default().lines().enumerate() {
                    if line_no == 0 {
                        write!(self, " ")?;
                    } else {
                        // for subsequent lines we need to redraw gutters and start drawing
                        // carets for non-trailing labels.
                        self.outer_gutter(outer_padding)?;
                        self.border_left()?;
                        self.inner_gutter(severity, num_multi_labels, &multi_labels)?;
                        write!(self, " ")?;
                        self.caret_pointers(
                            severity,
                            max_label_start,
                            single_labels,
                            trailing_label,
                            source.char_indices(),
                        )?;
                        self.render_part_as_replacement_using_metrics(
                            source,
                            label.range().clone(),
                            " ",
                        )?;
                        write!(self, "  ")?;
                    }
                    if let Some((color, _)) = label.label_color(self.styles(), severity) {
                        self.set_color(color)?;
                    }
                    write!(self, "{}", line)?;
                    self.reset()?;
                    writeln!(self)?;
                }

                if label.message().unwrap_or_default().is_empty() {
                    writeln!(self)?;
                }
            } else {
                writeln!(self)?;
            }

            // Write hanging labels pointing to carets
            //
            // ```text
            //   │     │ │
            //   │     │ first mutable borrow occurs here
            //   │     first borrow later used by call
            //   │     help: some help here
            // ```
            if num_messages > trailing_label.iter().count() {
                // Write first set of vertical lines before hanging labels
                //
                // ```text
                //   │     │ │
                // ```

                self.outer_gutter(outer_padding)?;
                self.border_left()?;
                self.inner_gutter(severity, num_multi_labels, &multi_labels)?;
                write!(self, " ")?;
                self.caret_pointers(
                    severity,
                    max_label_start,
                    single_labels,
                    trailing_label,
                    source.char_indices(),
                )?;
                writeln!(self)?;

                // Write hanging labels pointing to carets
                //
                // ```text
                //   │     │ first mutable borrow occurs here
                //   │     first borrow later used by call
                //   │     help: some help here
                // ```
                for label in hanging_labels(single_labels, trailing_label).rev() {
                    for (line_no, line) in label.message().unwrap_or_default().lines().enumerate() {
                        self.outer_gutter(outer_padding)?;
                        self.border_left()?;
                        self.inner_gutter(severity, num_multi_labels, &multi_labels)?;
                        write!(self, " ")?;
                        self.caret_pointers(
                            severity,
                            max_label_start,
                            single_labels,
                            trailing_label,
                            source
                                .char_indices()
                                .take_while(|(byte_index, _)| *byte_index < label.range().start),
                        )?;
                        if let Some((color, _)) = label.label_color(self.styles(), severity) {
                            self.set_color(color)?;
                        }
                        if line_no > 0 {
                            write!(self, "  ")?;
                        }
                        write!(self, "{}", line)?;
                        self.reset()?;
                        writeln!(self)?;
                    }
                }
            }
        }

        // Write top or bottom label carets underneath source
        //
        // ```text
        //     │ ╰───│──────────────────^ woops
        //     │   ╭─│─────────^
        // ```
        for (multi_label_index, (_, label_style, label)) in multi_labels.iter().enumerate() {
            let (label_style, range, bottom_message) = match label {
                MultiLabel::Left => continue, // no label caret needed
                // no label caret needed if this can be started in front of the line
                MultiLabel::Top(start) if *start <= source.len() - source.trim_start().len() => {
                    continue;
                }
                MultiLabel::Top(range) => (*label_style, range, None),
                MultiLabel::Bottom(range, message) => (*label_style, range, Some(message)),
            };

            // Finish the top or bottom caret
            match bottom_message {
                None => {
                    self.outer_gutter(outer_padding)?;
                    self.border_left()?;
                    self.multi_label_inner_gutter(
                        severity,
                        num_multi_labels,
                        multi_labels,
                        multi_label_index,
                        label_style,
                        false,
                    )?;
                    self.label_multi_top_caret(severity, label_style, source, *range)?
                }
                Some(message) => {
                    if message.is_empty() {
                        self.outer_gutter(outer_padding)?;
                        self.border_left()?;
                        self.multi_label_inner_gutter(
                            severity,
                            num_multi_labels,
                            multi_labels,
                            multi_label_index,
                            label_style,
                            false,
                        )?;
                        self.label_multi_bottom_caret(
                            severity,
                            label_style,
                            source,
                            *range,
                            message,
                        )?
                    }
                    for (line_no, line) in message.lines().enumerate() {
                        self.outer_gutter(outer_padding)?;
                        self.border_left()?;
                        if line_no == 0 {
                            self.multi_label_inner_gutter(
                                severity,
                                num_multi_labels,
                                multi_labels,
                                multi_label_index,
                                label_style,
                                false,
                            )?;
                            self.label_multi_bottom_caret(
                                severity,
                                label_style,
                                source,
                                *range,
                                line,
                            )?
                        } else {
                            self.multi_label_inner_gutter(
                                severity,
                                num_multi_labels,
                                multi_labels,
                                multi_label_index,
                                label_style,
                                true,
                            )?;
                            self.render_part_as_replacement_using_metrics(source, 0..*range, " ")?;
                            self.set_color(self.styles().label(severity, label_style))?;
                            write!(self, "    ")?;
                            write!(self, "{}", line)?;
                            self.reset()?;
                            writeln!(self)?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn write_char_or_replacement<Label: LabeledLine>(
        &mut self,
        peeked_char: char,
        peeked_metrics: Metrics,
        chars: &mut Peekable<impl Iterator<Item = (Metrics, char)>>,
        single_labels: &[Label],
        write_char: impl FnOnce(&mut Self, char, &Metrics) -> Result<(), Error>,
        write_replacement: impl FnOnce(&mut Self, &Label, &str) -> Result<(), Error>,
    ) -> Result<(), Error> {
        let replacement = single_labels.iter().find(|l| {
            l.range().start == peeked_metrics.byte_index
                && l.replacement().is_some_and(|r| !r.is_empty())
        });

        if let Some(label) = replacement {
            write_replacement(self, label, label.replacement().unwrap())?;

            if label.range().end == label.range().start {
                // if just an insertion, we also still need to write the char
                write_char(self, peeked_char, &peeked_metrics)?;
                chars.next();
            }

            // consume all chars within the range of the replacement;
            while let Some((peeked_metrics, _)) = chars.peek() {
                if peeked_metrics.byte_index >= label.range().end {
                    break;
                }
                chars.next();
            }
        } else {
            write_char(self, peeked_char, &peeked_metrics)?;
            chars.next();
        }


        Ok(())
    }

    fn multi_label_inner_gutter(
        &mut self,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel)],
        multi_label_index: usize,
        label_style: LabelStyle,
        newline_continuation: bool,
    ) -> Result<(), Error> {
        // Write inner gutter.
        //
        // ```text
        //  │ ╭─│───│
        // ```
        let mut underline = None;
        let mut multi_labels_iter = multi_labels.iter().enumerate().peekable();
        for label_column in 0..num_multi_labels {
            match multi_labels_iter.peek() {
                Some((i, (label_index, ls, label))) if *label_index == label_column => {
                    match label {
                        MultiLabel::Left => {
                            self.label_multi_left(severity, *ls, underline.map(|(s, _)| s))?;
                        }
                        MultiLabel::Top(..) if multi_label_index > *i => {
                            self.label_multi_left(severity, *ls, underline.map(|(s, _)| s))?;
                        }
                        MultiLabel::Bottom(..) if multi_label_index < *i => {
                            self.label_multi_left(severity, *ls, underline.map(|(s, _)| s))?;
                        }
                        MultiLabel::Top(..) if multi_label_index == *i => {
                            underline = Some((*ls, VerticalBound::Top));
                            self.label_multi_top_left(severity, label_style)?
                        }
                        MultiLabel::Bottom(..)
                            if multi_label_index == *i && !newline_continuation =>
                        {
                            underline = Some((*ls, VerticalBound::Bottom));
                            self.label_multi_bottom_left(severity, label_style)?;
                        }
                        MultiLabel::Top(..) | MultiLabel::Bottom(..) => {
                            self.inner_gutter_column(severity, underline)?;
                        }
                    }
                    multi_labels_iter.next();
                }
                Some((_, _)) | None => self.inner_gutter_column(severity, underline)?,
            }
        }
        Ok(())
    }

    /// An empty source line, for providing additional whitespace to source snippets.
    ///
    /// ```text
    /// │ │ │
    /// ```
    pub fn render_snippet_empty(
        &mut self,
        outer_padding: usize,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), Error> {
        self.outer_gutter(outer_padding)?;
        self.border_left()?;
        self.inner_gutter(severity, num_multi_labels, multi_labels)?;
        writeln!(self)?;
        Ok(())
    }

    /// A broken source line, for labeling skipped sections of source.
    ///
    /// ```text
    /// · │ │
    /// ```
    pub fn render_snippet_break(
        &mut self,
        outer_padding: usize,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), Error> {
        self.outer_gutter(outer_padding)?;
        self.border_left_break()?;
        self.inner_gutter(severity, num_multi_labels, multi_labels)?;
        writeln!(self)?;
        Ok(())
    }

    /// Just the gutter, for spacing between sections.
    ///
    /// ```text
    /// |
    /// ```
    pub fn render_gutter_line(&mut self, outer_padding: usize) -> Result<(), Error> {
        self.outer_gutter(outer_padding)?;
        self.border_left()?;
        self.render_empty()
    }

    /// Additional notes.
    ///
    /// ```text
    /// = expected type `Int`
    ///      found type `String`
    /// ```
    pub fn render_snippet_note(
        &mut self,
        outer_padding: usize,
        message: &str,
    ) -> Result<(), Error> {
        self.render_snippet_note_with_color(outer_padding, message, None)
    }

    /// The actual `render_snippet_note` but we can set the color of the message text.
    fn render_snippet_note_with_color(
        &mut self,
        outer_padding: usize,
        message: &str,
        message_color: Option<&ColorSpec>,
    ) -> Result<(), Error> {
        for (note_line_index, line) in message.lines().enumerate() {
            self.outer_gutter(outer_padding)?;
            match note_line_index {
                0 => {
                    self.set_color(&self.styles().note_bullet)?;
                    write!(self, "{}", self.chars().note_bullet)?;
                    self.reset()?;
                }
                _ => write!(self, " ")?,
            }
            // Write line of message
            if let Some(color) = message_color {
                self.set_color(color)?;
            }
            writeln!(self, " {}", line)?;
            if message_color.is_some() {
                self.reset()?;
            }
        }

        Ok(())
    }

    fn render_using_metrics(&mut self, s: &str) -> Result<(), Error> {
        for (metrics, ch) in Self::char_metrics(self.config.tab_width, s.char_indices()) {
            match ch {
                '\t' => (0..metrics.unicode_width).try_for_each(|_| write!(self, " "))?,
                _ => write!(self, "{}", ch)?,
            }
        }
        Ok(())
    }

    // FIXME: Ugly type
    fn render_part_using_metrics(
        &mut self,
        s: &str,
        range: impl RangeBounds<usize> + SliceIndex<str, Output = str> + Clone,
    ) -> Result<(), Error> {
        self.render_using_metrics(s.get(range.clone()).ok_or_else(|| Error::InvalidSlice {
            given: (range.start_bound().cloned(), range.end_bound().cloned()),
        })?)
    }

    // FIXME: Ugly type
    fn render_part_as_replacement_using_metrics(
        &mut self,
        s: &str,
        range: impl RangeBounds<usize> + SliceIndex<str, Output = str> + Clone,
        replacement: impl std::fmt::Display,
    ) -> Result<(), Error> {
        for (metrics, ch) in Self::char_metrics(
            self.config.tab_width,
            s.get(range.clone())
                .ok_or_else(|| Error::InvalidSlice {
                    given: (range.start_bound().cloned(), range.end_bound().cloned()),
                })?
                .char_indices(),
        ) {
            match ch {
                '\t' => {
                    (0..metrics.unicode_width).try_for_each(|_| write!(self, "{}", replacement))?
                }
                _ => write!(self, "{}", replacement)?,
            }
        }
        Ok(())
    }

    pub fn render_suggestion_header<FileId>(&mut self, suggestion: &Suggestion<FileId>, outer_padding: usize) -> Result<(), Error> {
        let (has_additions, has_removals) = suggestion.parts.iter().fold((false, false), |(a, r), p| {
            let has_addition = !p.replacement.is_empty();
            let has_removal = p.span.range.start != p.span.range.end;
            (a || has_addition, r || has_removal)
        });

        // Render message with different colors depending on if it's a removal,
        // addition or replacement (both removal and addition).
        let message_color = match (has_additions, has_removals) {
            (true, false) => &self.styles().suggest_add,
            (false, true) => &self.styles().suggest_remove,
            _ => &self.styles().suggest_replace,
        };
        self.render_snippet_note_with_color(outer_padding, &suggestion.message, Some(message_color))?;

        Ok(())
    }

    /// Suggestions for changes on a single line.
    ///
    /// Panics if:
    /// - The content spans multiple lines.
    /// - `parts` is empty.
    ///
    /// ```text
    ///   = help: consider borrowing here
    ///   |
    /// 2 | foo(&123);
    ///   |     +
    /// ```
    ///
    /// ```text
    ///   = help: consider removing the borrow
    ///   |
    /// 2 | foo(&123);
    ///   |     -
    /// ```
    ///
    /// ```text
    ///   = help: change this to `&[u32]`
    ///   |
    /// 2 | fn _foo(_: &[u32]) {}
    ///   |            ~~~~~~
    /// ```
    ///
    /// ```text
    ///   = help: arrays are initialized with `[]`
    ///   |
    /// 2 | let _numbers: [i32; 3] = [ 1, 2, 3 ];
    ///   |                          ~         ~
    /// ```
    ///
    /// ```text
    ///   = help: arrays are initialized with `[]`
    ///   |
    /// 2 | let _numbers: [i32; 3] = [ 1, 2, 3 ];
    ///   |               --------   ~         ~
    /// ```
    // - less newlines
    // - only show one "block" per suggestion
    pub fn render_suggestions(
        &mut self,
        outer_padding: usize,
        lines: Range<usize>,
        source: &str,
        parts: &[(Range<usize>, &str)],
        locus: &Locus,
        message: &str,
    ) -> Result<(), Error> {
        #[derive(PartialEq)]
        enum SuggestionType {
            Add,
            Remove,
            Replace,
        }

        impl SuggestionType {
            fn color_from<'s>(&'_ self, styles: &'s Styles) -> &'s ColorSpec {
                match self {
                    SuggestionType::Add => &styles.suggest_add,
                    SuggestionType::Remove => &styles.suggest_remove,
                    SuggestionType::Replace => &styles.suggest_replace,
                }
            }
        }

        assert_eq!(lines.start, lines.end);

        let mut parts: Vec<_> = parts
            .iter()
            .filter_map(|(range, replacement)| {
                let has_addition = !replacement.is_empty();
                let has_removal = range.start != range.end;
                let ty = match (has_addition, has_removal) {
                    (true, false) => SuggestionType::Add,
                    (false, true) => SuggestionType::Remove,
                    (true, true) => SuggestionType::Replace,
                    (false, false) => return None,
                };
                Some((ty, range, replacement))
            })
            .collect();

        assert!(!parts.is_empty());

        // Trim trailing newlines, linefeeds, and null chars from source, if they exist.
        let source = source.trim_end_matches(['\n', '\r', '\0'].as_ref());

        parts.sort_by_key(|(_, range, _)| range.start);

        // FIXME: verify that removals don't overlap
        // E.g. Iterator::scan with state (valid: bool, previous_end: usize)) and
        // Iterator::all(valid)

        let has_additions = parts.iter().any(|(ty, _, _)| ty == &SuggestionType::Add);
        let has_removals = parts.iter().any(|(ty, _, _)| ty == &SuggestionType::Remove);

        // Render message with different colors depending on if it's a removal,
        // addition or replacement (both removal and addition).
        let message_color = match (has_additions, has_removals) {
            (true, false) => Some(SuggestionType::Add.color_from(self.styles())),
            (false, true) => Some(SuggestionType::Remove.color_from(self.styles())),
            (_, _) => Some(SuggestionType::Replace.color_from(self.styles())),
        };
        self.render_snippet_note_with_color(outer_padding, message, message_color)?;

        self.render_snippet_start(outer_padding, locus)?;

        self.render_gutter_line(outer_padding)?;

        // Line number
        self.outer_gutter_number(lines.start, outer_padding)?;
        self.border_left()?;
        write!(self, " ")?;

        let mut cursor = 0;
        for (ty, remove, add) in &parts {
            self.render_part_using_metrics(source, cursor..remove.start)?;

            self.set_color(ty.color_from(self.styles()))?;
            match ty {
                SuggestionType::Add | SuggestionType::Replace => self.render_using_metrics(add)?,
                SuggestionType::Remove => {
                    self.render_part_using_metrics(source, remove.start..remove.end)?
                }
            }
            self.reset()?;

            cursor = remove.end;
        }

        // Render rest of original.
        self.render_part_using_metrics(source, cursor..)?;
        // ...and newline since the source doesn't contain it.
        self.render_empty()?;

        // Underline changes

        self.outer_gutter(outer_padding)?;
        self.border_left()?;
        write!(self, " ")?;

        let mut cursor = 0;
        for (ty, remove, add) in &parts {
            // Render spaces and indentation.
            self.render_part_as_replacement_using_metrics(source, cursor..remove.start, ' ')?;

            self.set_color(ty.color_from(self.styles()))?;
            match ty {
                SuggestionType::Add => {
                    self.render_part_as_replacement_using_metrics(add, .., '+')?
                }
                SuggestionType::Remove => self.render_part_as_replacement_using_metrics(
                    source,
                    remove.start..remove.end,
                    '-',
                )?,
                SuggestionType::Replace => {
                    self.render_part_as_replacement_using_metrics(add, .., '~')?
                }
            }
            self.reset()?;

            cursor = remove.end;
        }

        // Newline since the source doesn't contain it.
        self.render_empty()?;

        Ok(())
    }

    /// Adds tab-stop aware unicode-width computations to an iterator over
    /// character indices. Assumes that the character indices begin at the start
    /// of the line.
    fn char_metrics(
        tab_width: usize,
        char_indices: impl Iterator<Item = (usize, char)>,
    ) -> impl Iterator<Item = (Metrics, char)> {
        use unicode_width::UnicodeWidthChar;

        let mut unicode_column = 0;

        char_indices.map(move |(byte_index, ch)| {
            let metrics = Metrics {
                byte_index,
                unicode_width: match (ch, tab_width) {
                    ('\t', 0) => 0, // Guard divide-by-zero
                    ('\t', _) => tab_width - (unicode_column % tab_width),
                    (ch, _) => ch.width().unwrap_or(0),
                },
            };
            unicode_column += metrics.unicode_width;

            (metrics, ch)
        })
    }

    /// Location focus.
    fn snippet_locus(&mut self, locus: &Locus) -> Result<(), Error> {
        write!(
            self,
            "{name}:{line_number}:{column_number}",
            name = locus.name,
            line_number = locus.location.line_number,
            column_number = locus.location.column_number,
        )?;
        Ok(())
    }

    /// The outer gutter of a source line.
    fn outer_gutter(&mut self, outer_padding: usize) -> Result<(), Error> {
        write!(self, "{space: >width$} ", space = "", width = outer_padding)?;
        Ok(())
    }

    /// The outer gutter of a source line, with line number.
    fn outer_gutter_number(
        &mut self,
        line_number: usize,
        outer_padding: usize,
    ) -> Result<(), Error> {
        self.set_color(&self.styles().line_number)?;
        write!(
            self,
            "{line_number: >width$}",
            line_number = line_number,
            width = outer_padding,
        )?;
        self.reset()?;
        write!(self, " ")?;
        Ok(())
    }

    /// The left-hand border of a source line.
    fn border_left(&mut self) -> Result<(), Error> {
        self.set_color(&self.styles().source_border)?;
        write!(self, "{}", self.chars().source_border_left)?;
        self.reset()?;
        Ok(())
    }

    /// The broken left-hand border of a source line.
    fn border_left_break(&mut self) -> Result<(), Error> {
        self.set_color(&self.styles().source_border)?;
        write!(self, "{}", self.chars().source_border_left_break)?;
        self.reset()?;
        Ok(())
    }

    /// Write vertical lines pointing to carets.
    fn caret_pointers<Label: LabeledLine>(
        &mut self,
        severity: Severity,
        max_label_start: usize,
        single_labels: &[Label],
        trailing_label: Option<(usize, &Label)>,
        char_indices: impl Iterator<Item = (usize, char)>,
    ) -> Result<(), Error> {
        let mut chars = Self::char_metrics(self.config.tab_width, char_indices).peekable();
        while let Some((metrics, ch)) = chars.peek().cloned() {
            let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());

            if metrics.byte_index > max_label_start {
                break;
            }

            let color = hanging_labels(single_labels, trailing_label)
                .filter(|label| column_range.contains(&label.range().start))
                .flat_map(|label| label.label_color(self.styles(), severity))
                .max_by_key(|(_, priority)| *priority)
                .map(|(color, _)| color);

            self.write_char_or_replacement(
                ch,
                metrics,
                &mut chars,
                single_labels,
                |self_, _ch, metrics| {
                    let mut spaces = match color {
                        None => 0..metrics.unicode_width,
                        Some(color) => {
                            self_.set_color(color)?;
                            write!(self_, "{}", self_.chars().pointer_left)?;
                            self_.reset()?;
                            1..metrics.unicode_width
                        }
                    };
                    // Only print padding if we are before the end of the last single line caret
                    if metrics.byte_index <= max_label_start {
                        spaces.try_for_each(|_| write!(self_, " "))?;
                    }
                    Ok(())
                },
                |self_, _label, replacement| {
                    // write the padding for the replacement. The first char should be the caret, the rest should be spacing
                    let mut chars =
                        Self::char_metrics(self_.config.tab_width, replacement.char_indices());
                    if let Some((metrics, _ch)) = chars.next() {
                        if let Some(color) = color {
                            self_.set_color(color)?;
                        }
                        write!(self_, "{}", self_.chars().pointer_left)?;
                        if let Some(_) = color {
                            self_.reset()?;
                        }
                        // Only print padding if we are before the end of the last single line caret
                        if metrics.byte_index <= max_label_start {
                            (1..metrics.unicode_width).try_for_each(|_| write!(self_, " "))?;
                        }
                    }
                    // Only print padding if we are before the end of the last single line caret
                    for (metrics, _ch) in chars.take_while(|(m, _)| m.byte_index <= max_label_start)
                    {
                        (0..metrics.unicode_width).try_for_each(|_| write!(self_, " "))?;
                    }
                    Ok(())
                },
            )?;
        }

        Ok(())
    }

    /// The left of a multi-line label.
    ///
    /// ```text
    ///  │
    /// ```
    fn label_multi_left(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
        underline: Option<LabelStyle>,
    ) -> Result<(), Error> {
        match underline {
            None => write!(self, " ")?,
            // Continue an underline horizontally
            Some(label_style) => {
                self.set_color(self.styles().label(severity, label_style))?;
                write!(self, "{}", self.chars().multi_top)?;
                self.reset()?;
            }
        }
        self.set_color(self.styles().label(severity, label_style))?;
        write!(self, "{}", self.chars().multi_left)?;
        self.reset()?;
        Ok(())
    }

    /// The top-left of a multi-line label.
    ///
    /// ```text
    ///  ╭
    /// ```
    fn label_multi_top_left(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
    ) -> Result<(), Error> {
        write!(self, " ")?;
        self.set_color(self.styles().label(severity, label_style))?;
        write!(self, "{}", self.chars().multi_top_left)?;
        self.reset()?;
        Ok(())
    }

    /// The bottom left of a multi-line label.
    ///
    /// ```text
    ///  ╰
    /// ```
    fn label_multi_bottom_left(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
    ) -> Result<(), Error> {
        write!(self, " ")?;
        self.set_color(self.styles().label(severity, label_style))?;
        write!(self, "{}", self.chars().multi_bottom_left)?;
        self.reset()?;
        Ok(())
    }

    /// Multi-line label top.
    ///
    /// ```text
    /// ─────────────^
    /// ```
    fn label_multi_top_caret(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
        source: &str,
        start: usize,
    ) -> Result<(), Error> {
        self.set_color(self.styles().label(severity, label_style))?;

        for (metrics, _) in Self::char_metrics(self.config.tab_width, source.char_indices())
            .take_while(|(metrics, _)| metrics.byte_index < start + 1)
        {
            // FIXME: improve rendering of carets between character boundaries
            (0..metrics.unicode_width)
                .try_for_each(|_| write!(self, "{}", self.chars().multi_top))?;
        }

        let caret_start = match label_style {
            LabelStyle::Primary => self.config.chars.multi_primary_caret_start,
            LabelStyle::Secondary => self.config.chars.multi_secondary_caret_start,
        };
        write!(self, "{}", caret_start)?;
        self.reset()?;
        writeln!(self)?;
        Ok(())
    }

    /// Multi-line label bottom, with a message.
    ///
    /// ```text
    /// ─────────────^ expected `Int` but found `String`
    /// ```
    fn label_multi_bottom_caret(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
        source: &str,
        start: usize,
        message: &str,
    ) -> Result<(), Error> {
        self.set_color(self.styles().label(severity, label_style))?;

        for (metrics, _) in Self::char_metrics(self.config.tab_width, source.char_indices())
            .take_while(|(metrics, _)| metrics.byte_index < start)
        {
            // FIXME: improve rendering of carets between character boundaries
            (0..metrics.unicode_width)
                .try_for_each(|_| write!(self, "{}", self.chars().multi_bottom))?;
        }

        let caret_end = match label_style {
            LabelStyle::Primary => self.config.chars.multi_primary_caret_start,
            LabelStyle::Secondary => self.config.chars.multi_secondary_caret_start,
        };
        write!(self, "{}", caret_end)?;
        if !message.is_empty() {
            write!(self, " {}", message)?;
        }
        self.reset()?;
        writeln!(self)?;
        Ok(())
    }

    /// Writes an empty gutter space, or continues an underline horizontally.
    fn inner_gutter_column(
        &mut self,
        severity: Severity,
        underline: Option<Underline>,
    ) -> Result<(), Error> {
        match underline {
            None => self.inner_gutter_space(),
            Some((label_style, vertical_bound)) => {
                self.set_color(self.styles().label(severity, label_style))?;
                let ch = match vertical_bound {
                    VerticalBound::Top => self.config.chars.multi_top,
                    VerticalBound::Bottom => self.config.chars.multi_bottom,
                };
                write!(self, "{0}{0}", ch)?;
                self.reset()?;
                Ok(())
            }
        }
    }

    /// Writes an empty gutter space.
    fn inner_gutter_space(&mut self) -> Result<(), Error> {
        write!(self, "  ")?;
        Ok(())
    }

    /// Writes an inner gutter, with the left lines if necessary.
    fn inner_gutter(
        &mut self,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), Error> {
        let mut multi_labels_iter = multi_labels.iter().peekable();
        for label_column in 0..num_multi_labels {
            match multi_labels_iter.peek() {
                Some((label_index, ls, label)) if *label_index == label_column => match label {
                    MultiLabel::Left | MultiLabel::Bottom(..) => {
                        self.label_multi_left(severity, *ls, None)?;
                        multi_labels_iter.next();
                    }
                    MultiLabel::Top(..) => {
                        self.inner_gutter_space()?;
                        multi_labels_iter.next();
                    }
                },
                Some((_, _, _)) | None => self.inner_gutter_space()?,
            }
        }

        Ok(())
    }
}

impl Write for Renderer<'_, '_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

impl WriteColor for Renderer<'_, '_> {
    fn supports_color(&self) -> bool {
        self.writer.supports_color()
    }

    fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
        self.writer.set_color(spec)
    }

    fn reset(&mut self) -> io::Result<()> {
        self.writer.reset()
    }

    fn is_synchronous(&self) -> bool {
        self.writer.is_synchronous()
    }
}

#[derive(Clone, Copy)]
struct Metrics {
    byte_index: usize,
    unicode_width: usize,
}

/// Check if two ranges overlap
fn is_overlapping(range0: &Range<usize>, range1: &Range<usize>) -> bool {
    let start = std::cmp::max(range0.start, range1.start);
    let end = std::cmp::min(range0.end, range1.end);
    start < end
}

/// For prioritizing primary labels over secondary labels when rendering carets.
fn label_priority_key(label_style: &LabelStyle) -> u8 {
    match label_style {
        LabelStyle::Secondary => 0,
        LabelStyle::Primary => 1,
    }
}

/// Return an iterator that yields the labels that require hanging messages
/// rendered underneath them.
fn hanging_labels<'labels, 'diagnostic, Label: LabeledLine>(
    single_labels: &'labels [Label],
    trailing_label: Option<(usize, &'labels Label)>,
) -> impl 'labels + DoubleEndedIterator<Item = &'labels Label> {
    single_labels
        .iter()
        .enumerate()
        .filter(|(_, label)| !label.message().unwrap_or_default().is_empty())
        .filter(move |(i, _)| trailing_label.map_or(true, |(j, _)| *i != j))
        .map(|(_, label)| label)
}
