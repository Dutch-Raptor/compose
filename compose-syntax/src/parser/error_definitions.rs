use crate::kind::SyntaxKind;
use crate::parser::parser_impl::{Marker, Parser};
use crate::Label;
use ecow::eco_format;
use compose_error_codes::E0001_UNCLOSED_DELIMITER;

pub(super) fn err_unclosed_delim(
    p: &mut Parser,
    open_marker: Marker,
    expected_closing: SyntaxKind,
    opening: SyntaxKind,
) {
    let closing_delim_label;
    if p.current().is_closing_delimiter() {
        closing_delim_label = eco_format!(
            "unexpected closing delimiter `{}`",
            p.current().descriptive_name()
        );
    } else if p.end() {
        closing_delim_label = eco_format!(
            "expected a closing `{}` but reached the end of the file instead",
            expected_closing.descriptive_name()
        );
    } else {
        closing_delim_label = eco_format!(
            "expected closing `{}`, but found `{}` instead",
            expected_closing.descriptive_name(),
            p.current_text()
        );
    }
    let closing_span = p.current_span();
    p[open_marker]
        .convert_to_error("unclosed delimiter")
        .with_code(&E0001_UNCLOSED_DELIMITER)
        // label on the opening delimiter
        .with_label_message(eco_format!(
            "unclosed `{}` starts here",
            opening.descriptive_name()
        ))
        // label on (or near) the closing delimiter / EOF
        .with_label(Label::primary(closing_span, closing_delim_label))
        // trailing note
        .with_note(eco_format!(
            "expected `{}` to match the opening delimiter",
            expected_closing.descriptive_name()
        ));
}
