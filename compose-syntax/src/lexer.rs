use crate::file::FileId;
use crate::kind::SyntaxKind;
use crate::node::{SyntaxError, SyntaxNode};
use crate::span::Span;
use ecow::EcoString;
use std::ops::Range;
use unscanny::Scanner;

#[derive(Debug, Clone)]
pub struct Lexer<'s> {
    /// Scanner: Contains the source text and a cursor in the source text
    s: Scanner<'s>,
    newline: bool,
    error: Option<SyntaxError>,
    pub(crate) file_id: FileId,
}

impl<'s> Lexer<'s> {
    pub fn new(text: &'s str, file_id: FileId) -> Self {
        Self {
            s: Scanner::new(text),
            newline: false,
            error: None,
            file_id,
        }
    }

    /// The index in the string at which the last token ended and the next token will start
    pub fn cursor(&self) -> usize {
        self.s.cursor()
    }

    /// Jump to the given index in the string.
    pub fn jump(&mut self, index: usize) {
        self.s.jump(index);
    }

    /// Whether the last token had a trailing newline
    pub fn newline(&self) -> bool {
        self.newline
    }

    /// The number of characters until the most recent newline from an index.
    pub fn column(&self, index: usize) -> usize {
        let mut s = self.s; // Make a new temporary scanner (inexpensive).
        s.jump(index);
        s.before()
            .chars()
            .rev()
            .take_while(|&c| !is_newline(c))
            .count()
    }
}

/// Whether a character is interpreted as a newline
#[inline]
pub fn is_newline(character: char) -> bool {
    matches!(
        character,
        // Line Feed, Vertical Tab, Form Feed, Carriage Return.
        '\n' | '\x0B' | '\x0C' | '\r' |
        // Next Line, Line Separator, Paragraph Separator.
        '\u{0085}' | '\u{2028}' | '\u{2029}'
    )
}

impl Lexer<'_> {
    fn error(&mut self, message: impl Into<EcoString>, range: Range<usize>) -> SyntaxKind {
        self.error = Some(SyntaxError::new(message, Span::new(self.file_id, range)));
        SyntaxKind::Error
    }

    fn hint(&mut self, message: impl Into<EcoString>) {
        if let Some(error) = &mut self.error {
            error.hints.push(message.into())
        }
    }
}

impl Lexer<'_> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> (SyntaxKind, SyntaxNode) {
        debug_assert!(self.error.is_none());

        self.newline = self.skip_whitespace(self.cursor());
        let start = self.cursor();

        let kind = match self.s.eat() {
            Some(c) => self.kind(start, c),
            None => SyntaxKind::End,
        };

        let text = self.s.from(start);
        let span = Span::new(self.file_id, start..self.s.cursor());
        let node = match self.error.take() {
            Some(error) => SyntaxNode::error(error, text),
            None => SyntaxNode::leaf(kind, text, span),
        };

        (kind, node)
    }

    fn kind(&mut self, start: usize, c: char) -> SyntaxKind {
        match c {
            '/' if self.s.eat_if('/') => {
                if self.s.eat_if('/') {
                    self.lex_doc_comment()
                } else {
                    self.lex_line_comment()
                }
            }
            '/' if self.s.eat_if('*') => self.lex_block_comment(start),

            '=' if self.s.eat_if('=') => SyntaxKind::EqEq,
            '!' if self.s.eat_if('=') => SyntaxKind::BangEq,
            '+' if self.s.eat_if('=') => SyntaxKind::PlusEq,
            '-' | '\u{2212}' if self.s.eat_if('=') => SyntaxKind::MinusEq,
            '*' if self.s.eat_if('=') => SyntaxKind::StarEq,
            '/' if self.s.eat_if('=') => SyntaxKind::SlashEq,
            '!' if self.s.eat_if('=') => SyntaxKind::BangEq,

            '.' if self.s.at('.') && self.s.scout(1) == Some('.') => {
                self.s.eat();
                self.s.eat();
                SyntaxKind::Ellipsis
            }
            '.' if self.s.at('.') && self.s.scout(1) == Some('=') => {
                self.s.eat();
                self.s.eat();
                SyntaxKind::DotsEq
            }
            '.' if self.s.eat_if('.') => SyntaxKind::Dots,

            '<' if self.s.eat_if('<') => SyntaxKind::LtLt,
            '<' if self.s.eat_if('=') => SyntaxKind::LtEq,
            '>' if self.s.eat_if('>') => SyntaxKind::GtGt,
            '>' if self.s.eat_if('=') => SyntaxKind::GtEq,

            '|' if self.s.eat_if('|') => SyntaxKind::PipePipe,
            '|' if self.s.eat_if('=') => SyntaxKind::PipeEq,
            '&' if self.s.eat_if('&') => SyntaxKind::AmpAmp,
            '&' if self.s.eat_if('=') => SyntaxKind::AmpersandEq,

            '~' if self.s.eat_if('=') => SyntaxKind::TildeEq,
            '^' if self.s.eat_if('=') => SyntaxKind::HatEq,

            ':' if self.s.eat_if(':') => SyntaxKind::ColonColon,

            '=' if self.s.eat_if('>') => SyntaxKind::Arrow,

            '{' => SyntaxKind::LeftBrace,
            '}' => SyntaxKind::RightBrace,
            '[' => SyntaxKind::LeftBracket,
            ']' => SyntaxKind::RightBracket,
            '(' => SyntaxKind::LeftParen,
            ')' => SyntaxKind::RightParen,

            '.' => SyntaxKind::Dot,
            ',' => SyntaxKind::Comma,
            ';' => SyntaxKind::Semicolon,
            ':' => SyntaxKind::Colon,
            '*' => SyntaxKind::Star,
            '+' => SyntaxKind::Plus,
            '-' => SyntaxKind::Minus,
            '/' => SyntaxKind::Slash,
            '%' => SyntaxKind::Percent,
            '$' => SyntaxKind::Dollar,
            '#' => SyntaxKind::Hash,
            '@' => SyntaxKind::At,
            '^' => SyntaxKind::Hat,
            '_' => SyntaxKind::Underscore,
            '`' => SyntaxKind::Backtick,
            '\'' => SyntaxKind::Apostrophe,
            '!' => SyntaxKind::Bang,
            '~' => SyntaxKind::Tilde,
            '|' => SyntaxKind::Pipe,
            '&' => SyntaxKind::Amp,
            '>' => SyntaxKind::Gt,
            '<' => SyntaxKind::Lt,
            '=' => SyntaxKind::Eq,

            '"' => self.lex_string(start),
            '0'..='9' => self.lex_number(start),

            c if is_ident_start(c) => self.lex_ident(start),

            c => self.error(
                format!("unexpected character `{c}`"),
                self.range_from(start),
            ),
        }
    }

    fn lex_number(&mut self, start: usize) -> SyntaxKind {
        // Read until we find a non-digit (. or something else).
        self.s.eat_while(char::is_ascii_digit);

        let is_fractional = {
            let dot = self.s.at('.');
            let number = matches!(self.s.scout(1), Some('0'..='9' | 'e' | 'E'));
            dot && number
        };

        if is_fractional {
            // Read the fractional part.
            if self.s.eat_if('.') {
                self.s.eat_while(char::is_ascii_digit);
            }

            // Read the exponent.
            if self.s.eat_if('e') || self.s.eat_if('E') {
                self.s.eat_if(['+', '-']);
                self.s.eat_while(char::is_ascii_digit);
            }
        }

        let number = self.s.from(start);

        if number.parse::<i64>().is_ok() {
            SyntaxKind::Int
        } else if number.parse::<f64>().is_ok() {
            SyntaxKind::Float
        } else {
            self.error(format!("invalid number `{number}`"), self.range_from(start))
        }
    }

    fn lex_ident(&mut self, start: usize) -> SyntaxKind {
        self.s.eat_while(is_ident_mid);
        let ident = self.s.from(start);

        let prev = self.s.get(0..start);
        if !(prev.ends_with(['.']) || prev.ends_with("..")) {
            if let Some(keyword) = keyword(ident) {
                return keyword;
            }
        }

        if ident == "_" {
            SyntaxKind::Underscore
        } else {
            SyntaxKind::Ident
        }
    }

    /// Create a range from the start of the string to the current cursor.
    fn range_from(&self, start: usize) -> Range<usize> {
        start..self.s.cursor()
    }

    fn lex_string(&mut self, start: usize) -> SyntaxKind {
        let mut escaped = false;
        self.s.eat_until(|c| {
            let stop = c == '"' && !escaped;
            escaped = c == '\\' && !escaped;
            stop
        });

        if !self.s.eat_if('"') {
            return self.error("unclosed string", self.range_from(start));
        }

        SyntaxKind::Str
    }

    fn lex_line_comment(&mut self) -> SyntaxKind {
        self.s.eat_while(|c| !is_newline(c));
        SyntaxKind::Comment
    }

    fn lex_block_comment(&mut self, start: usize) -> SyntaxKind {
        while let Some(c) = self.s.eat() {
            if c == '*' && self.s.eat_if('/') {
                return SyntaxKind::Comment;
            }
        }
        self.error("unterminated block comment", self.range_from(start))
    }

    fn lex_doc_comment(&mut self) -> SyntaxKind {
        self.s.eat_while(|c| !is_newline(c));
        SyntaxKind::DocComment
    }

    fn skip_whitespace(&mut self, start: usize) -> bool {
        self.s.eat_while(is_space);

        // count newlines
        let mut newline_count = 0;
        let mut s = Scanner::new(self.s.from(start));
        while let Some(c) = s.eat() {
            if matches!(c, '\n' | '\x0B' | '\x0C' | '\r') {
                // Handle \r\n and \r as a single newline.
                if c == '\r' {
                    s.eat_if('\n');
                }
                newline_count += 1;
            }
        }

        newline_count > 0
    }
}

fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\x0B' | '\x0C' | '\r')
}

fn keyword(ident: &str) -> Option<SyntaxKind> {
    Some(match ident {
        "as" => SyntaxKind::As,
        "break" => SyntaxKind::Break,
        "continue" => SyntaxKind::Continue,
        "else" => SyntaxKind::Else,
        "enum" => SyntaxKind::Enum,
        "false" => SyntaxKind::Bool,
        "for" => SyntaxKind::For,
        "if" => SyntaxKind::If,
        "import" => SyntaxKind::Import,
        "in" => SyntaxKind::In,
        "let" => SyntaxKind::Let,
        "loop" => SyntaxKind::Loop,
        "mut" => SyntaxKind::Mut,
        "ref" => SyntaxKind::Ref,
        "pub" => SyntaxKind::Pub,
        "return" => SyntaxKind::Return,
        "true" => SyntaxKind::Bool,
        "while" => SyntaxKind::While,
        _ => return None,
    })
}

fn is_ident_mid(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_tokens;
    use crate::test_utils::{LexerAssert, test_file_id};

    #[test]
    fn test_int() {
        assert_tokens!("123", Int("123", 0..3));
    }

    #[test]
    fn test_float() {
        assert_tokens!("1.3", Float("1.3", 0..3))
    }

    #[test]
    fn test_integer_floats() {
        assert_tokens!("1.0", Float("1.0", 0..3));
    }

    #[test]
    fn test_integer_methods_disambiguation() {
        assert_tokens!(
            "1.method()",
            Int("1", 0..1)
            Dot(".", 1..2)
            Ident("method", 2..8)
            LeftParen("(", 8..9)
            RightParen(")", 9..10)
        );
    }

    #[test]
    fn test_int_methods_disambiguation() {
        assert_tokens!(
            "1.method()",
            Int("1", 0..1)
            Dot(".", 1..2)
            Ident("method", 2..8)
            LeftParen("(", 8..9)
            RightParen(")", 9..10)
        );
    }

    #[test]
    fn test_float_with_exponent() {
        assert_tokens!("1.0e1", Float("1.0e1", 0..5));
        assert_tokens!("1.0e-1", Float("1.0e-1", 0..6));
    }

    #[test]
    fn test_string() {
        assert_tokens!("\"abc\"", Str("abc", 0..5));
    }

    #[test]
    fn test_ident() {
        assert_tokens!("abc", Ident("abc", 0..3));
    }

    #[test]
    fn test_unterminated_string() {
        assert_tokens!("\"abc", !Error("unclosed string", "\"abc", 0..4));
    }

    #[test]
    fn test_escaped_strings() {
        assert_tokens!("\"abc\\\"def\"", Str("\"abc\\\"def\"", 0..10));
    }

    #[test]
    fn test_newline() {
        let file_id = test_file_id();
        let mut lexer = Lexer::new("a\nb", file_id);
        assert_eq!(
            lexer.next(),
            (
                SyntaxKind::Ident,
                SyntaxNode::leaf(SyntaxKind::Ident, "a", Span::new(file_id, 0..1))
            )
        );
        assert!(!lexer.newline());
        assert_eq!(
            lexer.next(),
            (
                SyntaxKind::Ident,
                SyntaxNode::leaf(SyntaxKind::Ident, "b", Span::new(file_id, 2..3))
            )
        );
        assert!(lexer.newline());
    }

    #[test]
    fn test_repeating_next_after_end() {
        let file_id = test_file_id();
        let mut lexer = Lexer::new("", file_id);
        assert_eq!(
            lexer.next(),
            (
                SyntaxKind::End,
                SyntaxNode::leaf(SyntaxKind::End, "", Span::new(file_id, 0..0))
            )
        );
        assert_eq!(
            lexer.next(),
            (
                SyntaxKind::End,
                SyntaxNode::leaf(SyntaxKind::End, "", Span::new(file_id, 0..0))
            )
        );
        assert_eq!(
            lexer.next(),
            (
                SyntaxKind::End,
                SyntaxNode::leaf(SyntaxKind::End, "", Span::new(file_id, 0..0))
            )
        );
    }

    #[test]
    fn test_comment_kinds() {
        assert_tokens!("// regular comment", Comment("// regular comment", 0..18));
        assert_tokens!("/// doc comment", DocComment("/// doc comment", 0..15));
        assert_tokens!("/* block\ncomment */", Comment("/* block\ncomment */", 0..19));
    }
}
