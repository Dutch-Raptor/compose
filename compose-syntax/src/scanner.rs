use crate::{Lexer, SyntaxKind, SyntaxNode};
use ecow::{EcoString, eco_format};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delimiter {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
}

impl Delimiter {
    pub fn is_opening(self) -> bool {
        match self {
            Delimiter::LeftParen | Delimiter::LeftBrace | Delimiter::LeftBracket => true,
            _ => false,
        }
    }

    pub fn is_closing(self) -> bool {
        match self {
            Delimiter::RightParen | Delimiter::RightBrace | Delimiter::RightBracket => true,
            _ => false,
        }
    }

    pub fn matching(self) -> Delimiter {
        match self {
            Delimiter::LeftParen => Delimiter::RightParen,
            Delimiter::LeftBrace => Delimiter::RightBrace,
            Delimiter::LeftBracket => Delimiter::RightBracket,
            Delimiter::RightParen => Delimiter::LeftParen,
            Delimiter::RightBrace => Delimiter::LeftBrace,
            Delimiter::RightBracket => Delimiter::LeftBracket,
        }
    }

    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        match kind {
            SyntaxKind::LeftParen => Some(Delimiter::LeftParen),
            SyntaxKind::RightParen => Some(Delimiter::RightParen),
            SyntaxKind::LeftBrace => Some(Delimiter::LeftBrace),
            SyntaxKind::RightBrace => Some(Delimiter::RightBrace),
            SyntaxKind::LeftBracket => Some(Delimiter::LeftBracket),
            SyntaxKind::RightBracket => Some(Delimiter::RightBracket),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    lexer: Lexer<'a>,
    delimiters: Vec<Delimiter>,
}

impl<'a> Scanner<'a> {}

impl<'a> Scanner<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            delimiters: Vec::new(),
        }
    }

    /// jump to a specific offset in the containing lexer
    pub fn with_offset(mut self, byte_index: usize) -> Self {
        self.lexer.jump(byte_index);
        self
    }

    pub fn with_entered_delim(mut self, delim: Delimiter) -> Self {
        debug_assert!(delim.is_opening(), "can only enter an opening delim");
        self.enter(delim);
        self
    }

    pub fn enter(&mut self, delimiter: Delimiter) {
        self.delimiters.push(delimiter);
    }

    pub fn exit(&mut self, delimiter: Delimiter) -> Result<(), EcoString> {
        if self.delimiters.last() != Some(&delimiter.matching()) {
            return Err(eco_format!(
                "unmatched delimiters, closed a {:?}, for opening {:?}",
                delimiter,
                self.delimiters.last()
            ));
        }
        self.delimiters.pop();
        Ok(())
    }

    /// Iterates through the current level of delimiters (does not enter nested delims) and checks if
    /// the given `kind` is contained within
    pub fn level_contains_kind(&mut self, expected_kind: SyntaxKind) -> Result<bool, EcoString> {
        self.find_in_level(expected_kind).map(|opt| opt.is_some())
    }
    pub(crate) fn find_in_level(
        &mut self,
        expected_kind: SyntaxKind,
    ) -> Result<Option<SyntaxNode>, EcoString> {
        let current_level = self.delimiters.len();

        self.scan_until(|delim_stack, node| {
            delim_stack.len() == current_level && node.kind() == expected_kind
        })
    }

    /// find the corresponding closing delim
    pub fn matching_closing_delim(&mut self) -> Result<Option<SyntaxNode>, EcoString> {
        let current_level = self.delimiters.len();
        debug_assert!(current_level > 0, "open a delimiter before calling this");
        let exit_level = current_level - 1;
        self.scan_until(|delim_stack, _| delim_stack.len() == exit_level)
    }

    /// Scans until the predicate yields true. Returns the yielded node if any
    pub fn scan_until(
        &mut self,
        predicate: impl Fn(&[Delimiter], &SyntaxNode) -> bool,
    ) -> Result<Option<SyntaxNode>, EcoString> {
        while let Some(node) = self.next() {
            if let Some(delimiter) = Delimiter::from_kind(node.kind()) {
                if delimiter.is_opening() {
                    self.enter(delimiter);
                }
                if delimiter.is_closing() {
                    self.exit(delimiter)?;
                }
            }
            if predicate(&self.delimiters, &node) {
                return Ok(Some(node));
            }
        }
        Ok(None)
    }

    pub fn next(&mut self) -> Option<SyntaxNode> {
        match self.lexer.next() {
            (SyntaxKind::End, _) => None,
            (_, node) => Some(node),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::FileId;

    fn from_str(code: &str) -> Scanner {
        let lexer = Lexer::new(code, FileId::new("main.cmps"));
        Scanner::new(lexer)
    }

    #[test]
    fn test_contains_kind() {
        let mut scanner = from_str("a, b, c => a + b + c }");

        scanner.enter(Delimiter::LeftBrace);
        assert!(
            scanner
                .clone()
                .level_contains_kind(SyntaxKind::Arrow)
                .unwrap()
        );
        assert!(
            !scanner
                .clone()
                .level_contains_kind(SyntaxKind::Dots)
                .unwrap()
        );
    }

    #[test]
    fn contains_kind_skips_nested() {
        let mut scanner = from_str("a = { a => a + 2 }, b = (2 + 4), c = [1,2,3] => a(b) }");

        scanner.enter(Delimiter::LeftBrace);
        assert!(
            scanner
                .clone()
                .level_contains_kind(SyntaxKind::Arrow)
                .unwrap()
        );
        assert!(
            !scanner
                .clone()
                .level_contains_kind(SyntaxKind::Int)
                .unwrap()
        );

        let mut s = from_str("{ { { => } } } }");
        s.enter(Delimiter::LeftBrace);
        assert!(!s.level_contains_kind(SyntaxKind::Arrow).unwrap());
    }

    #[test]
    fn closing_delim() {
        let mut s = from_str("a, b, c, { () } d, e, (abc = fed) => ) ... a b c d e f g h");
        s.enter(Delimiter::LeftParen);
        let closing_delim = s.matching_closing_delim().unwrap().unwrap();
        assert_eq!(closing_delim.kind(), SyntaxKind::RightParen);
        // check it is the right `)`
        assert_eq!(s.next().unwrap().kind(), SyntaxKind::Ellipsis);
        assert_eq!(s.next().unwrap().kind(), SyntaxKind::Ident);
    }
}
