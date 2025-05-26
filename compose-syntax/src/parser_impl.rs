use std::ops::{Index, IndexMut};
use ecow::{eco_format, EcoString};
use compose_utils::trace_fn;
use crate::parser::{Marker, SyntaxKindIter, Token};
use crate::{FileId, Lexer, SyntaxError, SyntaxNode};
use crate::kind::SyntaxKind;
use crate::set::SyntaxSet;

impl Index<Marker> for Parser<'_> {
    type Output = SyntaxNode;

    fn index(&self, index: Marker) -> &Self::Output {
        &self.nodes[index.0]
    }
}

impl IndexMut<Marker> for Parser<'_> {
    fn index_mut(&mut self, index: Marker) -> &mut Self::Output {
        &mut self.nodes[index.0]
    }
}

/// A parser for Compose syntax.
///
/// Builds an internal list of [SyntaxNodes](SyntaxNode) from the text.
///
/// The result is a CST where each node contains enough information
/// to turn into an AST node lazily when needed.
#[derive(Debug, Clone)]
pub struct Parser<'s> {
    /// The text being parsed.
    text: &'s str,
    /// The lexer used to tokenize the text.
    lexer: Lexer<'s>,
    // Current token
    pub(crate) token: Token,

    balanced: bool,
    nodes: Vec<SyntaxNode>,
    pub(crate) last_pos: usize,
}

impl<'s> Parser<'s> {
    /// A marker that will point to the current token in the parser once it has been eaten.
    pub(crate) fn marker(&self) -> Marker {
        Marker(self.nodes.len())
    }
}

impl<'s> Parser<'s> {
    pub(crate) fn new(text: &'s str, offset: usize, file_id: FileId) -> Self {
        let mut lexer = Lexer::new(text, file_id);
        lexer.jump(offset);

        let token = Self::lex(&mut lexer);

        Self {
            text,
            lexer,
            token,
            balanced: true,
            nodes: vec![],
            last_pos: 0,
        }
    }

    pub(crate) fn finish(self) -> Vec<SyntaxNode> {
        self.nodes
    }

    fn finish_into(self, kind: SyntaxKind) -> SyntaxNode {
        assert!(self.end());
        SyntaxNode::inner(kind, self.finish())
    }

    #[inline]
    pub(crate) fn current(&self) -> SyntaxKind {
        self.token.kind
    }

    pub(crate) fn current_text(&self) -> &'s str {
        &self.text[self.token.start..self.current_end()]
    }

    pub(crate) fn current_end(&self) -> usize {
        self.lexer.cursor()
    }

    // Peeks the token kind after current
    fn peek(&self) -> SyntaxKind {
        let (kind, _) = self.lexer.clone().next();
        kind
    }
    pub(crate) fn peeker(&self) -> SyntaxKindIter {
        SyntaxKindIter::new(self.lexer.clone())
    }

    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    pub(crate) fn at_set(&self, set: SyntaxSet) -> bool {
        set.contains(self.current())
    }

    pub(crate) fn end(&self) -> bool {
        self.at(SyntaxKind::End)
    }

    fn had_newline(&self) -> bool {
        self.token.newline
    }

    pub(crate) fn eat(&mut self) {
        self.nodes.push(std::mem::take(&mut self.token.node));

        let mut next = Self::lex(&mut self.lexer);
        while next.kind == SyntaxKind::Error {
            self.nodes.push(next.node);
            next = Self::lex(&mut self.lexer)
        }

        self.token = next;
    }

    pub(crate) fn eat_if(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        }
        at
    }

    /// Move the parser forward without adding the node to the nodes vec
    pub(crate) fn skip(&mut self) {
        self.token = Self::lex(&mut self.lexer);
    }

    /// Move the parser forward without adding the node to the nodes vec
    /// if the current kind == `kind`
    pub(crate) fn skip_if(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.skip();
        }
        at
    }

    fn convert_and_eat(&mut self, kind: SyntaxKind) {
        self.token.node.convert_to_kind(kind);
        self.eat();
    }

    pub(crate) fn wrap(&mut self, from: Marker, kind: SyntaxKind) {
        let to = self.marker().0;
        let from = from.0.min(to);

        let children = self.nodes.drain(from..to).collect();
        self.nodes.insert(from, SyntaxNode::inner(kind, children))
    }

    /// Assert that the current token is of kind `kind` and eat it.
    #[track_caller]
    pub(crate) fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind, "Expected {:?}", kind);
        self.eat();
    }

    /// Include a syntax error node at the current token's position indicating that `expected` was expected.
    #[track_caller]
    pub(crate) fn expected(&mut self, expected: &str) {
        let kind = self.current();
        let error = SyntaxNode::error(
            SyntaxError::new(
                eco_format!("expected {expected}, got {kind:?}"),
                self.token.node.span(),
            ),
            self.token.node.text(),
        );
        self.nodes.push(error);
    }

    pub(crate) fn unexpected(&mut self, message: impl Into<EcoString>, recover_set: Option<SyntaxSet>) {
        trace_fn!("error_unexpected");
        self.balanced &= !self.token.kind.is_grouping();
        let message = message.into();
        self.token.node.convert_to_error(match message.as_str() {
            "" => eco_format!("unexpected token {:?}", self.token.kind),
            _ => message,
        });

        match recover_set {
            None => self.eat(),
            Some(recover_set) => {
                self.eat();
                self.recover_until(recover_set);
            }
        }
    }

    /// Consume the given syntax kind or produce an error. Returns whether the expected token was found.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        } else if kind == SyntaxKind::Ident && self.token.kind.is_keyword() {
            self.token.node.expected(eco_format!("{kind:?}"));
            self.eat()
        } else {
            self.balanced &= !kind.is_grouping();
            self.expected(&format!("{kind:?}"));
            self.eat()
        }
        at
    }

    pub(crate) fn expect_or_recover(&mut self, expected: SyntaxKind, recover_set: SyntaxSet) -> bool {
        if self.at(expected) {
            self.eat();
            return true;
        }

        self.expected(&format!("{expected:?}"));
        if self.can_recover_with(recover_set) {
            self.eat()
        }
        false
    }

    fn can_recover_with(&self, recover_set: SyntaxSet) -> bool {
        recover_set.contains(self.current())
            || (recover_set.contains(SyntaxKind::NewLine) && self.had_newline())
    }

    pub(crate) fn expect_or_recover_until(&mut self, expected: SyntaxKind, recover_set: SyntaxSet) -> bool {
        if self.at(expected) {
            self.eat();
            return true;
        }

        self.expected(&format!("{expected:?}"));

        self.recover_until(recover_set);
        if self.current() == expected {
            self.eat();
            return false;
        }
        false
    }

    fn recover_until(&mut self, recovery_set: SyntaxSet) {
        while !self.can_recover_with(recovery_set) &&  !self.end() {
            self.eat();
        }
    }

    #[track_caller]
    /// Expect that the current token is the closing delimiter `kind` and eat it.
    /// If it is not convert the node at `open_marker` to an error indicating that its delimiter was unclosed
    pub(crate) fn expect_closing_delimiter(&mut self, open_marker: Marker, kind: SyntaxKind) {
        if !self.eat_if(kind) {
            self[open_marker].convert_to_error("unclosed delimiter")
        }
    }

    fn lex(lexer: &mut Lexer) -> Token {
        let prev_end = lexer.cursor();
        let start = prev_end;
        let (kind, node) = lexer.next();

        Token {
            kind,
            node,
            newline: lexer.newline(),
            start,
            prev_end,
        }
    }
}