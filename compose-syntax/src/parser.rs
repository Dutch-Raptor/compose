use crate::ast::BinOp;
use crate::file::FileId;
use crate::kind::SyntaxKind;
use crate::node::SyntaxNode;
use crate::precedence::{Precedence, PrecedenceTrait};
use crate::set::{SyntaxSet, UNARY_OP};
use crate::{Lexer, SyntaxError, ast, set};
use ecow::eco_format;
use std::collections::HashSet;
use std::ops::{Index, IndexMut};

pub fn parse(text: &str, file_id: FileId) -> Vec<SyntaxNode> {
    parse_with_offset(text, file_id, 0)
}

/// Assumes that the text at offset begins with a valid expression (or whitespace).
pub fn parse_with_offset(text: &str, file_id: FileId, offset: usize) -> Vec<SyntaxNode> {
    let mut p = Parser::new(text, offset, file_id);

    let mut pos = p.current_end();
    while !p.end() {
        code_expression(&mut p);

        p.skip_if(SyntaxKind::Semicolon);

        // If the parser is not progressing, then we have an error
        if pos == p.current_end() && !p.end() {
            // Eat the token and continue trying to parse
            p.unexpected();
        }
        pos = p.current_end();
    }

    p.finish()
}

fn code_expression(p: &mut Parser) {
    code_expr_prec(p, false, Precedence::Lowest);
}

fn code_expr_prec(p: &mut Parser, atomic: bool, min_prec: Precedence) {
    let m = p.marker();
    if !atomic && p.at_set(UNARY_OP) {
        let op = ast::UnOp::from_kind(p.current()).expect("Was checked to be a unary op");
        p.eat();
        code_expr_prec(p, true, op.precedence());
        p.wrap(m, SyntaxKind::Unary)
    } else {
        primary_expr(p, atomic);
    }

    loop {
        if p.at(SyntaxKind::LeftParen) {
            args(p);
            p.wrap(m, SyntaxKind::FuncCall);
            continue;
        }

        let at_field_or_index = p.at(SyntaxKind::Dot) || p.at(SyntaxKind::LeftBracket);

        if atomic && !at_field_or_index {
            break;
        }

        // Handle field access
        if p.eat_if(SyntaxKind::Dot) {
            p.expect(SyntaxKind::Ident);
            p.wrap(m, SyntaxKind::FieldAccess);
            continue;
        }

        // Handle index access
        if p.eat_if(SyntaxKind::LeftBracket) {
            code_expression(p);
            p.expect(SyntaxKind::RightBracket);
            p.wrap(m, SyntaxKind::IndexAccess)
        }

        let bin_op = BinOp::from_kind(p.current());

        if let Some(op) = bin_op {
            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            p.eat();
            code_expr_prec(p, false, prec);
            p.wrap(m, SyntaxKind::Binary);
            continue;
        }

        break;
    }
}

fn args(p: &mut Parser) {
    let m = p.marker();
    p.expect(SyntaxKind::LeftParen);

    let m_delim = p.marker();
    while !p.current().is_terminator() {
        arg(p);

        if !p.current().is_terminator() {
            p.expect(SyntaxKind::Comma);
        }
    }

    p.expect_closing_delimiter(m_delim, SyntaxKind::RightParen);

    p.wrap(m, SyntaxKind::Args);
}

fn arg(p: &mut Parser) {
    code_expression(p);
}

/// Parse a primary expression.
///
/// A primary expressions are the building blocks in composable expressions.
fn primary_expr(p: &mut Parser, atomic: bool) {
    let m = p.marker();
    match p.current() {
        SyntaxKind::Ident => {
            p.eat();
            // Parse a closure like `a => a + 1`
            if !atomic && p.at(SyntaxKind::Arrow) {
                p.wrap(m, SyntaxKind::Params);
                p.assert(SyntaxKind::Arrow);
                code_expression(p);
                p.wrap(m, SyntaxKind::Closure)
            }
        }
        SyntaxKind::Underscore if !atomic => {
            // Parse closure like `_ => 1` or destructure like `_ = foo()`
            if p.at(SyntaxKind::Arrow) {
                p.wrap(m, SyntaxKind::Params);
                p.assert(SyntaxKind::Arrow);
                code_expression(p);
                p.wrap(m, SyntaxKind::Closure)
            } else if p.eat_if(SyntaxKind::Eq) {
                code_expression(p);
                p.wrap(m, SyntaxKind::DestructureAssignment);
            } else {
                p[m].expected("expression")
            }
        }
        SyntaxKind::LeftBrace => block(p),
        SyntaxKind::LeftParen => closure(p, m),
        SyntaxKind::Let => let_binding(p),
        SyntaxKind::Int
        | SyntaxKind::Float
        | SyntaxKind::Bool
        | SyntaxKind::Str
        | SyntaxKind::Unit => p.eat(),
        _ => p.expected("expression"),
    }
}

fn closure(p: &mut Parser, m: Marker) {
    debug_assert_eq!(p.current(), SyntaxKind::LeftParen);
    params(p);

    p.assert(SyntaxKind::Arrow);

    code_expression(p);

    p.wrap(m, SyntaxKind::Closure)
}

/// Parse closure parameters.
fn params(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::LeftParen);

    let mut seen = HashSet::new();

    while !p.current().is_terminator() {
        if !p.at_set(set::PARAM) {
            p.unexpected();
            continue;
        }

        param(p, &mut seen);

        if !p.current().is_terminator() {
            p.assert(SyntaxKind::Comma);
        }
    }

    // allow trailing commas
    p.eat_if(SyntaxKind::Comma);

    p.expect_closing_delimiter(m, SyntaxKind::RightParen);
    p.wrap(m, SyntaxKind::Params)
}

fn param<'s>(p: &mut Parser<'s>, seen: &mut HashSet<&'s str>) {
    let m = p.marker();

    let was_at_pat = p.at_set(set::PATTERN);
    pattern(p, false, seen, Some("parameter"));

    // Parse named params like `a: 1`
    if p.eat_if(SyntaxKind::Colon) {
        if was_at_pat && p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }

        code_expression(p);
        p.wrap(m, SyntaxKind::Named)
    }
}

/// Parses a binding or reassignment pattern.
fn pattern<'s>(
    p: &mut Parser<'s>,
    reassignment: bool,
    seen: &mut HashSet<&'s str>,
    dupe: Option<&'s str>,
) {
    match p.current() {
        SyntaxKind::Underscore => p.eat(),
        SyntaxKind::At => destructuring(p, reassignment, seen, dupe),
        _ => pattern_leaf(p, reassignment, seen, dupe),
    }
}

fn pattern_leaf<'s>(
    p: &mut Parser<'s>,
    reassignment: bool,
    seen: &mut HashSet<&'s str>,
    dupe: Option<&'s str>,
) {
    if p.current().is_keyword() {
        p.token.node.expected("pattern");
        p.eat();
        return;
    } else if !p.at_set(set::PATTERN_LEAF) {
        p.expected("pattern");
        return;
    }

    let m = p.marker();
    let text = p.current_text();

    // Parse a full expression, even though we only care about an identifier.
    // This way the entire expression can be marked as an error if it is not.
    code_expr_prec(p, true, Precedence::Lowest);

    // If the pattern is not a reassignment, it can only be an identifier
    if !reassignment {
        let node = &mut p[m];
        if node.kind() != SyntaxKind::Ident {
            node.expected("pattern");
            return;
        }
        if !seen.insert(text) {
            node.convert_to_error(eco_format!(
                "duplicate {binding}: {text}",
                binding = dupe.unwrap_or("binding")
            ))
        }
    }
}

fn destructuring(p: &mut Parser, reassignment: bool, seen: &mut HashSet<&str>, dupe: Option<&str>) {
    p.assert(SyntaxKind::At);

    unimplemented!("destructuring")
}

fn let_binding(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::Let);

    p.eat_if(SyntaxKind::Mut);

    pattern(p, false, &mut HashSet::new(), None);

    if p.eat_if(SyntaxKind::Eq) {
        code_expression(p);
    }

    p.wrap(m, SyntaxKind::LetBinding)
}

fn parenthesized(p: &mut Parser, atomic: bool) {
    let m = p.marker();
    p.assert(SyntaxKind::LeftParen);
    code_expr_prec(p, atomic, Precedence::Lowest);
    p.assert(SyntaxKind::RightParen);
    p.wrap(m, SyntaxKind::Parenthesized)
}

fn block(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::LeftBrace);

    while !p.at(SyntaxKind::RightBrace) {
        code_expression(p);
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightBrace);
    p.wrap(m, SyntaxKind::CodeBlock)
}

// Represents a node's position in the parser.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Marker(usize);

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
struct Parser<'s> {
    /// The text being parsed.
    text: &'s str,
    /// The lexer used to tokenize the text.
    lexer: Lexer<'s>,
    // Current token
    token: Token,

    balanced: bool,
    nodes: Vec<SyntaxNode>,
}

impl<'s> Parser<'s> {
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
        }
        at
    }
}

impl<'s> Parser<'s> {
    /// A marker that will point to the current token in the parser once it has been eaten.
    pub(crate) fn marker(&self) -> Marker {
        Marker(self.nodes.len())
    }
}

#[derive(Debug, Clone)]
struct Token {
    kind: SyntaxKind,
    node: SyntaxNode,
    // Whether the preceding token had a trailing newline
    newline: bool,

    start: usize,

    // The index into `text` of the end of the previous token
    prev_end: usize,
}

impl<'s> Parser<'s> {
    fn new(text: &'s str, offset: usize, file_id: FileId) -> Self {
        let mut lexer = Lexer::new(text, file_id);
        lexer.jump(offset);

        let token = Self::lex(&mut lexer);

        Self {
            text,
            lexer,
            token,
            balanced: true,
            nodes: vec![],
        }
    }

    fn finish(self) -> Vec<SyntaxNode> {
        self.nodes
    }

    fn finish_into(self, kind: SyntaxKind) -> SyntaxNode {
        assert!(self.end());
        SyntaxNode::inner(kind, self.finish())
    }

    #[inline]
    fn current(&self) -> SyntaxKind {
        self.token.kind
    }

    pub(crate) fn current_text(&self) -> &'s str {
        &self.text[self.token.start..self.current_end()]
    }

    fn current_end(&self) -> usize {
        self.lexer.cursor()
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    fn at_set(&self, set: SyntaxSet) -> bool {
        set.contains(self.current())
    }

    fn end(&self) -> bool {
        self.at(SyntaxKind::End)
    }

    fn had_newline(&self) -> bool {
        self.token.newline
    }

    fn eat(&mut self) {
        self.nodes.push(std::mem::take(&mut self.token.node));
        self.token = Self::lex(&mut self.lexer);
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

    fn wrap(&mut self, from: Marker, kind: SyntaxKind) {
        let to = self.marker().0;
        let from = from.0.min(to);

        let children = self.nodes.drain(from..to).collect();
        self.nodes.insert(from, SyntaxNode::inner(kind, children))
    }

    /// Assert that the current token is of kind `kind` and eat it.
    #[track_caller]
    fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind, "Expected {:?}", kind);
        self.eat();
    }

    /// Include a syntax error node at the current token's position indicating that `expected` was expected.
    #[track_caller]
    fn expected(&mut self, expected: &str) {
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

    fn unexpected(&mut self) {
        self.balanced &= !self.token.kind.is_grouping();
        self.token
            .node
            .convert_to_error(eco_format!("unexpected token {:?}", self.token.kind));
        self.eat();
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
