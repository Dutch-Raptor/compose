mod control_flow;
mod expressions;
mod funcs;
mod patterns;
mod statements;

use crate::file::FileId;
use crate::kind::SyntaxKind;
use crate::node::SyntaxNode;
use crate::scanner::Scanner;
use crate::set::{SyntaxSet, syntax_set};
use crate::{Lexer, Span, SyntaxError};
use compose_utils::trace_log;
use ecow::{EcoString, eco_format};
use expressions::err_unclosed_delim;
use std::collections::HashMap;
use std::ops::{Index, IndexMut, Range};

/// Represents the context in which an expression is being parsed.
///
/// This affects whether certain constructs (like closures or assignments)
/// are valid. Used to disambiguate grammar cases during parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprContext {
    /// Regular expression context. Most expression forms are allowed.
    Expr,

    /// Atomic expression context. Disallows parsing of certain constructs
    /// like closures or binary operations. Typically used for expression
    /// heads or in disambiguation scenarios.
    AtomicExpr,

    /// Statement context. Used at the top level of a statement to allow
    /// constructs like assignments or let-bindings that are not valid in
    /// pure expression contexts.
    Statement,
}

impl ExprContext {
    /// Returns true if this context allows parsing a full expression.
    ///
    /// This includes both `Expr` and `AtomicExpr` contexts, but excludes
    /// `Statement`, which is expected to handle its own expression cases.
    fn is_expr(&self) -> bool {
        match self {
            ExprContext::Expr => true,
            ExprContext::AtomicExpr => true,
            ExprContext::Statement => false,
        }
    }

    /// Returns true if this context requires atomic (non-composite) expressions.
    ///
    /// This is typically used to suppress recursive constructs like closures,
    /// binary expressions, or grouping in places like destructuring patterns.
    fn is_atomic(&self) -> bool {
        match self {
            ExprContext::Expr => false,
            ExprContext::AtomicExpr => true,
            ExprContext::Statement => false,
        }
    }

    /// Converts this context to an expression context (used for sub-expressions).
    ///
    /// If currently in `Statement`, it downgrades to `Expr` to parse an inner
    /// expression. Otherwise, it preserves the current context.
    fn to_expr(self) -> ExprContext {
        match self {
            ExprContext::Expr => ExprContext::Expr,
            ExprContext::AtomicExpr => ExprContext::AtomicExpr,
            ExprContext::Statement => ExprContext::Expr,
        }
    }
}

/// Parses one or more statements from the given input text, producing a concrete syntax tree (CST).
///
/// This is the main entry point for parsing Compose code. It assumes the input
/// starts with a valid statement or leading whitespace. The parser is fault-tolerant,
/// so even if the input contains syntax errors, it will return a complete tree
/// including [`SyntaxError`] nodes where appropriate.
///
/// # Parameters
///
/// - `text`: The source text to parse.
/// - `file_id`: An identifier for the file the text originated from, used for diagnostics.
///
/// # Returns
///
/// A vector of [`SyntaxNode`]s representing the parsed statements and any encountered
/// syntax errors as [`SyntaxError`]s.
pub fn parse(text: &str, file_id: FileId) -> Vec<SyntaxNode> {
    parse_with_offset(text, file_id, 0)
}

/// Parses one or more statements from the input text, starting at a given offset, producing a CST.
///
/// This function is useful for parsing code fragments embedded in larger files,
/// such as REPL inputs or inline statements. It assumes the text at `offset`
/// begins with a valid statement or leading whitespace. The parser is fault-tolerant
/// and will include [`SyntaxError`] in the result to represent syntax issues.
///
/// # Parameters
///
/// - `text`: The source text containing the statement(s).
/// - `file_id`: An identifier for the file the text originated from, used for diagnostics.
/// - `offset`: The byte offset into `text` at which parsing should begin.
///
/// # Returns
///
/// A vector of [`SyntaxNode`]s representing the parsed statements, including
/// [`SyntaxError`] for any syntax issues encountered.
pub fn parse_with_offset(text: &str, file_id: FileId, offset: usize) -> Vec<SyntaxNode> {
    let mut p = Parser::new(text, offset, file_id);

    statements::code(&mut p, syntax_set!(End));

    p.finish()
}

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

/// A recursive-descent parser for Compose syntax.
///
/// This parser constructs a concrete syntax tree (CST) from a stream of tokens
/// produced by the lexer. The resulting CST is stored as a flat list of [`SyntaxNode`]s,
/// but supports a hierarchical structure via `InnerNode`s, which groups
/// child nodes under a parent [`SyntaxKind`].
///
/// # Design
///
/// - **Fault-tolerant:** Instead of failing on invalid input, the parser emits
///   `InnerNode` entries and continues parsing, making it robust in
///   interactive environments.
/// - **Flat representation with nesting:** While all nodes are stored in a flat
///   `Vec<SyntaxNode>`, hierarchical structure is expressed through `SyntaxNode::Inner`,
///   which wraps a contiguous range of child nodes.
/// - **Lazy AST:** The CST nodes contain enough information to construct the AST
///   on demand, deferring costly semantic interpretation until needed.
/// - **Memoized backtracking:** Internal memoization supports efficient parsing with
///   lookahead and recovery.
///
/// # Fields
///
/// - `text`: The original source text being parsed.
/// - `lexer`: The tokenizer that produces a stream of tokens from the input text.
/// - `token`: The current token the parser is examining.
/// - `balanced`: Internal state used for delimiter tracking (e.g., braces, parentheses).
/// - `nodes`: The flat list of syntax nodes making up the CST. Includes nested
///   structures via `SyntaxNode::Inner` and error recovery via `SyntaxNode::Error`.
/// - `last_pos`: The byte offset of the last consumed token.
/// - `memo`: A memoization arena used for backtracking and error recovery.
#[derive(Debug)]
pub struct Parser<'s> {
    /// The full source text being parsed.
    text: &'s str,

    /// The lexer that tokenizes the input text on demand.
    lexer: Lexer<'s>,

    /// The current token being examined by the parser.
    pub(crate) token: Token,

    /// Tracks whether the parser is currently inside a balanced region (e.g., matching braces).
    balanced: bool,

    /// The list of syntax nodes produced during parsing.
    ///
    /// This includes both well-formed constructs and `SyntaxNode::Error` entries
    /// for malformed or incomplete input. Nesting is expressed using
    /// `SyntaxNode::Inner`, which groups a contiguous sequence of child nodes.
    pub(crate) nodes: Vec<SyntaxNode>,

    /// The byte offset of the last consumed token in the input text.
    pub(crate) last_pos: usize,

    /// Memoization table used to support efficient backtracking and recovery.
    memo: MemoArena,
}

#[derive(Debug, Clone)]
pub struct CheckPoint {
    pub(crate) nodes_len: usize,
    lexer_cursor: usize,
    token: Token,
}

/// Represents the result of a fallible parsing expectation.
///
/// `ExpectResult` is returned when the parser tries to match an expected syntax element
/// but may fail. If the expectation is not met, a [SyntaxError] is inserted into the CST,
/// allowing error recovery without aborting parsing.
///
/// This allows for post-processing of errors or transforming them inline.
///
/// ## Variants
/// - `Ok`: The expected syntax was found and consumed successfully.
/// - `SyntaxError`: An error occurred, and a [SyntaxError] was inserted into the CST.
pub enum ExpectResult<'a> {
    Ok,
    SyntaxError(&'a mut SyntaxError),
}

impl<'a> ExpectResult<'a> {
    pub(crate) fn is_ok(&self) -> bool {
        matches!(self, ExpectResult::Ok)
    }
}

impl ExpectResult<'_> {
    /// Applies a function to the inner `SyntaxError` if one exists.
    ///
    /// This is useful for annotating or augmenting syntax errors in-place.
    pub fn map(self, f: impl FnOnce(&mut SyntaxError)) -> Self {
        match self {
            Self::Ok => self,
            Self::SyntaxError(err) => {
                f(err);
                Self::SyntaxError(err)
            }
        }
    }
}

// Error related methods
impl<'s> Parser<'s> {
    /// Asserts that the current token is of the expected kind and consumes it.
    ///
    /// Panics if the current token does not match `kind`. Use for invariant assumptions.
    #[track_caller]
    pub(crate) fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind, "Expected {:?}", kind);
        self.eat();
    }

    /// Inserts a syntax error node before the current token position.
    ///
    /// The span will be 0 sized.
    pub(crate) fn insert_error_before(
        &mut self,
        message: impl Into<EcoString>,
    ) -> &mut SyntaxError {
        let (span, text) = match self.last_node() {
            Some(v) => (v.span().after(), ""),
            None => (self.token.node.span(), ""),
        };
        let error = SyntaxNode::error(SyntaxError::new(message.into(), span), text);

        trace_log!("inserting error: {:?}", error);
        self.nodes.push(error);

        self.last_err().unwrap()
    }

    /// Inserts a syntax error node at the current token position.
    ///
    /// The error message is stored and will be included in the final CST as a [SyntaxNode::Error].
    /// Returns a mutable reference to the inserted error for immediate editing or tagging.
    pub(crate) fn insert_error_here(&mut self, message: impl Into<EcoString>) -> &mut SyntaxError {
        let error = SyntaxNode::error(
            SyntaxError::new(message.into(), self.token.node.span()),
            self.token.node.text(),
        );
        trace_log!("inserting error: {:?}", error);
        self.nodes.push(error);

        self.last_err().unwrap()
    }

    pub(crate) fn insert_error(&mut self, error: SyntaxError) -> &mut SyntaxError {
        trace_log!("inserting error: {:?}", error);
        let span = error.span;

        let error = SyntaxNode::error(
            error,
            span.range()
                .and_then(|r| self.get_text(r))
                .unwrap_or_default(),
        );
        self.nodes.push(error);

        self.last_err().unwrap()
    }

    /// Returns a mutable reference to the error at the given `Marker` if it exists.
    pub(crate) fn err_at(&mut self, at: Marker) -> Option<&mut SyntaxError> {
        self.nodes.get_mut(at.0).and_then(|n| n.error_mut())
    }

    /// Returns the most recently inserted syntax error node in the parser.
    pub fn last_err(&mut self) -> Option<&mut SyntaxError> {
        self.nodes.iter_mut().rev().find_map(|n| n.error_mut())
    }

    /// Emits a generic `expected ... got ...` syntax error for the current token.
    ///
    /// Example: `expected "Ident", got "Comma"`
    #[track_caller]
    pub(crate) fn expected(&mut self, expected: &str) {
        let kind = self.current();
        self.insert_error_here(eco_format!("expected {expected}, got {kind:?}"));
    }

    /// Emits a custom error message for an unexpected token and optionally recovers.
    ///
    /// If `recover_set` is provided, the parser will skip tokens until it finds a token
    /// in the recovery set or reaches EOF. Returns the inserted [SyntaxError].
    pub(crate) fn unexpected(
        &mut self,
        message: impl Into<EcoString>,
        recover_set: Option<SyntaxSet>,
    ) -> &mut SyntaxError {
        self.balanced &= !self.token.kind.is_grouping();
        let message = message.into();
        trace_log!("error_unexpected: {}", message);
        self.token.node.convert_to_error(match message.as_str() {
            "" => eco_format!("unexpected token {:?}", self.token.kind),
            _ => message,
        });

        let err_marker = self.marker();

        self.eat();

        if let Some(recover_set) = recover_set {
            self.recover_until(recover_set);
        }

        self.err_at(err_marker).expect("An error was just inserted")
    }

    /// Consumes the current token if it matches `kind`. Otherwise inserts an error.
    ///
    /// Returns `true` if the expected token was present; otherwise returns `false`.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        } else if kind == SyntaxKind::Ident && self.token.kind.is_keyword() {
            self.token.node.expected(eco_format!("{kind:?}"));
        } else {
            self.balanced &= !kind.is_grouping();
            self.expected(&format!("{kind:?}"));
        }
        at
    }

    /// Like `expect`, but attempts to recover by consuming the current token if it matches
    /// a recovery set. Returns `true` if the expected token or a recovery token was consumed.
    pub(crate) fn expect_or_recover(
        &mut self,
        expected: SyntaxKind,
        recover_set: SyntaxSet,
    ) -> bool {
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

    /// Returns true if the current token is in the recovery set or if a newline
    /// can act as a soft boundary and is considered recoverable.
    fn can_recover_with(&self, recover_set: SyntaxSet) -> bool {
        recover_set.contains(self.current())
            || (recover_set.contains(SyntaxKind::NewLine) && self.had_leading_newline())
    }

    /// Like `expect_or_recover`, but returns an [ExpectResult] for further error handling.
    ///
    /// An error node is inserted if the expected token is not found.
    /// The parser then skips forward until a token in the recovery set is found.
    pub(crate) fn expect_or_recover_until(
        &mut self,
        expected: SyntaxKind,
        message: impl Into<EcoString>,
        recover_set: SyntaxSet,
    ) -> ExpectResult {
        if self.at(expected) {
            self.eat();
            return ExpectResult::Ok;
        }

        self.insert_error_here(message);

        self.recover_until(recover_set);
        if self.current() == expected {
            self.eat();
        }
        ExpectResult::SyntaxError(self.last_err().expect("An error was just inserted"))
    }

    /// Skips tokens until a token in the `recovery_set` is found or EOF is reached.
    ///
    /// Used in combination with error reporting for safe continuation.
    ///
    /// ### Behaviour
    ///
    /// Stops advancing when the current token is in the recovery set or at EOF.
    pub(crate) fn recover_until(&mut self, recovery_set: SyntaxSet) {
        while !self.can_recover_with(recovery_set) && !self.end() {
            self.eat();
        }
    }

    /// skips tokens until the given node, based on its span
    pub(crate) fn recover_until_node(&mut self, node: &SyntaxNode) {
        while !self.end() {
            if self.token.node.span() == node.span() {
                break;
            }
            self.eat();
        }
    }

    /// Tries to consume a closing delimiter like `}` or `)` or reports a matching open delimiter as unclosed.
    ///
    /// This is used for detecting unbalanced groupings.
    /// If the current token is a closing delimiter, it is consumed; otherwise, an error is inserted
    /// pointing to the `open_marker` as the source of the unclosed delimiter.
    #[track_caller]
    pub(crate) fn expect_closing_delimiter(
        &mut self,
        open_marker: Marker,
        expected_closing: SyntaxKind,
    ) -> bool {
        debug_assert!(expected_closing.is_grouping());
        if self.eat_if(expected_closing) {
            return true;
        }

        err_unclosed_delim(self, open_marker, expected_closing);

        false
    }
}

#[derive(Debug, Default)]
struct MemoArena {
    nodes: Vec<SyntaxNode>,
    memo_map: HashMap<MemoKey, (Range<usize>, CheckPoint)>,
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
            memo: Default::default(),
        }
    }

    pub fn scanner(&self) -> Scanner<'s> {
        Scanner::new(self.lexer.clone()).with_offset(self.token.start)
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

    pub(crate) fn last_text(&self) -> &'s str {
        let Some(last) = self.nodes.last() else {
            return "";
        };
        let Some(range) = last.span().range() else {
            return "";
        };

        self.get_text(range).unwrap_or_default()
    }

    pub(crate) fn last_node(&self) -> Option<&SyntaxNode> {
        self.nodes.last()
    }

    pub(crate) fn get_text(&self, range: Range<usize>) -> Option<&'s str> {
        self.text.get(range)
    }

    pub(crate) fn current_text(&self) -> &'s str {
        match self.token.node.span().range() {
            Some(s) => self.text.get(s).expect("text should exist"),
            None => &self.text[self.token.start..self.current_end()],
        }
    }

    #[inline]
    pub(crate) fn current_node(&self) -> &SyntaxNode {
        &self.token.node
    }

    #[inline]
    pub(crate) fn current_span(&self) -> Span {
        self.current_node().span()
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

    pub(crate) fn peek_at(&self, kind: SyntaxKind) -> bool {
        self.peek() == kind
    }

    pub(crate) fn peek_at_set(&self, set: SyntaxSet) -> bool {
        set.contains(self.peek())
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

    /// A marker that will point to the current token in the parser once it has been eaten.
    pub(crate) fn marker(&self) -> Marker {
        Marker(self.nodes.len())
    }

    fn had_leading_newline(&self) -> bool {
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

    fn lex(lexer: &mut Lexer) -> Token {
        let prev_end = lexer.cursor();
        let start = prev_end;
        let (mut kind, mut node) = lexer.next();

        while kind == SyntaxKind::Comment {
            (kind, node) = lexer.next();
        }

        Token {
            kind,
            node,
            newline: lexer.newline(),
            start,
            prev_end,
        }
    }

    pub(crate) fn checkpoint(&self) -> CheckPoint {
        trace_log!("Creating checkpoint: {}", self.nodes.len());
        CheckPoint {
            nodes_len: self.nodes.len(),
            lexer_cursor: self.lexer.cursor(),
            token: self.token.clone(),
        }
    }

    pub(crate) fn restore(&mut self, checkpoint: CheckPoint) {
        trace_log!("Restoring checkpoint: {}", checkpoint.nodes_len);
        self.nodes.truncate(checkpoint.nodes_len);
        self.restore_partial(checkpoint);
    }

    pub(crate) fn restore_partial(&mut self, checkpoint: CheckPoint) {
        self.lexer.jump(checkpoint.lexer_cursor);
        self.token = checkpoint.token;
    }

    pub(crate) fn restore_memo_or_checkpoint(&mut self) -> Option<(MemoKey, CheckPoint)> {
        let key: MemoKey = self.current_start();
        match self.memo.memo_map.get(&key).cloned() {
            Some((range, checkpoint)) => {
                // restore the memo
                self.nodes.extend_from_slice(&self.memo.nodes[range]);
                self.restore_partial(checkpoint);
                None
            }
            None => Some((key, self.checkpoint())),
        }
    }

    pub(crate) fn memoize_parsed_nodes(&mut self, key: MemoKey, prev_len: usize) {
        let prev_memo_len = self.memo.nodes.len();

        self.memo.nodes.extend_from_slice(&self.nodes[prev_len..]);
        let checkpoint = self.checkpoint();
        self.memo
            .memo_map
            .insert(key, (prev_memo_len..self.memo.nodes.len(), checkpoint));
    }

    fn current_start(&self) -> MemoKey {
        self.token.start
    }
}

type MemoKey = usize;

// Represents a node's position in the parser.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Marker(pub(crate) usize);

pub(crate) struct SyntaxKindIter<'s> {
    lexer: Lexer<'s>,
    yielded_at_end: bool,
}

impl<'a> SyntaxKindIter<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            yielded_at_end: false,
        }
    }
}

impl<'s> Iterator for SyntaxKindIter<'s> {
    type Item = SyntaxKind;

    fn next(&mut self) -> Option<Self::Item> {
        if self.yielded_at_end {
            return None;
        }
        let (kind, _) = self.lexer.next();
        if kind == SyntaxKind::End {
            self.yielded_at_end = true;
        }
        Some(kind)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) kind: SyntaxKind,
    pub(crate) node: SyntaxNode,
    // Whether the preceding token had a trailing newline
    pub(crate) newline: bool,

    pub(crate) start: usize,

    // The index into `text` of the end of the previous token
    pub(crate) prev_end: usize,
}
