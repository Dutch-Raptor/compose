use crate::SyntaxNode;
use crate::file::FileId;
use crate::kind::SyntaxKind;
use compose_error_codes::ErrorCode;
use extension_traits::extension;
use std::num::NonZeroU16;

/// Creates a non-interned file id for testing.
pub(crate) const fn test_file_id() -> FileId {
    FileId::from_raw(NonZeroU16::new(1).unwrap())
}

pub fn test_parse(code: &str) -> Vec<SyntaxNode> {
    let file_id = FileId::new("main.comp");
    let nodes = crate::parse(code, file_id);
    nodes
}

#[track_caller]
pub fn assert_parse(code: &str) -> NodesTester {
    let nodes = test_parse(code);

    let errors = nodes
        .iter()
        .flat_map(|node| node.errors())
        .map(|error| error.code)
        .collect::<Vec<_>>();

    assert_eq!(errors, vec![]);

    NodesTester::new(nodes)
}

pub fn assert_parse_with_warnings(code: &str, expected_warnings: &[ErrorCode]) -> NodesTester {
    let nodes = test_parse(code);
    let actual = nodes
        .iter()
        .flat_map(|node| node.warnings())
        .map(|warning| warning.code)
        .collect::<Vec<_>>();

    assert_eq!(actual.len(), expected_warnings.len());

    for (actual, expected) in actual.iter().zip(expected_warnings.iter()) {
        assert_eq!(actual, &Some(expected));
    }

    let errors = nodes
        .iter()
        .flat_map(|node| node.errors())
        .map(|error| error.code)
        .collect::<Vec<_>>();

    assert_eq!(errors, vec![]);

    NodesTester::new(nodes)
}

pub fn assert_parse_with_errors(code: &str, expected_errors: &[ErrorCode]) -> NodesTester {
    let nodes = test_parse(code);
    let actual = nodes
        .iter()
        .flat_map(|node| node.errors())
        .map(|error| error.code)
        .collect::<Vec<_>>();

    assert_eq!(actual.len(), expected_errors.len());

    for (actual, expected) in actual.iter().zip(expected_errors.iter()) {
        assert_eq!(actual, &Some(expected));
    }

    NodesTester::new(nodes)
}

#[extension(trait SyntaxNodeExt)]
impl SyntaxNode {
    #[track_caller]
    fn test_assert(&self, kind: SyntaxKind, text: &str) {
        assert_eq!(
            self.kind(),
            kind,
            "expected: {:?}, got: {:?}",
            kind,
            self.kind()
        );
        assert_eq!(
            self.text(),
            text,
            "expected: {:?}, got: {:?}",
            text,
            self.text()
        );
    }

    #[track_caller]
    fn test_children(&self, kind: SyntaxKind) -> NodesTester {
        assert_eq!(self.kind(), kind);

        let children = self.children().cloned().collect::<Vec<_>>();
        NodesTester::new(children)
    }
}

pub struct NodesTester {
    path: Vec<SyntaxKind>,
    pub nodes: Vec<SyntaxNode>,
    pos: usize,
}

impl NodesTester {
    pub fn new(nodes: Vec<SyntaxNode>) -> Self {
        Self {
            nodes,
            pos: 0,
            path: vec![],
        }
    }

    pub fn with_path(mut self, path: Vec<SyntaxKind>) -> Self {
        self.path = path;
        self
    }

    #[track_caller]
    pub fn assert_next(&mut self, kind: SyntaxKind, text: &str) -> &mut Self {
        let node = self.nodes.get(self.pos).or_else(|| panic!("No more nodes at {:?}. Expected: {kind:?}", self.path)).cloned().unwrap();

        assert_eq!(
            node.kind(),
            kind,
            "expected: {:?}, got: {:?} at {:?} ({})",
            kind,
            node.kind(),
            self.path,
            node.to_text(),
        );
        assert_eq!(
            node.text(),
            text,
            "expected: {:?}, got: {:?} at {:?}",
            text,
            node.text(),
            self.path
        );
        
        self.pos += 1;

        self
    }


    #[track_caller]
    pub fn assert_next_warning(&mut self, warning: ErrorCode) -> &mut Self {
        let node = self.nodes.get(self.pos).or_else(|| panic!("No more nodes at {:?}. Expected warning: {warning:?}", self.path)).cloned().unwrap();

        assert_eq!(
            node.kind(),
            SyntaxKind::Error,
            "Expected an error, got {:?} at {:?}",
            node.kind(),
            self.path
        );

        let warnings = node.warnings();
        assert_eq!(
            warnings.len(),
            1,
            "Expected an error, got {} warnings at {:?}",
            warnings.len(),
            self.path
        );
        assert_eq!(
            warnings[0].code,
            Some(&warning),
            "Expected an error with code {:?}, got {:?} at {:?}",
            warning,
            warnings[0].code,
            self.path
        );

        self.pos += 1;

        self
    }

    pub fn move_to_end(&mut self) {
        self.pos = self.nodes.len();
    }

    #[track_caller]
    pub fn assert_next_error(&mut self, error: ErrorCode) -> &mut Self {
        let node = self.nodes.get(self.pos).or_else(|| panic!("No more nodes at {:?}. Expected error: {error:?}", self.path)).cloned().unwrap();

        assert_eq!(
            node.kind(),
            SyntaxKind::Error,
            "Expected an error, got {:?} at {:?}",
            node.kind(),
            self.path
        );

        let errors = node.errors();
        assert_eq!(
            errors.len(),
            1,
            "Expected an error, got {} errors at {:?}",
            errors.len(),
            self.path
        );
        assert_eq!(
            errors[0].code,
            Some(&error),
            "Expected an error with code {:?}, got {:?} at {:?}",
            error,
            errors[0].code,
            self.path
        );

        self.pos += 1;

        self
    }

    #[track_caller]
    pub fn assert_next_children(
        &mut self,
        kind: SyntaxKind,
        test_children: impl FnOnce(&mut Self),
    ) -> &mut Self {
        let node = self.nodes.get(self.pos).or_else(|| panic!("No more nodes at {:?}. Expected: {kind:?}", self.path)).cloned().unwrap();

        assert_eq!(
            node.kind(),
            kind,
            "expected: {:?}, got: {:?} at {:?} ({})",
            kind,
            node.kind(),
            self.path,
            node.to_text(),
        );

        let children = node.children().cloned().collect::<Vec<_>>();
        let mut tester = NodesTester::new(children)
            .with_path(self.path.iter().copied().chain(vec![kind]).collect());

        test_children(&mut tester);
        self.pos += 1;

        self
    }

    #[track_caller]
    pub fn assert_end(&self) {
        assert_eq!(
            self.pos,
            self.nodes.len(),
            "Not all nodes were consumed. at {:?}. Remaining: {:#?}",
            self.path,
            &self.nodes[self.pos..]
        );
    }
}


/// Macro to assert the structure and content of the parsed syntax tree from source code.
///
/// # Purpose
///
/// `assert_parse_tree!` allows you to declaratively specify the expected abstract syntax tree (AST)
/// shape and tokens produced by your parser for a given source code snippet. It recursively
/// matches nodes by their kinds (`SyntaxKind`) and token text, verifying both the hierarchical
/// structure and token lexemes precisely.
///
/// This macro is primarily useful in parser/unit tests where you want to:
/// - Assert nodes and their nested children are present in expected order.
/// - Assert leaf tokens have expected text content.
/// - Assert presence of parser errors and warnings at specific nodes.
///
///
/// # Syntax Overview
///
/// ```ignore
/// assert_parse_tree!(
///     "source code string",
///     RootNodeKind [            // Node with children
///         ChildNodeKind1 ( "token_text" )  // Leaf node with exact text
///         ChildNodeKind2 [                  // Nested node with children
///             ...
///         ]
///         Warn(WARNING_CODE)               // Warning node with an error code enum or const
///         Error(ERROR_CODE)                // Error node with an error code enum or const
///         ...                             // Ellipsis to skip remaining children in this subtree
///     ]
/// );
/// ```
///
/// - **Root node**: The first argument is the source code string to parse.
/// - **Nodes with children**: Use `KindName [ ... ]` where `KindName` is a variant of your `SyntaxKind` enum.
/// - **Leaf tokens**: Use `KindName("token_text")` where the string is the exact matched source token text.
/// - **Warnings and errors**: Use `Warn(WARNING_CODE)` and `Error(ERROR_CODE)` where the code is a known error/warning enum or constant.
/// - **Ellipsis (`...`)**: Use to ignore remaining children in a node subtree (useful to reduce test verbosity).
///
///
/// # Notes
///
/// - `KindName` must be a variant of your `SyntaxKind` enum (without the `SyntaxKind::` prefix).
/// - Leaf nodes *must* specify the exact token text as a string literal.
/// - `Warn` and `Error` are reserved special nodes recognized by the macro for diagnostics.
/// - The macro performs strict ordering checks: children must appear in exact order as specified.
///
///
/// # Example: Simple function call
///
/// ```rust
/// compose_syntax::test_utils::assert_parse_tree!(
///     "f(a, b: c)",
///     FuncCall [
///         Ident("f")
///         Args [
///             LeftParen("(")
///             Ident("a")
///             Comma(",")
///             Named [ Ident("b") Colon(":") Ident("c") ]
///             RightParen(")")
///         ]
///     ]
/// );
/// ```
///
/// # Example: Error recovery with warnings and errors
///
/// ```rust
/// compose_syntax::test_utils::assert_parse_tree!(
///     r#"
///     f(a, b, c
///     1 + 2
///     "#,
///     FuncCall [
///         Ident("f")
///         Args [
///             Error(compose_error_codes::E0001_UNCLOSED_DELIMITER)
///             Ident("a")
///             Comma(",")
///             Ident("b")
///             Comma(",")
///             Ident("c")
///             Error(compose_error_codes::E0009_ARGS_MISSING_COMMAS)
///             Binary [
///                 Int("1")
///                 Plus("+")
///                 Int("2")
///             ]
///         ]
///     ]
/// );
/// ```
///
/// # Example: Skipping subtree content
///
/// ```rust
/// compose_syntax::test_utils::assert_parse_tree!(
///     "(ref mut a) => {}",
///     Closure [
///         Params [
///             LeftParen("(")
///             Param [
///                 Ref("ref")
///                 Mut("mut")
///                 Ident("a")
///             ]
///             RightParen(")")
///         ]
///         ...
///     ]
/// );
/// ```
///
/// # Common mistakes
///
/// - Forgetting to provide the exact token text string for leaf nodes results in a compile error.
/// - Using `Warn` or `Error` as `SyntaxKind` variants inside children nodes causes errors; they must use the special macro arms.
/// # See also
/// - [`SyntaxKind`] enum variants — the node/kind names used in this macro.
/// - Parser error codes for `Warn` and `Error` nodes.
/// - [`NodesTester`] struct for underlying test traversal and assertions.
#[macro_export]
macro_rules! assert_parse_tree {
    // Entry point
    ($src:expr, $($tree:tt)+) => {{
        let nodes = $crate::test_utils::test_parse($src);
        let mut p = $crate::test_utils::NodesTester::new(nodes);
        $crate::test_utils::assert_parse_tree!(@seq p, $($tree)+);
    }};

    // Warning node
    (@seq $parser:ident, Warn ( $code:expr ) $($rest:tt)*) => {
        $parser.assert_next_warning($code);
        $crate::test_utils::assert_parse_tree!(@seq $parser, $($rest)*);
    };

    // Error node
    (@seq $parser:ident, Error ( $code:expr ) $($rest:tt)*) => {
        $parser.assert_next_error($code);
        $crate::test_utils::assert_parse_tree!(@seq $parser, $($rest)*);
    };

    // Node with children
    (@seq $parser:ident, $kind:ident [ $($children:tt)+ ] $($rest:tt)*) => {
        $parser.assert_next_children($crate::SyntaxKind::$kind, |p| {
            $crate::test_utils::assert_parse_tree!(@seq p, $($children)+);
        });
        $crate::test_utils::assert_parse_tree!(@seq $parser, $($rest)*);
    };


    // Leaf node with string text
    (@seq $parser:ident, $kind:ident ( $text:expr ) $($rest:tt)*) => {
        $parser.assert_next($crate::SyntaxKind::$kind, $text);
        $crate::test_utils::assert_parse_tree!(@seq $parser, $($rest)*);
    };

    // Invalid usage: leaf node without text — catch this as a helpful error
    (@seq $parser:ident, $kind:ident $($rest:tt)*) => {
        compile_error!(concat!("Leaf node `", stringify!($kind), "` must provide a string literal as text, like `", stringify!($kind), "(\"...\")`."))
    };

    // Deliberately empty children
    (@seq $parser:ident, ...) => {
        // ignore children
        $parser.move_to_end();
    };

    // End
    (@seq $parser:ident,) => {
        $parser.assert_end();
    };
}

pub use assert_parse_tree;
