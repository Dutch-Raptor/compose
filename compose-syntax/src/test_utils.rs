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
    dbg!(&nodes);
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
        let node = self.assert_next_node();

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

        self
    }

    #[track_caller]
    fn assert_next_node(&mut self) -> SyntaxNode {
        let node = self.nodes.get(self.pos).cloned().unwrap_or_else(|| panic!("Expected a node at {:?}. Out of nodes!",
            self.path));
        self.pos += 1;
        node
    }

    #[track_caller]
    pub fn assert_next_warning(&mut self, warning: ErrorCode) -> &mut Self {
        let node = self.nodes.get(self.pos).expect("No more nodes");

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

    #[track_caller]
    pub fn assert_next_error(&mut self, error: ErrorCode) -> &mut Self {
        let node = self.nodes.get(self.pos).expect("No more nodes");

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
        let node = self.nodes.get(self.pos).expect("No more nodes");

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
            "Not all nodes were consumed. at {:?}",
            self.path
        );
    }
}
