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
    fn test_assert(&self, kind: SyntaxKind, text: &str) {
        assert_eq!(self.kind(), kind);
        assert_eq!(self.text(), text);
    }

    fn test_children(&self, kind: SyntaxKind) -> NodesTester {
        assert_eq!(self.kind(), kind);

        let children = self.children().cloned().collect::<Vec<_>>();
        NodesTester::new(children)
    }
}

pub struct NodesTester {
    pub nodes: Vec<SyntaxNode>,
    pos: usize,
}

impl NodesTester {
    pub fn new(nodes: Vec<SyntaxNode>) -> Self {
        Self { nodes, pos: 0 }
    }

    pub fn assert_next(&mut self, kind: SyntaxKind, text: &str) -> &mut Self {
        let node = self.nodes.get(self.pos).expect("No more nodes");

        node.test_assert(kind, text);

        self.pos += 1;

        self
    }

    pub fn assert_next_warning(&mut self, warning: ErrorCode) -> &mut Self {
        let node = self.nodes.get(self.pos).expect("No more nodes");

        assert_eq!(node.kind(), SyntaxKind::Error);

        let warnings = node.warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].code, Some(&warning));

        self.pos += 1;

        self
    }
    
    pub fn assert_next_error(&mut self, error: ErrorCode) -> &mut Self {
        let node = self.nodes.get(self.pos).expect("No more nodes");
        
        assert_eq!(node.kind(), SyntaxKind::Error);
        
        let errors = node.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, Some(&error));
        
        self.pos += 1;
        
        self   
    }

    pub fn assert_next_children(
        &mut self,
        kind: SyntaxKind,
        test_children: impl FnOnce(&mut Self),
    ) -> &mut Self {
        let node = self.nodes.get(self.pos).expect("No more nodes");

        test_children(&mut node.test_children(kind));
        self.pos += 1;

        self
    }

    pub fn assert_end(&self) {
        assert_eq!(self.pos, self.nodes.len());
    }
}
