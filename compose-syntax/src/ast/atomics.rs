use std::ops::Deref;
use ecow::EcoString;
use crate::ast::Expr;
use crate::ast::macros::node;
use crate::SyntaxNode;

node! {
    struct Ident
}

impl<'a> Ident<'a> {
    pub fn get(self) -> &'a EcoString {
        self.0.text()
    }

    pub fn as_str(self) -> &'a str {
        self.get()
    }
}

impl Deref for Ident<'_> {
    type Target = str;

    /// Dereference to a string. Note that this shortens the lifetime, so you
    /// may need to use [`get()`](Self::get) instead in some situations.
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

node! {
    /// A boolean: `true`, `false`.
    struct Bool
}

impl Bool<'_> {
    /// Get the boolean value.
    pub fn get(self) -> bool {
        self.0.text() == "true"
    }
}

node! {
    struct Int
}

impl Int<'_> {
    pub fn get(self) -> i64 {
        self.0.text().parse().unwrap()
    }
}


node! {
    struct Unit
}

node! {
    struct CodeBlock
}

impl<'a> CodeBlock<'a> {
    pub fn exprs(self) -> impl DoubleEndedIterator<Item = Expr<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

#[cfg(test)]
mod tests {
    use crate::kind::SyntaxKind;
    use crate::test_utils::test_parse;
    use super::*;
    
    #[test]
    fn parse_ident() {
        let parsed = test_parse("abc");
        assert_eq!(parsed.len(), 1);
        let first = &parsed[0];
        assert_eq!(first.kind(), SyntaxKind::Ident);
        let ident: Ident = first.cast().unwrap();
        assert_eq!(ident.get(), "abc");
    }
}

