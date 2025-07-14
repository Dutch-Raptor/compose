use crate::ast::macros::node;
use crate::ast::{Expr, Statement};
use crate::SyntaxNode;
use ecow::EcoString;
use std::ops::Deref;
use unscanny::Scanner;

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
    pub fn statements(self) -> impl DoubleEndedIterator<Item = Statement<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct Str
}

impl<'a> Str<'a> {
    ///  Get the string value with resolved escape sequences
    pub fn get(self) -> EcoString {
        let text = self.0.text();
        let unquoted = &text[1..text.len() - 1];
        if !unquoted.contains('\\') {
            return unquoted.into();
        }

        let mut out = EcoString::with_capacity(unquoted.len());
        let mut s = Scanner::new(unquoted);

        while let Some(c) = s.eat() {
            if c != '\\' {
                out.push(c);
                continue;
            }

            let start = s.locate(-1);
            match s.eat() {
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                Some('"') => out.push('"'),
                Some('t') => out.push('\t'),
                Some('\\') => out.push('\\'),
                Some('u') if s.eat_if('{') => {
                    let sequence = s.eat_while(char::is_ascii_hexdigit);
                    s.eat_if('}');

                    match u32::from_str_radix(sequence, 16)
                        .ok()
                        .and_then(std::char::from_u32)
                    {
                        Some(c) => out.push(c),
                        None => out.push_str(s.from(start)),
                    }
                }
                _ => out.push_str(s.from(start)),
            }
        }

        out
    }
}

node! {
    struct Array
}

impl<'a> Array<'a> {
    pub fn elements(self) -> impl DoubleEndedIterator<Item = Expr<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ast;

    #[test]
    fn parse_ident() {
        assert_ast!(
            "foo",
            ident as Ident {
                assert_eq!(ident.get(), "foo");
            }
        )
    }

    #[test]
    fn parse_bool() {
        assert_ast!(
            "true",
            bool as Bool {
                assert_eq!(bool.get(), true);
            }
        )
    }

    #[test]
    fn parse_int() {
        assert_ast!(
            "123",
            int as Int {
                assert_eq!(int.get(), 123);
            }
        )
    }

    #[test]
    fn parse_str() {
        assert_ast!(
            "\"foo\"",
            str as Str {
                assert_eq!(str.get(), "foo");
            }
        )
    }

    #[test]
    fn parse_array() {
        assert_ast!(
            "[1, 2, 3]",
            array as Array {
                array.elements() => [
                    int as Int { assert_eq!(int.get(), 1); }
                    int as Int { assert_eq!(int.get(), 2); }
                    int as Int { assert_eq!(int.get(), 3); }
                ]
            }
        )
    }

    #[test]
    fn parse_code_block() {
        assert_ast!(
            "{ 1; 2; 3; }",
            code_block as CodeBlock {
                code_block.statements() => [
                    int as Int { assert_eq!(int.get(), 1); }
                    int as Int { assert_eq!(int.get(), 2); }
                    int as Int { assert_eq!(int.get(), 3); }
                ]
            }
        )
    }

    #[test]
    fn parse_unit() {
        assert_ast!(
            "()",
            _unit as Unit {}
        )
    }
}
