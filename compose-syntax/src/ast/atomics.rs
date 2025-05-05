use crate::SyntaxNode;
use crate::ast::Expr;
use crate::ast::macros::node;
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
    pub fn exprs(self) -> impl DoubleEndedIterator<Item = Expr<'a>> {
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
                        Option::None => out.push_str(s.from(start)),
                    }
                }
                _ => out.push_str(s.from(start)),
            }
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kind::SyntaxKind;
    use crate::test_utils::test_parse;

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
