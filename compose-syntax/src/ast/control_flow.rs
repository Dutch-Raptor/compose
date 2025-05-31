use crate::ast::macros::node;
use crate::ast::{CodeBlock, Expr};
use crate::SyntaxNode;

node! {
    struct Conditional
}

impl<'a> Conditional<'a> {
    pub fn condition(self) -> Condition<'a> {
        self.0.cast_first()
    }
    
    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
    
    pub fn cond_alternates(self) -> impl DoubleEndedIterator<Item = ConditionalAlternate<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
    
    pub fn cond_else(self) -> Option<ConditionalElse<'a>> {
        self.0.try_cast_last()
    }
}

node! {
    struct ConditionalAlternate
}
impl<'a> ConditionalAlternate<'a> {
    pub fn condition(self) -> Condition<'a> {
        self.0.cast_first()
    }

    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
}

node! {
    struct ConditionalElse
}

impl<'a> ConditionalElse<'a> {
    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
}

node! {
    struct Condition
}

impl<'a> Condition<'a> {
    pub fn expr(self) -> Expr<'a> {
        self.0.cast_first()
    }
}