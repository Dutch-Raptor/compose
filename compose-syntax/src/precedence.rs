#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Range,       // .. or ..=
    LogicalOr,   // ||
    LogicalAnd,  // &&
    Equals,      // ==
    LessGreater, // > or <
    BitwiseOr,   // |
    BitwiseXor,  // ^
    BitwiseAnd,  // &
    BitShift,    // << or >>
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -x or !x
    Index,       // array[index]
    Call,        // myFunction(X)
    Member,      // foo.member
}

pub trait PrecedenceTrait {
    fn precedence(&self) -> Precedence;
}
