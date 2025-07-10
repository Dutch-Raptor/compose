use crate::kind::SyntaxKind;

#[derive(Default, Copy, Clone)]
pub struct SyntaxSet(u128);

const BITS: u8 = 128;

impl SyntaxSet {
    pub const fn new() -> Self {
        Self(0)
    }

    pub const fn add(self, kind: SyntaxKind) -> Self {
        assert!((kind as u8) < BITS);
        Self(self.0 | bit(kind))
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn contains(self, kind: SyntaxKind) -> bool {
        (kind as u8) < BITS && (self.0 & bit(kind)) != 0
    }
}

const fn bit(kind: SyntaxKind) -> u128 {
    1 << (kind as usize)
}

macro_rules! syntax_set {
    ($($kind:ident),* $(,)?) => {{
        const SET: crate::set::SyntaxSet = crate::set::SyntaxSet::new()
        $(.add(crate::kind::SyntaxKind:: $kind))*;
        SET
    }}
}

pub(crate) use syntax_set;

pub const STMT: SyntaxSet = syntax_set![Let].union(ATOMIC_EXPR);

pub const ARG_RECOVER: SyntaxSet = syntax_set![
    Comma,
    RightParen,
    NewLine,
];

pub const UNARY_OP: SyntaxSet = syntax_set![Plus, Minus, Bang, Tilde, Star];

pub const BINARY_OP: SyntaxSet = syntax_set![
    // Basic arithmetic
    Plus, Minus, Star, Slash, Percent, 
    
    // Bitwise
    Hat, Amp, Pipe,
    
    // Comparison
    Gt, GtEq, Lt, LtEq, EqEq, BangEq,
    
    // Logical
    PipePipe, AmpAmp,
];

pub const ASSIGN_OP: SyntaxSet = syntax_set![Eq, MinusEq, PlusEq, StarEq, SlashEq, AmpersandEq, HatEq, PipeEq, AmpersandEq,];

pub const ATOMIC_EXPR: SyntaxSet = syntax_set![
    Ident,
    LeftBrace,
    LeftBracket,
    LeftParen,
    If,
    While,
    Loop,
    For,
    Break,
    Continue,
    Import,
    Return,
    Unit,
    Int,
    Float,
    Str,
    Bool,
    Hash,
];

pub const EXPR: SyntaxSet = ATOMIC_EXPR.union(UNARY_OP);

pub const PATTERN_LEAF: SyntaxSet = syntax_set![
    Ident,
    Int,
    Float,
    Str,
    Bool,
];

pub const PATTERN: SyntaxSet = syntax_set![
    At,
    Underscore,
    Ident,
];

pub const PARAM: SyntaxSet = PATTERN.union(syntax_set![Ref, Mut]);

pub const CAPTURE: SyntaxSet = syntax_set![Ident, Ref, Mut];

pub const CAPTURE_RECOVER: SyntaxSet = syntax_set![Comma, Pipe].union(CAPTURE);
