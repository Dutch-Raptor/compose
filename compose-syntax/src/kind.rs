#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    End,
    DoubleQuote,
    Apostrophe,
    Backtick,
    Underscore,
    Hat,
    At,
    Bang,
    Hash,
    Dollar,
    Percent,
    Slash,
    Minus,
    Plus,
    Star,
    Colon,
    Semicolon,
    Comma,
    RightParen,
    LeftParen,
    RightBracket,
    LeftBracket,
    RightBrace,
    LeftBrace,
    Arrow,
    ColonColon,
    HatEq,
    TildeEq,
    AmpersandEq,
    AmpersandAmpersand,
    PipeEq,
    PipePipe,
    GtEq,
    GtGt,
    LtEq,
    LtLt,
    Dots,
    DotsEq,
    Ellipsis,
    SlashEq,
    StarEq,
    MinusEq,
    PlusEq,
    ExclEq,
    EqEq,
    Float,
    Int,
    Return,
    Pub,
    Mut,
    Loop,
    LetBinding,
    In,
    Import,
    If,
    For,
    Enum,
    Else,
    Continue,
    Break,
    As,
    Unit,
    Ident,
    Error,
    Str,
    Tilde,
    Ampersand,
    Pipe,
    Gt,
    Lt,
    BangEq,
    Eq,
    Bool,
    Unary,
    Params,
    Closure,
    DestructureAssignment,
    Named,
    Destructuring,
    CodeBlock,
    Parenthesized,
    Let,
    While,
    Binary,
    Dot,
    FuncCall,
    FieldAccess,
}

impl SyntaxKind {
    pub(crate) fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::End | Self::Semicolon | Self::RightBrace | Self::RightParen | Self::RightBracket
        )
    }
    pub(crate) fn is_grouping(&self) -> bool {
        matches!(
            self,
            Self::LeftParen
                | Self::LeftBrace
                | Self::LeftBracket
                | Self::RightBrace
                | Self::RightParen
                | Self::RightBracket
        )
    }
    pub(crate) fn is_keyword(&self) -> bool {
        matches!(
            self,
            Self::Pub
                | Self::Mut
                | Self::Loop
                | Self::LetBinding
                | Self::In
                | Self::Import
                | Self::If
                | Self::For
                | Self::Enum
                | Self::Else
                | Self::Continue
                | Self::Break
                | Self::As
                | Self::Unit
                | Self::Return
                | Self::Let
                | Self::While
        )
    }
}
