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
    IndexAccess,
    Args,
    PathAccess,
    NewLine,
    Assignment,
    Condition,
    Conditional,
    ConditionalAlternate,
    ConditionalElse,
    Comment,
    DocComment,
    WhileLoop,
    ForLoop,
}

impl SyntaxKind {
    pub(crate) fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::End | Self::Semicolon | Self::RightBrace | Self::RightParen | Self::RightBracket
        )
    }

    pub(crate) fn descriptive_name(&self) -> &'static str {
        match self {
            SyntaxKind::LeftBrace => "{",
            SyntaxKind::LeftParen => "(",
            SyntaxKind::LeftBracket => "[",
            SyntaxKind::RightBrace => "}",
            SyntaxKind::RightParen => ")",
            SyntaxKind::RightBracket => "]",
            SyntaxKind::Eq => "=",
            SyntaxKind::EqEq => "==",
            SyntaxKind::End => "end of file",
            SyntaxKind::DoubleQuote => "\"",
            SyntaxKind::Apostrophe => "'",
            SyntaxKind::Backtick => "`",
            SyntaxKind::Underscore => "_",
            SyntaxKind::Hat => "^",
            SyntaxKind::At => "@",
            SyntaxKind::Bang => "!",
            SyntaxKind::Hash => "#",
            SyntaxKind::Dollar => "$",
            SyntaxKind::Percent => "%",
            SyntaxKind::Slash => "/",
            SyntaxKind::Minus => "-",
            SyntaxKind::Plus => "+",
            SyntaxKind::Star => "*",
            SyntaxKind::Colon => ":",
            SyntaxKind::Semicolon => ";",
            SyntaxKind::Comma => ",",
            SyntaxKind::Arrow => "=>",
            SyntaxKind::ColonColon => "::",
            SyntaxKind::HatEq => "^=",
            SyntaxKind::TildeEq => "~=",
            SyntaxKind::AmpersandEq => "&=",
            SyntaxKind::AmpersandAmpersand => "&&",
            SyntaxKind::PipeEq => "|=",
            SyntaxKind::PipePipe => "||",
            SyntaxKind::GtEq => ">=",
            SyntaxKind::GtGt => ">>",
            SyntaxKind::LtEq => "<=",
            SyntaxKind::LtLt => "<<",
            SyntaxKind::Dots => "..",
            SyntaxKind::DotsEq => "..=",
            SyntaxKind::Ellipsis => "...",
            SyntaxKind::SlashEq => "/=",
            SyntaxKind::StarEq => "*=",
            SyntaxKind::MinusEq => "-=",
            SyntaxKind::PlusEq => "+=",
            SyntaxKind::ExclEq => "!=",
            SyntaxKind::Float => "float literal",
            SyntaxKind::Int => "integer literal",
            SyntaxKind::Return => "return",
            SyntaxKind::Pub => "pub",
            SyntaxKind::Mut => "mut",
            SyntaxKind::Loop => "loop",
            SyntaxKind::LetBinding => "let binding",
            SyntaxKind::In => "in",
            SyntaxKind::Import => "import",
            SyntaxKind::If => "if",
            SyntaxKind::For => "for",
            SyntaxKind::Enum => "enum",
            SyntaxKind::Else => "else",
            SyntaxKind::Continue => "continue",
            SyntaxKind::Break => "break",
            SyntaxKind::As => "as",
            SyntaxKind::Unit => "()",
            SyntaxKind::Ident => "identifier",
            SyntaxKind::Error => "error",
            SyntaxKind::Str => "string literal",
            SyntaxKind::Tilde => "~",
            SyntaxKind::Ampersand => "&",
            SyntaxKind::Pipe => "|",
            SyntaxKind::Gt => ">",
            SyntaxKind::Lt => "<",
            SyntaxKind::BangEq => "!=",
            SyntaxKind::Bool => "boolean literal",
            SyntaxKind::Unary => "unary expression",
            SyntaxKind::Params => "parameter list",
            SyntaxKind::Closure => "closure",
            SyntaxKind::DestructureAssignment => "destructuring assignment",
            SyntaxKind::Named => "named binding",
            SyntaxKind::Destructuring => "destructuring",
            SyntaxKind::CodeBlock => "code block",
            SyntaxKind::Parenthesized => "parenthesized expression",
            SyntaxKind::Let => "let",
            SyntaxKind::While => "while",
            SyntaxKind::Binary => "binary expression",
            SyntaxKind::Dot => ".",
            SyntaxKind::FuncCall => "function call",
            SyntaxKind::FieldAccess => "field access",
            SyntaxKind::IndexAccess => "index access",
            SyntaxKind::Args => "argument list",
            SyntaxKind::PathAccess => "path",
            SyntaxKind::NewLine => "newline",
            SyntaxKind::Assignment => "assignment",
            SyntaxKind::Conditional => "if expression",
            SyntaxKind::ConditionalAlternate => "else if expression",
            SyntaxKind::ConditionalElse => "else expression",
            SyntaxKind::Condition => "condition",
            SyntaxKind::Comment => "comment",
            SyntaxKind::DocComment => "doc comment",
            SyntaxKind::WhileLoop => "while loop",
            SyntaxKind::ForLoop => "for loop",
        }
    }

    pub(crate) fn matching_delimiter(&self) -> Option<SyntaxKind> {
        match self {
            Self::LeftBrace => Some(Self::RightBrace),
            Self::LeftParen => Some(Self::RightParen),
            Self::LeftBracket => Some(Self::RightBracket),
            Self::RightBrace => Some(Self::LeftBrace),
            Self::RightParen => Some(Self::LeftParen),
            Self::RightBracket => Some(Self::LeftBracket),
            _ => None,
        }
    }

    pub(crate) fn is_closing_delimiter(&self) -> bool {
        matches!(
            self,
            Self::RightBrace | Self::RightParen | Self::RightBracket
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
