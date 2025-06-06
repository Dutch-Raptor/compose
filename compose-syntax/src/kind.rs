#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    Ampersand,
    AmpersandAmpersand,
    AmpersandEq,
    Apostrophe,
    Args,
    Arrow,
    As,
    Assignment,
    At,
    Backtick,
    Bang,
    BangEq,
    Binary,
    Bool,
    Break,
    Closure,
    CodeBlock,
    Colon,
    ColonColon,
    Comma,
    Comment,
    Condition,
    Conditional,
    ConditionalAlternate,
    ConditionalElse,
    Continue,
    DestructureAssignment,
    Destructuring,
    DocComment,
    Dollar,
    Dot,
    Dots,
    DotsEq,
    DoubleQuote,
    Ellipsis,
    Else,
    End,
    Enum,
    Eq,
    EqEq,
    Error,
    FieldAccess,
    Float,
    For,
    ForLoop,
    FuncCall,
    Gt,
    GtEq,
    GtGt,
    Hash,
    Hat,
    HatEq,
    Ident,
    If,
    Import,
    In,
    IndexAccess,
    Int,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Let,
    LetBinding,
    Loop,
    Lt,
    LtEq,
    LtLt,
    Minus,
    MinusEq,
    Mut,
    Named,
    NewLine,
    Param,
    Params,
    Parenthesized,
    PathAccess,
    Percent,
    Pipe,
    PipeEq,
    PipePipe,
    Plus,
    PlusEq,
    Pub,
    Return,
    Ref,
    RightBrace,
    RightBracket,
    RightParen,
    Semicolon,
    Slash,
    SlashEq,
    Star,
    StarEq,
    Str,
    Tilde,
    TildeEq,
    Unary,
    Underscore,
    Unit,
    While,
    WhileLoop,
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
            SyntaxKind::Ampersand => "&",
            SyntaxKind::AmpersandAmpersand => "&&",
            SyntaxKind::AmpersandEq => "&=",
            SyntaxKind::Apostrophe => "'",
            SyntaxKind::Args => "argument list",
            SyntaxKind::Arrow => "=>",
            SyntaxKind::As => "as",
            SyntaxKind::Assignment => "assignment",
            SyntaxKind::At => "@",
            SyntaxKind::Backtick => "`",
            SyntaxKind::Bang => "!",
            SyntaxKind::BangEq => "!=",
            SyntaxKind::Binary => "binary expression",
            SyntaxKind::Bool => "boolean literal",
            SyntaxKind::Break => "break",
            SyntaxKind::Closure => "closure",
            SyntaxKind::CodeBlock => "code block",
            SyntaxKind::Colon => ":",
            SyntaxKind::ColonColon => "::",
            SyntaxKind::Comma => ",",
            SyntaxKind::Comment => "comment",
            SyntaxKind::Condition => "condition",
            SyntaxKind::Conditional => "if expression",
            SyntaxKind::ConditionalAlternate => "else if expression",
            SyntaxKind::ConditionalElse => "else expression",
            SyntaxKind::Continue => "continue",
            SyntaxKind::DestructureAssignment => "destructuring assignment",
            SyntaxKind::Destructuring => "destructuring",
            SyntaxKind::DocComment => "doc comment",
            SyntaxKind::Dollar => "$",
            SyntaxKind::Dot => ".",
            SyntaxKind::Dots => "..",
            SyntaxKind::DotsEq => "..=",
            SyntaxKind::DoubleQuote => "\"",
            SyntaxKind::Ellipsis => "...",
            SyntaxKind::Else => "else",
            SyntaxKind::End => "end of file",
            SyntaxKind::Enum => "enum",
            SyntaxKind::Eq => "=",
            SyntaxKind::EqEq => "==",
            SyntaxKind::Error => "error",
            SyntaxKind::FieldAccess => "field access",
            SyntaxKind::Float => "float literal",
            SyntaxKind::For => "for",
            SyntaxKind::ForLoop => "for loop",
            SyntaxKind::FuncCall => "function call",
            SyntaxKind::Gt => ">",
            SyntaxKind::GtEq => ">=",
            SyntaxKind::GtGt => ">>",
            SyntaxKind::Hash => "#",
            SyntaxKind::Hat => "^",
            SyntaxKind::HatEq => "^=",
            SyntaxKind::Ident => "identifier",
            SyntaxKind::If => "if",
            SyntaxKind::Import => "import",
            SyntaxKind::In => "in",
            SyntaxKind::IndexAccess => "index access",
            SyntaxKind::Int => "integer literal",
            SyntaxKind::LeftBrace => "{",
            SyntaxKind::LeftBracket => "[",
            SyntaxKind::LeftParen => "(",
            SyntaxKind::Let => "let",
            SyntaxKind::LetBinding => "let binding",
            SyntaxKind::Loop => "loop",
            SyntaxKind::Lt => "<",
            SyntaxKind::LtEq => "<=",
            SyntaxKind::LtLt => "<<",
            SyntaxKind::Minus => "-",
            SyntaxKind::MinusEq => "-=",
            SyntaxKind::Mut => "mut",
            SyntaxKind::Named => "named binding",
            SyntaxKind::NewLine => "newline",
            SyntaxKind::Param => "parameter",           
            SyntaxKind::Params => "parameter list",
            SyntaxKind::Parenthesized => "parenthesized expression",
            SyntaxKind::PathAccess => "path",
            SyntaxKind::Percent => "%",
            SyntaxKind::Pipe => "|",
            SyntaxKind::PipeEq => "|=",
            SyntaxKind::PipePipe => "||",
            SyntaxKind::Plus => "+",
            SyntaxKind::PlusEq => "+=",
            SyntaxKind::Pub => "pub",
            SyntaxKind::Return => "return",
            SyntaxKind::Ref => "ref",
            SyntaxKind::RightBrace => "}",
            SyntaxKind::RightBracket => "]",
            SyntaxKind::RightParen => ")",
            SyntaxKind::Semicolon => ";",
            SyntaxKind::Slash => "/",
            SyntaxKind::SlashEq => "/=",
            SyntaxKind::Star => "*",
            SyntaxKind::StarEq => "*=",
            SyntaxKind::Str => "string literal",
            SyntaxKind::Tilde => "~",
            SyntaxKind::TildeEq => "~=",
            SyntaxKind::Unary => "unary expression",
            SyntaxKind::Underscore => "_",
            SyntaxKind::Unit => "()",
            SyntaxKind::While => "while",
            SyntaxKind::WhileLoop => "while loop",
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
            Self::As
                | Self::Break
                | Self::Continue
                | Self::Else
                | Self::Enum
                | Self::For
                | Self::If
                | Self::Import
                | Self::In
                | Self::Let
                | Self::LetBinding
                | Self::Loop
                | Self::Mut
                | Self::Ref
                | Self::Return
                | Self::Unit
                | Self::While
                | Self::Pub
        )
    }
}
