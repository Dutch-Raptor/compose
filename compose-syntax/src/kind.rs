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
}


impl SyntaxKind {
    pub(crate) fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::End | Self::Semicolon | Self::RightBrace | Self::RightParen | Self::RightBracket
        )
    }
    
    pub(crate) fn descriptive_name(&self) -> &'static str {
        use SyntaxKind::*;
        match self {
            LeftBrace => "{",
            LeftParen => "(",
            LeftBracket => "[",
            RightBrace => "}",
            RightParen => ")",
            RightBracket => "]",
            Eq => "=",
            EqEq => "==",
            End => "end of file",
            DoubleQuote => "\"",
            Apostrophe => "'",
            Backtick => "`",
            Underscore => "_",
            Hat => "^",
            At => "@",
            Bang => "!",
            Hash => "#",
            Dollar => "$",
            Percent => "%",
            Slash => "/",
            Minus => "-",
            Plus => "+",
            Star => "*",
            Colon => ":",
            Semicolon => ";",
            Comma => ",",
            Arrow => "=>",
            ColonColon => "::",
            HatEq => "^=",
            TildeEq => "~=",
            AmpersandEq => "&=",
            AmpersandAmpersand => "&&",
            PipeEq => "|=",
            PipePipe => "||",
            GtEq => ">=",
            GtGt => ">>",
            LtEq => "<=",
            LtLt => "<<",
            Dots => "..",
            DotsEq => "..=",
            Ellipsis => "...",
            SlashEq => "/=",
            StarEq => "*=",
            MinusEq => "-=",
            PlusEq => "+=",
            ExclEq => "!=",
            Float => "float literal",
            Int => "integer literal",
            Return => "return",
            Pub => "pub",
            Mut => "mut",
            Loop => "loop",
            LetBinding => "let binding",
            In => "in",
            Import => "import",
            If => "if",
            For => "for",
            Enum => "enum",
            Else => "else",
            Continue => "continue",
            Break => "break",
            As => "as",
            Unit => "()",
            Ident => "identifier",
            Error => "error",
            Str => "string literal",
            Tilde => "~",
            Ampersand => "&",
            Pipe => "|",
            Gt => ">",
            Lt => "<",
            BangEq => "!=",
            Bool => "boolean literal",
            Unary => "unary expression",
            Params => "parameter list",
            Closure => "closure",
            DestructureAssignment => "destructuring assignment",
            Named => "named binding",
            Destructuring => "destructuring",
            CodeBlock => "code block",
            Parenthesized => "parenthesized expression",
            Let => "let",
            While => "while",
            Binary => "binary expression",
            Dot => ".",
            FuncCall => "function call",
            FieldAccess => "field access",
            IndexAccess => "index access",
            Args => "argument list",
            PathAccess => "path",
            NewLine => "newline",
            Assignment => "assignment",
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
