use aura_common::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Def,
    Let,
    Mut,
    Type,
    Concept,
    Instance,
    Match,
    If,
    Else,
    For,
    While,
    In,
    Return,
    Break,
    Continue,
    Pub,
    Use,
    Module,
    Async,
    Parallel,
    Race,
    Timeout,
    Yield,
    Where,
    Forall,
    Requires,
    Ensures,
    With,
    And,
    Or,
    Not,
    Lazy,
    Collect,
    True,
    False,

    // Literals
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),

    // String interpolation tokens
    StringStart(String), // text before first interpolation
    StringMid(String),   // text between interpolations
    StringEnd(String),   // text after last interpolation
    InterpStart,         // {
    InterpEnd,           // }

    // Identifiers
    Ident(String),      // lowercase: value/variable
    UpperIdent(String), // uppercase: type/variant

    // Operators
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Percent,  // %
    Eq,       // ==
    NotEq,    // !=
    Lt,       // <
    Gt,       // >
    LtEq,     // <=
    GtEq,     // >=
    Assign,   // =
    Pipeline, // |>
    Arrow,    // ->
    Question, // ?
    DotDot,   // ..
    DotDotEq, // ..=
    FatArrow, // =>

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    Comma,      // ,
    Colon,      // :
    ColonColon, // ::
    Dot,        // .
    Semicolon,  // ;
    Pipe,       // |

    // Special
    At,                 // @
    DocComment(String), // /// ...
    Underscore,         // _

    // Control
    Eof,
    Error(String),
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::UpperIdent(s) => write!(f, "{}", s),
            TokenKind::IntLit(n) => write!(f, "{}", n),
            TokenKind::FloatLit(n) => write!(f, "{}", n),
            TokenKind::StringLit(s) => write!(f, "\"{}\"", s),
            TokenKind::Error(s) => write!(f, "error: {}", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl TokenKind {
    pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
        match s {
            "def" => Some(TokenKind::Def),
            "let" => Some(TokenKind::Let),
            "mut" => Some(TokenKind::Mut),
            "type" => Some(TokenKind::Type),
            "concept" => Some(TokenKind::Concept),
            "instance" => Some(TokenKind::Instance),
            "match" => Some(TokenKind::Match),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "for" => Some(TokenKind::For),
            "while" => Some(TokenKind::While),
            "in" => Some(TokenKind::In),
            "return" => Some(TokenKind::Return),
            "break" => Some(TokenKind::Break),
            "continue" => Some(TokenKind::Continue),
            "pub" => Some(TokenKind::Pub),
            "use" => Some(TokenKind::Use),
            "module" => Some(TokenKind::Module),
            "async" => Some(TokenKind::Async),
            "parallel" => Some(TokenKind::Parallel),
            "race" => Some(TokenKind::Race),
            "timeout" => Some(TokenKind::Timeout),
            "yield" => Some(TokenKind::Yield),
            "where" => Some(TokenKind::Where),
            "forall" => Some(TokenKind::Forall),
            "requires" => Some(TokenKind::Requires),
            "ensures" => Some(TokenKind::Ensures),
            "with" => Some(TokenKind::With),
            "and" => Some(TokenKind::And),
            "or" => Some(TokenKind::Or),
            "not" => Some(TokenKind::Not),
            "lazy" => Some(TokenKind::Lazy),
            "collect" => Some(TokenKind::Collect),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            _ => None,
        }
    }
}
