use std::fmt;

pub const KEYWORDS: &[&str] = &[
    "include", "fn",
    "let", "ret", "if", "else", "while",
    "true", "false",
];

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    // Keywords, ident and literals for basic types
    Keyword,    // let, fn, include etc.
    Identifier, // main
    Number,     // 69
    String,     // "string"
    Char,       // 'c'

    OpenParen,    // (
    CloseParen,   // )
    OpenCurly,    // {
    CloseCurly,   // }
    OpenBracket,  // [
    CloseBracket, // ]

    Colon,     // :
    Semicolon, // ;
    Comma,     // ,

    Plus,        // +
    Minus,       // -
    Star,        // *
    Slash,       // /
    Assign,      // =
    Exclamation, // !
    Percent,     // %

    EqualEqual,   // ==
    NotEqual,     // !=
    Greater,      // >
    Less,         // <
    GreaterEqual, // >=
    LessEqual,    // <=

    Ampersand,       // &
    DoubleAmpersand, // &&
    Pipe,            // |
    DoublePipe,      // ||

    FatArrow, // =>

    // Special tokens
    EOF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}",
            match self {
                TokenKind::Keyword => "keyword",
                TokenKind::Identifier => "identifier",
                TokenKind::Number => "number",
                TokenKind::String => "string",
                TokenKind::Char => "char",
                TokenKind::OpenParen => "(",
                TokenKind::CloseParen => ")",
                TokenKind::OpenCurly => "{",
                TokenKind::CloseCurly => "}",
                TokenKind::OpenBracket => "[",
                TokenKind::CloseBracket => "]",
                TokenKind::Colon => ";",
                TokenKind::Semicolon => ";",
                TokenKind::Comma => ",",
                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Star => "*",
                TokenKind::Slash => "/",
                TokenKind::Assign => "=",
                TokenKind::EqualEqual => "==",
                TokenKind::NotEqual => "!=",
                TokenKind::Greater => ">",
                TokenKind::Less => "<",
                TokenKind::GreaterEqual => ">=",
                TokenKind::LessEqual => "<=",
                TokenKind::Exclamation => "!",
                TokenKind::Percent => "%",
                TokenKind::Pipe => "|",
                TokenKind::DoublePipe => "||",
                TokenKind::Ampersand => "&",
                TokenKind::DoubleAmpersand => "&&",
                TokenKind::FatArrow => "=>",
                TokenKind::EOF => "EOF",
            }
        )
    }
}

pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub number: i64,
    pub loc: Loc,
}

impl Token {
    pub fn with_text(kind: TokenKind, text: &str, loc: Loc) -> Self {
        Token {
            kind,
            text: text.to_string(),
            number: 0,
            loc,
        }
    }

    pub fn with_number(kind: TokenKind, number: i64, loc: Loc) -> Self {
        Token {
            kind,
            text: number.to_string(),
            number,
            loc,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Loc {
    pub path: String,
    pub line: usize,
    pub col: usize,
}

impl Loc {
    pub fn new(path: &str, line: usize, col: usize) -> Self {
        Loc {
            path: path.to_string(),
            line,
            col,
        }
    }

    // NOTE: Use this function, only if you SURE that this location will never used.
    // For example in syntactical sugar, that isn't reported for user
    pub fn empty() -> Self {
        Loc {
            path: String::new(),
            line: 0,
            col: 0,
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.path, self.line + 1, self.col + 1)
    }
}
