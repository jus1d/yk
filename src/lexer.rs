use std::fmt;
use std::iter::Peekable;

macro_rules! exit {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Word,
    Number,
    String,

    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,

    Semicolon,
    Comma,

    Plus,
    Minus,
    Star,
    Slash,
}

#[allow(unreachable_patterns)]
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Word => "`word`",
                TokenKind::Number => "`number`",
                TokenKind::String => "`string`",
                TokenKind::OpenParen => "`(`",
                TokenKind::CloseParen => "`)`",
                TokenKind::OpenCurly => "`{`",
                TokenKind::CloseCurly => "`}`",
                TokenKind::Semicolon => "`;`",
                TokenKind::Comma => "`,`",
                TokenKind::Plus => "`+`",
                TokenKind::Minus => "`-`",
                TokenKind::Star => "`*`",
                TokenKind::Slash => "`/`",
                _ => unreachable!(),
            }
        )
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub number: i64,
    pub loc: Loc,
}

impl Token {
    fn with_text(kind: TokenKind, text: String, loc: Loc) -> Self {
        Token {
            kind,
            text,
            number: 0,
            loc,
        }
    }

    fn with_number(kind: TokenKind, number: i64, loc: Loc) -> Self {
        Token {
            kind,
            text: number.to_string(),
            number,
            loc,
        }
    }
}

#[derive(Debug)]
pub struct Loc {
    filename: String,
    line: usize,
    col: usize,
}

impl Loc {
    fn new(filename: String, line: usize, col: usize) -> Self {
        Loc {
            filename,
            line,
            col,
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line + 1, self.col + 1)
    }
}

#[derive(Clone)]
pub struct Lexer<Chars: Iterator<Item = char> + Clone> {
    chars: Peekable<Chars>,
    filename: String,
    cur: usize,
    line: usize,
    bol: usize,
}

impl<Chars: Iterator<Item = char> + Clone> Lexer<Chars> {
    pub fn new(chars: Chars, filename: String) -> Self {
        Lexer {
            chars: chars.clone().peekable(),
            filename,
            cur: 0,
            line: 0,
            bol: 0,
        }
    }

    fn trim_whitespace(&mut self) {
        while let Some(_) = self.chars.next_if(|ch| {
            if *ch == '\n' {
                self.line += 1;
                self.bol = self.cur + 1;
            }

            ch.is_whitespace()
        }) {
            self.cur += 1;
        }
    }
}

impl<Chars: Iterator<Item = char> + Clone> Iterator for Lexer<Chars> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.trim_whitespace();

        if self.chars.peek().is_none() {
            return None;
        }

        let loc = Loc::new(self.filename.clone(), self.line, self.cur - self.bol);
        let mut text = String::new();

        let ch = self.chars.next().unwrap();
        self.cur += 1;

        if ch == '"' {
            while let Some(ch) = self.chars.next() {
                self.cur += 1;
                match ch {
                    '\\' => exit!("{}: error: escaping strings is not supported yet", loc),
                    '"' => return Some(Token::with_text(TokenKind::String, text, loc)),
                    _ => text.push(ch),
                }
            }

            exit!("{}: error: unclosed string literal", loc);
        }

        text.push(ch);

        if ch.is_alphanumeric() || ch == '_' {
            while let Some(ch) = self.chars.next_if(|ch| ch.is_alphanumeric() || *ch == '_') {
                self.cur += 1;
                text.push(ch);
            }

            if let Ok(number) = text.parse::<i64>() {
                return Some(Token::with_number(TokenKind::Number, number, loc));
            }

            return Some(Token::with_text(TokenKind::Word, text, loc));
        }

        match ch {
            '(' => return Some(Token::with_text(TokenKind::OpenParen, text, loc)),
            ')' => return Some(Token::with_text(TokenKind::CloseParen, text, loc)),
            '{' => return Some(Token::with_text(TokenKind::OpenCurly, text, loc)),
            '}' => return Some(Token::with_text(TokenKind::CloseCurly, text, loc)),
            ';' => return Some(Token::with_text(TokenKind::Semicolon, text, loc)),
            ',' => return Some(Token::with_text(TokenKind::Comma, text, loc)),
            '+' => return Some(Token::with_text(TokenKind::Plus, text, loc)),
            '-' => return Some(Token::with_text(TokenKind::Minus, text, loc)),
            '*' => return Some(Token::with_text(TokenKind::Star, text, loc)),
            '/' => return Some(Token::with_text(TokenKind::Slash, text, loc)),
            _ => exit!("{}: error: unexpected character `{}`", loc, ch),
        }
    }
}
