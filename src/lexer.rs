use crate::diag;

use std::fmt;
use std::iter::Peekable;
use std::path::Path;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Word,
    Number,
    String,
    Char,

    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,

    Colon,
    Semicolon,
    Comma,

    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    Exclamation,
    Percent,

    EqualEqual,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Word => "`word`",
                TokenKind::Number => "`number`",
                TokenKind::String => "`string`",
                TokenKind::Char => "`char`",
                TokenKind::OpenParen => "`(`",
                TokenKind::CloseParen => "`)`",
                TokenKind::OpenCurly => "`{`",
                TokenKind::CloseCurly => "`}`",
                TokenKind::OpenBracket => "`[`",
                TokenKind::CloseBracket => "`]`",
                TokenKind::Colon => "`;`",
                TokenKind::Semicolon => "`;`",
                TokenKind::Comma => "`,`",
                TokenKind::Plus => "`+`",
                TokenKind::Minus => "`-`",
                TokenKind::Star => "`*`",
                TokenKind::Slash => "`/`",
                TokenKind::Equals => "`=`",
                TokenKind::EqualEqual => "`==`",
                TokenKind::NotEqual => "`!=`",
                TokenKind::Greater => "`>`",
                TokenKind::Less => "`<`",
                TokenKind::GreaterEqual => "`>=`",
                TokenKind::LessEqual => "`<=`",
                TokenKind::Exclamation => "`!`",
                TokenKind::Percent => "`%`",
                TokenKind::Pipe => "`|`",
                TokenKind::DoublePipe => "`||`",
                TokenKind::Ampersand => "`&`",
                TokenKind::DoubleAmpersand => "`&&`",
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
    fn with_text(kind: TokenKind, text: &str, loc: Loc) -> Self {
        Token {
            kind,
            text: text.to_string(),
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

#[derive(Clone, Debug)]
pub struct Loc {
    pub filename: String,
    pub line: usize,
    pub col: usize,
}

impl Loc {
    pub fn new(path: &str, line: usize, col: usize) -> Self {
        let filename = Path::new(path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or_else(|| {
                diag::fatal!("invalid path: {path}");
            });

        Loc {
            filename: filename.to_string(),
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

pub struct Lexer<Chars: Iterator<Item = char> + Clone> {
    chars: Peekable<Chars>,
    filename: String,
    cur: usize,
    line: usize,
    bol: usize,
}

impl<Chars: Iterator<Item = char> + Clone> Lexer<Chars> {
    pub fn new(chars: Chars, filename: &str) -> Self {
        Lexer {
            chars: chars.clone().peekable(),
            filename: filename.to_string(),
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

        let loc = Loc::new(&self.filename, self.line, self.cur - self.bol);
        let mut text = String::new();

        let ch = self.chars.next().unwrap();
        self.cur += 1;

        if ch == '"' {
            while let Some(ch) = self.chars.next() {
                self.cur += 1;
                match ch {
                    '\\' => {
                        self.cur += 1;
                        if let Some(ch) = self.chars.next() {
                            match ch {
                                'n' => text.push('\n'),
                                _ => diag::fatal!(loc, "only escaping new lines supported yet")
                            }
                        }
                    },
                    '"' => return Some(Token::with_text(TokenKind::String, &text, loc)),
                    _ => text.push(ch),
                }
            }

            diag::fatal!(loc, "unclosed string literal");
        }

        if ch == '\'' {
            match self.chars.next() {
                Some(ch) => match ch {
                    '\'' => diag::fatal!(loc, "empty character literal"),
                    '\\' => diag::fatal!(loc, "escaping character literals is not supported yet"),
                    _ => text.push(ch),
                },
                None => diag::fatal!(loc, "unclosed character literal"),
            }

            if let Some(_) = self.chars.next_if(|ch| *ch == '\'') {
                return Some(Token::with_text(TokenKind::Char, &text, loc))
            } else {
                diag::fatal!(loc, "expected terminating `'` for character literal")
            }
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

            return Some(Token::with_text(TokenKind::Word, &text, loc));
        }

        match ch {
            '(' => return Some(Token::with_text(TokenKind::OpenParen, &text, loc)),
            ')' => return Some(Token::with_text(TokenKind::CloseParen, &text, loc)),
            '{' => return Some(Token::with_text(TokenKind::OpenCurly, &text, loc)),
            '}' => return Some(Token::with_text(TokenKind::CloseCurly, &text, loc)),
            '[' => return Some(Token::with_text(TokenKind::OpenBracket, &text, loc)),
            ']' => return Some(Token::with_text(TokenKind::CloseBracket, &text, loc)),
            ';' => return Some(Token::with_text(TokenKind::Semicolon, &text, loc)),
            ':' => return Some(Token::with_text(TokenKind::Colon, &text, loc)),
            ',' => return Some(Token::with_text(TokenKind::Comma, &text, loc)),
            '+' => return Some(Token::with_text(TokenKind::Plus, &text, loc)),
            '-' => return Some(Token::with_text(TokenKind::Minus, &text, loc)),
            '*' => return Some(Token::with_text(TokenKind::Star, &text, loc)),
            '/' => {
                if self.chars.next_if(|ch| *ch == '/').is_some() {
                    while let Some(ch) = self.chars.next() {
                        if ch == '\n' { break; }
                    }
                    return self.next();
                }
                return Some(Token::with_text(TokenKind::Slash, &text, loc))
            },
            '%' => return Some(Token::with_text(TokenKind::Percent, &text, loc)),
            '|' => {
                if self.chars.next_if(|ch| *ch == '|').is_some() {
                    return Some(Token::with_text(TokenKind::DoublePipe, "||", loc));
                }
                return Some(Token::with_text(TokenKind::Pipe, "|", loc));
            },
            '&' => {
                if self.chars.next_if(|ch| *ch == '&').is_some() {
                    return Some(Token::with_text(TokenKind::DoubleAmpersand, "&&", loc));
                }
                return Some(Token::with_text(TokenKind::Ampersand, "&", loc));
            },
            '>' => {
                if self.chars.next_if(|ch| *ch == '=').is_some() {
                    return Some(Token::with_text(TokenKind::GreaterEqual, ">=", loc));
                }
                return Some(Token::with_text(TokenKind::Greater, ">", loc));
            },
            '<' => {
                if self.chars.next_if(|ch| *ch == '=').is_some() {
                    return Some(Token::with_text(TokenKind::LessEqual, "<=", loc));
                }
                return Some(Token::with_text(TokenKind::Less, "<", loc));
            },
            '=' => {
                if self.chars.next_if(|ch| *ch == '=').is_some() {
                    return Some(Token::with_text(TokenKind::EqualEqual, "==", loc));
                }
                return Some(Token::with_text(TokenKind::Equals, "=", loc));
            },
            '!' => {
                if self.chars.next_if(|ch| *ch == '=').is_some() {
                    return Some(Token::with_text(TokenKind::NotEqual, "!=", loc));
                }
                return Some(Token::with_text(TokenKind::Exclamation, "!", loc));
            },
            _ => diag::fatal!(loc, "unexpected character `{}`", ch),
        }
    }
}
