use std::iter::Peekable;

#[derive(Clone)]
pub struct Lexer<Chars: Iterator<Item = char> + Clone> {
    chars: Peekable<Chars>,
}

impl<Chars: Iterator<Item = char> + Clone> Lexer<Chars> {
    pub fn new(chars: Chars) -> Self {
        Lexer {
            chars: chars.clone().peekable(),
        }
    }
}

impl<Chars: Iterator<Item = char> + Clone> Iterator for Lexer<Chars> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while let Some(_) = self.chars.next_if(|ch| ch.is_whitespace()) {}

        let mut text = String::new();

        if self.chars.peek().is_none() {
            return None;
        }

        let ch = self.chars.next().unwrap();

        if ch == '"' {
            while let Some(ch) = self.chars.next() {
                match ch {
                    '\\' => todo!("Escaping strings is not supported"),
                    '"' => return Some(Token::with_text(TokenKind::String, text)),
                    _ => text.push(ch),
                }
            }

            todo!("Report unclosed string literal error");
        }

        text.push(ch);

        if ch.is_alphanumeric() || ch == '_' {
            while let Some(ch) = self.chars.next_if(|ch| ch.is_alphanumeric() || *ch == '_') {
                text.push(ch);
            }

            if let Ok(number) = text.parse::<i64>() {
                return Some(Token::with_number(TokenKind::Number, number));
            }

            return Some(Token::with_text(TokenKind::Word, text));
        }

        match ch {
            '(' => return Some(Token::with_text(TokenKind::OpenParen, text)),
            ')' => return Some(Token::with_text(TokenKind::CloseParen, text)),
            '{' => return Some(Token::with_text(TokenKind::OpenCurly, text)),
            '}' => return Some(Token::with_text(TokenKind::CloseCurly, text)),
            ';' => return Some(Token::with_text(TokenKind::Semicolon, text)),
            ',' => return Some(Token::with_text(TokenKind::Comma, text)),
            '+' => return Some(Token::with_text(TokenKind::Plus, text)),
            '-' => return Some(Token::with_text(TokenKind::Minus, text)),
            '*' => return Some(Token::with_text(TokenKind::Star, text)),
            '/' => return Some(Token::with_text(TokenKind::Slash, text)),
            _ => todo!("Unexpected token: '{}'", ch),
        }
    }
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

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub number: i64,
}

impl Token {
    fn with_text(kind: TokenKind, text: String) -> Self {
        Token {
            kind,
            text,
            number: 0,
        }
    }

    fn with_number(kind: TokenKind, number: i64) -> Self {
        Token {
            kind,
            text: number.to_string(),
            number,
        }
    }
}
