#[derive(Clone)]
pub struct Lexer {
    source: String,
    cursor: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer { source, cursor: 0 }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut text = String::new();

        while self.cursor < self.source.len() {
            let mut ch: char = self.source.chars().nth(self.cursor).unwrap();

            if ch.is_whitespace() {
                if !text.is_empty() {
                    if let Ok(number) = text.parse::<i64>() {
                        return Some(Token::with_number(TokenKind::Number, number));
                    } else {
                        return Some(Token::with_text(TokenKind::Word, text));
                    }
                }
                self.cursor += 1;
                continue;
            }

            if ch.is_alphanumeric() || ch == '_' {
                text.push(ch);
            } else {
                if !text.is_empty() {
                    if let Ok(number) = text.parse::<i64>() {
                        return Some(Token::with_number(TokenKind::Number, number));
                    } else {
                        return Some(Token::with_text(TokenKind::Word, text));
                    }
                }

                self.cursor += 1;
                match ch {
                    '(' => return Some(Token::with_text(TokenKind::OpenParen, text)),
                    ')' => return Some(Token::with_text(TokenKind::CloseParen, text)),
                    '{' => return Some(Token::with_text(TokenKind::OpenCurly, text)),
                    '}' => return Some(Token::with_text(TokenKind::CloseCurly, text)),
                    ';' => return Some(Token::with_text(TokenKind::Semicolon, text)),
                    ':' => return Some(Token::with_text(TokenKind::Colon, text)),
                    ',' => return Some(Token::with_text(TokenKind::Comma, text)),
                    '.' => return Some(Token::with_text(TokenKind::Dot, text)),
                    '+' => return Some(Token::with_text(TokenKind::Plus, text)),
                    '-' => return Some(Token::with_text(TokenKind::Minus, text)),
                    '*' => return Some(Token::with_text(TokenKind::Star, text)),
                    '/' => return Some(Token::with_text(TokenKind::Slash, text)),
                    '"' => {
                        let mut content = String::new();

                        while self.cursor < self.source.len() {
                            ch = self.source.chars().nth(self.cursor).unwrap();
                            match ch {
                                '"' => {
                                    self.cursor += 1;
                                    return Some(Token::with_text(TokenKind::String, content));
                                }
                                _ => {
                                    content.push(ch);
                                }
                            }
                            self.cursor += 1;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            self.cursor += 1;
        }

        if !text.is_empty() {
            if let Ok(number) = text.parse::<i64>() {
                return Some(Token::with_number(TokenKind::Number, number));
            } else {
                return Some(Token::with_text(TokenKind::Word, text));
            }
        }

        return None;
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
    Colon,
    Comma,
    Dot,

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
