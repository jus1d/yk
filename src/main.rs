use std::fs;

#[allow(dead_code)]
#[derive(Debug)]
enum Token {
    Word(String),
    Number(i64),
    Str(String),

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
    Asterisk,
    Slash,
    Modulo,
    Exponent,
}

struct Lexer {
    source: String,
    cursor: usize,
}

impl Lexer {
    fn new(source: String) -> Self {
        Lexer { source, cursor: 0 }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut current = String::new();

        while self.cursor < self.source.len() {
            let mut ch: char = self.source.chars().nth(self.cursor).unwrap();

            if ch.is_whitespace() {
                if !current.is_empty() {
                    if let Ok(number) = current.parse::<i64>() {
                        return Some(Token::Number(number));
                    } else {
                        return Some(Token::Word(current));
                    }
                }
                self.cursor += 1;
                continue;
            }

            if ch.is_alphanumeric() || ch == '_' {
                current.push(ch);
            } else {
                if !current.is_empty() {
                    if let Ok(number) = current.parse::<i64>() {
                        return Some(Token::Number(number));
                    } else {
                        return Some(Token::Word(current));
                    }
                }

                self.cursor += 1;
                match ch {
                    '(' => return Some(Token::OpenParen),
                    ')' => return Some(Token::CloseParen),
                    '{' => return Some(Token::OpenCurly),
                    '}' => return Some(Token::CloseCurly),
                    ';' => return Some(Token::Semicolon),
                    ':' => return Some(Token::Colon),
                    ',' => return Some(Token::Comma),
                    '+' => return Some(Token::Plus),
                    '-' => return Some(Token::Minus),
                    '*' => return Some(Token::Asterisk),
                    '/' => return Some(Token::Slash),
                    '%' => return Some(Token::Modulo),
                    '^' => return Some(Token::Exponent),
                    '.' => return Some(Token::Dot),
                    '"' => {
                        let mut content = String::new();

                        while self.cursor < self.source.len() {
                            ch = self.source.chars().nth(self.cursor).unwrap();
                            match ch {
                                '"' => {
                                    return Some(Token::Str(content));
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

        return None;
    }
}

fn main() {
    let path = "main.w";
    let source = fs::read_to_string(path).unwrap();

    let tokens = Lexer::new(source.clone());
    for token in tokens {
        println!("{:?}", token);
    }
}
