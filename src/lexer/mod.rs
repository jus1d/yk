pub mod token;

use crate::parser::ast::Type;

use token::{Token, Loc, TokenKind, KEYWORDS};
use std::iter::Peekable;
use std::process::exit;

pub struct Lexer<Chars: Iterator<Item = char> + Clone> {
    chars: Peekable<Chars>,
    filename: String,
    cur: usize,
    line: usize,
    bol: usize,
    terminated: bool,
}

impl<Chars: Iterator<Item = char> + Clone> Lexer<Chars> {
    pub fn new(chars: Chars, filename: &str) -> Self {
        Lexer {
            chars: chars.clone().peekable(),
            filename: filename.to_string(),
            cur: 0,
            line: 0,
            bol: 0,
            terminated: false,
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

    fn drop_line(&mut self) {
        self.cur += 1;
        while let Some(ch) = self.chars.next() {
            self.cur += 1;
            if ch == '\n' {
                self.line += 1;
                self.bol = self.cur;
                break;
            }
        }
    }

    fn parse_string_or_char(&mut self, quote_char: char, loc: Loc) -> Token {
        self.cur += 1;
        let mut text = String::new();
        let is_char = quote_char == '\'';

        while let Some(ch) = self.chars.next() {
            self.cur += 1;

            if ch == '\\' {
                self.cur += 1;
                if let Some(escaped_ch) = self.chars.next() {
                    match escaped_ch {
                        'n' => text.push('\n'),
                        't' => text.push('\t'),
                        'r' => text.push('\r'),
                        '0' => text.push('\0'),
                        '\\' => text.push('\\'),
                        '\'' => text.push('\''),
                        '"' => text.push('"'),
                        _ => {
                            eprintln!("{}: error: unsupported escape sequence: \\{}", loc, escaped_ch);
                            eprintln!("{}: note: if you wanted to print `\\` character, use escaped one: `\\\\`", loc);
                        },
                    }
                    continue;
                } else {
                    eprintln!("{}: error: unfinished escape sequence", loc);
                    eprintln!("{}: note: if you wanted to print `\\` character, use escaped one: `\\\\`", loc);
                    exit(1);
                }
            }

            if ch == quote_char {
                if is_char {
                    if text.len() != 1 {
                        eprintln!("{}: error: char literal must be one character long, found `{}`", loc, text);
                        exit(1);
                    }
                    return Token::with_text(TokenKind::Char, &text, loc);
                } else {
                    return Token::with_text(TokenKind::String, &text, loc);
                }
            }

            text.push(ch);
        }

        eprintln!("{}: error: unclosed {} literal", loc, if is_char { Type::Char } else { Type::String });
        exit(1);
    }
}

impl<Chars: Iterator<Item = char> + Clone> Iterator for Lexer<Chars> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.trim_whitespace();

        let loc = Loc::new(&self.filename, self.line, self.cur - self.bol);

        if self.chars.peek().is_none() {
            if self.terminated {
                return None;
            }
            self.terminated = true;
            return Some(Token { kind: TokenKind::EOF, text: String::from("EOF"), number: 0, loc })
        }

        let mut text = String::new();

        let ch = self.chars.next().unwrap();
        self.cur += 1;

        if ch == '\'' || ch == '"' {
            return Some(self.parse_string_or_char(ch, loc));
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

            if KEYWORDS.contains(&text.as_str()) {
                return Some(Token::with_text(TokenKind::Keyword, &text, loc));
            }
            if is_identifier(&text) {
                return Some(Token::with_text(TokenKind::Identifier, &text, loc));
            }

            eprintln!("{}: error: cannot parse token as identifier: `{}`", loc, text);
            exit(1);
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
                    self.drop_line();
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
                if self.chars.next_if(|ch| *ch == '>').is_some() {
                    return Some(Token::with_text(TokenKind::FatArrow, "=>", loc));
                }
                if self.chars.next_if(|ch| *ch == '=').is_some() {
                    return Some(Token::with_text(TokenKind::EqualEqual, "==", loc));
                }
                return Some(Token::with_text(TokenKind::Assign, "=", loc));
            },
            '!' => {
                if self.chars.next_if(|ch| *ch == '=').is_some() {
                    return Some(Token::with_text(TokenKind::NotEqual, "!=", loc));
                }
                return Some(Token::with_text(TokenKind::Exclamation, "!", loc));
            },
            _ => {
                eprintln!("{}: error: unexpected character `{}`", loc, ch);
                exit(1);
            },
        }
    }
}

fn is_identifier(s: &str) -> bool {
    let first = match s.chars().next() {
        Some(ch) => ch,
        None => return false,
    };

    if !(first.is_alphabetic() || first == '_') {
        return false;
    }

    s.chars().all(|c| c.is_alphanumeric() || c == '_')
}
