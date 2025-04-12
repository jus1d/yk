use std::{fs, iter::Peekable};

struct Program {
    functions: Vec<Function>,
}

struct Function {
    name: String,
    body: Vec<Statement>,
}

enum Statement {
    Funcall { name: String, args: Vec<Expr> },
    Ret { value: Option<Expr> },
}

// TODO: Move number and string to literal
#[derive(Debug)]
enum Expr {
    Variable(String),
    Number(i64),
    String(String),
    Funcall {
        name: String,
        args: Vec<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    fn parse(lexer: &mut Peekable<Lexer>) -> Self {
        let current = lexer.next().unwrap();

        match current.kind {
            TokenKind::Word => match lexer.peek() {
                Some(token) => {
                    if token.kind == TokenKind::OpenParen {
                        // consume open paren
                        lexer.next();

                        let mut args = Vec::new();

                        if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_some() {
                            // consume close paren
                            lexer.next();
                        } else {
                            args.push(Expr::parse(lexer));
                            while lexer.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                args.push(Expr::parse(lexer));
                            }
                        }

                        if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            todo!("Expected ')' after function arguments");
                        }

                        return Expr::Funcall {
                            name: current.text,
                            args,
                        };
                    } else {
                        return Expr::Variable(current.text);
                    }
                }
                _ => return Expr::Variable(current.text),
            },
            TokenKind::Number => Expr::Number(current.number),
            TokenKind::String => Expr::String(current.text),
            _ => panic!("Unexpected token"),
        }
    }
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
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
    Asterisk,
    Slash,
    Modulo,
    Exponent,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    text: String,
    number: i64,
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

#[derive(Clone)]
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
                    '*' => return Some(Token::with_text(TokenKind::Asterisk, text)),
                    '/' => return Some(Token::with_text(TokenKind::Slash, text)),
                    '%' => return Some(Token::with_text(TokenKind::Modulo, text)),
                    '^' => return Some(Token::with_text(TokenKind::Exponent, text)),
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

fn main() {
    // let path = "main.w";
    // let source = fs::read_to_string(path).unwrap();

    let source = String::from("println(\"Hello, world!\")");

    let mut tokens = Lexer::new(source.clone());
    for token in tokens.clone() {
        println!("{:?}", token);
    }

    let expr = Expr::parse(&mut tokens.peekable());

    println!("{:?}", expr);

    // let program = Program {
    //     functions: vec![Function {
    //         name: "main".to_string(),
    //         body: vec![
    //             Statement::Funcall {
    //                 name: "println".to_string(),
    //                 args: vec![Expr::String("Hello, world!".to_string())],
    //             },
    //             Statement::Ret {
    //                 value: Some(Expr::Number(0)),
    //             },
    //         ],
    //     }],
    // };
}
