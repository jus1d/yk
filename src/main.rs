use std::iter::Peekable;

#[allow(dead_code)]
struct Program {
    functions: Vec<Function>,
}

#[allow(dead_code)]
struct Function {
    name: String,
    body: Vec<Statement>,
}

#[allow(dead_code)]
#[derive(Debug)]
enum Statement {
    Funcall { name: String, args: Vec<Expr> },
    Ret { value: Option<Expr> },
}

impl Statement {
    fn parse(lexer: &mut Peekable<Lexer>) -> Self {
        match lexer.next() {
            Some(token) => match token.kind {
                TokenKind::Word => match token.text.as_str() {
                    "ret" => {
                        let value = Expr::parse(lexer);
                        if lexer.next_if(|t| t.kind == TokenKind::Semicolon).is_none() {
                            todo!("Missing semicolon after return statement");
                        }

                        Statement::Ret { value: Some(value) }
                    }
                    _ => {
                        let name = token.text;
                        let mut args = Vec::new();

                        if lexer.next_if(|t| t.kind == TokenKind::OpenParen).is_some() {
                            if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_some() {
                                // empty argument list
                            } else {
                                args.push(Expr::parse(lexer));

                                while lexer.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                    args.push(Expr::parse(lexer));
                                }
                            }

                            if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                                todo!("Missing close paren after arguments list");
                            }
                        }

                        if lexer.next_if(|t| t.kind == TokenKind::Semicolon).is_none() {
                            todo!("Missing semicolon after funcall statement");
                        }

                        Statement::Funcall { name, args }
                    }
                },
                _ => todo!(),
            },
            None => todo!(),
        }
    }
}

// TODO: Move number and string to literal
#[derive(Debug)]
#[allow(dead_code)]
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
        let primary = lexer.next().unwrap();

        match primary.kind {
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
                            name: primary.text,
                            args,
                        };
                    } else {
                        if let Some(op_kind) = lexer.peek().map(|t| t.kind) {
                            match op_kind {
                                TokenKind::Plus
                                | TokenKind::Minus
                                | TokenKind::Star
                                | TokenKind::Slash => {
                                    lexer.next();
                                    let rhs = Box::new(Expr::parse(lexer));

                                    let op = match op_kind {
                                        TokenKind::Plus => BinaryOp::Add,
                                        TokenKind::Minus => BinaryOp::Sub,
                                        TokenKind::Star => BinaryOp::Mul,
                                        TokenKind::Slash => BinaryOp::Div,
                                        _ => unreachable!(),
                                    };

                                    return Expr::Binary {
                                        op,
                                        lhs: Box::new(Expr::Variable(primary.text)),
                                        rhs,
                                    };
                                }
                                _ => return Expr::Variable(primary.text),
                            }
                        } else {
                            todo!("Unexpected end of input");
                        }
                    }
                }
                _ => return Expr::Variable(primary.text),
            },
            TokenKind::Number => {
                if let Some(op_kind) = lexer.peek().map(|t| t.kind) {
                    match op_kind {
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                            lexer.next();
                            let rhs = Box::new(Expr::parse(lexer));

                            let op = match op_kind {
                                TokenKind::Plus => BinaryOp::Add,
                                TokenKind::Minus => BinaryOp::Sub,
                                TokenKind::Star => BinaryOp::Mul,
                                TokenKind::Slash => BinaryOp::Div,
                                _ => unreachable!(),
                            };

                            return Expr::Binary {
                                op,
                                lhs: Box::new(Expr::Number(primary.number)),
                                rhs,
                            };
                        }
                        _ => return Expr::Variable(primary.text),
                    }
                } else {
                    todo!("Unexpected end of input");
                }
            }
            TokenKind::String => Expr::String(primary.text),
            _ => panic!("Unexpected token: {:?}", primary.kind),
        }
    }
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    Star,
    Slash,
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

fn main() {
    let source = String::from("println(x, \"hui\", 2 * 2);");

    let lexer = Lexer::new(source.clone());
    let stmt = Statement::parse(&mut lexer.peekable());

    println!("{:?}", stmt);

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
