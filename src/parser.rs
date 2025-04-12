use crate::lexer::{Token, TokenKind};

use std::iter::Peekable;

#[allow(dead_code)]
pub struct Program {
    functions: Vec<Function>,
}

#[allow(dead_code)]
pub struct Function {
    name: String,
    body: Vec<Statement>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Statement {
    Funcall { name: String, args: Vec<Expr> },
    Ret { value: Option<Expr> },
}

// TODO: Move number and string to literal
#[derive(Debug)]
#[allow(dead_code)]
pub enum Expr {
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

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub struct Parser<Tokens>
where
    Tokens: Iterator<Item = Token>,
{
    tokens: Peekable<Tokens>,
}

impl<Tokens> Parser<Tokens>
where
    Tokens: Iterator<Item = Token>,
{
    pub fn from_iter(tokens: Tokens) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        let primary = self.tokens.next().unwrap();

        match primary.kind {
            TokenKind::Word => match self.tokens.peek() {
                Some(token) => {
                    if token.kind == TokenKind::OpenParen {
                        // consume open paren
                        self.tokens.next();

                        let mut args = Vec::new();

                        if self
                            .tokens
                            .next_if(|t| t.kind == TokenKind::CloseParen)
                            .is_some()
                        {
                            // consume close paren
                            self.tokens.next();
                        } else {
                            args.push(self.parse_expr().unwrap());

                            while self
                                .tokens
                                .next_if(|t| t.kind == TokenKind::Comma)
                                .is_some()
                            {
                                args.push(self.parse_expr().unwrap());
                            }
                        }

                        if self
                            .tokens
                            .next_if(|t| t.kind == TokenKind::CloseParen)
                            .is_none()
                        {
                            todo!("Expected ')' after function arguments");
                        }

                        return Some(Expr::Funcall {
                            name: primary.text,
                            args,
                        });
                    } else {
                        if let Some(op_kind) = self.tokens.peek().map(|t| t.kind) {
                            match op_kind {
                                TokenKind::Plus
                                | TokenKind::Minus
                                | TokenKind::Star
                                | TokenKind::Slash => {
                                    self.tokens.next();
                                    let rhs = Box::new(self.parse_expr().unwrap());

                                    let op = match op_kind {
                                        TokenKind::Plus => BinaryOp::Add,
                                        TokenKind::Minus => BinaryOp::Sub,
                                        TokenKind::Star => BinaryOp::Mul,
                                        TokenKind::Slash => BinaryOp::Div,
                                        _ => unreachable!(),
                                    };

                                    return Some(Expr::Binary {
                                        op,
                                        lhs: Box::new(Expr::Variable(primary.text)),
                                        rhs,
                                    });
                                }
                                _ => return Some(Expr::Variable(primary.text)),
                            }
                        } else {
                            todo!("Unexpected end of input");
                        }
                    }
                }
                _ => return Some(Expr::Variable(primary.text)),
            },
            TokenKind::Number => {
                if let Some(op_kind) = self.tokens.peek().map(|t| t.kind) {
                    match op_kind {
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                            self.tokens.next();
                            let rhs = Box::new(self.parse_expr().unwrap());

                            let op = match op_kind {
                                TokenKind::Plus => BinaryOp::Add,
                                TokenKind::Minus => BinaryOp::Sub,
                                TokenKind::Star => BinaryOp::Mul,
                                TokenKind::Slash => BinaryOp::Div,
                                _ => unreachable!(),
                            };

                            return Some(Expr::Binary {
                                op,
                                lhs: Box::new(Expr::Number(primary.number)),
                                rhs,
                            });
                        }
                        _ => return Some(Expr::Number(primary.number)),
                    }
                } else {
                    todo!("Unexpected end of input");
                }
            }
            TokenKind::String => Some(Expr::String(primary.text)),
            _ => panic!("Unexpected token: {:?}", primary.kind),
        }
    }

    pub fn parse_stmt(&mut self) -> Option<Statement> {
        match self.tokens.next() {
            Some(token) => match token.kind {
                TokenKind::Word => match token.text.as_str() {
                    "ret" => {
                        let value = self.parse_expr().unwrap();

                        if self
                            .tokens
                            .next_if(|t| t.kind == TokenKind::Semicolon)
                            .is_none()
                        {
                            todo!("Missing semicolon after return statement");
                        }

                        return Some(Statement::Ret { value: Some(value) });
                    }
                    _ => {
                        let name = token.text;
                        let mut args = Vec::new();

                        if self
                            .tokens
                            .next_if(|t| t.kind == TokenKind::OpenParen)
                            .is_some()
                        {
                            if self
                                .tokens
                                .next_if(|t| t.kind == TokenKind::CloseParen)
                                .is_some()
                            {
                                // empty argument list
                            } else {
                                args.push(self.parse_expr().unwrap());

                                while self
                                    .tokens
                                    .next_if(|t| t.kind == TokenKind::Comma)
                                    .is_some()
                                {
                                    args.push(self.parse_expr().unwrap());
                                }
                            }

                            if self
                                .tokens
                                .next_if(|t| t.kind == TokenKind::CloseParen)
                                .is_none()
                            {
                                todo!("Missing close paren after arguments list");
                            }
                        }

                        if self
                            .tokens
                            .next_if(|t| t.kind == TokenKind::Semicolon)
                            .is_none()
                        {
                            todo!("Missing semicolon after funcall statement");
                        }

                        return Some(Statement::Funcall { name, args });
                    }
                },
                _ => todo!(),
            },
            None => todo!(),
        }
    }
}
