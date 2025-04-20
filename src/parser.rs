use crate::lexer::{Token, TokenKind};
use crate::diag;

use std::fmt;
use std::iter::Peekable;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub ret_type: String,
    pub params: Vec<Param>,
    pub body: Vec<Statement>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub typ: String,
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

pub struct Parser<Tokens> where Tokens: Iterator<Item = Token> {
    pub tokens: Peekable<Tokens>,
}

impl<Tokens> Parser<Tokens> where Tokens: Iterator<Item = Token> {
    pub fn from_iter(tokens: Tokens) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { functions: vec![] };

        while let Some(function) = self.parse_fn() {
            program.functions.push(function);
        }

        return program;
    }

    pub fn parse_param(&mut self) -> Option<Param> {
        match self.tokens.peek() {
            Some(token) => match token.kind {
                TokenKind::Word => {
                    let typ = token.text.clone();
                    if !is_type(&typ) {
                        diag::fatal!(token.loc, "unknown type `{}`", typ);
                    }

                    self.tokens.next();
                    if let Some(token) = self.tokens.next() {
                        if token.kind != TokenKind::Word {
                            diag::fatal!(token.loc, "expected token kind {}, got {}", TokenKind::Word, token.kind);
                        }

                        return Some(Param {
                            typ,
                            name: token.text,
                        });
                    }

                    return None;
                }
                other => diag::fatal!(token.loc, "expected token kind {}, got {}", TokenKind::Word, other),
            },
            None => None,
        }
    }

    pub fn parse_fn(&mut self) -> Option<Function> {
        let kw = self.tokens.next()?;
        if kw.kind != TokenKind::Word || kw.text != "fn" {
            diag::fatal!(kw.loc, "unexpected token. expected keyword `fn`, got `{}`", kw.text);
        }

        let name = self.tokens.next().unwrap();
        if name.kind != TokenKind::Word {
            diag::fatal!(name.loc, "unexpected token. expected `word`, got {}", name.kind);
        }

        self.expect(TokenKind::OpenParen);

        let mut params = Vec::new();

        if self.tokens.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
            params.push(self.parse_param().unwrap());

            while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                params.push(self.parse_param().unwrap());
            }

            self.expect(TokenKind::CloseParen);
        }

        match self.tokens.peek() {
            Some(token) => match token.kind {
                TokenKind::OpenCurly => {
                    let body = self.parse_block();

                    return Some(Function {
                        name: name.text,
                        ret_type: "void".to_string(),
                        params,
                        body,
                    });
                }
                TokenKind::Word => {
                    let ret_type = self.tokens.next().unwrap();
                    if ret_type.kind != TokenKind::Word {
                        diag::fatal!(ret_type.loc, "expected token kind {}, got {}", TokenKind::Word, ret_type.kind);
                    }

                    if !is_type(&ret_type.text) {
                        diag::fatal!(ret_type.loc, "unknown type `{}`", ret_type.text);
                    }

                    let body = self.parse_block();

                    return Some(Function {
                        name: name.text,
                        ret_type: ret_type.text,
                        params,
                        body,
                    });
                }
                other => diag::fatal!(token.loc, "expected block or return type, got {}", other),
            },
            None => diag::fatal!("error: expected block or return type, got EOF"),
        }
    }

    pub fn parse_block(&mut self) -> Vec<Statement> {
        let lbrace = self.tokens.next().unwrap();
        if lbrace.kind != TokenKind::OpenCurly {
            diag::fatal!(lbrace.loc, "expected {}, got {}", TokenKind::OpenCurly, lbrace.kind);
        }

        let mut statements = Vec::new();

        loop {
            statements.push(self.parse_stmt());
            if let Some(token) = self.tokens.peek() {
                if token.kind == TokenKind::CloseCurly {
                    break;
                }
            } else {
                diag::fatal!("expected terminating {} or next statement, got EOF", TokenKind::CloseCurly);
            }
        }

        let rbrace = self.tokens.next().unwrap();
        if rbrace.kind != TokenKind::CloseCurly {
            diag::fatal!(rbrace.loc, "expected {}, got {}", TokenKind::CloseCurly, rbrace.kind);
        }

        return statements;
    }

    pub fn parse_expr(&mut self) -> Expr {
        self.parse_expr_prec(0)
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> Expr {
        if self.tokens.peek().is_none() {
            diag::fatal!("expected expression, got EOF");
        }

        let mut lhs = self.parse_primary_expr();

        while let Some(token) = self.tokens.peek() {
            let op = match token.kind {
                TokenKind::Plus => Some(BinaryOp::Add),
                TokenKind::Minus => Some(BinaryOp::Sub),
                TokenKind::Star => Some(BinaryOp::Mul),
                TokenKind::Slash => Some(BinaryOp::Div),
                _ => None,
            };

            if let Some(op) = op {
                let prec = get_op_precedence(&op);
                if prec < min_prec {
                    break;
                }

                self.tokens.next();
                let rhs = self.parse_expr_prec(prec + 1);
                lhs = Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
            } else {
                break;
            }
        }

        return lhs;
    }

    fn parse_primary_expr(&mut self) -> Expr {
        if let Some(token) = self.tokens.next() {
            match token.kind {
                TokenKind::Number => return Expr::Number(token.number),
                TokenKind::String => return Expr::String(token.text),
                TokenKind::Word => {
                    if self.tokens.peek().is_none() {
                        return Expr::Variable(token.text);
                    }

                    let next_token = self.tokens.peek().unwrap();
                    if next_token.kind == TokenKind::OpenParen {
                        self.tokens.next();
                        let mut args = Vec::new();

                        if self.tokens.peek().is_some_and(|t| t.kind != TokenKind::CloseParen) {
                            args.push(self.parse_expr());
                            while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                args.push(self.parse_expr());
                            }
                        }

                        self.expect(TokenKind::CloseParen);
                        return Expr::Funcall {
                            name: token.text,
                            args,
                        };
                    } else {
                        return Expr::Variable(token.text);
                    }
                }
                TokenKind::OpenParen => {
                    let expr = self.parse_expr();
                    self.expect(TokenKind::CloseParen);
                    return expr;
                }
                _ => diag::fatal!(token.loc, "unexpected token: {:?}", token.kind),
            }
        } else {
            diag::fatal!("expected expression, got EOF");
        }
    }

    pub fn parse_stmt(&mut self) -> Statement {
        match self.tokens.next() {
            Some(token) => match token.kind {
                TokenKind::Word => match token.text.as_str() {
                    "ret" => {
                        let value = self.parse_expr();

                        if self.tokens.next_if(|t| t.kind == TokenKind::Semicolon).is_none() {
                            diag::fatal!(token.loc, "missing semicolon after return");
                        }

                        return Statement::Ret { value: Some(value) };
                    }
                    _ => {
                        let name = token.text;
                        let mut args = Vec::new();

                        if self.tokens.next_if(|t| t.kind == TokenKind::OpenParen).is_some() {
                            if self.tokens.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                                args.push(self.parse_expr());

                                while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                    args.push(self.parse_expr());
                                }
                            }

                            if self.tokens.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                                diag::fatal!("expected {} after arguments", TokenKind::CloseParen);
                            }
                        } else {
                            diag::fatal!(token.loc, "expected {} after function name", TokenKind::OpenParen);
                        }

                        if self.tokens.next_if(|t| t.kind == TokenKind::Semicolon).is_none() {
                            diag::fatal!("expected {} after function call", TokenKind::Semicolon);
                        }

                        return Statement::Funcall { name, args };
                    }
                },
                _ => diag::fatal!(token.loc, "expected statement, got {}\nonly `ret` and `funcall` statements are supported yet", token.kind),
            },
            None => diag::fatal!("expected statement, got EOF"),
        }
    }

    fn expect(&mut self, kind: TokenKind) {
        match self.tokens.next() {
            None => diag::fatal!("expected token of kind {}, got EOF", kind),
            Some(token) => {
                if token.kind != kind {
                    diag::fatal!(token.loc, "expected token {}, got {}", kind, token.kind)
                };
            }
        }
    }
}

fn is_type(s: &str) -> bool {
    match s {
        "int64" | "string" | "void" => return true,
        _ => return false,
    }
}

fn get_op_precedence(op: &BinaryOp) -> u8 {
    match op {
        BinaryOp::Add | BinaryOp::Sub => 1,
        BinaryOp::Mul | BinaryOp::Div => 2,
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, function) in self.functions.iter().enumerate() {
            write!(f, "{}", function)?;
            if i != self.functions.len() - 1 {
                writeln!(f, "\n")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i == 0 {
                write!(f, "{} {}", param.typ, param.name)?;
            } else {
                write!(f, ", {} {}", param.typ, param.name)?;
            }
        }
        write!(f, ") {} {{\n", self.ret_type)?;
        for statement in &self.body {
            writeln!(f, "    {}", statement)?;
        }
        write!(f, "}}")
    }
}

#[allow(unreachable_patterns)]
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Funcall { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ");")
            }
            Statement::Ret { value } => match value {
                Some(value) => write!(f, "ret {};", value),
                None => write!(f, "ret;"),
            },
            _ => unreachable!(),
        }
    }
}

#[allow(unreachable_patterns)]
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Variable(ident) => write!(f, "{}", ident),
            Expr::Number(value) => write!(f, "{}", value),
            Expr::String(value) => write!(f, "\"{}\"", value),
            Expr::Funcall { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Binary { op, lhs, rhs } => {
                write!(f, "({} {} {})", lhs, op, rhs)
            }
            _ => unreachable!(),
        }
    }
}

#[allow(unreachable_patterns)]
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                _ => unreachable!(),
            }
        )
    }
}
