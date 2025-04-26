use crate::lexer::{Token, TokenKind};
use crate::diag;

use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;

#[derive(Clone)]
pub struct Ast {
    pub functions: HashMap<String, Function>
}

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub ret_type: String,
    pub params: Vec<Variable>,
    pub body: Vec<Statement>,
}

#[derive(Clone)]
pub struct Variable {
    pub name: String,
    pub typ: String,
}

#[derive(Clone)]
pub enum Statement {
    Funcall { name: String, args: Vec<Expr> },
    Ret { value: Option<Expr> },
}

#[derive(Clone)]
pub enum Literal {
    Number(i64),
    String(String),
}

#[derive(Clone)]
pub enum Expr {
    Variable(String),
    Literal(Literal),
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

#[derive(Clone)]
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

    pub fn parse_ast(&mut self) -> Ast {
        let mut program = Ast { functions: HashMap::new() };

        while let Some(function) = self.parse_fn() {
            program.functions.insert(function.name.clone(), function);
        }

        return program;
    }

    pub fn parse_param(&mut self) -> Option<Variable> {
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

                        return Some(Variable {
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
                TokenKind::Number => return Expr::Literal(Literal::Number(token.number)),
                TokenKind::String => return Expr::Literal(Literal::String(token.text)),
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
                _ => diag::fatal!(token.loc, "unexpected token: {}", token.kind),
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
                        let name = token.text.clone();
                        let mut args = Vec::new();

                        // Require opening parenthesis
                        if self.tokens.next_if(|t| t.kind == TokenKind::OpenParen).is_none() {
                            let mut loc = token.loc.clone();
                            loc.col += token.text.len();
                            diag::fatal!(loc, "expected {} after function name", TokenKind::OpenParen);
                        }

                        // Parse arguments only if not immediately followed by closing parenthesis
                        if self.tokens.peek().map(|t| t.kind) != Some(TokenKind::CloseParen) {
                            args.push(self.parse_expr());

                            while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                args.push(self.parse_expr());
                            }
                        }

                        // Require closing parenthesis
                        if self.tokens.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            diag::fatal!("expected {} after arguments", TokenKind::CloseParen);
                        }

                        // Require semicolon
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

pub fn get_variable_position(ast: &Ast, func_name: &str, variable_name: &str) -> usize {
    if let Some(func) = ast.functions.get(func_name) {
        if let Some(_) = func.params.iter().find(|param| param.name == variable_name) {
            let position = func.params.iter().position(|p| p.name == variable_name).unwrap();
            return position;
        } else {
            todo!();
        }
    } else {
        todo!();
    }
}


impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (_, function)) in self.functions.iter().enumerate() {
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
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(value) => write!(f, "{}", value),
            Literal::String(content) => write!(f, "\"{}\"", content),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Variable(ident) => write!(f, "{}", ident),
            Expr::Literal(lit) => write!(f, "{}", lit),
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
        }
    }
}

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
            }
        )
    }
}
