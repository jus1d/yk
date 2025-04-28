use crate::lexer::{Token, TokenKind};
use crate::diag;

use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;

pub const KEYWORDS: &[&'static str] = &[
    "fn", "ret",
    "if", "else", "while",
    "true", "false",
];

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
    Funcall {
        name: String,
        args: Vec<Expr>
    },
    If {
        condition: Expr,
        consequence: Vec<Statement>,
        otherwise: Vec<Statement>
    },
    While {
        condition: Option<Expr>,
        block: Vec<Statement>,
    },
    Declaration {
        name: String,
        typ: String,
        value: Option<Expr>,
    },
    Assignment {
        name: String,
        value: Expr,
    },
    Ret {
        value: Option<Expr>
    },
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
    Add, Sub, Mul, Div, Mod,
    EQ, NE, GT, LT, GE, LE,
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

        while let Some(function) = self.parse_function() {
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

    pub fn parse_function(&mut self) -> Option<Function> {
        let kw = self.tokens.next()?;
        if kw.kind != TokenKind::Word || kw.text != "fn" {
            diag::fatal!(kw.loc, "unexpected token. expected keyword `fn`, got `{}`", kw.text);
        }

        let name = self.tokens.next().unwrap();
        if name.kind != TokenKind::Word {
            diag::fatal!(name.loc, "unexpected token. expected `word`, got {}", name.kind);
        }

        if KEYWORDS.contains(&name.text.as_str()) {
            diag::fatal!(name.loc, "function name collides with reserved keyword `{}`", name.text);
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
                TokenKind::Word | TokenKind::Exclamation => {
                    let ret_type = self.tokens.next().unwrap();
                    if !is_type(&ret_type.text) {
                        diag::fatal!(ret_type.loc, "unknown return type `{}`", ret_type.text);
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
        if self.tokens.next_if(|token| token.kind == TokenKind::CloseCurly).is_some() {
            return statements;
        }

        loop {
            statements.push(self.parse_statement());
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

    pub fn parse_expression(&mut self) -> Expr {
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
                TokenKind::EqualEqual => Some(BinaryOp::EQ),
                TokenKind::NotEqual => Some(BinaryOp::NE),
                TokenKind::Greater => Some(BinaryOp::GT),
                TokenKind::Less => Some(BinaryOp::LT),
                TokenKind::GreaterEqual => Some(BinaryOp::GE),
                TokenKind::LessEqual => Some(BinaryOp::LE),
                TokenKind::Percent => Some(BinaryOp::Mod),
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
                            args.push(self.parse_expression());
                            while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                args.push(self.parse_expression());
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
                    let expr = self.parse_expression();
                    self.expect(TokenKind::CloseParen);
                    return expr;
                }
                _ => diag::fatal!(token.loc, "unexpected token: {}", token.kind),
            }
        } else {
            diag::fatal!("expected expression, got EOF");
        }
    }

    pub fn parse_statement(&mut self) -> Statement {
        match self.tokens.next() {
            Some(token) => match token.kind {
                TokenKind::Word => match token.text.as_str() {
                    "ret" => {
                        if self.tokens.next_if(|t| t.kind == TokenKind::Semicolon).is_some() {
                            return Statement::Ret { value: None };
                        }

                        let value = self.parse_expression();
                        if self.tokens.next_if(|t| t.kind == TokenKind::Semicolon).is_none() {
                            diag::fatal!(token.loc, "missing semicolon after return");
                        }

                        return Statement::Ret { value: Some(value) };
                    },
                    "if" => {
                        let condition = self.parse_expression();

                        let consequence = self.parse_block();

                        if self.tokens.next_if(|t| t.kind == TokenKind::Word && &t.text == "else").is_some() {
                            let otherwise = self.parse_block();

                            return Statement::If { condition, consequence, otherwise }
                        }

                        return Statement::If { condition, consequence, otherwise: Vec::new() }
                    },
                    "while" => {
                        if let Some(next) = self.tokens.peek() {
                            if next.kind == TokenKind::OpenCurly {
                                let block = self.parse_block();

                                return Statement::While { condition: None, block }
                            }
                        }

                        let condition = self.parse_expression();
                        let block = self.parse_block();

                        return Statement::While { condition: Some(condition), block };
                    },
                    "let" => {
                        let name = match self.tokens.next() {
                            Some(token) => {
                                if token.kind == TokenKind::Word {
                                    token.text
                                } else {
                                    diag::fatal!(token.loc, "expected identifier, got `{}`", token.text);
                                }
                            },
                            None => {
                                diag::fatal!("expected identifier, got EOF");
                            }
                        };

                        self.expect(TokenKind::Colon);

                        let typ = match self.tokens.next() {
                            Some(token) => {
                                if token.kind == TokenKind::Word && is_type(&token.text) {
                                    token.text
                                } else {
                                    diag::fatal!(token.loc, "expected type, got `{}`", token.text);
                                }
                            },
                            None => {
                                diag::fatal!("expected type, got EOF");
                            }
                        };

                        if self.tokens.next_if(|token| token.kind == TokenKind::Semicolon).is_some() {
                            return Statement::Declaration { name, typ, value: None };
                        }

                        self.expect(TokenKind::Equals);

                        let value = self.parse_expression();

                        self.expect(TokenKind::Semicolon);

                        return Statement::Declaration { name, typ, value: Some(value) };
                    },
                    _ => {
                        let name = token.text.clone();

                        if self.tokens.next_if(|token| token.kind == TokenKind::Equals).is_some() {
                            // Assignment
                            let value = self.parse_expression();
                            self.expect(TokenKind::Semicolon);
                            return Statement::Assignment { name, value }
                        }

                        // Function call
                        let mut args = Vec::new();

                        if self.tokens.next_if(|t| t.kind == TokenKind::OpenParen).is_none() {
                            let mut loc = token.loc.clone();
                            loc.col += token.text.len();
                            diag::fatal!(loc, "expected {} after function name", TokenKind::OpenParen);
                        }

                        if self.tokens.peek().map(|t| t.kind) != Some(TokenKind::CloseParen) {
                            args.push(self.parse_expression());

                            while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                                args.push(self.parse_expression());
                            }
                        }

                        if self.tokens.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            diag::fatal!("expected {} after arguments", TokenKind::CloseParen);
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
        // NOTE: ! is a never type
        "int64" | "string" | "void" | "bool" | "!" => return true,
        _ => return false,
    }
}

fn get_op_precedence(op: &BinaryOp) -> u8 {
    match op {
        BinaryOp::EQ | BinaryOp::NE => 1,
        BinaryOp::LT | BinaryOp::LE | BinaryOp::GT | BinaryOp::GE => 2,
        BinaryOp::Add | BinaryOp::Sub => 3,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 4,
    }
}

pub fn get_variable_position(name: &str, func: &Function) -> usize {
    if let Some(_) = func.params.iter().find(|param| param.name == name) {
        let position = func.params.iter().position(|p| p.name == name).unwrap();
        return position;
    } else {
        let mut position = func.params.len();
        for statement in &func.body {
            if let Statement::Declaration { name: declared, .. } = statement {
                position += 1;
                if name == declared {
                    return position;
                }
            }
        }

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
                write!(f, ");")?;
                Ok(())
            }
            Statement::If { condition, consequence, otherwise } => {
                writeln!(f, "if {} {{", condition)?;
                for stmt in consequence {
                    writeln!(f, "    {}", stmt)?;
                }
                write!(f, "    }}")?;
                writeln!(f, "else {} {{", condition)?;
                for stmt in otherwise {
                    writeln!(f, "    {}", stmt)?;
                }
                write!(f, "    }}")?;
                Ok(())
            },
            Statement::While { condition, block } => {
                if let Some(expr) = condition {
                    writeln!(f, "while {} {{", expr)?;
                } else {
                    writeln!(f, "while true {{")?;
                }

                for stmt in block {
                    writeln!(f, "    {}", stmt)?;
                }
                write!(f, "    }}")?;
                Ok(())
            },
            Statement::Ret { value } => match value {
                Some(value) => write!(f, "ret {};", value),
                None => write!(f, "ret;"),
            },
            Statement::Declaration { name, typ, value } => {
                if let Some(expr) = value {
                    write!(f, "let {}: {} = {}", name, typ, expr)?;
                } else {
                    write!(f, "let {}: {};", name, typ)?;
                }

                Ok(())
            },
            Statement::Assignment { name, value } => {
                write!(f, "{} = {}", name, value)
            }
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
        write!(f, "{}", match self {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%",
                BinaryOp::EQ => "==",
                BinaryOp::NE => "!=",
                BinaryOp::GT => ">",
                BinaryOp::LT => "<",
                BinaryOp::GE => ">=",
                BinaryOp::LE => "<=",
            }
        )
    }
}
