use crate::lexer::{Token, TokenKind};

use std::fmt;
use std::iter::Peekable;

macro_rules! exit {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Program {
    functions: Vec<Function>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Function {
    name: String,
    ret_type: String,
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

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { functions: vec![] };

        while let Some(function) = self.parse_fn() {
            program.functions.push(function);
        }

        return program;
    }

    pub fn parse_fn(&mut self) -> Option<Function> {
        let kw = self.tokens.next()?;
        if kw.kind != TokenKind::Word || kw.text != "fn" {
            todo!("Implement error handling for invalid function definition");
        }

        let name = self.tokens.next().unwrap();
        if name.kind != TokenKind::Word {
            todo!("Implement error handling for invalid function name");
        }

        self.expect(TokenKind::OpenParen);

        // TODO: parse args
        while let Some(_) = self.tokens.next_if(|t| t.kind != TokenKind::CloseParen) {}

        self.expect(TokenKind::CloseParen);

        match self.tokens.peek() {
            Some(token) => match token.kind {
                TokenKind::OpenCurly => {
                    let body = self.parse_block()?;

                    return Some(Function {
                        name: name.text,
                        ret_type: "void".to_string(),
                        body,
                    });
                }
                TokenKind::Word => {
                    let ret_type = self.tokens.next().unwrap();
                    if ret_type.kind != TokenKind::Word {
                        todo!("Implement error handling for invalid function name");
                    }

                    if !is_type(ret_type.text.clone()) {
                        todo!("Unknown type `{}`", ret_type.text);
                    }

                    let body = self.parse_block()?;

                    return Some(Function {
                        name: name.text,
                        ret_type: ret_type.text,
                        body,
                    });
                }
                _ => todo!("Expected block or return type"),
            },
            None => {
                todo!("Expected block or return type, got none");
            }
        }
    }

    pub fn parse_block(&mut self) -> Option<Vec<Statement>> {
        let lbrace = self.tokens.next().unwrap();
        if lbrace.kind != TokenKind::OpenCurly {
            todo!("Implement error handling for invalid block");
        }

        let mut statements = Vec::new();

        while let Some(stmt) = self.parse_stmt() {
            statements.push(stmt);
            if let Some(token) = self.tokens.peek() {
                if token.kind == TokenKind::CloseCurly {
                    break;
                }
            }
        }

        let rbrace = self.tokens.next().unwrap();
        if rbrace.kind != TokenKind::CloseCurly {
            todo!("Implement error handling for invalid block");
        }

        Some(statements)
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        if self.tokens.peek().is_none() {
            exit!("error: unexpected EOF");
        }

        let primary = self.parse_primary_expr();

        if self.is_operator() {
            return Some(self.parse_binary_op(primary));
        }

        return Some(primary);
    }

    fn parse_primary_expr(&mut self) -> Expr {
        if let Some(token) = self.tokens.next() {
            match token.kind {
                TokenKind::Number => return Expr::Number(token.number),
                TokenKind::String => return Expr::String(token.text),
                TokenKind::Word => {
                    if self.tokens.peek().is_none() {
                        exit!("error: unexpected EOF");
                    }

                    let next_token = self.tokens.peek().unwrap();
                    match next_token.kind {
                        TokenKind::OpenParen => {
                            self.expect(TokenKind::OpenParen);

                            let mut args = Vec::new();

                            if self.tokens.peek().is_none() {
                                exit!("error: unexpected EOF");
                            }

                            if self.tokens.peek().unwrap().kind == TokenKind::CloseParen {
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

                                self.expect(TokenKind::CloseParen);
                            }

                            return Expr::Funcall {
                                name: token.text,
                                args,
                            };
                        }
                        _ => return Expr::Variable(token.text),
                    }
                }
                TokenKind::OpenParen => {
                    exit!(
                        "{}: error: grouping expression are not supported yet",
                        token.loc,
                    )
                }
                _ => exit!("{}: error: unexpected token: {:?}", token.loc, token.kind),
            }
        } else {
            exit!("error: unexpected EOF");
        }
    }

    fn is_operator(&mut self) -> bool {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                    return true
                }
                _ => return false,
            }
        }

        return false;
    }

    fn parse_binary_op(&mut self, lhs: Expr) -> Expr {
        match self.tokens.next() {
            Some(token) => match token.kind {
                TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                    let op = match token.kind {
                        TokenKind::Plus => BinaryOp::Add,
                        TokenKind::Minus => BinaryOp::Sub,
                        TokenKind::Star => BinaryOp::Mul,
                        TokenKind::Slash => BinaryOp::Div,
                        _ => unreachable!(),
                    };

                    return Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(self.parse_expr().unwrap()),
                    };
                }
                _ => {
                    exit!("{}: error: unexpected token: {:?}", token.loc, token.kind)
                }
            },
            None => exit!("error: unexpected EOF"),
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

    fn expect(&mut self, kind: TokenKind) {
        match self.tokens.next() {
            None => exit!("error: expected token of kind {:?}, got EOF", kind),
            Some(token) => {
                if token.kind != kind {
                    exit!(
                        "{}: error: expected token of kind {:?}, got {:?}",
                        token.loc,
                        kind,
                        token.kind
                    )
                };
            }
        }
    }
}

fn is_type(s: String) -> bool {
    match s.as_str() {
        "int64" | "string" | "void" => return true,
        _ => return false,
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
        writeln!(f, "fn {}() {} {{", self.name, self.ret_type)?;
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
