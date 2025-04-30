use crate::lexer::{self, Token, TokenKind};
use crate::diag;

use std::collections::HashMap;
use std::path::Path;
use std::{fmt, fs};
use std::iter::Peekable;
use std::process::Command;

pub const KEYWORDS: &[&'static str] = &[
    "fn", "ret",
    "if", "else", "while",
    "true", "false",
];

#[derive(Clone, Debug)]
pub struct Ast {
    pub functions: HashMap<String, Function>
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub ret_type: String,
    pub params: Vec<Variable>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub typ: String,
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub condition: Expr,
    pub block: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Funcall {
        name: String,
        args: Vec<Expr>
    },
    If {
        branches: Vec<Branch>,
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

#[derive(Clone, Debug)]
pub enum Literal {
    Number(i64),
    String(String),
    Bool(bool),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    EQ, NE, GT, LT, GE, LE,
    LogicalOr, LogicalAnd,
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
        let mut ast = Ast { functions: HashMap::new() };

        while let Some(token) = self.tokens.next() {
            match token.kind {
                TokenKind::Word => {
                    match token.text.as_str() {
                        "fn" => {
                            let func = self.parse_function();
                            ast.functions.insert(func.name.clone(), func);
                        },
                        "include" => {
                            let include_path = if let Some(path) = self.tokens.next() {
                                if path.kind != TokenKind::String {
                                    diag::fatal!("expected include string, got token kind {}", path.kind);
                                }
                                path.text
                            }
                            else {
                                diag::fatal!(token.loc, "expected include string, got EOF");
                            };

                            if let Some(ext) = Path::new(&include_path).extension().and_then(|ext| ext.to_str()) {
                                if ext != "yk" {
                                    diag::fatal!("included file '{}' has wrong extension: `{}`, expected `yk`", include_path, ext);
                                }
                            }
                            else {
                                diag::fatal!("included file '{}' has wrong extension", include_path);
                            }

                            let source = if include_path.starts_with("https://") {
                                read_source_via_https(&include_path)
                            } else {
                                fs::read_to_string(&include_path).unwrap_or_else(|_| {
                                    diag::fatal!("included file '{}' not found", include_path);
                                })
                            };

                            if source.is_empty() {
                                continue;
                            }

                            let lexer = lexer::Lexer::new(source.chars(), &include_path);
                            let mut parser = Parser::from_iter(lexer);

                            let included_ast = parser.parse_ast();

                            for (name, func) in included_ast.functions {
                                ast.functions.insert(name, func);
                            }

                            self.expect(TokenKind::Semicolon);
                        },
                        _ => diag::fatal!(token.loc, "unexpected token `{}`. expected `fn ...` or `include` statement", token.text),
                    }
                },
                _ => diag::fatal!(token.loc, "unexpected token kind {}. expected `fn ...` or `include` statement", token.kind),
            }
        }

        ast
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

    pub fn parse_function(&mut self) -> Function {
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

                    return Function {
                        name: name.text,
                        ret_type: "void".to_string(),
                        params,
                        body,
                    };
                }
                TokenKind::Word | TokenKind::Exclamation => {
                    let ret_type = self.tokens.next().unwrap();
                    if !is_type(&ret_type.text) {
                        diag::fatal!(ret_type.loc, "unknown return type `{}`", ret_type.text);
                    }

                    let body = self.parse_block();
                    return Function {
                        name: name.text,
                        ret_type: ret_type.text,
                        params,
                        body,
                    };
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
                TokenKind::DoublePipe => Some(BinaryOp::LogicalOr),
                TokenKind::DoubleAmpersand => Some(BinaryOp::LogicalAnd),
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
                    // Funcall
                    if self.tokens.next_if(|token| token.kind == TokenKind::OpenParen).is_some() {
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
                    }

                    // Boolean literals
                    match token.text.as_str() {
                        "true" => return Expr::Literal(Literal::Bool(true)),
                        "false" => return Expr::Literal(Literal::Bool(false)),
                        _ => {},
                    }

                    return Expr::Variable(token.text);
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
                        let mut branches = Vec::new();

                        let mut condition = self.parse_expression();
                        let mut consequence = self.parse_block();

                        branches.push(Branch {
                            condition,
                            block: consequence,
                        });

                        while let Some(_) = self.tokens.next_if(|t| t.kind == TokenKind::Word && &t.text == "else") {
                            if let Some(_) = self.tokens.next_if(|t| t.kind == TokenKind::Word && &t.text == "if") {
                                condition = self.parse_expression();
                                consequence = self.parse_block();

                                branches.push(Branch {
                                    condition,
                                    block: consequence,
                                });
                            } else {
                                let otherwise = self.parse_block();
                                return Statement::If { branches, otherwise }
                            }
                        }

                        return Statement::If { branches, otherwise: Vec::new() }
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
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => 1,
        BinaryOp::EQ | BinaryOp::NE => 2,
        BinaryOp::LT | BinaryOp::LE | BinaryOp::GT | BinaryOp::GE => 3,
        BinaryOp::Add | BinaryOp::Sub => 4,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 5,
    }
}

pub fn get_variable_position(name: &str, func: &Function) -> usize {
    match func.params.iter().position(|p| p.name == name) {
        Some(pos) => return pos,
        None => {
            let mut pos = func.params.len();
            for statement in &func.body {
                if let Statement::Declaration { name: declared, .. } = statement {
                    pos += 1;
                    if name == declared {
                        return pos;
                    }
                }
            }

            diag::fatal!("variable `{}` not found in current scope", name);
        }
    }
}

fn read_source_via_https(url: &str) -> String {
    let output = match Command::new("curl").arg("-s").arg("-S").arg("-L").arg("-f").arg(url).output() {
        Ok(out) => out,
        Err(err) => diag::fatal!("can't get source via HTTPS: {}", err)
    };

    if !output.status.success() {
        diag::fatal!("can't include via HTTPS: {}", url);
    }

    let source = String::from_utf8(output.stdout).unwrap_or_else(|err| {
        diag::fatal!("can't read source from response: {}", err);
    });

    source
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
                BinaryOp::LogicalOr => "||",
                BinaryOp::LogicalAnd => "||",
            }
        )
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(value) => write!(f, "{}", value),
            Literal::String(content) => write!(f, "\"{}\"", content),
            Literal::Bool(value) => write!(f, "{}", value),
        }
    }
}

// TODO: Introduce support for constants
