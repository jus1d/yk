use crate::lexer::{self, Loc, Token, TokenKind};
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
    pub ret_type: Type,
    pub params: Vec<Variable>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub condition: Expr,
    pub block: Vec<Statement>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Never,
    Void,
    Int64,
    String,
    Bool,
    Char,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Funcall {
        name: String,
        args: Vec<Expr>,
        loc: Loc,
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
        typ: Type,
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
    Char(char),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Variable {
        name: String,
        loc: Loc,
    },
    Literal {
        lit: Literal,
        loc: Loc,
    },
    Funcall {
        name: String,
        args: Vec<Expr>,
        loc: Loc,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        loc: Loc,
    },
    Index {
        collection: Box<Expr>,
        index: Box<Expr>,
        loc: Loc,
    },
}

impl Expr {
    pub fn loc(self) -> Loc {
        match self {
            Expr::Variable { loc, .. } => loc,
            Expr::Literal { loc, .. } => loc,
            Expr::Funcall { loc, .. } => loc,
            Expr::Binary { loc, .. } => loc,
            Expr::Index { loc, .. } => loc,
        }
    }
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
        let mut ast = Ast {
            functions: HashMap::new(),
        };

        while let Some(token) = self.tokens.next() {
            match token.kind {
                TokenKind::Word => {
                    match token.text.as_str() {
                        "fn" => {
                            let func = self.parse_function();
                            ast.functions.insert(func.name.clone(), func);
                        },
                        "include" => {
                            let include_path_token = match self.tokens.next() {
                                Some(token) => token,
                                None => diag::fatal!(token.loc, "expected include string, got EOF"),
                            };

                            if include_path_token.kind != TokenKind::String {
                                diag::fatal!(include_path_token.loc, "expected `string` as include path, got token kind {}", include_path_token.kind);
                            }

                            let include_path = include_path_token.text;
                            if let Some(ext) = Path::new(&include_path).extension().and_then(|ext| ext.to_str()) {
                                if ext != "yk" {
                                    diag::fatal!(include_path_token.loc, "included file '{}' has wrong extension: `{}`, expected `yk`", include_path, ext);
                                }
                            }
                            else {
                                diag::fatal!(include_path_token.loc, "included file '{}' has wrong extension", include_path);
                            }

                            let source = if include_path.starts_with("https://") {
                                read_source_via_https(&include_path, include_path_token.loc)
                            } else {
                                fs::read_to_string(&include_path).unwrap_or_else(|_| {
                                    diag::fatal!(include_path_token.loc, "included file '{}' not found", include_path);
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
                    let typ_str = token.text.clone();
                    let typ = match get_primitive_type(&typ_str) {
                        Some(typ) => typ,
                        None => diag::fatal!(token.loc, "unknown type `{}`", typ_str),
                    };

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
                        ret_type: Type::Void,
                        params,
                        body,
                    };
                }
                TokenKind::Word | TokenKind::Exclamation => {
                    let return_type_token = self.tokens.next().unwrap();
                    let return_type = match get_primitive_type(&return_type_token.text) {
                        Some(typ) => typ,
                        None => diag::fatal!(return_type_token.loc, "unknown return type `{}`", return_type_token.text),
                    };

                    let body = self.parse_block();
                    return Function {
                        name: name.text,
                        ret_type: return_type,
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

        let mut primary = self.parse_primary_expr();
        if let Some(_) = self.tokens.next_if(|token| token.kind == TokenKind::OpenBracket) {
            let index = self.parse_primary_expr();
            self.expect(TokenKind::CloseBracket);

            primary = Expr::Index { collection: Box::new(primary.clone()), index: Box::new(index), loc: primary.loc() }
        }

        loop {
            let op = match self.tokens.peek() {
                Some(token) => match token.kind {
                    TokenKind::Plus => BinaryOp::Add,
                    TokenKind::Minus => BinaryOp::Sub,
                    TokenKind::Star => BinaryOp::Mul,
                    TokenKind::Slash => BinaryOp::Div,
                    TokenKind::EqualEqual => BinaryOp::EQ,
                    TokenKind::NotEqual => BinaryOp::NE,
                    TokenKind::Greater => BinaryOp::GT,
                    TokenKind::Less => BinaryOp::LT,
                    TokenKind::GreaterEqual => BinaryOp::GE,
                    TokenKind::LessEqual => BinaryOp::LE,
                    TokenKind::Percent => BinaryOp::Mod,
                    TokenKind::DoublePipe => BinaryOp::LogicalOr,
                    TokenKind::DoubleAmpersand => BinaryOp::LogicalAnd,
                    _ => break,
                },
                None => break,
            };

            let prec = get_op_precedence(&op);
            if prec < min_prec {
                break;
            }

            let token = self.tokens.next().unwrap();

            let rhs = self.parse_expr_prec(prec + 1);
            primary = Expr::Binary {
                op,
                lhs: Box::new(primary),
                rhs: Box::new(rhs),
                loc: token.loc.clone(),
            };
        }

        primary
    }

    fn parse_primary_expr(&mut self) -> Expr {
        if let Some(token) = self.tokens.next() {
            match token.kind {
                TokenKind::Number => return Expr::Literal { lit: Literal::Number(token.number), loc: token.loc },
                TokenKind::String => return Expr::Literal { lit: Literal::String(token.text), loc: token.loc },
                TokenKind::Char => return Expr::Literal { lit: Literal::Char(token.text.chars().nth(0).unwrap()), loc: token.loc },
                // Negative integer
                TokenKind::Minus => {
                    match self.tokens.next() {
                        None => diag::fatal!(token.loc, "expected expression, found EOF"),
                        Some(number_token) => {
                            match number_token.kind {
                                TokenKind::Number => return Expr::Literal { lit: Literal::Number(-number_token.number), loc: token.loc },
                                _ => diag::fatal!(token.loc, "expected integer after `-`, got `{}`", token.kind),
                            }
                        }
                    }
                },
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
                            loc: token.loc,
                        };
                    }

                    // Boolean literals
                    match token.text.as_str() {
                        "true" => return Expr::Literal { lit: Literal::Bool(true), loc: token.loc },
                        "false" => return Expr::Literal { lit: Literal::Bool(false), loc: token.loc },
                        _ => {},
                    }

                    return Expr::Variable { name: token.text, loc: token.loc };
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
                                match get_primitive_type(&token.text) {
                                    Some(typ) => typ,
                                    None => diag::fatal!(token.loc, "expected type, got `{}`", token.text),
                                }
                            },
                            None => {
                                diag::fatal!("expected type, got EOF");
                            }
                        };

                        if typ == Type::Never {
                            diag::fatal!("cannot declare variable with `{}` type", typ);
                        }

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
                        let funcall_loc = token.loc.clone();

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

                        self.expect(TokenKind::CloseParen);
                        self.expect(TokenKind::Semicolon);

                        return Statement::Funcall { name, args, loc: funcall_loc };
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

fn get_op_precedence(op: &BinaryOp) -> u8 {
    match op {
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => 1,
        BinaryOp::EQ | BinaryOp::NE => 2,
        BinaryOp::LT | BinaryOp::LE | BinaryOp::GT | BinaryOp::GE => 3,
        BinaryOp::Add | BinaryOp::Sub => 4,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 5,
    }
}

fn read_source_via_https(url: &str, include_loc: Loc) -> String {
    let output = match Command::new("curl").arg("-s").arg("-S").arg("-L").arg("-f").arg(url).output() {
        Ok(out) => out,
        Err(err) => diag::fatal!(include_loc, "can't get source via HTTPS: {}", err)
    };

    if !output.status.success() {
        diag::fatal!(include_loc, "can't include via HTTPS: {}", url);
    }

    let source = String::from_utf8(output.stdout).unwrap_or_else(|err| {
        diag::fatal!(include_loc, "can't read source from response: {}", err);
    });

    source
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Variable { name, .. } => write!(f, "{}", name),
            Expr::Literal { lit, .. } => write!(f, "{}", lit),
            Expr::Funcall { name, args, .. } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Binary { op, lhs, rhs, .. } => {
                write!(f, "({} {} {})", lhs, op, rhs)
            },
            Expr::Index { collection, index, .. } => {
                write!(f, "{}[{}]", collection, index)
            },
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
            Literal::String(content) => write!(f, "\"{}\"", content.replace("\n", "\\n")),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::Char(ch) => write!(f, "{}", ch),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Type::Never => "!",
            Type::Void => "void",
            Type::Int64 => "int64",
            Type::String => "string",
            Type::Bool => "bool",
            Type::Char => "char",
        })
    }
}

pub fn get_primitive_type(typ: &str) -> Option<Type> {
    match typ {
        "void" => Some(Type::Void),
        "string" => Some(Type::String),
        "int64" => Some(Type::Int64),
        "bool" => Some(Type::Bool),
        "char" => Some(Type::Char),
        "!" => Some(Type::Never),
        _ => None,
    }
}

// TODO: Allow not to specify type of declaration, where it can be known from value:
//     let x = 69;
//             ^^
//             69 - is definitely an int64 => typeof(x) = int64
//
// TODO: Introduce support for constants
