pub mod ast;

use crate::diag;
use crate::lexer;
use crate::lexer::token::{Loc, Token, TokenKind, KEYWORDS};

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::{fmt, fs};
use std::iter::Peekable;
use std::process::{exit, Command};
use ast::{Ast, Function, Branch, Statement, Expr, Literal, BinaryOp, UnaryOp, Type, Variable};

pub struct Parser<Tokens> where Tokens: Iterator<Item = Token> {
    pub tokens: Peekable<Tokens>,
    pub include_folders: Vec<String>,
}

impl<Tokens> Parser<Tokens> where Tokens: Iterator<Item = Token> {
    pub fn from_iter(tokens: Tokens, include_folders: Vec<String>) -> Self {
        Parser {
            tokens: tokens.peekable(),
            include_folders,
        }
    }

    pub fn parse_ast(&mut self) -> Ast {
        let mut ast = Ast {
            functions: HashMap::new(),
        };

        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Keyword => match token.text.as_str() {
                    "fn" => {
                        let func = self.parse_function();
                        ast.functions.insert(func.name.clone(), func);
                    },
                    "include" => {
                        let included = self.parse_include();
                        for (name, func) in included.functions {
                            ast.functions.insert(name, func);
                        }
                    },
                    _ => {
                        eprintln!("{}: error: unexpected keyword `{}`", token.loc, token.text);
                        eprintln!("{}: note: items can start with: `include` or `fn`", token.loc);
                        exit(1);
                    }
                },
                _ => {
                    eprintln!("{}: error: expected keyword, but got `{}`", token.loc, token.text);
                    eprintln!("{}: note: items can start with next keywords: `include` or `fn`", token.loc);
                    exit(1);
                },
            }
        }

        ast
    }

    fn parse_identifier(&mut self) -> Token {
        match self.tokens.next() {
            None => {
                eprintln!("error: expected identifier, but got EOF");
                exit(1);
            },
            Some(token) if token.kind == TokenKind::Identifier => token,
            Some(token) => {
                eprintln!("{}: error: expected identifier, but got `{}`", token.loc, token.text);
                exit(1);
            }
        }
    }

    fn parse_function(&mut self) -> Function {
        self.expect_keyword("fn");
        let name_token = self.parse_identifier();

        if KEYWORDS.contains(&name_token.text.as_str()) {
            eprintln!("{}: error: function name collides with reserved keyword `{}`", name_token.loc, name_token.text);
            exit(1);
        }

        self.expect(TokenKind::OpenParen);

        let mut params = Vec::new();

        if self.tokens.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
            loop {
                params.push(self.parse_type_and_name());

                match self.tokens.next() {
                    None => {
                        eprintln!("error: expected either comma or close paren, but got EOF");
                        exit(1);
                    },
                    Some(token) => match token.kind {
                        TokenKind::Comma => continue,
                        TokenKind::CloseParen => break,
                        _ => {
                            eprintln!("{}: error: expected either comma or close paren, but got `{}`", token.loc, token.text);
                            exit(1);
                        },
                    }
                }
            }
        }

        let return_type = if let Some(token) = self.tokens.next_if(|token| token.kind == TokenKind::Identifier || token.kind == TokenKind::Exclamation) {
            match get_primitive_type(&token.text) {
                Some(typ) => typ,
                None => {
                    eprintln!("{}: error: unknown type `{}`", token.loc, token.text);
                    exit(1);
                },
            }
        } else {
            Type::Void
        };

        match self.tokens.peek() {
            None => {
                eprintln!("expected function body or return type, but got EOF");
                exit(1);
            },
            Some(token) => match token.kind {
                TokenKind::FatArrow => {
                    self.expect(TokenKind::FatArrow);
                    let expr = self.parse_expression();
                    self.expect(TokenKind::Semicolon);

                    let mut body = Vec::new();
                    body.push(Statement::Ret { value: Some(expr) });

                    return Function {
                        name: name_token.text,
                        name_loc: name_token.loc,
                        ret_type: return_type,
                        params,
                        body,
                    };
                },
                _ => {
                    let body = self.parse_block();

                    return Function {
                        name: name_token.text,
                        name_loc: name_token.loc,
                        ret_type: return_type,
                        params,
                        body,
                    };
                }
            },
        }
    }

    // this function parses constructions like: `int64 counter` (<type> <identifier>)
    fn parse_type_and_name(&mut self) -> Variable {
        match self.tokens.next() {
            None => diag::fatal!("expected type, but got EOF"),
            Some(token) => match token.kind {
                TokenKind::Identifier => {
                    let typ = match get_primitive_type(&token.text) {
                        Some(typ) => typ,
                        None => {
                            eprintln!("{}: error: unknown type `{}`", token.loc, token.text);
                            exit(1);
                        },
                    };

                    match self.tokens.next() {
                        Some(token) => {
                            if token.kind != TokenKind::Identifier {
                                eprintln!("{}: error: expected identifier, but got `{}`", token.loc, token.text);
                                exit(1);
                            }

                            return Variable {
                                typ,
                                name: token.text,
                            };
                        },
                        None => diag::fatal!("expected identifier, but got EOF")
                    }
                }
                _ => {
                    eprintln!("{}: error: expected identifier, got `{}`", token.loc, token.text);
                    exit(1);
                },
            },
        }
    }

    fn parse_include(&mut self) -> Ast {
        self.expect_keyword("include");
        let include_path_token = match self.tokens.next() {
            Some(token) => token,
            None => diag::fatal!("expected include string, got EOF"),
        };

        if include_path_token.kind != TokenKind::String {
            eprintln!("{}: error: expected string as include path, but got token kind `{}`", include_path_token.loc, include_path_token.kind);
            exit(1);
        }

        self.expect(TokenKind::Semicolon);

        let include_path = include_path_token.text;
        if let Some(ext) = Path::new(&include_path).extension().and_then(|ext| ext.to_str()) {
            if ext != "yk" {
                eprintln!("{}: error: included file '{}' has wrong extension: `{}`, expected `yk`", include_path_token.loc, include_path, ext);
                exit(1);
            }
        } else {
            eprintln!("{}: error: included file '{}' has wrong extension", include_path_token.loc, include_path);
            exit(1);
        }

        let source = if include_path.starts_with("https://") {
            read_source_via_https(&include_path, include_path_token.loc)
        } else {
            if self.include_folders.len() == 0 {
                eprintln!("{}: error: no include folders added, to find file `{}`",
                    include_path_token.loc, include_path);
                exit(1);
            }

            let mut found_path: Option<PathBuf> = None;
            for folder in &self.include_folders {
                let path = Path::new(&folder).join(&include_path);
                if path.exists() {
                    if let Some(found_path) = found_path {
                        eprintln!("{}: error: cannot determine which file to include, some options: {}, {}",
                            include_path_token.loc, path.to_str().unwrap(), found_path.to_str().unwrap());
                        exit(1);
                    }
                    found_path = Some(path);
                }
            }

            match found_path {
                Some(path) => fs::read_to_string(&path).unwrap_or_else(|err| {
                    eprintln!("{}: error: could not read included file '{}': {}", include_path_token.loc, path.display(), err);
                    exit(1);
                }),
                None => {
                    let searched_paths = self.include_folders.iter()
                        .map(|f| format!("{}/{}", f, include_path))
                        .collect::<Vec<_>>()
                        .join(", ");

                    eprintln!("{}: error: included file '{}' not found in any of include folders: {}",
                        include_path_token.loc, include_path, searched_paths);
                    exit(1);
                }
            }
        };

        let lexer = lexer::Lexer::new(source.chars(), &include_path);
        let mut parser = Parser::from_iter(lexer, self.include_folders.clone());

        return parser.parse_ast();
    }

    pub fn parse_block(&mut self) -> Vec<Statement> {
        let lbrace = self.tokens.next().unwrap();
        if lbrace.kind != TokenKind::OpenCurly {
            diag::fatal!(lbrace.loc, "expected `{}`, got `{}`", TokenKind::OpenCurly, lbrace.kind);
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
                diag::fatal!("expected terminating `{}` or next statement, got EOF", TokenKind::CloseCurly);
            }
        }

        let rbrace = self.tokens.next().unwrap();
        if rbrace.kind != TokenKind::CloseCurly {
            diag::fatal!(rbrace.loc, "expected `{}`, got `{}`", TokenKind::CloseCurly, rbrace.kind);
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

        if let Some(token) =  self.tokens.next_if(|token| token.kind == TokenKind::Minus) {
            let operand = self.parse_expr_prec(10);
            if matches!(operand, Expr::Unary { .. }) {
                diag::fatal!(operand.clone().loc(), "cannot nest unary operations");
            }

            return Expr::Unary { op: UnaryOp::Negate, operand: Box::new(operand), loc: token.loc }
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
                TokenKind::Identifier => {
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
                TokenKind::Keyword => {
                    // Boolean literals
                    match token.text.as_str() {
                        "true" => return Expr::Literal { lit: Literal::Bool(true), loc: token.loc },
                        "false" => return Expr::Literal { lit: Literal::Bool(false), loc: token.loc },
                        _ => diag::fatal!("unexpected keyword `{}`", token.text),
                    }
                }
                TokenKind::OpenParen => {
                    let expr = self.parse_expression();
                    self.expect(TokenKind::CloseParen);
                    return expr;
                }
                _ => diag::fatal!(token.loc, "unexpected token: `{}`", token.kind),
            }
        } else {
            eprintln!("error: expected expression, got EOF");
            exit(1);
        }
    }

    pub fn parse_statement(&mut self) -> Statement {
        match self.tokens.next() {
            Some(token) => match token.kind {
                TokenKind::Keyword => match token.text.as_str() {
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

                        while let Some(_) = self.tokens.next_if(|t| t.kind == TokenKind::Keyword && &t.text == "else") {
                            if let Some(_) = self.tokens.next_if(|t| t.kind == TokenKind::Keyword && &t.text == "if") {
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
                        let name_token = match self.tokens.next() {
                            Some(token) => {
                                if token.kind == TokenKind::Identifier {
                                    token
                                } else {
                                    eprintln!("{}: error: expected identifier, got `{}`", token.loc, token.text);
                                    exit(1);
                                }
                            },
                            None => {
                                eprintln!("expected identifier, got EOF");
                                exit(1);
                            }
                        };

                        match self.tokens.next() {
                            Some(token) => match token.kind {
                                TokenKind::Colon => {
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

                                    if typ == Type::Never || typ == Type::Void {
                                        diag::fatal!(token.loc, "cannot declare variable with `{}` type", typ);
                                    }

                                    if self.tokens.next_if(|token| token.kind == TokenKind::Semicolon).is_some() {
                                        let value = get_zero_value(typ.clone(), Loc::empty());
                                        return Statement::Declaration { name: name_token.text, name_loc: name_token.loc, typ: Some(typ), value };
                                    }

                                    self.expect(TokenKind::Assign);
                                    let value = self.parse_expression();
                                    self.expect(TokenKind::Semicolon);
                                    return Statement::Declaration { name: name_token.text, name_loc: name_token.loc, typ: Some(typ), value };
                                },
                                TokenKind::Assign => {
                                    let value = self.parse_expression();
                                    self.expect(TokenKind::Semicolon);
                                    return Statement::Declaration { name: name_token.text, name_loc: name_token.loc, typ: None, value };
                                },
                                TokenKind::Semicolon => diag::fatal!(token.loc, "cannot know type of variable at compile time, specify type annotation or assign a value"),
                                _ => diag::fatal!(token.loc, "expected `{}` or `{}`, got `{}`", TokenKind::Colon, TokenKind::Assign, token.text),
                            },
                            None => diag::fatal!(token.loc, "expected `{}` or `{}`, got EOF", TokenKind::Colon, TokenKind::Assign),
                        }
                    },
                    _ => diag::fatal!("unexpected keyword `{}`", token.text),
                },
                TokenKind::Identifier => {
                    let name = token.text.clone();
                    let loc = token.loc.clone();

                    if self.tokens.next_if(|token| token.kind == TokenKind::Assign).is_some() {
                        // Assignment
                        let value = self.parse_expression();
                        self.expect(TokenKind::Semicolon);
                        return Statement::Assignment { name, name_loc: loc, value }
                    }

                    // Function call
                    let mut args = Vec::new();

                    if self.tokens.next_if(|t| t.kind == TokenKind::OpenParen).is_none() {
                        let mut loc = token.loc.clone();
                        loc.col += token.text.len();
                        diag::fatal!(loc, "expected `{}` after function name", TokenKind::OpenParen);
                    }

                    if self.tokens.peek().map(|t| t.kind) != Some(TokenKind::CloseParen) {
                        args.push(self.parse_expression());

                        while self.tokens.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                            args.push(self.parse_expression());
                        }
                    }

                    self.expect(TokenKind::CloseParen);
                    self.expect(TokenKind::Semicolon);

                    return Statement::Funcall { name, args, loc };
                },
                _ => diag::fatal!(token.loc, "unexpected token: `{}`\n", token.text),
            },
            None => diag::fatal!("expected statement, got EOF"),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        match self.tokens.next() {
            Some(token) => {
                if token.kind != kind {
                    diag::fatal!("expected token kind {}, but got {}", kind, token.kind);
                }
                token
            },
            None => diag::fatal!("expected token kind {}, but got EOF", kind),
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Token {
        match self.tokens.next() {
            Some(token) => {
                if token.kind != TokenKind::Keyword {
                    diag::fatal!("expected keyword, but got `{}`", token.kind);
                }
                if token.text != keyword {
                    diag::fatal!("expected keyword `{}`, but got `{}`", keyword, token.text);
                }
                token
            },
            None => diag::fatal!("expected keyword `{}`, but got EOF", keyword),
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
        eprintln!("{}: error: can't include via HTTPS: {}", include_loc, url);
        exit(1);
    }

    let source = String::from_utf8(output.stdout).unwrap_or_else(|err| {
        eprintln!("{}: error: can't read source from response: {}", include_loc, err);
        exit(1);
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
            Expr::Unary { op, operand, .. } => {
                write!(f, "{}{}", op, operand)
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

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
                UnaryOp::Negate => "-",
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

fn get_zero_value(typ: Type, loc: Loc) -> Expr {
    match typ {
        Type::Int64 => Expr::Literal { lit: Literal::Number(0), loc },
        Type::String => Expr::Literal { lit: Literal::String(String::new()), loc },
        Type::Char => Expr::Literal { lit: Literal::Char('\0'), loc },
        Type::Bool => Expr::Literal { lit: Literal::Bool(false), loc },
        Type::Never => unreachable!(),
        Type::Void => unreachable!(),
    }
}
// TODO: Introduce support for constants
