pub mod ast;

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
                TokenKind::EOF => return ast,
                _ => {
                    eprintln!("{}: error: expected keyword, but got `{}`", token.loc, token.text);
                    eprintln!("{}: note: items can start with next keywords: `include` or `fn`", token.loc);
                    exit(1);
                },
            }
        }

        unreachable!()
    }

    fn parse_function(&mut self) -> Function {
        self.expect_keyword("fn");
        let name_token = self.expect(TokenKind::Identifier);

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
                    Some(token) => match token.kind {
                        TokenKind::Comma => continue,
                        TokenKind::CloseParen => break,
                        _ => {
                            eprintln!("{}: error: expected either {} or {}, but got `{}`", token.loc, TokenKind::Comma, TokenKind::CloseParen, token.text);
                            exit(1);
                        },
                    }
                    None => unreachable!(),
                }
            }
        }

        let return_type = match self.tokens.peek() {
            Some(token) => match token.kind {
                // NOTE: return type is void, if it is not specified
                TokenKind::FatArrow | TokenKind::OpenCurly | TokenKind::Semicolon => Type::Void,
                _ => self.parse_type(),
            },
            None => unreachable!(),
        };

        if let Some(_) = self.tokens.next_if(|token| token.kind == TokenKind::Semicolon) {
            return Function {
                name: name_token.text,
                name_loc: name_token.loc,
                ret_type: return_type,
                params,
                body: vec![],
                is_external: true,
            };
        }

        if let Some(_) = self.tokens.next_if(|token| token.kind == TokenKind::FatArrow) {
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
                is_external: false,
            };
        }

        let body = self.parse_block();

        return Function {
            name: name_token.text,
            name_loc: name_token.loc,
            ret_type: return_type,
            params,
            body,
            is_external: false,
        };
    }

    fn parse_include(&mut self) -> Ast {
        self.expect_keyword("include");
        let include_path_token = self.expect(TokenKind::String);

        self.expect(TokenKind::Semicolon);

        let include_path = include_path_token.text;
        if let Some(ext) = Path::new(&include_path).extension().and_then(|ext| ext.to_str()) {
            if ext != "yk" {
                eprintln!("{}: error: included file '{}' has wrong extension: '.{}', expected '.yk'", include_path_token.loc, include_path, ext);
                exit(1);
            }
        } else {
            eprintln!("{}: error: included file '{}' expected to have '.yk' extension", include_path_token.loc, include_path);
            exit(1);
        }

        let source = if include_path.starts_with("https://") {
            read_source_via_https(&include_path, include_path_token.loc)
        } else {
            if self.include_folders.len() == 0 {
                eprintln!("{}: error: no include folders added, to find file `{}`", include_path_token.loc, include_path);
                eprintln!("{}: note: use `-I<path>` compiler flag to add folder to search list", include_path_token.loc);
                exit(1);
            }

            let mut found_path: Option<PathBuf> = None;
            for folder in &self.include_folders {
                let path = Path::new(&folder).join(&include_path);
                if path.exists() {
                    if let Some(found_path) = found_path {
                        eprintln!("{}: error: cannot determine which file to include, some options: {}, {}",
                            include_path_token.loc, path.display(), found_path.display());
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

    fn parse_block(&mut self) -> Vec<Statement> {
        self.expect(TokenKind::OpenCurly);

        let mut statements = Vec::new();
        if self.tokens.next_if(|token| token.kind == TokenKind::CloseCurly).is_some() {
            return statements;
        }

        loop {
            statements.push(self.parse_statement());
            if let Some(_) = self.tokens.next_if(|token| token.kind == TokenKind::CloseCurly) {
                break;
            }
        }

        return statements;
    }

    fn parse_statement(&mut self) -> Statement {
        if let Some(token) = self.tokens.next_if(|token| token.kind == TokenKind::Keyword) {
            match token.text.as_str() {
                "ret" => {
                    if self.tokens.next_if(|t| t.kind == TokenKind::Semicolon).is_some() {
                        return Statement::Ret { value: None };
                    }

                    let value = self.parse_expression();
                    self.expect(TokenKind::Semicolon);

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
                    let name_token = self.expect(TokenKind::Identifier);

                    match self.tokens.next() {
                        Some(token) => match token.kind {
                            TokenKind::Colon => {
                                let typ = self.parse_type();
                                if typ == Type::Never || typ == Type::Void {
                                    eprintln!("{}: error: cannot declare variable with type `{}`", token.loc, typ);
                                    exit(1);
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
                            TokenKind::Semicolon => {
                                eprintln!("{}: error: cannot know type of variable at compile time, specify type annotation or assign a value with known type", token.loc);
                                exit(1);
                            },
                            _ => {
                                eprintln!("{}: error: expected {} or {}, but got {}", token.loc, TokenKind::Colon, TokenKind::Assign, token);
                                exit(1);
                            },
                        },
                        None => unreachable!(),
                    }
                },
                _ => {
                    eprintln!("{}: error: unexpected {}", token.loc, token);
                    exit(1);
                },
            }
        }

        let lhs = self.parse_expression();

        if let Expr::Funcall { name, args, loc } = lhs {
            self.expect(TokenKind::Semicolon);

            return Statement::Funcall { name, args, loc };
        }

        self.expect(TokenKind::Assign);

        let value = self.parse_expression();
        self.expect(TokenKind::Semicolon);

        return Statement::Assignment { lhs: lhs.clone(), value };
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_expr_prec(0)
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> Expr {
        if let Some(token) = self.tokens.next_if(|token| token.kind == TokenKind::EOF) {
            eprintln!("{}: error: expected expression, got EOF", token.loc);
            exit(1);
        }

        // TODO: merge these 3 branches
        if let Some(token) =  self.tokens.next_if(|token| token.kind == TokenKind::Minus) {
            let operand = self.parse_expr_prec(10);
            if matches!(operand, Expr::Unary { .. }) {
                eprintln!("{}: error: cannot nest unary operations", operand.clone().loc());
                exit(1);
            }

            return Expr::Unary { op: UnaryOp::Negate, operand: Box::new(operand), loc: token.loc }
        }

        if let Some(token) =  self.tokens.next_if(|token| token.kind == TokenKind::Ampersand) {
            let operand = self.parse_expr_prec(10);
            if matches!(operand, Expr::Unary { .. }) {
                eprintln!("{}: error: cannot nest unary operations", operand.clone().loc());
                exit(1);
            }

            return Expr::Unary { op: UnaryOp::AddressOf, operand: Box::new(operand), loc: token.loc }
        }

        if let Some(token) =  self.tokens.next_if(|token| token.kind == TokenKind::Star) {
            let operand = self.parse_expr_prec(10);
            if matches!(operand, Expr::Unary { .. }) {
                eprintln!("{}: error: cannot nest unary operations", operand.clone().loc());
                exit(1);
            }

            return Expr::Unary { op: UnaryOp::Dereference, operand: Box::new(operand), loc: token.loc }
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
        match self.tokens.next() {
            Some(token) => match token.kind {
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
                        _ => {
                            eprintln!("{}: error: unexpected {}", token.loc, token);
                            exit(1);
                        },
                    }
                },
                TokenKind::OpenParen => {
                    let expr = self.parse_expression();
                    self.expect(TokenKind::CloseParen);
                    return expr;
                },
                _ => {
                    eprintln!("{}: error: expected expression, but got {}", token.loc, token);
                    exit(1);
                },
            },
            None => unreachable!(),
        }
    }

    // this function parses constructions like: `int64 counter` (<type> <identifier>)
    fn parse_type_and_name(&mut self) -> Variable {
        let name_token = self.expect(TokenKind::Identifier);
        self.expect(TokenKind::Colon);
        let typ = self.parse_type();

        return Variable {
            typ,
            name: name_token.text,
        };
    }

    fn parse_type(&mut self) -> Type {
        use TokenKind::*;
        match self.tokens.next() {
            None => unreachable!(),
            Some(token) => match token.kind {
                Keyword| Identifier | Exclamation => {
                    if let Some(typ) = get_primitive_type(&token.text) {
                        return typ;
                    }
                    eprintln!("{}: error: expected type, but got {}", token.loc, token);
                    exit(1);
                },
                Star => {
                    let basetype = self.parse_type();
                    return Type::Ptr(Box::new(basetype));
                },
                _ => {
                    eprintln!("{}: error: expected type, but got {}", token.loc, token);
                    exit(1);
                },
            },
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        match self.tokens.next() {
            Some(token) if token.kind == kind => token,
            Some(token) => {
                eprintln!("{}: error: expected {}, but got {}", token.loc, kind, token);
                exit(1);
            }
            None => unreachable!(),
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Token {
        match self.tokens.next() {
            Some(token) => {
                if token.kind != TokenKind::Keyword || token.text != keyword {
                    eprintln!("{}: error: expected keyword `{}`, but got {}", token.loc, keyword, token);
                    exit(1);
                }
                token
            },
            None => unreachable!(),
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
        Err(err) => {
            eprint!("{}: error: can't get source via HTTPS: {}", include_loc, err);
            exit(1);
        }
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
                UnaryOp::AddressOf => "&",
                UnaryOp::Dereference => "*",
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
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Never => write!(f, "!"),
            Type::Void => write!(f, "void"),
            Type::Int64 => write!(f, "int64"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Ptr(basetype) => write!(f, "*{}", basetype),
        }
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
        Type::Ptr(_) => Expr::Literal { lit: Literal::Nil, loc },
        Type::Never => unreachable!(),
        Type::Void => unreachable!(),
    }
}

// TODO: Introduce support for constants
