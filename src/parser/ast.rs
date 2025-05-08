use crate::lexer;

use std::collections::HashMap;
use lexer::Loc;

#[derive(Clone, Debug)]
pub struct Ast {
    pub functions: HashMap<String, Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub name_loc: Loc,
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
        name_loc: Loc,
        typ: Option<Type>,
        value: Expr,
    },
    Assignment {
        name: String,
        name_loc: Loc,
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
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
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
            Expr::Unary { loc, .. } => loc,
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Negate,
}
