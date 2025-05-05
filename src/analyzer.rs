use std::collections::HashMap;

use crate::lexer::Loc;
use crate::parser::{Ast, BinaryOp, Expr, Function, Literal, Statement, Type, UnaryOp, Variable, KEYWORDS};
use crate::diag;

pub fn typecheck(ast: &Ast) {
    let builtin_funcs = HashMap::from([
        (String::from("write"), Function {
            name: String::from("write"),
            ret_type: Type::Int64,
            params: vec![
                Variable {
                    name: String::from("fd"),
                    typ: Type::Int64,
                },
                Variable {
                    name: String::from("buf"),
                    typ: Type::String,
                },
                Variable {
                    name: String::from("n"),
                    typ: Type::Int64,
                },
            ],
            body: vec![],
            is_public: true,
        }),
        (String::from("putc"), Function {
            name: String::from("putc"),
            ret_type: Type::Void,
            params: vec![Variable {
                name: String::from("ch"),
                typ: Type::Char,
            }],
            body: vec![],
            is_public: true,
        }),
        (String::from("puti"), Function {
            name: String::from("puti"),
            ret_type: Type::Void,
            params: vec![Variable {
                name: String::from("val"),
                typ: Type::Int64,
            }],
            body: vec![],
            is_public: true,
        }),
        (String::from("exit"), Function {
            name: String::from("exit"),
            ret_type: Type::Never,
            params: vec![Variable {
                name: String::from("code"),
                typ: Type::Int64,
            }],
            body: vec![],
            is_public: true,
        }),
        (String::from("strlen"), Function {
            name: String::from("strlen"),
            ret_type: Type::Int64,
            params: vec![Variable {
                name: String::from("s"),
                typ: Type::String,
            }],
            body: vec![],
            is_public: true,
        }),
    ]);

    for (name, function) in &ast.functions {
        if builtin_funcs.contains_key(name.as_str()) {
            diag::fatal!("symbol `{name}` is a builtin function name");
        }

        let mut vars: Vec<Variable> = vec![];
        for param in &function.params {
            vars.push(Variable {
                name: param.name.clone(),
                typ: param.typ.clone(),
            });
        }

        for statement in &function.body {
            typecheck_statement(ast, function, statement, &mut vars, &builtin_funcs);
        }
    }
}

fn typecheck_statement(ast: &Ast, func: &Function, statement: &Statement, vars: &mut Vec<Variable>, builtin_funcs: &HashMap<String, Function>) {
    match statement {
        Statement::Funcall { name, args, loc } => {
            typecheck_funcall(ast, name, args, loc, vars, &builtin_funcs, &ast.functions);
        },
        Statement::Ret { value } => {
            if let Some(value) = value {
                let expected_type = &func.ret_type;
                let actual_type = &get_expression_type(ast, value, vars, builtin_funcs);
                if actual_type != expected_type {
                    diag::fatal!("mismatched type of return expression. expected `{}`, but got `{}`", expected_type, actual_type);
                }

                typecheck_expr(ast, value, vars, builtin_funcs, &ast.functions);
            }
        },
        Statement::If { branches, otherwise } => {
            for branch in branches {
                let actual_type = get_expression_type(ast, &branch.condition, vars, builtin_funcs);
                let expected_type = Type::Bool;
                if actual_type != expected_type {
                    diag::fatal!(branch.condition.clone().loc(), "expected a `{}` condition, got `{}`", expected_type, actual_type);
                }

                for statement in &branch.block {
                    typecheck_statement(ast, func, statement, vars, builtin_funcs);
                }
            }

            for statement in otherwise {
                typecheck_statement(ast, func, statement, vars, builtin_funcs);
            }
        },
        Statement::While { condition, block } => {
            if let Some(condition) = condition {
                let actual_type = get_expression_type(ast, condition, vars, builtin_funcs);
                let expected_type = Type::Bool;
                if actual_type != expected_type {
                    diag::fatal!(condition.clone().loc(), "expected a `{}` condition, got `{}`", expected_type, actual_type);
                }
            }

            for statement in block {
                typecheck_statement(ast, func, statement, vars, builtin_funcs);
            }
        },
        Statement::Declaration { name, typ, value } => {
            if KEYWORDS.contains(&name.as_str()) {
                diag::fatal!("variable name collides with reserved keyword `{}`", name);
            }

            if ast.functions.get(name).is_some() {
                diag::fatal!("variable name collides with defined function `{}`", name);
            }

            match typ {
                Some(typ) => {
                    let actual_type = get_expression_type(ast, value, vars, builtin_funcs);
                    let expected_type = typ.clone();
                    if actual_type != expected_type {
                        diag::fatal!(value.clone().loc(), "expected expression of type `{}`, but got `{}`", expected_type, actual_type);
                    }

                    vars.push(Variable {
                        name: name.clone(),
                        typ: typ.clone(),
                    });
                },
                None => {
                    let value_type = get_expression_type(ast, value, vars, builtin_funcs);

                    vars.push(Variable {
                        name: name.clone(),
                        typ: value_type,
                    });
                },
            }
        },
        Statement::Assignment { name, value } => {
            let actual_type = get_expression_type(ast, value, vars, builtin_funcs);
            if let Some(variable) = vars.iter().find(|var| var.name == *name) {
                let expected_type = variable.typ.clone();
                if actual_type != expected_type {
                    diag::fatal!(value.clone().loc(), "assignment to `{}`: expected type `{}`, but got `{}`", name, expected_type, actual_type);
                }
            }
            else {
                diag::fatal!("variable `{}` not found in this scope", name);
            }
        },
    }
}

fn typecheck_binop(ast: &Ast, op: &BinaryOp, lhs: &Expr, rhs: &Expr, loc: &Loc, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) {
    let lhs_type = get_expression_type(ast, lhs, vars, builtin_funcs);
    let rhs_type = get_expression_type(ast, rhs, vars, builtin_funcs);
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            if lhs_type != Type::Int64 {
                diag::fatal!(loc, "binary operation `{}` only supported for type `int64`", op);
            }
            if rhs_type != Type::Int64 {
                diag::fatal!(loc, "binary operation `{}` only supported for type `int64`", op);
            }
        },
        BinaryOp::EQ | BinaryOp::NE | BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            if lhs_type != rhs_type {
                diag::fatal!(loc, "operands of different types. lhs: `{}`, rhs: `{}`", lhs_type, rhs_type);
            }
            if lhs_type != Type::Int64 && lhs_type != Type::Bool {
                diag::fatal!(loc, "operands of different types. logical operations can be applied only for `int64` or `bool`");
            }
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
            if lhs_type != Type::Bool {
                diag::fatal!(lhs.clone().loc(), "logical operations only supported between booleans");
            }
            if rhs_type != Type::Bool {
                diag::fatal!(rhs.clone().loc(), "logical operations only supported between booleans");
            }
        }
    }
}

fn typecheck_unary(ast: &Ast, op: &UnaryOp, operand: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) {
    let operand_type = get_expression_type(ast, operand, vars, builtin_funcs);
    match op {
        UnaryOp::Negate => {
            if operand_type != Type::Int64 {
                diag::fatal!(operand.clone().loc(), "unary operator `{}` cannot be applied to `{}`, expected `{}`", UnaryOp::Negate, operand_type, Type::Int64);
            }
        },
    }
}

fn typecheck_expr(ast: &Ast, expr: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>, user_funcs: &HashMap<String, Function>) {
    match expr {
        Expr::Literal { .. } => {}
        Expr::Binary { op, lhs, rhs, loc } => {
            typecheck_binop(ast, op, lhs, rhs, loc, vars, builtin_funcs);
        },
        Expr::Unary { op, operand, .. } => {
            typecheck_unary(ast, op, operand, vars, builtin_funcs);
        },
        Expr::Funcall { name, args, loc } => {
            typecheck_funcall(ast, name, args, loc, vars, builtin_funcs, user_funcs);
        }
        Expr::Variable { name, loc } => {
            if vars.iter().find(|var| var.name == *name).is_none() {
                diag::fatal!(loc, "variable `{}` not found in this scope", name);
            }
        },
        Expr::Index { collection, index, loc } => {
            let collection_type = get_expression_type(ast, collection, vars, builtin_funcs);
            if collection_type != Type::String {
                diag::fatal!(loc, "type `{}` is not indexable", collection_type)
            }

            let actual_index_type = get_expression_type(ast, index, vars, builtin_funcs);
            let expected_index_type = Type::Int64;
            if actual_index_type != Type::Int64 {
                diag::fatal!(loc, "expected expression of type `{}` as an index, got `{}`", expected_index_type, actual_index_type);
            }
        },
    }
}

fn typecheck_funcall(ast: &Ast, name: &str, args: &[Expr], loc: &Loc, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>, user_funcs: &HashMap<String, Function>) {
    let func = if builtin_funcs.contains_key(name) {
        &builtin_funcs[name]
    } else if let Some(func) = user_funcs.get(name) {
        func
    } else {
        diag::fatal!("call to undeclared function `{}`", name);
    };

    check_arguments_count(name, loc, args.len(), func.params.len());
    for arg in args {
        typecheck_expr(ast, arg, vars, builtin_funcs, user_funcs);
    }

    for i in 0..func.params.len() {
        let arg = &args[i];
        let expected_type = &func.params[i].typ;
        let actual_type = &get_expression_type(ast, arg, vars, builtin_funcs);
        if expected_type != actual_type {
            diag::fatal!(arg.clone().loc(), "mismatched arguments types. expected `{}`, but got `{}`", expected_type, actual_type);
        }
    }
}

fn check_arguments_count(func_name: &str, loc: &Loc, actual: usize, expected: usize) {
    if actual < expected {
        diag::fatal!(loc, "too few arguments to function call `{}`, expected {}, got {}", func_name, expected, actual);
    }
    else if actual > expected {
        diag::fatal!(loc, "too many arguments to function call `{}`, expected {}, got {}", func_name, expected, actual);
    }
}

pub fn check_entrypoint_declaration(ast: &Ast) {
    match ast.functions.get("main") {
        Some(func) => {
            if func.params.len() > 0 {
                diag::fatal!("function `main` should not accept any parameters");
            }
            if func.ret_type != Type::Int64 {
                diag::fatal!("expect `fn main` to return `{}`", Type::Int64);
            }
        }
        None => diag::fatal!("undefined entry point, expected: `fn main() int64`"),
    }
}

fn get_expression_type(ast: &Ast, expr: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) -> Type {
    match expr {
        Expr::Literal { lit, .. } => get_literal_type(lit),
        Expr::Binary { op, .. } => get_binary_operation_type(op),
        Expr::Unary { op, .. } => get_unary_operation_type(op),
        Expr::Funcall { name, loc, .. } => get_funcall_type(ast, name, loc, builtin_funcs),
        Expr::Variable { name, loc } => get_variable_type(name, vars, loc),
        Expr::Index { collection, .. } => get_index_type(ast, collection, vars, builtin_funcs),
    }
}

fn get_literal_type(lit: &Literal) -> Type {
    match lit {
        Literal::Number(_) => return Type::Int64,
        Literal::String(_) => return Type::String,
        Literal::Bool(_) => return Type::Bool,
        Literal::Char(_) => return Type::Char,
    }
}

fn get_binary_operation_type(op: &BinaryOp) -> Type {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            return Type::Int64;
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::EQ | BinaryOp::NE |
        BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            return Type::Bool;
        },
    }
}

fn get_unary_operation_type(op: &UnaryOp) -> Type {
    match op {
        UnaryOp::Negate => {
            return Type::Int64;
        },
    }
}

fn get_funcall_type(ast: &Ast, name: &str, loc: &Loc, builtin_funcs: &HashMap<String, Function>) -> Type {
    match ast.functions.get(name) {
        Some(func) => return func.ret_type.clone(),
        None => {},
    };

    match builtin_funcs.get(name) {
        Some(func) => func.ret_type.clone(),
        None => diag::fatal!(loc, "function `{}` not found in current scope", name),
    }
}

fn get_variable_type(name: &str, vars: &Vec<Variable>, loc: &Loc) -> Type {
    for var in vars {
        if var.name == *name {
            return var.typ.clone();
        }
    }

    diag::fatal!(loc, "variable `{}` not found in this scope", name);
}

fn get_index_type(ast: &Ast, expr: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) -> Type {
    let expr_type = get_expression_type(ast, expr, vars, builtin_funcs);
    match expr_type {
        Type::String => Type::Char,
        _ => unreachable!(),
    }
}
