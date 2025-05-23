use crate::lexer::token::{Loc, KEYWORDS};
use crate::parser::ast;

use std::collections::HashMap;
use std::process::exit;
use ast::{Ast, Function, Statement, Expr, Literal, BinaryOp, UnaryOp, Type, Variable};

pub fn typecheck(ast: &Ast) {
    let builtin_funcs = HashMap::from([
        (String::from("write"), Function {
            name: String::from("write"),
            name_loc: Loc::empty(),
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
            is_external: false,
        }),
        (String::from("putc"), Function {
            name: String::from("putc"),
            name_loc: Loc::empty(),
            ret_type: Type::Void,
            params: vec![Variable {
                name: String::from("ch"),
                typ: Type::Char,
            }],
            body: vec![],
            is_external: false,
        }),
        (String::from("puti"), Function {
            name: String::from("puti"),
            name_loc: Loc::empty(),
            ret_type: Type::Void,
            params: vec![Variable {
                name: String::from("val"),
                typ: Type::Int64,
            }],
            body: vec![],
            is_external: false,
        }),
        (String::from("exit"), Function {
            name: String::from("exit"),
            name_loc: Loc::empty(),
            ret_type: Type::Never,
            params: vec![Variable {
                name: String::from("code"),
                typ: Type::Int64,
            }],
            body: vec![],
            is_external: false,
        }),
    ]);

    for (name, function) in &ast.functions {
        if builtin_funcs.contains_key(name.as_str()) {
            eprintln!("{}: error: symbol `{}` is a builtin function name", function.name_loc, name);
            exit(1);
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
                    eprintln!("{}: error: mismatched type of return expression. expected `{}`, but got `{}`", value.clone().loc(), expected_type, actual_type);
                    exit(1);
                }

                typecheck_expr(ast, value, vars, builtin_funcs, &ast.functions);
            }
        },
        Statement::If { branches, otherwise } => {
            for branch in branches {
                let actual_type = get_expression_type(ast, &branch.condition, vars, builtin_funcs);
                let expected_type = Type::Bool;
                if actual_type != expected_type {
                    eprintln!("{}: error: expected a `{}` condition, got `{}`", branch.condition.clone().loc(), expected_type, actual_type);
                    exit(1);
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
                    eprintln!("{}: error: expected a `{}` condition, got `{}`", condition.clone().loc(), expected_type, actual_type);
                    exit(1);
                }
            }

            for statement in block {
                typecheck_statement(ast, func, statement, vars, builtin_funcs);
            }
        },
        Statement::Declaration { name, name_loc, typ, value } => {
            if KEYWORDS.contains(&name.as_str()) {
                eprintln!("{}: error: variable name collides with reserved keyword `{}`", name_loc, name);
            }

            if ast.functions.get(name).is_some() {
                eprintln!("{}: error: variable name collides with defined function `{}`", name_loc, name);
            }

            match typ {
                Some(typ) => {
                    let actual_type = get_expression_type(ast, value, vars, builtin_funcs);
                    let expected_type = typ.clone();
                    if actual_type != expected_type {
                        eprintln!("{}: error: expected expression of type `{}`, but got `{}`", value.clone().loc(), expected_type, actual_type);
                        exit(1);
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
        Statement::Assignment { lhs, value } => {
            let rhs_type = get_expression_type(ast, value, vars, builtin_funcs);

            match lhs {
                Expr::Variable { name, loc } => {
                    if let Some(variable) = vars.iter().find(|var| var.name == *name) {
                        let expected_type = variable.typ.clone();
                        if rhs_type != expected_type {
                            eprintln!("{}: error: assignment to `{}`: expected type `{}`, but got `{}`",
                                     loc, name, expected_type, rhs_type);
                            exit(1);
                        }
                    } else {
                        eprintln!("{}: error: variable `{}` not found in this scope", loc, name);
                        exit(1);
                    }
                },
                Expr::Unary { op: UnaryOp::Dereference, operand, loc } => {
                    let pointer_type = get_expression_type(ast, operand, vars, builtin_funcs);
                    if let Type::Ptr(basetype) = pointer_type {
                        if rhs_type != *basetype {
                            eprintln!("{}: error: dereference assignment: expected type `{}`, but got `{}`",
                                     loc, basetype, rhs_type);
                            exit(1);
                        }
                    } else {
                        eprintln!("{}: error: cannot dereference non-pointer type `{}`",
                                 loc, pointer_type);
                        exit(1);
                    }
                },
                _ => {
                    eprintln!("{}: error: invalid left-hand side of assignment", lhs.clone().loc());
                    exit(1);
                }
            }
        },
    }
}

fn typecheck_binop(ast: &Ast, op: &BinaryOp, lhs: &Expr, rhs: &Expr, loc: &Loc, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) {
    let lhs_type = get_expression_type(ast, lhs, vars, builtin_funcs);
    let rhs_type = get_expression_type(ast, rhs, vars, builtin_funcs);
    match op {
        BinaryOp::Add => {
            match (lhs_type.clone(), rhs_type.clone()) {
                (Type::Int64, Type::Int64)  => {},
                (Type::Ptr(_), Type::Int64) => {},
                (Type::Int64, Type::Ptr(_)) => {},
                _ => {
                    eprintln!("{}: error: binary operation `{}` requires either two operands of type `int64` or combination of `int64` and pointer in any order, but got `{}` and `{}`", loc, op, lhs_type, rhs_type);
                    exit(1);
                },
            }
        },
        BinaryOp::Sub => {
            match (lhs_type.clone(), rhs_type.clone()) {
                (Type::Int64, Type::Int64)  => {},
                (Type::Ptr(_), Type::Int64) => {},
                _ => {
                    eprintln!("{}: error: binary operation `{}` requires either two operands of type `int64` or combination of pointer and `int64` exactly in that order, but got `{}` and `{}`", loc, op, lhs_type, rhs_type);
                    exit(1);
                },
            }
        },
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            if lhs_type != Type::Int64 {
                eprintln!("{}: error: binary operation `{}` only supported for type `int64`", loc, op);
                exit(1);
            }
            if rhs_type != Type::Int64 {
                eprintln!("{}: error: binary operation `{}` only supported for type `int64`", loc, op);
                exit(1);
            }
        },
        BinaryOp::EQ | BinaryOp::NE | BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            if lhs_type != rhs_type {
                eprintln!("{}: error: operands of different types. lhs: `{}`, rhs: `{}`", loc, lhs_type, rhs_type);
                exit(1);
            }
            if lhs_type != Type::Int64 && lhs_type != Type::Bool {
                eprintln!("{}: error: operands of different types. logical operations can be applied only for `int64` or `bool`", loc);
                exit(1);
            }
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
            if lhs_type != Type::Bool {
                eprintln!("{}: error: logical operations only supported between booleans", lhs.clone().loc());
                exit(1);
            }
            if rhs_type != Type::Bool {
                eprintln!("{}: error: logical operations only supported between booleans", rhs.clone().loc());
                exit(1);
            }
        }
    }
}

fn typecheck_unary(ast: &Ast, op: &UnaryOp, operand: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) {
    let operand_type = get_expression_type(ast, operand, vars, builtin_funcs);
    let operand_loc = operand.clone().loc();
    match op {
        UnaryOp::Negate => {
            if operand_type != Type::Int64 {
                eprintln!("{}: error: unary operator `{}` cannot be applied to `{}`, expected `{}`", operand_loc, UnaryOp::Negate, operand_type, Type::Int64);
                exit(1);
            }
        },
        UnaryOp::AddressOf => {
            match operand {
                Expr::Variable { .. } => {},
                _ => {
                    eprintln!("{}: error: cannot take address of expression, taking address only of variables is allowed", operand_loc);
                    exit(1);
                }
            }
        },
        UnaryOp::Dereference => {
            if let Type::Ptr(_) = operand_type { }
            else {
                eprintln!("{}: error: cannot dereference expression of type {}", operand_loc, operand_type);
                exit(1);
            }
        }
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
                eprintln!("{}: error: variable `{}` not found in this scope", loc, name);
                exit(1);
            }
        },
        Expr::Index { collection, index, loc } => {
            let collection_type = get_expression_type(ast, collection, vars, builtin_funcs);
            if collection_type != Type::String {
                eprintln!("{}: error: type `{}` is not indexable", loc, collection_type);
                exit(1);
            }

            let actual_index_type = get_expression_type(ast, index, vars, builtin_funcs);
            let expected_index_type = Type::Int64;
            if actual_index_type != Type::Int64 {
                eprintln!("{}: error: expected expression of type `{}` as an index, got `{}`", loc, expected_index_type, actual_index_type);
                exit(1);
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
        eprintln!("{}: error: call to undeclared function `{}`", loc, name);
        exit(1);
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
            eprintln!("{}: error: mismatched arguments types. expected `{}`, but got `{}`", arg.clone().loc(), expected_type, actual_type);
            exit(1);
        }
    }
}

fn check_arguments_count(func_name: &str, loc: &Loc, actual: usize, expected: usize) {
    if actual < expected {
        eprintln!("{}: error: too few arguments to function call `{}`, expected {}, got {}", loc, func_name, expected, actual);
        exit(1);
    }
    else if actual > expected {
        eprintln!("{}: error: too many arguments to function call `{}`, expected {}, got {}", loc, func_name, expected, actual);
        exit(1);
    }
}

pub fn check_entrypoint_declaration(ast: &Ast) {
    match ast.functions.get("main") {
        Some(func) => {
            if func.params.len() > 0 {
                eprintln!("{}: error: function `main` should not accept any parameters", func.name_loc);
                exit(1);
            }
            if func.ret_type != Type::Int64 {
                eprintln!("error: expect `fn main` to return `{}`", Type::Int64);
                exit(1);
            }
        }
        None => {
            eprintln!("error: no entry point found, expected: `fn main() int64`");
            exit(1);
        },
    }
}

fn get_expression_type(ast: &Ast, expr: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) -> Type {
    match expr {
        Expr::Literal { lit, .. } => get_literal_type(lit),
        Expr::Binary { op, lhs, rhs, .. } => {
            let lhs_type = get_expression_type(ast, lhs, vars, builtin_funcs);
            let rhs_type = get_expression_type(ast, rhs, vars, builtin_funcs);
            get_binary_operation_type(op, lhs_type, rhs_type, expr.clone().loc())
        },
        Expr::Unary { op, operand, .. } => get_unary_operation_type(ast, op, operand, vars, builtin_funcs),
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
        Literal::Nil => Type::Ptr(Box::new(Type::Void)),
    }
}

fn get_binary_operation_type(op: &BinaryOp, lhs_type: Type, rhs_type: Type, loc: Loc) -> Type {
    match op {
        BinaryOp::Add => {
            match (lhs_type.clone(), rhs_type.clone()) {
                (Type::Int64, Type::Int64)  => Type::Int64,
                (Type::Ptr(basetype), Type::Int64) | (Type::Int64, Type::Ptr(basetype)) => Type::Ptr(basetype),
                _ => {
                    eprintln!("{}: error: binary operation `{}` requires either two operands of type `int64` or combination of `int64` and pointer in any order, but got `{}` and `{}`", loc, op, lhs_type, rhs_type);
                    exit(1);
                },
            }
        },
        BinaryOp::Sub => {
            match (lhs_type.clone(), rhs_type.clone()) {
                (Type::Int64, Type::Int64)  => Type::Int64,
                (Type::Ptr(basetype), Type::Int64) => Type::Ptr(basetype),
                _ => {
                    eprintln!("{}: error: binary operation `{}` requires either two operands of type `int64` or combination of pointer and `int64` exactly in that order, but got `{}` and `{}`", loc, op, lhs_type, rhs_type);
                    exit(1);
                },
            }
        },
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            return Type::Int64;
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::EQ | BinaryOp::NE |
        BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            return Type::Bool;
        },
    }
}

fn get_unary_operation_type(ast: &Ast, op: &UnaryOp, operand: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) -> Type {
    match op {
        UnaryOp::Negate => Type::Int64,
        UnaryOp::AddressOf => {
            let basetype = get_expression_type(ast, operand, vars, builtin_funcs);
            Type::Ptr(Box::new(basetype))
        },
        UnaryOp::Dereference => {
            let ptr_type = get_expression_type(ast, &operand.clone(), vars, builtin_funcs);
            if let Type::Ptr(basetype) = ptr_type {
                return *basetype;
            }

            eprintln!("{}: error: cannot dereference expression of type {}", operand.clone().loc(), ptr_type);
            exit(1);
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
        None => {
            eprintln!("{}: error: function `{}` not found in current scope", loc, name);
            exit(1);
        },
    }
}

fn get_variable_type(name: &str, vars: &Vec<Variable>, loc: &Loc) -> Type {
    for var in vars {
        if var.name == *name {
            return var.typ.clone();
        }
    }

    eprintln!("{}: error: variable `{}` not found in current scope", loc, name);
    exit(1);
}

fn get_index_type(ast: &Ast, expr: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<String, Function>) -> Type {
    let expr_type = get_expression_type(ast, expr, vars, builtin_funcs);
    match expr_type {
        Type::String => Type::Char,
        _ => unreachable!(),
    }
}

// TODO: merge functions like: `typecheck_something()` and `get_something_type()` together
