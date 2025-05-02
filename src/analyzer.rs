use std::collections::HashMap;

use crate::parser::{Ast, BinaryOp, Expr, Function, Literal, Statement, Type, Variable, KEYWORDS};
use crate::diag;

pub fn analyze(ast: &Ast) {
    check_entrypoint_declaration(ast);
    typecheck(ast);
}

fn typecheck(ast: &Ast) {
    let builtin_funcs = HashMap::from([
        ("puts", Function {
            name: String::from("puts"),
            ret_type: Type::Void,
            params: vec![Variable {
                name: String::from("str"),
                typ: Type::String,
            }],
            body: vec![],
        }),
        ("puti", Function {
            name: String::from("puti"),
            ret_type: Type::Void,
            params: vec![Variable {
                name: String::from("val"),
                typ: Type::Int64,
            }],
            body: vec![],
        }),
        ("exit", Function {
            name: String::from("exit"),
            ret_type: Type::Never,
            params: vec![Variable {
                name: String::from("code"),
                typ: Type::Int64,
            }],
            body: vec![],
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

fn typecheck_statement(ast: &Ast, func: &Function, statement: &Statement, vars: &mut Vec<Variable>, builtin_funcs: &HashMap<&str, Function>) {
    match statement {
        Statement::Funcall { name, args } => {
            typecheck_funcall(ast, name, args, vars, &builtin_funcs, &ast.functions);
        },
        Statement::Ret { value } => {
            if let Some(value) = value {
                let expected_type = &func.ret_type;
                let actual_type = &get_expr_type(ast, value, vars);
                if actual_type != expected_type {
                    diag::fatal!("mismatched type of return expression. expected `{}`, but got `{}`", expected_type, actual_type);
                }

                typecheck_expr(ast, value, vars, builtin_funcs, &ast.functions);
            }
        },
        Statement::If { branches, otherwise } => {
            for branch in branches {
                let actual_type = get_expr_type(ast, &branch.condition, vars);
                let expected_type = Type::Bool;
                if actual_type != expected_type {
                    diag::fatal!("expected a `{}` condition, got `{}`", expected_type, actual_type);
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
                let actual_type = get_expr_type(ast, condition, vars);
                let expected_type = Type::Bool;
                if actual_type != expected_type {
                    diag::fatal!("expected a `{}` condition, got `{}`", expected_type, actual_type);
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

            if let Some(value) = value {
                let actual_type = get_expr_type(ast, value, vars);
                let expected_type = typ.clone();
                if actual_type != expected_type {
                    diag::fatal!("expected expression of type `{}`, but got `{}`", expected_type, actual_type);
                }
            }

            vars.push(Variable {
                name: name.clone(),
                typ: typ.clone(),
            });
        },
        Statement::Assignment { name, value } => {
            let actual_type = get_expr_type(ast, value, vars);
            if let Some(variable) = vars.iter().find(|var| var.name == *name) {
                let expected_type = variable.typ.clone();
                if actual_type != expected_type {
                    diag::fatal!("assignment to `{}`: expected type `{}`, but got `{}`", name, expected_type, actual_type);
                }
            }
            else {
                diag::fatal!("variable `{}` not found in this scope", name);
            }
        },
    }
}

fn typecheck_binop(ast: &Ast, op: &BinaryOp, lhs: &Expr, rhs: &Expr, vars: &Vec<Variable>) {
    let lhs_type = get_expr_type(ast, lhs, vars);
    let rhs_type = get_expr_type(ast, rhs, vars);
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            if lhs_type != Type::Int64 {
                diag::fatal!("binary operations only supported for type `int64`");
            }
            if rhs_type != Type::Int64 {
                diag::fatal!("binary operations only supported for type `int64`");
            }
        },
        BinaryOp::EQ | BinaryOp::NE | BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            if lhs_type != rhs_type {
                diag::fatal!("operands of different types. lhs: `{}`, rhs: `{}`", lhs_type, rhs_type);
            }
            if lhs_type != Type::Int64 && lhs_type != Type::Bool {
                diag::fatal!("operands of different types. logical operations can be applied only for `int64` or `bool`");
            }
            if rhs_type != Type::Int64 && rhs_type != Type::Bool {
                diag::fatal!("operands of different types. logical operations can be applied only for `int64` or `bool`");
            }
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
            if lhs_type != Type::Bool {
                diag::fatal!("logical operations only supported between booleans");
            }
            if rhs_type != Type::Bool {
                diag::fatal!("logical operations only supported between booleans");
            }
        }
    }
}

fn typecheck_expr(ast: &Ast, expr: &Expr, vars: &Vec<Variable>, builtin_funcs: &HashMap<&str, Function>, user_funcs: &HashMap<String, Function>) {
    match expr {
        Expr::Literal(_) => {}
        Expr::Binary { op, lhs, rhs } => {
            typecheck_binop(ast, op, lhs, rhs, vars);
        }
        Expr::Funcall { name, args } => {
            typecheck_funcall(ast, name, args, vars, builtin_funcs, user_funcs);
        }
        Expr::Variable(name) => {
            if vars.iter().find(|var| var.name == *name).is_none() {
                diag::fatal!("variable `{}` not found in this scope", name);
            }
        }
    }
}

fn typecheck_funcall(ast: &Ast, name: &str, args: &[Expr], vars: &Vec<Variable>, builtin_funcs: &HashMap<&str, Function>, user_funcs: &HashMap<String, Function>) {
    let func = if builtin_funcs.contains_key(name) {
        &builtin_funcs[name]
    } else if let Some(func) = user_funcs.get(name) {
        func
    } else {
        diag::fatal!("call to undeclared function `{}`", name);
    };

    check_arguments_count(name, args.len(), func.params.len());

    for i in 0..func.params.len() {
        let expected_type = &func.params[i].typ;
        let actual_type = &get_expr_type(ast, &args[i], vars);
        if expected_type != actual_type {
            diag::fatal!("mismatched arguments types. expected `{}`, but got `{}`", expected_type, actual_type);
        }
    }

    for arg in args {
        typecheck_expr(ast, arg, vars, builtin_funcs, user_funcs);
    }
}

fn check_arguments_count(func_name: &str, actual: usize, expected: usize) {
    if actual < expected {
        diag::fatal!("too few arguments to function call `{}`, expected {}, got {}", func_name, expected, actual);
    }
    else if actual > expected {
        diag::fatal!("too many arguments to function call `{}`, expected {}, got {}", func_name, expected, actual);
    }
}

fn check_entrypoint_declaration(ast: &Ast) {
    if !ast.functions.contains_key("main") {
        diag::fatal!("entry point not declared. expected: `fn main() int64`");
    }

    match ast.functions.get("main") {
        None => diag::fatal!("entry point not declared. expected: `fn main() int64`"),
        Some(func) => {
            if func.params.len() > 0 {
                diag::fatal!("function `main` should not have parameters");
            }
            if func.ret_type != Type::Int64 {
                diag::fatal!("unexpected main function declaration, expected `fn main() int64`");
            }
        },
    }

}

fn get_binop_type(op: &BinaryOp) -> Type {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            return Type::Int64;
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::EQ | BinaryOp::NE |
        BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            return Type::Int64;
        },
    }
}

fn get_expr_type(ast: &Ast, expr: &Expr, vars: &Vec<Variable>) -> Type {
    match expr {
        Expr::Literal(literal) => match literal {
            Literal::Number(_) => return Type::Int64,
            Literal::String(_) => return Type::String,
            Literal::Bool(_) => return Type::Bool,
        },
        Expr::Binary { op, lhs: _, rhs: _ } => {
            return get_binop_type(op);
        },
        Expr::Funcall { name, args: _ } => {
            for (func_name, func) in ast.functions.clone() {
                if func_name == *name {
                    return func.ret_type;
                }
            }

            diag::fatal!("function `{}` not found in this scope", name);
        },
        Expr::Variable(name) => {
            for var in vars {
                if var.name == *name {
                    return var.typ.clone();
                }
            }

            diag::fatal!("variable `{}` not found in this scope", name);
        },
    }
}
