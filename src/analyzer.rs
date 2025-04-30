use std::collections::HashMap;

use crate::parser::{Ast, BinaryOp, Expr, Function, Literal, Statement, Variable, KEYWORDS};
use crate::diag;

pub fn analyze(ast: &Ast) {
    check_entrypoint_declaration(ast);
    typecheck(ast);
}

fn typecheck(ast: &Ast) {
    let builtin_funcs = HashMap::from([
        ("puts", Function {
            name: String::from("puts"),
            ret_type: String::from("void"),
            params: vec![Variable {
                name: String::from("str"),
                typ: String::from("string"),
            }],
            body: vec![],
        }),
        ("puti", Function {
            name: String::from("puti"),
            ret_type: String::from("void"),
            params: vec![Variable {
                name: String::from("val"),
                typ: String::from("int64"),
            }],
            body: vec![],
        }),
        ("exit", Function {
            name: String::from("exit"),
            ret_type: String::from("never"),
            params: vec![Variable {
                name: String::from("code"),
                typ: String::from("int64"),
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
            if let Some(expr) = value {
                let expected_type = &func.ret_type;
                let actual_type = &get_expr_type(ast, expr, vars);
                if actual_type != expected_type {
                    diag::fatal!("mismatched type of return expression. expected `{}`, but got `{}`", expected_type, actual_type);
                }

                typecheck_expr(ast, expr, vars, builtin_funcs, &ast.functions);
            }
        },
        Statement::If { branches, otherwise } => {
            for branch in branches {
                let condition_type = get_expr_type(ast, &branch.condition, vars);
                if condition_type != "bool" {
                    diag::fatal!("expected a `bool` condition, got `{}`", condition_type);
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
            if let Some(expr) = condition {
                let condition_type = get_expr_type(ast, expr, vars);
                if condition_type != "bool" {
                    diag::fatal!("expected a `bool` condition, got `{}`", condition_type);
                }
            }

            for s in block {
                typecheck_statement(ast, func, s, vars, builtin_funcs);
            }
        },
        Statement::Declaration { name, typ, value } => {
            if KEYWORDS.contains(&name.as_str()) {
                diag::fatal!("variable name collides with reserved keyword `{}`", name);
            }

            if let Some(expr) = value {
                let value_type = get_expr_type(ast, expr, vars);
                if value_type != *typ {
                    diag::fatal!("expected expression of type `{}`, but got `{}`", typ, value_type);
                }
            }

            vars.push(Variable {
                name: name.clone(),
                typ: typ.clone(),
            });
        },
        Statement::Assignment { name, value } => {
            let value_type = get_expr_type(ast, value, vars);
            if let Some(variable) = vars.iter().find(|var| var.name == *name) {
                if variable.typ != value_type {
                    diag::fatal!("assignment to `{}` expected type `{}`, but got `{}`", name, variable.typ, value_type);
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
            if lhs_type != "int64" {
                diag::fatal!("binary operations only supported for type `int64`");
            }
            if rhs_type != "int64" {
                diag::fatal!("binary operations only supported for type `int64`");
            }
        },
        BinaryOp::EQ | BinaryOp::NE | BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            if lhs_type != rhs_type {
                diag::fatal!("operands of different types. lhs: `{}`, rhs: `{}`", lhs_type, rhs_type);
            }
            if lhs_type != "int64" && lhs_type != "bool" {
                diag::fatal!("operands of different types. logical operations can be applied only for `int64` or `bool`");
            }
            if rhs_type != "int64" && rhs_type != "bool" {
                diag::fatal!("operands of different types. logical operations can be applied only for `int64` or `bool`");
            }
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
            if lhs_type != "bool" {
                diag::fatal!("logical operations only supported between booleans");
            }
            if rhs_type != "bool" {
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
            let mut found = false;
            for var in vars {
                if var.name == *name {
                    found = true;
                }
            }
            if !found {
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
        diag::fatal!("call to undeclared function `{name}`");
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
    match actual.cmp(&expected) {
        std::cmp::Ordering::Greater => {
            diag::fatal!("too many arguments to function call `{func_name}`, expected {expected} arguments, have {actual}");
        }
        std::cmp::Ordering::Less => {
            diag::fatal!("too few arguments to function call `{func_name}`, expected {expected} arguments, have {actual}");
        }
        std::cmp::Ordering::Equal => {}
    }
}

fn check_entrypoint_declaration(ast: &Ast) {
    if !ast.functions.contains_key("main") {
        diag::fatal!("entry point not declared. expected: `fn main() int64`");
    }

    let func = ast.functions.get("main").unwrap();
    if func.params.len() > 0 {
        diag::fatal!("function `main` should not have parameters");
    }
    if func.ret_type.as_str() != "int64" {
        diag::fatal!("unexpected main function declaration, expected `fn main() int64`");
    }
}

fn get_binop_type(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            return String::from("int64");
        },
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::EQ | BinaryOp::NE |
        BinaryOp::GT | BinaryOp::LT | BinaryOp::LE | BinaryOp::GE => {
            return String::from("bool");
        },
    }
}

fn get_expr_type(ast: &Ast, expr: &Expr, vars: &Vec<Variable>) -> String {
    match expr {
        Expr::Literal(literal) => match literal {
            Literal::Number(_) => return String::from("int64"),
            Literal::String(_) => return String::from("string"),
            Literal::Bool(_) => return String::from("bool"),
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
