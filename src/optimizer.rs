use crate::diag;
use crate::lexer::token::Loc;
use crate::parser::ast;

use std::collections::HashSet;
use ast::{Ast, Statement, Expr, Literal, BinaryOp, UnaryOp};

pub fn precompute_expressions(ast: &mut Ast) {
    ast.functions.values_mut().for_each(|func| {
        func.body.iter_mut().for_each(precompute_statement);
    });
}

fn precompute_statement(statement: &mut Statement) {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                precompute_expression(expr);
            }
        },
        Statement::If { branches, otherwise } => {
            for branch in branches.iter_mut() {
                precompute_expression(&mut branch.condition);
                for statement in &mut branch.block {
                    precompute_statement(statement);
                }
            }
            for statement in otherwise {
                precompute_statement(statement);
            }
        },
        Statement::While { condition, block } => {
            if let Some(expr) = condition {
                precompute_expression(expr);
            }
            for statement in block {
                precompute_statement(statement);
            }
        },
        Statement::Funcall { args, .. } => {
            for expr in args {
                precompute_expression(expr);
            }
        },
        Statement::Declaration { value, .. } => {
            precompute_expression(value);
        },
        Statement::Assignment { value, .. } => {
            precompute_expression(value);
        },
    };
}

fn precompute_expression(expr: &mut Expr) {
    match expr {
        Expr::Variable { .. } => {},
        Expr::Literal { .. } => {},
        Expr::Funcall { args, .. } => {
            for arg in args {
                precompute_expression(arg);
            }
        },
        Expr::Binary { op, lhs, rhs, loc } => {
            precompute_expression(lhs);
            precompute_expression(rhs);

            if !is_binary_precomputable(op) {
                return;
            }

            if let (Expr::Literal { .. }, Expr::Literal { .. }) = (lhs.as_ref(), rhs.as_ref()) {
                if let Some(evaluated_expr) = evaluate_binary(op, lhs, rhs, loc) {
                    *expr = evaluated_expr;
                }
            }
        },
        Expr::Unary { op, operand, loc } => {
            precompute_expression(operand);
            if let Some(evaluated_expr) = evaluate_unary(op, operand, loc) {
                *expr = evaluated_expr;
            }
        },
        Expr::Index { collection, index, .. } => {
            precompute_expression(collection);
            precompute_expression(index);
        },
    }
}

fn is_binary_precomputable(op: &BinaryOp) -> bool {
    matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div)
}

fn evaluate_unary(op: &UnaryOp, operand: &Expr, loc: &Loc) -> Option<Expr> {
    match get_integer_value(operand) {
        Some(value) => match op {
            UnaryOp::Negate => {
                Some(Expr::Literal { lit: Literal::Number(-value), loc: loc.clone() })
            }
        },
        _ => None
    }
}

fn evaluate_binary(op: &BinaryOp, lhs: &Expr, rhs: &Expr, loc: &Loc) -> Option<Expr> {
    match (get_integer_value(lhs), get_integer_value(rhs)) {
        (Some(l), Some(r)) => {
            let value = match op {
                BinaryOp::Add => l + r,
                BinaryOp::Sub => l - r,
                BinaryOp::Mul => l * r,
                BinaryOp::Div => l / r,
                _ => unreachable!(),
            };
            Some(Expr::Literal { lit: Literal::Number(value), loc: loc.clone() })
        }
        _ => None,
    }
}

fn get_integer_value(expr: &Expr) -> Option<i64> {
    if let Expr::Literal { lit: Literal::Number(value), .. } = expr {
        Some(*value)
    } else {
        None
    }
}

pub fn eliminate_unused_functions(ast: &mut Ast) {
    let main = match ast.functions.get("main") {
        Some(func) => func,
        None => diag::fatal!("no `main` function found"),
    };

    let mut used_funcs = HashSet::new();
    let mut visited = HashSet::new();
    used_funcs.insert(String::from("main"));

    for statement in &main.body {
        mark_unused_functions_statement(ast, statement, &mut used_funcs, &mut visited);
    }

    let to_remove: Vec<_> = ast.functions.keys()
        .filter(|name| !used_funcs.contains(*name))
        .cloned()
        .collect();

    for name in to_remove {
        ast.functions.remove(&name);
    }
}

fn mark_unused_functions_statement(ast: &Ast, statement: &Statement, used_funcs: &mut HashSet<String>, visited: &mut HashSet<String>) {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                mark_unused_functions_expression(ast, expr, used_funcs, visited);
            }
        }
        Statement::If { branches, otherwise } => {
            for branch in branches {
                mark_unused_functions_expression(ast, &branch.condition, used_funcs, visited);
                for mut statement in &branch.block {
                    mark_unused_functions_statement(ast, &mut statement, used_funcs, visited);
                }
            }
            for statement in otherwise {
                mark_unused_functions_statement(ast, statement, used_funcs, visited);
            }
        },
        Statement::While { condition, block } => {
            if let Some(expr) = condition {
                mark_unused_functions_expression(ast, expr, used_funcs, visited);
            }
            for statement in block {
                mark_unused_functions_statement(ast, statement, used_funcs, visited);
            }
        },
        Statement::Funcall { name, args, loc: _ } => {
            used_funcs.insert(name.clone());

            if !visited.contains(name) {
                visited.insert(name.clone());
                if let Some(func) = ast.functions.get(name) {
                    for statement in &func.body {
                        mark_unused_functions_statement(ast, statement, used_funcs, visited);
                    }
                }
            }

            for expr in args {
                mark_unused_functions_expression(ast, expr, used_funcs, visited);
            }
        },
        Statement::Declaration { value, .. } => {
            mark_unused_functions_expression(ast, value, used_funcs, visited);
        },
        Statement::Assignment { value, .. } => {
            mark_unused_functions_expression(ast, value, used_funcs, visited);
        }
    }
}

fn mark_unused_functions_expression(ast: &Ast, expr: &Expr, used_funcs: &mut HashSet<String>, visited: &mut HashSet<String>) {
    match expr {
        Expr::Literal { .. } => { },
        Expr::Binary { lhs, rhs, .. } => {
            mark_unused_functions_expression(ast, lhs, used_funcs, visited);
            mark_unused_functions_expression(ast, rhs, used_funcs, visited);
        },
        Expr::Unary { operand, .. } => {
            mark_unused_functions_expression(ast, operand, used_funcs, visited);
        },
        Expr::Funcall { name, args, .. } => {
            used_funcs.insert(name.clone());

            if !visited.contains(name) {
                visited.insert(name.clone());
                if let Some(func) = ast.functions.get(name) {
                    for statement in &func.body {
                        mark_unused_functions_statement(ast, statement, used_funcs, visited);
                    }
                }
            }

            for expr in args {
                mark_unused_functions_expression(ast, expr, used_funcs, visited);
            }
        },
        Expr::Variable { .. } => { },
        Expr::Index { collection, index, .. } => {
            mark_unused_functions_expression(ast, collection, used_funcs, visited);
            mark_unused_functions_expression(ast, index, used_funcs, visited);
        }
    }
}
