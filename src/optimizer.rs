use std::collections::HashSet;

use crate::diag;
use crate::lexer::Loc;
use crate::parser::{Ast, BinaryOp, Expr, Literal, Statement};

pub fn precompute_expressions(ast: &mut Ast) {
    ast.functions.values_mut().for_each(|func| {
        func.body.iter_mut().for_each(precompute_statement);
    });
}

pub fn eliminate_deadcode(ast: &mut Ast) {
    eliminate_unused_functions(ast);
}

fn eliminate_unused_functions(ast: &mut Ast) {
    let main = match ast.functions.get("main") {
        Some(func) => func,
        None => diag::fatal!("no `main` function found"),
    };

    let mut used_funcs: HashSet<String> = HashSet::new();
    used_funcs.insert(String::from("main"));

    for statement in &main.body {
        mark_unused_functions_statement(ast, statement, &mut used_funcs);
    }

    for (name, _) in &ast.functions.clone() {
        if !used_funcs.contains(name) {
            ast.functions.remove(name);
        }
    }
}

fn mark_unused_functions_statement(ast: &Ast, statement: &Statement, used_funcs: &mut HashSet<String>) {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                mark_unused_functions_expression(ast, expr, used_funcs);
            }
        }
        Statement::If { branches, otherwise } => {
            for branch in branches {
                mark_unused_functions_expression(ast, &branch.condition, used_funcs);
                for mut statement in &branch.block {
                    mark_unused_functions_statement(ast, &mut statement, used_funcs);
                }
            }
            for statement in otherwise {
                mark_unused_functions_statement(ast, statement, used_funcs);
            }
        },
        Statement::While { condition, block } => {
            if let Some(expr) = condition {
                mark_unused_functions_expression(ast, expr, used_funcs);
            }
            for statement in block {
                mark_unused_functions_statement(ast, statement, used_funcs);
            }
        },
        Statement::Funcall { name, args, loc: _ } => {
            used_funcs.insert(name.clone());
            if let Some(func) = ast.functions.get(name) {
                for statement in &func.body {
                    mark_unused_functions_statement(ast, statement, used_funcs);
                }
            }
            for expr in args {
                mark_unused_functions_expression(ast, expr, used_funcs);
            }
        },
        Statement::Declaration { name: _, typ: _, value } => {
            if let Some(expr) = value {
                mark_unused_functions_expression(ast, expr, used_funcs);
            }
        },
        Statement::Assignment { name: _, value } => {
            mark_unused_functions_expression(ast, value, used_funcs);
        }
    }
}

fn mark_unused_functions_expression(ast: &Ast, expr: &Expr, used_funcs: &mut HashSet<String>) {
    match expr {
        Expr::Literal { .. } => { },
        Expr::Binary { lhs, rhs, .. } => {
            mark_unused_functions_expression(ast, lhs, used_funcs);
            mark_unused_functions_expression(ast, rhs, used_funcs);
        },
        Expr::Funcall { name, args, .. } => {
            used_funcs.insert(name.clone());
            if let Some(func) = ast.functions.get(name) {
                for statement in &func.body {
                    mark_unused_functions_statement(ast, statement, used_funcs);
                }
            }
            for expr in args {
                mark_unused_functions_expression(ast, expr, used_funcs);
            }
        },
        Expr::Variable { .. } => { },
        Expr::Index { collection, index, .. } => {
            mark_unused_functions_expression(ast, collection, used_funcs);
            mark_unused_functions_expression(ast, index, used_funcs);
        }
    }
}

fn precompute_statement(statement: &mut Statement) {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                precompute_expr(expr);
            }
        },
        Statement::If { branches, otherwise } => {
            for branch in branches.iter_mut() {
                precompute_expr(&mut branch.condition);
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
                precompute_expr(expr);
            }
            for statement in block {
                precompute_statement(statement);
            }
        },
        Statement::Funcall { args, .. } => {
            for expr in args {
                precompute_expr(expr);
            }
        },
        Statement::Declaration { value, .. } => {
            if let Some(expr) = value {
                precompute_expr(expr);
            }
        },
        Statement::Assignment { value, .. } => {
            precompute_expr(value);
        },
    };
}

fn precompute_expr(expr: &mut Expr) {
    if let Expr::Binary { op, lhs, rhs, loc } = expr {
        precompute_expr(lhs);
        precompute_expr(rhs);

        if !is_precomputable(op) {
            return;
        }

        if let (Expr::Literal { .. }, Expr::Literal { .. }) = (lhs.as_ref(), rhs.as_ref()) {
            *expr = evaluate_integer(op, lhs, rhs, loc);
        }
    }
}

fn is_precomputable(op: &BinaryOp) -> bool {
    matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div)
}

fn evaluate_integer(op: &BinaryOp, lhs: &Expr, rhs: &Expr, loc: &Loc) -> Expr {
    match (get_integer_value(lhs), get_integer_value(rhs)) {
        (Some(l), Some(r)) => {
            let value = match op {
                BinaryOp::Add => l + r,
                BinaryOp::Sub => l - r,
                BinaryOp::Mul => l * r,
                BinaryOp::Div => l / r,
                _ => unreachable!(),
            };
            Expr::Literal { lit: Literal::Number(value), loc: loc.clone() }
        }
        _ => Expr::Binary {
            op: op.clone(),
            lhs: Box::new(lhs.clone()),
            rhs: Box::new(rhs.clone()),
            loc: loc.clone(),
        },
    }
}

fn get_integer_value(expr: &Expr) -> Option<i64> {
    if let Expr::Literal { lit: Literal::Number(value), .. } = expr {
        Some(*value)
    } else {
        None
    }
}
