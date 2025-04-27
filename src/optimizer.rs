use crate::parser::{Ast, BinaryOp, Expr, Literal, Statement};

pub fn precompute_expressions(ast: &mut Ast) {
    ast.functions.values_mut().for_each(|func| {
        func.body.iter_mut().for_each(precompute_statement);
    });
}

fn precompute_statement(statement: &mut Statement) {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                precompute_expr(expr);
            }
        },
        Statement::If { condition, consequence, otherwise } => {
            precompute_expr(condition);
            for statement in consequence {
                precompute_statement(statement);
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
    if let Expr::Binary { op, lhs, rhs } = expr {
        precompute_expr(lhs);
        precompute_expr(rhs);

        if !is_precomputable(op) {
            return;
        }

        if let (Expr::Literal(_), Expr::Literal(_)) = (lhs.as_ref(), rhs.as_ref()) {
            *expr = evaluate(op, lhs, rhs);
        }
    }
}

fn is_precomputable(op: &BinaryOp) -> bool {
    matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div)
}

fn evaluate(op: &BinaryOp, lhs: &Expr, rhs: &Expr) -> Expr {
    match (get_integer_value(lhs), get_integer_value(rhs)) {
        (Some(l), Some(r)) => {
            let value = match op {
                BinaryOp::Add => l + r,
                BinaryOp::Sub => l - r,
                BinaryOp::Mul => l * r,
                BinaryOp::Div => l / r,
                _ => unreachable!(),
            };
            Expr::Literal(Literal::Number(value))
        }
        _ => Expr::Binary {
            op: op.clone(),
            lhs: Box::new(lhs.clone()),
            rhs: Box::new(rhs.clone()),
        },
    }
}

fn get_integer_value(expr: &Expr) -> Option<i64> {
    if let Expr::Literal(Literal::Number(value)) = expr {
        Some(*value)
    } else {
        None
    }
}
