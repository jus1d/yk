// TODO: function args are corrupted, when I call puts/exit from function.
// I have to play with SP, when one function calls another

use crate::parser::{Ast, Function, Statement, BinaryOp, Expr, Literal};
use crate::compiler;

use std::io;
use std::io::Write;

pub fn generate_aarch64_darwin_assembly<W: Write>(ast: &Ast, out: &mut W) -> io::Result<()> {
    let ast = ast.clone();
    let mut strings = Vec::<String>::new();

    generate_aarch64_darwin_preamble(out)?;
    generate_aarch64_darwin_puts(out)?;
    generate_aarch64_darwin_exit(out)?;

    for (_, function) in &ast.functions {
        generate_aarch64_darwin_func_body(&ast, out, &mut strings, function)?;
    }
    generate_aarch64_darwin_data_section(out, &strings)?;
    Ok(())
}

fn generate_aarch64_darwin_preamble<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, ".global _main")?;
    writeln!(out, ".align 2")?;
    writeln!(out)?;
    Ok(())
}

fn generate_aarch64_darwin_func_prologue<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, "    ; prologue")?;
    writeln!(out, "    stp     x29, x30, [sp, -16]!")?;
    writeln!(out, "    mov     x29, sp")?;
    Ok(())
}

fn generate_aarch64_darwin_func_body<W: Write>(ast: &Ast, out: &mut W, strings: &mut Vec<String>, function: &Function) -> io::Result<()> {
    writeln!(out, "; function {}", function.name)?;
    writeln!(out, "_{}:", function.name)?;

    generate_aarch64_darwin_func_prologue(out)?;
    for statement in &function.body {
        generate_aarch64_darwin_statement(ast, out, statement, strings, &function.name)?;
    }
    writeln!(out)?;

    Ok(())
}

fn generate_aarch64_darwin_func_epilogue<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, "    ; epilogue")?;
    writeln!(out, "    mov     sp, x29")?;
    writeln!(out, "    ldp     x29, x30, [sp], 16")?;
    writeln!(out, "    ret")?;
    Ok(())
}

fn generate_aarch64_darwin_data_section<W: Write>(out: &mut W, strings: &Vec<String>) -> io::Result<()> {
    if strings.len() < 1 {
        return Ok(());
    }

    writeln!(out)?;
    writeln!(out, "; data section")?;
    for (i, s) in strings.iter().enumerate() {
        writeln!(out, "string.{}:", i)?;
        writeln!(out, "    .ascii \"{}\\n\"", s)?;
    }
    Ok(())
}

fn generate_aarch64_darwin_puts<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, "_puts:")?;
    writeln!(out, "    mov     x1, x0")?;
    writeln!(out, "    mov     x2, 0")?;
    writeln!(out, "1:")?;
    writeln!(out, "    ldrb    w3, [x1, x2]")?;
    writeln!(out, "    cbz     w3, 2f")?;
    writeln!(out, "    add     x2, x2, 1")?;
    writeln!(out, "    b       1b")?;
    writeln!(out, "2:")?;
    writeln!(out, "    mov     x0, 1")?;
    writeln!(out, "    mov     x16, 4")?;
    writeln!(out, "    svc     0")?;
    writeln!(out, "    ret")?;
    writeln!(out)?;
    Ok(())
}

fn generate_aarch64_darwin_exit<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, "_exit:")?;
    writeln!(out, "    mov     x16, 1")?;
    writeln!(out, "    svc     0")?;
    writeln!(out)?;
    Ok(())
}

fn generate_aarch64_darwin_statement<W: Write>(ast: &Ast, out: &mut W, statement: &Statement, strings: &mut Vec<String>, current_func_name: &str) -> io::Result<()> {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                generate_aarch64_darwin_expression(ast, out, &expr, strings, current_func_name, "x0")?;
            }
            generate_aarch64_darwin_func_epilogue(out)?;
        }
        Statement::Funcall { name, args } => {
            for (i, arg) in args.iter().enumerate() {
                let reg = format!("x{}", i);
                generate_aarch64_darwin_expression(ast, out, arg, strings, current_func_name, &reg)?;
            }
            writeln!(out, "    ; call {}", name)?;
            writeln!(out, "    bl      _{}", name)?;
        }
    }
    Ok(())
}

fn generate_aarch64_darwin_expression<W: Write>(ast: &Ast, out: &mut W, expr: &Expr, strings: &mut Vec<String>, current_func_name: &str, register: &str) -> io::Result<()> {
    match expr {
        Expr::Literal(lit) => {
            generate_aarch64_darwin_literal(out, lit, strings, register)?;
        }
        Expr::Binary { op, lhs, rhs } => {
            generate_aarch64_darwin_expression(ast, out, lhs, strings, current_func_name, "x9")?;
            generate_aarch64_darwin_expression(ast, out, rhs, strings, current_func_name, "x10")?;
            writeln!(out, "    ; binop: {} {} {}", lhs, op, rhs)?;
            match op {
                BinaryOp::Add => writeln!(out, "    add     {}, x10, x9", register)?,
                BinaryOp::Sub => writeln!(out, "    sub     {}, x10, x9", register)?,
                BinaryOp::Mul => writeln!(out, "    mul     {}, x10, x9", register)?,
                BinaryOp::Div => writeln!(out, "    sdiv    {}, x10, x9", register)?,
            }
        },
        Expr::Funcall { name, args } => {
            writeln!(out, "    ; args for funcall {}", name)?;
            for (i, arg) in args.iter().enumerate() {
                let register = format!("x{}", i);
                generate_aarch64_darwin_expression(ast, out, arg, strings, current_func_name, &register)?;
            }
            writeln!(out, "    ; call {}", name)?;
            writeln!(out, "    bl      _{}", name)?;
        },
        Expr::Variable(name) => {
            let src = &format!("x{}", compiler::get_variable_position(ast, current_func_name, name));
            let dst = register;
            if src != dst {
                writeln!(out, "    ; variable {}", name)?;
                writeln!(out, "    mov     {}, {}", dst, src)?;
            }
        },
    }
    Ok(())
}

fn generate_aarch64_darwin_literal<W: Write>(out: &mut W, lit: &Literal, strings: &mut Vec<String>, register: &str) -> io::Result<()> {
    match lit {
        Literal::Number(n) => {
            writeln!(out, "    ; number: {}", n)?;
            writeln!(out, "    mov     {}, {}", register, n)?;
        },
        Literal::String(text) => {
            let idx = strings.len();
            strings.push(text.to_string());
            writeln!(out, "    ; string: \"{}\"", text)?;
            writeln!(out, "    adrp    x0, string.{}@PAGE", idx)?;
            writeln!(out, "    add     x0, x0, string.{}@PAGEOFF", idx)?;
        },
    }
    Ok(())
}
