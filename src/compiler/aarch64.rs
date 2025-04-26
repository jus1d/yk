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
        generate_aarch64_darwin_func_body(out, &ast, &mut strings, function)?;
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

// offset in quadwords (64 bits)
fn generate_aarch64_darwin_func_prologue<W: Write>(out: &mut W, mut offset: usize) -> io::Result<()> {
    if offset % 2 == 1 {
        offset += 1;
    }

    let stack_offset = 16 + (offset * 8);
    writeln!(out, "    ; prologue")?;
    writeln!(out, "    stp     x29, x30, [sp, -{}]!", stack_offset)?;
    writeln!(out, "    mov     x29, sp")?;
    Ok(())
}

fn generate_aarch64_darwin_func_body<W: Write>(out: &mut W, ast: &Ast, strings: &mut Vec<String>, function: &Function) -> io::Result<()> {
    writeln!(out, "; function {}", function.name)?;
    writeln!(out, "_{}:", function.name)?;

    generate_aarch64_darwin_func_prologue(out, function.params.len())?;
    for statement in &function.body {
        generate_aarch64_darwin_statement(out, ast, function, statement, strings)?;
    }
    writeln!(out)?;

    Ok(())
}

// offset in quadwords (64 bits)
fn generate_aarch64_darwin_func_epilogue<W: Write>(out: &mut W, mut offset: usize) -> io::Result<()> {
    if offset % 2 == 1 {
        offset += 1;
    }

    let stack_offset = 16 + (offset * 8);
    writeln!(out, "    ; epilogue")?;
    writeln!(out, "    mov     sp, x29")?;
    writeln!(out, "    ldp     x29, x30, [sp], {}", stack_offset)?;
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
        writeln!(out, "    .asciz \"{}\\n\"", s)?;
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

fn generate_aarch64_darwin_statement<W: Write>(out: &mut W, ast: &Ast, func: &Function, statement: &Statement, strings: &mut Vec<String>) -> io::Result<()> {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                generate_aarch64_darwin_expression(out, ast, &expr, strings, &func.name, "x0")?;
            }
            generate_aarch64_darwin_func_epilogue(out, func.params.len())?;
        }
        Statement::Funcall { name, args } => {
            // TODO: save arguments to stack only if next function polutes x0-x7 registers
            writeln!(out, "    ; store arguments of {}", func.name)?;
            for i in 0..func.params.len() {
                writeln!(out, "    str     x{}, [x29, {}]", i, 16 + 8 * i)?;
            }
            writeln!(out, "    ; load arguments for {}", name)?;
            for (i, arg) in args.iter().enumerate() {
                let reg = format!("x{}", i);
                generate_aarch64_darwin_expression(out, ast, arg, strings, &func.name, &reg)?;
            }
            writeln!(out, "    ; call {}", name)?;
            writeln!(out, "    bl      _{}", name)?;
            writeln!(out, "    ; restore arguments for {}", func.name)?;
            for i in 0..func.params.len() {
                writeln!(out, "    ldr     x{}, [x29, {}]", i, 16 + 8 * i)?;
            }
        }
    }
    Ok(())
}

fn generate_aarch64_darwin_expression<W: Write>(out: &mut W, ast: &Ast, expr: &Expr, strings: &mut Vec<String>, current_func_name: &str, register: &str) -> io::Result<()> {
    match expr {
        Expr::Literal(lit) => {
            generate_aarch64_darwin_literal(out, lit, strings, register)?;
        }
        Expr::Binary { op, lhs, rhs } => {
            generate_aarch64_darwin_expression(out, ast, lhs, strings, current_func_name, "x9")?;
            generate_aarch64_darwin_expression(out, ast, rhs, strings, current_func_name, "x10")?;
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
                generate_aarch64_darwin_expression(out, ast, arg, strings, current_func_name, &register)?;
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
            writeln!(out, "    adrp    {}, string.{}@PAGE", register, idx)?;
            writeln!(out, "    add     {}, {}, string.{}@PAGEOFF", register, register, idx)?;
        },
    }
    Ok(())
}
