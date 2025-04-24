use crate::parser::BinaryOp;
use crate::parser::Expr;
use crate::parser::Function;
use crate::parser::Program;
use crate::parser::Statement;

use std::io;
use std::io::Write;

#[allow(unreachable_patterns)]
pub fn generate_asm_aarch64_darwin<W: Write>(out: &mut W, program: &Program) -> io::Result<()> {
    let mut strings = Vec::<String>::new();

    generate_preamble(out)?;
    for (_, function) in &program.functions {
        generate_function(out, &mut strings, function)?;
    }
    generate_data(out, &strings)?;
    Ok(())
}

fn generate_function<W: Write>(out: &mut W, strings: &mut Vec<String>, function: &Function) -> io::Result<()> {
    writeln!(out, "; function {}", function.name)?;
    writeln!(out, "_{}:", function.name)?;

    generate_function_prologue(out)?;
    for statement in &function.body {
        generate_statement(out, statement, strings)?;
    }
    generate_function_epilogue(out)?;

    Ok(())
}

#[allow(unreachable_patterns)]
fn generate_statement<W: Write>(out: &mut W, statement: &Statement, strings: &mut Vec<String>) -> io::Result<()> {
    match statement {
        Statement::Ret { value } => {
            if let Some(expr) = value {
                generate_expression(out, &expr)?;
            } else {
                writeln!(out, "    ret")?;
            }
        }
        Statement::Funcall { name, args } => {
            match name.as_str() {
                "println" => {
                    let text = match &args[0] {
                        Expr::String(s) => s.as_str(),
                        _ => ""
                    };
                    strings.push(text.to_string());

                    generate_println(out, text, strings.len() - 1)?;
                }
                "exit" => {
                    let code: i64 = match &args[0] {
                        Expr::Number(n) => *n,
                        _ => 69,
                    };

                    generate_exit(out, code as u8)?;
                }
                _ => {
                    for (i, arg) in args.iter().rev().enumerate() {
                        generate_expression(out, arg)?;
                        writeln!(out, "    mov     x{}, x0", i)?;
                    }
                    writeln!(out, "    ; call {}", name)?;
                    writeln!(out, "    bl      _{}", name)?;
                }
            }
        }
        _ => todo!()
    }
    Ok(())
}

fn generate_println<W: Write>(out: &mut W, text: &str, idx: usize) -> io::Result<()> {
    let length = text.len();
    writeln!(out, "    ; println(\"{}\")", text)?;
    writeln!(out, "    mov     x0, 1")?;
    writeln!(out, "    adrp    x1, strings.{}@PAGE", idx)?;
    writeln!(out, "    add     x1, x1, strings.{}@PAGEOFF", idx)?;
    writeln!(out, "    mov     x2, {}", length + 1)?; // +1 for \n
    writeln!(out, "    mov     x16, 4")?;
    writeln!(out, "    svc     0x80")?;
    Ok(())
}

fn generate_exit<W: Write>(out: &mut W, code: u8) -> io::Result<()> {
    writeln!(out, "    ; exit({})", code)?;
    writeln!(out, "    mov     x0, {}", code)?;
    writeln!(out, "    mov     x16, 1")?;
    writeln!(out, "    svc     0x80")?;
    Ok(())
}

fn generate_expression<W: Write>(out: &mut W, expr: &Expr) -> io::Result<()> {
    match expr {
        Expr::Number(n) => {
            writeln!(out, "    ; number: {}", n)?;
            writeln!(out, "    mov     x0, {}", n)?;
        }
        Expr::Binary { op, lhs, rhs } => {
            generate_expression(out, lhs)?;
            writeln!(out, "    ; store {}", lhs)?;
            writeln!(out, "    str     x0, [sp, -16]!")?;
            generate_expression(out, rhs)?;
            writeln!(out, "    ; load {}", lhs)?;
            writeln!(out, "    ldr     x1, [sp], 16")?;
            writeln!(out, "    ; binop: {} {} {}", lhs, op, rhs)?;
            match op {
                BinaryOp::Add => writeln!(out, "    add     x0, x1, x0")?,
                BinaryOp::Sub => writeln!(out, "    sub     x0, x1, x0")?,
                BinaryOp::Mul => writeln!(out, "    mul     x0, x1, x0")?,
                BinaryOp::Div => writeln!(out, "    sdiv    x0, x1, x0")?,
            }
        },
        Expr::Funcall { name, args } => {
            for (i, arg) in args.iter().rev().enumerate() {
                generate_expression(out, arg)?;
                writeln!(out, "    mov     x{}, x0", i)?;
            }
            writeln!(out, "    ; call {}", name)?;
            writeln!(out, "    bl      _{}", name)?;
        },
        _ => todo!("{}", expr),
    }
    Ok(())
}

fn generate_preamble<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, ".global _main")?;
    writeln!(out, ".align 2")?;
    writeln!(out)?;
    Ok(())
}

fn generate_function_prologue<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, "    ; prologue")?;
    writeln!(out, "    stp     x29, x30, [sp, -16]!")?;
    writeln!(out, "    mov     x29, sp")?;
    Ok(())
}

fn generate_function_epilogue<W: Write>(out: &mut W) -> io::Result<()> {
    writeln!(out, "    ; epilogue")?;
    writeln!(out, "    mov     sp, x29")?;
    writeln!(out, "    ldp     x29, x30, [sp], 16")?;
    writeln!(out, "    ret")?;
    Ok(())
}

fn generate_data<W: Write>(out: &mut W, strings: &Vec<String>) -> io::Result<()> {
    if strings.len() < 1 {
        return Ok(());
    }

    writeln!(out)?;
    writeln!(out, "; data section")?;
    for (i, s) in strings.iter().enumerate() {
        writeln!(out, "strings.{}:", i)?;
        writeln!(out, "    .ascii \"{}\\n\"", s)?;
    }
    Ok(())
}
