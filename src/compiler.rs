use crate::parser::{Ast, Function, Statement, BinaryOp, Expr};

use std::io;
use std::io::Write;

pub struct Compiler {
    ast: Ast,
}

impl Compiler {
    pub fn new(ast: Ast) -> Self {
        Compiler {
            ast: ast.clone(),
        }
    }

    pub fn generate_asm_aarch64_darwin<W: Write>(&self, out: &mut W) -> io::Result<()> {
        let ast = self.ast.clone();
        let mut strings = Vec::<String>::new();

        self.generate_preamble(out)?;
        for (_, function) in &ast.functions {
            self.generate_func_body(out, &mut strings, function)?;
        }
        self.generate_data_section(out, &strings)?;
        Ok(())
    }

    fn generate_preamble<W: Write>(&self, out: &mut W) -> io::Result<()> {
        writeln!(out, ".global _main")?;
        writeln!(out, ".align 2")?;
        writeln!(out)?;
        Ok(())
    }

    fn generate_func_prologue<W: Write>(&self, out: &mut W) -> io::Result<()> {
        writeln!(out, "    ; prologue")?;
        writeln!(out, "    stp     x29, x30, [sp, -16]!")?;
        writeln!(out, "    mov     x29, sp")?;
        Ok(())
    }

    fn generate_func_body<W: Write>(&self, out: &mut W, strings: &mut Vec<String>, function: &Function) -> io::Result<()> {
        writeln!(out, "; function {}", function.name)?;
        writeln!(out, "_{}:", function.name)?;

        self.generate_func_prologue(out)?;
        for statement in &function.body {
            self.generate_statement(out, statement, strings, function.name.clone())?;
        }
        self.generate_func_epilogue(out)?;

        Ok(())
    }

    fn generate_func_epilogue<W: Write>(&self, out: &mut W) -> io::Result<()> {
        writeln!(out, "    ; epilogue")?;
        writeln!(out, "    mov     sp, x29")?;
        writeln!(out, "    ldp     x29, x30, [sp], 16")?;
        writeln!(out, "    ret")?;
        Ok(())
    }

    fn generate_data_section<W: Write>(&self, out: &mut W, strings: &Vec<String>) -> io::Result<()> {
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

    fn generate_println<W: Write>(&self, out: &mut W, text: &str, idx: usize) -> io::Result<()> {
        let length = text.len();
        writeln!(out, "    ; println(\"{}\")", text)?;
        writeln!(out, "    mov     x0, 1")?;
        writeln!(out, "    adrp    x1, string.{}@PAGE", idx)?;
        writeln!(out, "    add     x1, x1, string.{}@PAGEOFF", idx)?;
        writeln!(out, "    mov     x2, {}", length + 1)?; // +1 for \n
        writeln!(out, "    mov     x16, 4")?;
        writeln!(out, "    svc     0x80")?;
        Ok(())
    }

    fn generate_exit<W: Write>(&self, out: &mut W, code: u8) -> io::Result<()> {
        writeln!(out, "    ; exit({})", code)?;
        writeln!(out, "    mov     x0, {}", code)?;
        writeln!(out, "    mov     x16, 1")?;
        writeln!(out, "    svc     0x80")?;
        Ok(())
    }


    fn generate_statement<W: Write>(&self, out: &mut W, statement: &Statement, strings: &mut Vec<String>, current_func_name: String) -> io::Result<()> {
        match statement {
            Statement::Ret { value } => {
                if let Some(expr) = value {
                    self.generate_expression(out, &expr, current_func_name)?;
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

                        self.generate_println(out, text, strings.len() - 1)?;
                    }
                    "exit" => {
                        let code: i64 = match &args[0] {
                            Expr::Number(n) => *n,
                            _ => 69,
                        };
                        self.generate_exit(out, code as u8)?;
                    }
                    _ => {
                        for (i, arg) in args.iter().rev().enumerate() {
                            self.generate_expression(out, arg, current_func_name.clone())?;
                            writeln!(out, "    mov     x{}, x0", i)?;
                        }
                        writeln!(out, "    ; call {}", name)?;
                        writeln!(out, "    bl      _{}", name)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn generate_expression<W: Write>(&self, out: &mut W, expr: &Expr, current_func_name: String) -> io::Result<()> {
        match expr {
            Expr::Number(n) => {
                writeln!(out, "    ; number: {}", n)?;
                writeln!(out, "    mov     x0, {}", n)?;
            }
            Expr::Binary { op, lhs, rhs } => {
                self.generate_expression(out, lhs, current_func_name.clone())?;
                writeln!(out, "    ; store {}", lhs)?;
                writeln!(out, "    str     x0, [sp, -16]!")?;
                self.generate_expression(out, rhs, current_func_name.clone())?;
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
                writeln!(out, "    ; store args")?;
                for arg in args {
                    self.generate_expression(out, arg, current_func_name.clone())?;
                    writeln!(out, "    str     x0, [sp, -16]!")?;
                }
                writeln!(out, "    ; load args")?;
                for i in 0..args.len() {
                    writeln!(out, "    ldr     x{}, [sp], 16", args.len() - i - 1)?;
                }
                writeln!(out, "    ; call {}", name)?;
                writeln!(out, "    bl      _{}", name)?;
            },
            Expr::Variable(name) => {
                writeln!(out, "    ; variable {}", name)?;
                writeln!(out, "    mov     x0, x{}", self.get_variable_position(current_func_name.as_str(), name))?;
            },
            Expr::String(_) => todo!(),
        }
        Ok(())
    }

    fn get_variable_position(&self, func_name: &str, variable_name: &str) -> usize {
        if let Some(func) = self.ast.functions.get(func_name) {
            if let Some(_) = func.params.iter().find(|param| param.name == variable_name) {
                let position = func.params.iter().position(|p| p.name == variable_name).unwrap();
                return position;
            } else {
                todo!();
            }
        } else {
            todo!();
        }
    }
}
