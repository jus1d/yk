use std::io::{self, Write};

use crate::parser::{Ast, Function, Statement, BinaryOp, Expr, Literal};
use crate::compiler;

pub struct Generator<'a, W: Write> {
    output: W,
    ast: &'a Ast,
    strings: Vec<String>,
    use_exit: bool,
    use_puts: bool,
    use_puti: bool,
}

impl<'a, W: Write> Generator<'a, W> {
    pub fn new(ast: &'a Ast, output: W) -> Self {
        Self {
            output,
            ast,
            strings: Vec::new(),
            use_exit: false,
            use_puts: false,
            use_puti: false,
        }
    }

    pub fn generate(&mut self) -> io::Result<()> {
        self.write_preamble()?;

        for function in self.ast.functions.values() {
            self.write_func(function)?;
        }

        if self.use_puts {
            self.write_puts()?;
        }
        if self.use_puti {
            self.write_puti()?;
        }
        if self.use_exit {
            self.write_exit()?;
        }
        self.write_data_section()
    }

    fn write_preamble(&mut self) -> io::Result<()> {
        writeln!(self.output, ".global _main")?;
        writeln!(self.output, ".align 2\n")?;
        Ok(())
    }

    fn write_func_prologue(&mut self, offset: usize) -> io::Result<()> {
        let offset = if offset % 2 == 1 { offset + 1 } else { offset };
        let stack_size = 16 + (offset * 8);

        writeln!(self.output, "    ; prologue")?;
        writeln!(self.output, "    stp     x29, x30, [sp, -{}]!", stack_size)?;
        writeln!(self.output, "    mov     x29, sp")
    }

    fn write_func_epilogue(&mut self, offset: usize) -> io::Result<()> {
        let offset = if offset % 2 == 1 { offset + 1 } else { offset };
        let stack_size = 16 + (offset * 8);

        writeln!(self.output, "    ; epilogue")?;
        writeln!(self.output, "    mov     sp, x29")?;
        writeln!(self.output, "    ldp     x29, x30, [sp], {}", stack_size)?;
        writeln!(self.output, "    ret")
    }

    fn write_func(&mut self, function: &Function) -> io::Result<()> {
        writeln!(self.output, "; function {}", function.name)?;
        writeln!(self.output, "_{}:", function.name)?;

        self.write_func_prologue(function.params.len())?;

        for statement in &function.body {
            self.write_statement(function, statement)?;
        }

        writeln!(self.output)?;
        Ok(())
    }

    fn write_statement(&mut self, function: &Function, statement: &Statement) -> io::Result<()> {
        match statement {
            Statement::Ret { value } => {
                if let Some(expr) = value {
                    self.write_expression(expr, &function.name, "x0")?;
                }
                self.write_func_epilogue(function.params.len())
            }
            Statement::Funcall { name, args } => {
                self.write_funcall(function, name, args)
            }
        }
    }

    fn write_funcall(&mut self, current_func: &Function, callee_name: &str, args: &[Expr]) -> io::Result<()> {
        match callee_name {
            "exit" => self.use_exit = true,
            "puts" => self.use_puts = true,
            "puti" => self.use_puti = true,
            _ => {},
        }

        writeln!(self.output, "    ; store args of {}", current_func.name)?;
        for (i, _) in current_func.params.iter().enumerate() {
            writeln!(self.output, "    str     x{}, [x29, {}]", i, 16 + 8 * i)?;
        }

        writeln!(self.output, "    ; load args for {}", callee_name)?;
        for (i, arg) in args.iter().enumerate() {
            let reg = format!("x{}", i);
            self.write_expression(arg, &current_func.name, &reg)?;
        }

        writeln!(self.output, "    ; call {}", callee_name)?;
        writeln!(self.output, "    bl      _{}", callee_name)?;

        writeln!(self.output, "    ; restore arguments for {}", current_func.name)?;
        for (i, _) in current_func.params.iter().enumerate() {
            writeln!(self.output, "    ldr     x{}, [x29, {}]", i, 16 + 8 * i)?;
        }

        Ok(())
    }

    fn write_expression(&mut self, expr: &Expr, current_func: &str, target_reg: &str) -> io::Result<()> {
        match expr {
            Expr::Literal(lit) => self.write_literal(lit, target_reg),
            Expr::Binary { op, lhs, rhs } => self.write_binary_expr(op, lhs, rhs, current_func, target_reg),
            Expr::Funcall { name, args } => self.write_funcall_expr(name, args, current_func, target_reg),
            Expr::Variable(name) => self.write_variable(name, current_func, target_reg),
        }
    }

    fn write_literal(&mut self, lit: &Literal, reg: &str) -> io::Result<()> {
        match lit {
            Literal::Number(n) => {
                writeln!(self.output, "    ; number: {}", n)?;
                writeln!(self.output, "    mov     {}, {}", reg, n)
            },
            Literal::String(text) => {
                let idx = self.strings.len();
                self.strings.push(text.clone());
                writeln!(self.output, "    ; string: \"{}\"", text)?;
                writeln!(self.output, "    adrp    {}, string.{}@PAGE", reg, idx)?;
                writeln!(self.output, "    add     {}, {}, string.{}@PAGEOFF", reg, reg, idx)
            },
        }
    }

    fn write_binary_expr(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr, current_func: &str, target_reg: &str) -> io::Result<()> {
        self.write_expression(lhs, current_func, "x9")?;
        self.write_expression(rhs, current_func, "x10")?;

        writeln!(self.output, "    ; binop: {} {} {}", lhs, op, rhs)?;

        match op {
            BinaryOp::Add => writeln!(self.output, "    add     {}, x9, x10", target_reg),
            BinaryOp::Sub => writeln!(self.output, "    sub     {}, x9, x10", target_reg),
            BinaryOp::Mul => writeln!(self.output, "    mul     {}, x9, x10", target_reg),
            BinaryOp::Div => writeln!(self.output, "    sdiv    {}, x9, x10", target_reg),
        }
    }

    fn write_funcall_expr(&mut self, name: &str, args: &[Expr], current_func: &str, target_reg: &str) -> io::Result<()> {
        writeln!(self.output, "    ; args for funcall {}", name)?;
        for (i, arg) in args.iter().enumerate() {
            let reg = format!("x{}", i);
            self.write_expression(arg, current_func, &reg)?;
        }

        writeln!(self.output, "    ; call {}", name)?;
        writeln!(self.output, "    bl      _{}", name)?;

        if target_reg != "x0" {
            writeln!(self.output, "    mov     {}, x0", target_reg)?;
        }

        Ok(())
    }

    fn write_variable(&mut self, name: &str, current_func: &str, target_reg: &str) -> io::Result<()> {
        let src_reg = format!("x{}", compiler::get_variable_position(self.ast, current_func, name));
        if src_reg != target_reg {
            writeln!(self.output, "    ; variable {}", name)?;
            writeln!(self.output, "    mov     {}, {}", target_reg, src_reg)?;
        }
        Ok(())
    }

    fn write_puts(&mut self) -> io::Result<()> {
        writeln!(self.output, "; std::puts")?;
        writeln!(self.output, "_puts:")?;
        writeln!(self.output, "    mov     x1, x0")?;
        writeln!(self.output, "    mov     x2, 0")?;
        writeln!(self.output, "1:")?;
        writeln!(self.output, "    ldrb    w3, [x1, x2]")?;
        writeln!(self.output, "    cbz     w3, 2f")?;
        writeln!(self.output, "    add     x2, x2, 1")?;
        writeln!(self.output, "    b       1b")?;
        writeln!(self.output, "2:")?;
        writeln!(self.output, "    mov     x0, 1")?;
        writeln!(self.output, "    mov     x16, 4")?;
        writeln!(self.output, "    svc     0")?;
        writeln!(self.output, "    ret")?;
        writeln!(self.output)?;
        Ok(())
    }

    fn write_puti(&mut self) -> io::Result<()> {
        writeln!(self.output, "; std::puti")?;
        writeln!(self.output, "_puti:")?;
        writeln!(self.output, "    stp     x29, x30, [sp, -48]!")?;
        writeln!(self.output, "    mov     x29, sp")?;
        writeln!(self.output, "    mov     x9, 0xCCCC")?;
        writeln!(self.output, "    movk    x9, 0xCCCC, lsl 16")?;
        writeln!(self.output, "    movk    x9, 0xCCCC, lsl 32")?;
        writeln!(self.output, "    movk    x9, 0xCCCD, lsl 48")?;
        writeln!(self.output, "    mov     w11, 10")?;
        writeln!(self.output, "    strb    w11, [x29, 47]")?;
        writeln!(self.output, "    add     x2, x29, 46")?;
        writeln!(self.output, "    mov     x5, x0")?;
        writeln!(self.output, "    cmp     x5, 0")?;
        writeln!(self.output, "    b.ne    1f")?;
        writeln!(self.output, "    mov     w0, 48")?;
        writeln!(self.output, "    strb    w0, [x2], -1")?;
        writeln!(self.output, "    b       2f")?;
        writeln!(self.output, "1:")?;
        writeln!(self.output, "    umulh   x11, x5, x9")?;
        writeln!(self.output, "    lsr     x11, x11, 3")?;
        writeln!(self.output, "    add     x12, x11, x11, lsl 2")?;
        writeln!(self.output, "    add     x12, x12, x12")?;
        writeln!(self.output, "    sub     x12, x5, x12")?;
        writeln!(self.output, "    add     w12, w12, 48")?;
        writeln!(self.output, "    strb    w12, [x2], -1")?;
        writeln!(self.output, "    mov     x5, x11")?;
        writeln!(self.output, "    cmp     x5, 0")?;
        writeln!(self.output, "    b.ne    1b")?;
        writeln!(self.output, "2:")?;
        writeln!(self.output, "    add     x1, x2, 1")?;
        writeln!(self.output, "    add     x3, x29, 48")?;
        writeln!(self.output, "    sub     x2, x3, x1")?;
        writeln!(self.output, "    mov     x0, 1")?;
        writeln!(self.output, "    mov     x16, 4")?;
        writeln!(self.output, "    svc     0")?;
        writeln!(self.output, "    ldp     x29, x30, [sp], 48")?;
        writeln!(self.output, "    ret")?;
        writeln!(self.output)?;
        Ok(())
    }

    fn write_exit(&mut self) -> io::Result<()> {
        writeln!(self.output, "; std::exit")?;
        writeln!(self.output, "_exit:")?;
        writeln!(self.output, "    mov     x16, 1")?;
        writeln!(self.output, "    svc     0")?;
        writeln!(self.output)?;
        Ok(())
    }

    fn write_data_section(&mut self) -> io::Result<()> {
        if self.strings.is_empty() {
            return Ok(());
        }

        writeln!(self.output, "; data section")?;
        for (i, s) in self.strings.iter().enumerate() {
            writeln!(self.output, "string.{}:", i)?;
            writeln!(self.output, "    .asciz \"{}\\n\"", s)?;
        }
        Ok(())
    }
}
