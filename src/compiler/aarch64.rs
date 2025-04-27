use std::io::{self, Write};

use crate::parser::{self, Ast, BinaryOp, Expr, Function, Literal, Statement};

pub struct Generator<'a, W: Write> {
    output: W,
    ast: &'a Ast,
    strings: Vec<String>,
    emit_comments: bool,
    use_exit: bool,
    use_puts: bool,
    use_puti: bool,
}

impl<'a, W: Write> Generator<'a, W> {
    pub fn new(ast: &'a Ast, output: W, emit_comments: bool) -> Self {
        Self {
            output,
            ast,
            strings: Vec::new(),
            emit_comments,
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

        self.c("prologue", true)?;
        writeln!(self.output, "    stp     x29, x30, [sp, -{}]!", stack_size)?;
        writeln!(self.output, "    mov     x29, sp")
    }

    fn write_func_epilogue(&mut self, offset: usize) -> io::Result<()> {
        let offset = if offset % 2 == 1 { offset + 1 } else { offset };
        let stack_size = 16 + (offset * 8);

        self.c("epilogue", true)?;
        writeln!(self.output, "    mov     sp, x29")?;
        writeln!(self.output, "    ldp     x29, x30, [sp], {}", stack_size)?;
        writeln!(self.output, "    ret")
    }

    fn write_func(&mut self, function: &Function) -> io::Result<()> {
        self.c(&format!("function {}", function.name), false)?;
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
            Statement::If { condition, consequence, otherwise } => {
                // TODO: here I'm generating duplicating labels like 1:, 2: for all if blocks, it may shoot into my leg in the future :)
                // but maybe it's fine, who knows

                self.write_expression(condition, &function.name, "x11")?;
                self.c(&format!("check if {} != 0", condition), true)?;
                writeln!(self.output, "    cmp     x11, 0")?;
                writeln!(self.output, "    b.eq    1f")?;
                self.c("consequence", true)?;
                for statement in consequence {
                    self.write_statement(function, statement)?;
                }
                writeln!(self.output, "    b       2f")?;
                writeln!(self.output, "1:")?;
                self.c("otherwise", true)?;
                for statement in otherwise {
                    self.write_statement(function, statement)?;
                }
                writeln!(self.output, "2:")?;
                Ok(())
            },
            Statement::While { condition, block } => {
                writeln!(self.output, "3:")?;
                if let Some(expr) = condition {
                    self.write_expression(expr, &function.name, "x12")?;
                } else {
                    writeln!(self.output, "    mov     x12, 1")?;
                }
                writeln!(self.output, "    cmp     x12, 0")?;
                writeln!(self.output, "    b.eq    4f")?;
                for s in block {
                    self.write_statement(function, s)?;
                }
                writeln!(self.output, "    b       3b")?;
                writeln!(self.output, "4:")?;
                Ok(())
            },
            Statement::Funcall { name, args } => {
                self.write_funcall(function, name, args)
            },
        }
    }

    fn write_funcall(&mut self, current_func: &Function, callee_name: &str, args: &[Expr]) -> io::Result<()> {
        match callee_name {
            "exit" => self.use_exit = true,
            "puts" => self.use_puts = true,
            "puti" => self.use_puti = true,
            _ => {},
        }

        if current_func.params.len() > 0 {
            self.c(&format!("store args of {}", current_func.name), true)?;
            for (i, _) in current_func.params.iter().enumerate() {
                writeln!(self.output, "    str     x{}, [x29, {}]", i, 16 + 8 * i)?;
            }
        }

        if args.len() > 0 {
            self.c(&format!("load args for {}", callee_name), true)?;
            for (i, arg) in args.iter().enumerate() {
                let reg = format!("x{}", i);
                self.write_expression(arg, &current_func.name, &reg)?;
            }
        }

        self.c(&format!("call {}", callee_name), true)?;
        writeln!(self.output, "    bl      _{}", callee_name)?;

        if current_func.params.len() > 0 {
            self.c(&format!("restore args for {}", current_func.name), true)?;
            for (i, _) in current_func.params.iter().enumerate() {
                writeln!(self.output, "    ldr     x{}, [x29, {}]", i, 16 + 8 * i)?;
            }
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
                self.c(&format!("number {}", n), true)?;
                writeln!(self.output, "    mov     {}, {}", reg, n)
            },
            Literal::String(text) => {
                let idx = self.strings.len();
                self.strings.push(text.clone());
                self.c(&format!("string \"{}\"", text.replace("\n", "\\n")), true)?;
                writeln!(self.output, "    adrp    {}, string.{}@PAGE", reg, idx)?;
                writeln!(self.output, "    add     {}, {}, string.{}@PAGEOFF", reg, reg, idx)
            },
        }
    }

    fn write_binary_expr(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr, current_func: &str, target_reg: &str) -> io::Result<()> {
        self.write_expression(lhs, current_func, "x9")?;
        let is_rhs_binop = matches!(rhs, Expr::Binary { .. });
        if is_rhs_binop {
            writeln!(self.output, "    str     x9, [sp, 16]")?;
        }
        self.write_expression(rhs, current_func, "x10")?;
        if is_rhs_binop {
            writeln!(self.output, "    ldr     x9, [sp, 16]")?;
        }

        self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

        match op {
            BinaryOp::Add => writeln!(self.output, "    add     {}, x9, x10", target_reg),
            BinaryOp::Sub => writeln!(self.output, "    sub     {}, x9, x10", target_reg),
            BinaryOp::Mul => writeln!(self.output, "    mul     {}, x9, x10", target_reg),
            BinaryOp::Div => writeln!(self.output, "    sdiv    {}, x9, x10", target_reg),
            BinaryOp::EQ => {
                writeln!(self.output, "    cmp     x9, x10")?;
                writeln!(self.output, "    cset    {}, eq", target_reg)?;
                Ok(())
            },
            BinaryOp::NE => {
                writeln!(self.output, "    cmp     x9, x10")?;
                writeln!(self.output, "    cset    {}, ne", target_reg)?;
                Ok(())
            },
            BinaryOp::GT => {
                writeln!(self.output, "    cmp     x9, x10")?;
                writeln!(self.output, "    cset    {}, gt", target_reg)?;
                Ok(())
            },
            BinaryOp::LT => {
                writeln!(self.output, "    cmp     x9, x10")?;
                writeln!(self.output, "    cset    {}, lt", target_reg)?;
                Ok(())
            },
            BinaryOp::GE => {
                writeln!(self.output, "    cmp     x9, x10")?;
                writeln!(self.output, "    cset    {}, ge", target_reg)?;
                Ok(())
            },
            BinaryOp::LE => {
                writeln!(self.output, "    cmp     x9, x10")?;
                writeln!(self.output, "    cset    {}, le", target_reg)?;
                Ok(())
            },
        }
    }

    fn write_funcall_expr(&mut self, name: &str, args: &[Expr], current_func: &str, target_reg: &str) -> io::Result<()> {
        self.c(&format!("args for funcall {}", name), true)?;
        for (i, arg) in args.iter().enumerate() {
            let reg = format!("x{}", i);
            self.write_expression(arg, current_func, &reg)?;
        }

        self.c(&format!("call {}", name), true)?;
        writeln!(self.output, "    bl      _{}", name)?;

        if target_reg != "x0" {
            writeln!(self.output, "    mov     {}, x0", target_reg)?;
        }

        Ok(())
    }

    fn write_variable(&mut self, name: &str, current_func: &str, target_reg: &str) -> io::Result<()> {
        let src_reg = format!("x{}", parser::get_variable_position(self.ast, current_func, name));
        if src_reg != target_reg {
            self.c(&format!("variable {}", name), true)?;
            writeln!(self.output, "    mov     {}, {}", target_reg, src_reg)?;
        }
        Ok(())
    }

    fn write_puts(&mut self) -> io::Result<()> {
        self.c("std::puts", false)?;
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
        self.c("std::puti", false)?;
        writeln!(self.output, "_puti:")?;
        writeln!(self.output, "    stp     x29, x30, [sp, -48]!")?;
        writeln!(self.output, "    mov     x29, sp")?;
        writeln!(self.output, "    cmp     x0, 0")?;
        writeln!(self.output, "    b.ge    1f")?;
        writeln!(self.output, "    neg     x0, x0")?;
        writeln!(self.output, "    str     x0, [sp, 16]")?;
        writeln!(self.output, "    mov     x0, 1")?;
        writeln!(self.output, "    adrp    x1, minus_char@PAGE")?;
        writeln!(self.output, "    add     x1, x1, minus_char@PAGEOFF")?;
        writeln!(self.output, "    mov     x2, 1")?;
        writeln!(self.output, "    mov     x16, 4")?;
        writeln!(self.output, "    svc     0")?;
        writeln!(self.output, "    ldr     x0, [sp, 16]")?;
        writeln!(self.output, "1:")?;
        writeln!(self.output, "    mov     x9, 0xCCCC")?;
        writeln!(self.output, "    movk    x9, 0xCCCC, lsl 16")?;
        writeln!(self.output, "    movk    x9, 0xCCCC, lsl 32")?;
        writeln!(self.output, "    movk    x9, 0xCCCD, lsl 48")?;
        writeln!(self.output, "    mov     w11, 10")?;
        writeln!(self.output, "    strb    w11, [x29, 47]")?;
        writeln!(self.output, "    add     x2, x29, 46")?;
        writeln!(self.output, "    mov     x5, x0")?;
        writeln!(self.output, "    cmp     x5, 0")?;
        writeln!(self.output, "    b.ne    2f")?;
        writeln!(self.output, "    mov     w0, 48")?;
        writeln!(self.output, "    strb    w0, [x2], -1")?;
        writeln!(self.output, "    b       2f")?;
        writeln!(self.output, "2:")?;
        writeln!(self.output, "    umulh   x11, x5, x9")?;
        writeln!(self.output, "    lsr     x11, x11, 3")?;
        writeln!(self.output, "    add     x12, x11, x11, lsl 2")?;
        writeln!(self.output, "    add     x12, x12, x12")?;
        writeln!(self.output, "    sub     x12, x5, x12")?;
        writeln!(self.output, "    add     w12, w12, 48")?;
        writeln!(self.output, "    strb    w12, [x2], -1")?;
        writeln!(self.output, "    mov     x5, x11")?;
        writeln!(self.output, "    cmp     x5, 0")?;
        writeln!(self.output, "    b.ne    2b")?;
        writeln!(self.output, "3:")?;
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
        self.c("std::exit", false)?;
        writeln!(self.output, "_exit:")?;
        writeln!(self.output, "    mov     x16, 1")?;
        writeln!(self.output, "    svc     0")?;
        writeln!(self.output)?;
        Ok(())
    }

    fn write_data_section(&mut self) -> io::Result<()> {
        self.c("data section", false)?;
        writeln!(self.output, "minus_char:")?;
        writeln!(self.output, "    .asciz \"-\"")?;

        for (i, s) in self.strings.iter().enumerate() {
            let escaped = s.replace("\n", "\\n");
            writeln!(self.output, "string.{}:", i)?;
            writeln!(self.output, "    .asciz \"{}\"", escaped)?;
        }
        Ok(())
    }

    fn c(&mut self, s: &str, padding: bool) -> io::Result<()> {
        if self.emit_comments {
            let pad = if padding {
                "    "
            } else {
                ""
            };
            writeln!(self.output, "{}; {}", pad, s)?;
        }
        Ok(())
    }
}
