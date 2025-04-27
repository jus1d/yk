use std::io::{self, Write};

use crate::parser::{self, Ast, BinaryOp, Expr, Function, Literal, Statement};

pub struct Generator<'a, W: Write> {
    output: W,
    ast: &'a Ast,
    strings: Vec<String>,
    label_counter: usize,
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
            label_counter: 0,
            emit_comments,
            use_exit: false,
            use_puts: false,
            use_puti: false,
        }
    }

    fn label(&mut self) -> String {
        let l = format!("L{}", self.label_counter);
        self.label_counter += 1;
        return l;
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

    fn write_func(&mut self, func: &Function) -> io::Result<()> {
        self.c(&format!("function: {}", func.name), false)?;
        writeln!(self.output, "_{}:", func.name)?;

        let params_count = func.params.len();
        let declarations_count = func.body.iter().filter(|s| matches!(s, Statement::Declaration { .. })).count();
        self.write_func_prologue(params_count + declarations_count)?;

        self.c("push locals onto the stack", true)?;
        for i in 0..params_count {
            writeln!(self.output, "    str     x{}, [x29, {}]", i, 16 + 8 * i)?;
        }

        self.c("body", true)?;
        for statement in &func.body {
            self.write_statement(statement, func, declarations_count)?;
        }

        if !func.body.iter().any(|s| matches!(s, Statement::Ret { .. })) {
            self.write_func_epilogue(params_count + declarations_count)?;
        }

        writeln!(self.output)?;
        Ok(())
    }

    fn write_func_prologue(&mut self, offset: usize) -> io::Result<()> {
        let stack_size = stack_size(offset);

        self.c("prologue", true)?;
        writeln!(self.output, "    stp     x29, x30, [sp, -{}]!", stack_size)?;
        writeln!(self.output, "    mov     x29, sp")
    }

    fn write_func_epilogue(&mut self, offset: usize) -> io::Result<()> {
        let stack_size = stack_size(offset);

        self.c("epilogue", true)?;
        writeln!(self.output, "    mov     sp, x29")?;
        writeln!(self.output, "    ldp     x29, x30, [sp], {}", stack_size)?;
        writeln!(self.output, "    ret")
    }

    fn write_statement(&mut self, statement: &Statement, current_func: &Function, declarations_count: usize) -> io::Result<()> {
        match statement {
            Statement::Ret { value } => {
                if let Some(expr) = value {
                    self.write_expression(expr, current_func, "x0")?;
                }
                self.write_func_epilogue(current_func.params.len() + declarations_count)
            }
            Statement::If { condition, consequence, otherwise } => {
                let else_label = self.label();
                let end_label = self.label();

                self.write_expression(condition, current_func, "x11")?;
                self.c(&format!("condition: {}", condition), true)?;
                writeln!(self.output, "    cmp     x11, 0")?;
                writeln!(self.output, "    b.eq    {}", else_label)?;

                self.c("consequence", true)?;
                for statement in consequence {
                    self.write_statement(statement, current_func, declarations_count)?;
                }
                writeln!(self.output, "    b       {}", end_label)?;

                writeln!(self.output, "{}:", else_label)?;
                self.c("otherwise", true)?;
                for statement in otherwise {
                    self.write_statement(statement, current_func, declarations_count)?;
                }

                writeln!(self.output, "{}:", end_label)?;
                Ok(())
            },
            Statement::While { condition, block } => {
                let start_label = self.label();
                let end_label = self.label();

                writeln!(self.output, "{}:", start_label)?;
                if let Some(expr) = condition {
                    self.write_expression(expr, current_func, "x12")?;
                    writeln!(self.output, "    cmp     x12, 0")?;
                    writeln!(self.output, "    b.eq    {}", end_label)?;
                } else {
                    writeln!(self.output, "    mov     x12, 1")?;
                    writeln!(self.output, "    cmp     x12, 0")?;
                    writeln!(self.output, "    b.eq    {}", end_label)?;
                }

                for s in block {
                    self.write_statement(s, current_func, declarations_count)?;
                }
                writeln!(self.output, "    b       {}", start_label)?;
                writeln!(self.output, "{}:", end_label)?;
                Ok(())
            },
            Statement::Funcall { name, args } => {
                self.write_funcall(name, args, current_func, "x0")
            },
            Statement::Declaration { name, typ: _, value } => {
                if let Some(expr) = value {
                    self.write_expression(expr, current_func, "x8")?;
                    writeln!(self.output, "    str     x8, [x29, {}]", 16 + parser::get_variable_position(name, current_func) * 8)?;
                }

                Ok(())
            },
            Statement::Assignment { name, value } => {
                self.write_expression(value, current_func, "x8")?;
                writeln!(self.output, "    str     x8, [x29, {}]", 16 + parser::get_variable_position(name, current_func) * 8)?;

                Ok(())
            }
        }
    }

    fn write_expression(&mut self, expr: &Expr, current_func: &Function, target_reg: &str) -> io::Result<()> {
        match expr {
            Expr::Literal(lit) => self.write_literal(lit, target_reg),
            Expr::Binary { op, lhs, rhs } => self.write_binop(op, lhs, rhs, current_func, target_reg),
            Expr::Funcall { name, args } => self.write_funcall(name, args, current_func, target_reg),
            Expr::Variable(name) => self.write_variable(name, current_func, target_reg),
        }
    }

    fn write_literal(&mut self, lit: &Literal, target_reg: &str) -> io::Result<()> {
        match lit {
            Literal::Number(n) => {
                self.c(&format!("number: {}", n), true)?;
                writeln!(self.output, "    mov     {}, {}", target_reg, n)
            },
            Literal::String(text) => {
                let idx = self.strings.len();
                self.strings.push(text.clone());
                self.c(&format!("string: \"{}\"", text.replace("\n", "\\n")), true)?;
                writeln!(self.output, "    adrp    {}, string.{}@PAGE", target_reg, idx)?;
                writeln!(self.output, "    add     {}, {}, string.{}@PAGEOFF", target_reg, target_reg, idx)
            },
        }
    }

    fn write_binop(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr, current_func: &Function, target_reg: &str) -> io::Result<()> {
        self.write_expression(lhs, current_func, "x9")?;
        self.write_expression(rhs, current_func, "x10")?;

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

    fn write_funcall(&mut self, name: &str, args: &[Expr], current_func: &Function, target_reg: &str) -> io::Result<()> {
        match name {
            "exit" => self.use_exit = true,
            "puts" => self.use_puts = true,
            "puti" => self.use_puti = true,
            _ => {},
        }

        self.c("save temporary registers", true)?;
        writeln!(self.output, "    stp     x9, x10, [sp, -16]!")?;
        writeln!(self.output, "    stp     x11, x12, [sp, -16]!")?;

        if args.len() > 0 {
            self.c(&format!("load args for {}", name), true)?;
            for (i, arg) in args.iter().enumerate() {
                let reg = format!("x{}", i);
                self.write_expression(arg, current_func, &reg)?;
            }
        }

        self.c(&format!("call {}", name), true)?;
        writeln!(self.output, "    bl      _{}", name)?;

        self.c("restore temporary registers", true)?;
        writeln!(self.output, "    ldp     x11, x12, [sp], 16")?;
        writeln!(self.output, "    ldp     x9, x10, [sp], 16")?;

        if target_reg != "x0" {
            writeln!(self.output, "    mov     {}, x0", target_reg)?;
        }

        Ok(())
    }

    fn write_variable(&mut self, name: &str, current_func: &Function, target_reg: &str) -> io::Result<()> {
        let pos = parser::get_variable_position(name, current_func);
        self.c(&format!("var: {}", name), true)?;
        writeln!(self.output, "    ldr     {}, [x29, {}]", target_reg, 16 + 8 * pos)
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
        writeln!(self.output, "    b       3f")?;
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
            let pad = if padding { "    " } else { "" };
            writeln!(self.output, "{}; {}", pad, s)?;
        }
        Ok(())
    }
}

fn stack_size(offset: usize) -> usize {
    let offset = if offset % 2 == 1 { offset + 1 } else { offset };
    let stack_size = 16 + (offset * 8);
    stack_size
}
