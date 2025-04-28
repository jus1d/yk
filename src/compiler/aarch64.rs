use std::io::{self, Write};

use crate::parser::{self, Ast, BinaryOp, Expr, Function, Literal, Statement};

pub struct Generator<'a, W: Write> {
    out: W,
    ast: &'a Ast,
    strings: Vec<String>,
    label_counter: usize,
    emit_comments: bool,
    use_exit: bool,
    use_puts: bool,
    use_puti: bool,
}

impl<'a, W: Write> Generator<'a, W> {
    pub fn new(ast: &'a Ast, out: W, emit_comments: bool) -> Self {
        Self {
            out,
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
        writeln!(self.out, ".global _main")?;
        writeln!(self.out, ".align 2\n")?;
        Ok(())
    }

    fn write_func(&mut self, func: &Function) -> io::Result<()> {
        writeln!(self.out, "_{}:", func.name)?;

        let params_count = func.params.len();
        let declarations_count = func.body.iter().filter(|s| matches!(s, Statement::Declaration { .. })).count();
        self.write_func_prologue(params_count + declarations_count)?;

        if params_count > 0 {
            self.c("push arguments onto the stack", true)?;
            for i in 0..params_count {
                writeln!(self.out, "    str     x{}, [x29, {}]", i, 16 + 8 * i)?;
            }
        }

        if func.body.len() > 0 {
            self.c(&format!("body: {}", func.name), true)?;
            for statement in &func.body {
                self.write_statement(statement, func, declarations_count)?;
            }
        }


        if !func.body.iter().any(|s| matches!(s, Statement::Ret { .. })) {
            self.write_func_epilogue(params_count + declarations_count)?;
        }

        writeln!(self.out)?;
        Ok(())
    }

    fn write_func_prologue(&mut self, offset: usize) -> io::Result<()> {
        let stack_size = stack_size(offset);

        self.c("prologue", true)?;
        writeln!(self.out, "    stp     x29, x30, [sp, -{}]!", stack_size)?;
        writeln!(self.out, "    mov     x29, sp")
    }

    fn write_func_epilogue(&mut self, offset: usize) -> io::Result<()> {
        let stack_size = stack_size(offset);

        self.c("epilogue", true)?;
        writeln!(self.out, "    mov     sp, x29")?;
        writeln!(self.out, "    ldp     x29, x30, [sp], {}", stack_size)?;
        writeln!(self.out, "    ret")
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
                writeln!(self.out, "    cmp     x11, 0")?;
                writeln!(self.out, "    b.eq    {}", else_label)?;

                if consequence.len() > 0 {
                    self.c("consequence", true)?;
                    for statement in consequence {
                        self.write_statement(statement, current_func, declarations_count)?;
                    }
                }
                writeln!(self.out, "    b       {}", end_label)?;

                writeln!(self.out, "{}:    ; else-label", else_label)?;
                if otherwise.len() > 0 {
                    self.c("otherwise", true)?;
                    for statement in otherwise {
                        self.write_statement(statement, current_func, declarations_count)?;
                    }
                }

                writeln!(self.out, "{}:    ; end-if", end_label)?;
                Ok(())
            },
            Statement::While { condition, block } => {
                let start_label = self.label();
                let end_label = self.label();

                writeln!(self.out, "{}:    ; while", start_label)?;
                if let Some(expr) = condition {
                    self.write_expression(expr, current_func, "x12")?;
                    writeln!(self.out, "    cmp     x12, 0")?;
                    writeln!(self.out, "    b.eq    {}", end_label)?;
                } else {
                    writeln!(self.out, "    mov     x12, 1")?;
                    writeln!(self.out, "    cmp     x12, 0")?;
                    writeln!(self.out, "    b.eq    {}", end_label)?;
                }

                for s in block {
                    self.write_statement(s, current_func, declarations_count)?;
                }
                writeln!(self.out, "    b       {}", start_label)?;
                writeln!(self.out, "{}:    ; while-end", end_label)?;
                Ok(())
            },
            Statement::Funcall { name, args } => {
                self.write_funcall(name, args, current_func, "x0")
            },
            Statement::Declaration { name, typ: _, value } => {
                if let Some(expr) = value {
                    self.write_expression(expr, current_func, "x8")?;
                    self.c(&format!("declaration: {} = {}", name, expr), true)?;
                    writeln!(self.out, "    str     x8, [x29, {}]", 16 + parser::get_variable_position(name, current_func) * 8)?;
                }
                // Nothing to do, if value is empty. Variable will just allocated on the stack.
                Ok(())
            },
            Statement::Assignment { name, value } => {
                self.write_expression(value, current_func, "x8")?;
                self.c(&format!("assignment: {} = {}", name, value), true)?;
                writeln!(self.out, "    str     x8, [x29, {}]", 16 + parser::get_variable_position(name, current_func) * 8)?;
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
                writeln!(self.out, "    mov     {}, {}", target_reg, n)
            },
            Literal::String(text) => {
                let idx = self.strings.len();
                self.strings.push(text.clone());
                self.c(&format!("string: \"{}\"", text.replace("\n", "\\n")), true)?;
                writeln!(self.out, "    adrp    {}, string.{}@PAGE", target_reg, idx)?;
                writeln!(self.out, "    add     {}, {}, string.{}@PAGEOFF", target_reg, target_reg, idx)
            },
            Literal::Bool(value) => {
                self.c(&format!("bool: {}", value), true)?;
                writeln!(self.out, "    mov     {}, {}", target_reg, *value as i8)?;
                Ok(())
            }
        }
    }

    fn write_binop(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr, current_func: &Function, target_reg: &str) -> io::Result<()> {
        self.write_expression(lhs, current_func, "x9")?;

        match op {
            BinaryOp::LogicalOr => {
                let end_label = self.label();
                let true_label = self.label();

                self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

                writeln!(self.out, "    cmp     x9, 0")?;
                writeln!(self.out, "    b.ne    {}", true_label)?;

                self.write_expression(rhs, current_func, "x10")?;
                writeln!(self.out, "    cmp     x10, 0")?;
                writeln!(self.out, "    b.ne    {}", true_label)?;

                writeln!(self.out, "    mov     {}, 0", target_reg)?;
                writeln!(self.out, "    b       {}", end_label)?;
                writeln!(self.out, "{}:", true_label)?;
                writeln!(self.out, "    mov     {}, 1", target_reg)?;
                writeln!(self.out, "{}:", end_label)?;
                Ok(())
            },
            BinaryOp::LogicalAnd => {
                let end_label = self.label();
                let false_label = self.label();

                self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

                writeln!(self.out, "    cmp     x9, 0")?;
                writeln!(self.out, "    b.eq    {}", false_label)?;

                self.write_expression(rhs, current_func, "x10")?;
                writeln!(self.out, "    cmp     x10, 0")?;
                writeln!(self.out, "    b.eq    {}", false_label)?;

                writeln!(self.out, "    mov     {}, 1", target_reg)?;
                writeln!(self.out, "    b       {}", end_label)?;
                writeln!(self.out, "{}:", false_label)?;
                writeln!(self.out, "    mov     {}, 0", target_reg)?;
                writeln!(self.out, "{}:", end_label)?;
                Ok(())
            },
            _ => {
                self.write_expression(rhs, current_func, "x10")?;
                self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

                match op {
                    BinaryOp::Add => writeln!(self.out, "    add     {}, x9, x10", target_reg),
                    BinaryOp::Sub => writeln!(self.out, "    sub     {}, x9, x10", target_reg),
                    BinaryOp::Mul => writeln!(self.out, "    mul     {}, x9, x10", target_reg),
                    BinaryOp::Div => writeln!(self.out, "    sdiv    {}, x9, x10", target_reg),
                    BinaryOp::Mod => {
                        writeln!(self.out, "    sdiv    x11, x9, x10")?;
                        writeln!(self.out, "    msub    {}, x11, x10, x9", target_reg)?;
                        Ok(())
                    },
                    BinaryOp::EQ => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    {}, eq", target_reg)
                    },
                    BinaryOp::NE => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    {}, ne", target_reg)
                    },
                    BinaryOp::GT => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    {}, gt", target_reg)
                    },
                    BinaryOp::LT => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    {}, lt", target_reg)
                    },
                    BinaryOp::GE => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    {}, ge", target_reg)
                    },
                    BinaryOp::LE => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    {}, le", target_reg)
                    },
                    BinaryOp::LogicalOr => unreachable!(),
                    BinaryOp::LogicalAnd => unreachable!(),
                }
            }
        }
    }

    fn write_funcall(&mut self, name: &str, args: &[Expr], current_func: &Function, target_reg: &str) -> io::Result<()> {
        match name {
            "exit" => self.use_exit = true,
            "puts" => self.use_puts = true,
            "puti" => self.use_puti = true,
            _ => {},
        }

        self.c("save temp registers", true)?;
        writeln!(self.out, "    stp     x9, x10, [sp, -16]!")?;
        writeln!(self.out, "    stp     x11, x12, [sp, -16]!")?;

        if args.len() > 0 {
            self.c(&format!("load args for {}", name), true)?;
            for (i, arg) in args.iter().enumerate() {
                let reg = format!("x{}", i);
                self.write_expression(arg, current_func, &reg)?;
            }
        }

        self.c(&format!("call: {}", name), true)?;
        writeln!(self.out, "    bl      _{}", name)?;

        self.c("restore temp registers", true)?;
        writeln!(self.out, "    ldp     x11, x12, [sp], 16")?;
        writeln!(self.out, "    ldp     x9, x10, [sp], 16")?;

        if target_reg != "x0" {
            writeln!(self.out, "    mov     {}, x0", target_reg)?;
        }

        Ok(())
    }

    fn write_variable(&mut self, name: &str, current_func: &Function, target_reg: &str) -> io::Result<()> {
        let pos = parser::get_variable_position(name, current_func);
        self.c(&format!("var: {}", name), true)?;
        writeln!(self.out, "    ldr     {}, [x29, {}]", target_reg, 16 + 8 * pos)
    }

    fn write_puts(&mut self) -> io::Result<()> {
        self.c("std::puts", false)?;
        writeln!(self.out, "_puts:")?;
        writeln!(self.out, "    mov     x1, x0")?;
        writeln!(self.out, "    mov     x2, 0")?;
        writeln!(self.out, "1:")?;
        writeln!(self.out, "    ldrb    w3, [x1, x2]")?;
        writeln!(self.out, "    cbz     w3, 2f")?;
        writeln!(self.out, "    add     x2, x2, 1")?;
        writeln!(self.out, "    b       1b")?;
        writeln!(self.out, "2:")?;
        writeln!(self.out, "    mov     x0, 1")?;
        writeln!(self.out, "    mov     x16, 4")?;
        writeln!(self.out, "    svc     0")?;
        writeln!(self.out, "    ret")?;
        writeln!(self.out)?;
        Ok(())
    }

    fn write_puti(&mut self) -> io::Result<()> {
        self.c("std::puti", false)?;
        writeln!(self.out, "_puti:")?;
        writeln!(self.out, "    stp     x29, x30, [sp, -48]!")?;
        writeln!(self.out, "    mov     x29, sp")?;
        writeln!(self.out, "    cmp     x0, 0")?;
        writeln!(self.out, "    b.ge    1f")?;
        writeln!(self.out, "    neg     x0, x0")?;
        writeln!(self.out, "    str     x0, [sp, 16]")?;
        writeln!(self.out, "    mov     x0, 1")?;
        writeln!(self.out, "    adrp    x1, minus_char@PAGE")?;
        writeln!(self.out, "    add     x1, x1, minus_char@PAGEOFF")?;
        writeln!(self.out, "    mov     x2, 1")?;
        writeln!(self.out, "    mov     x16, 4")?;
        writeln!(self.out, "    svc     0")?;
        writeln!(self.out, "    ldr     x0, [sp, 16]")?;
        writeln!(self.out, "1:")?;
        writeln!(self.out, "    mov     x9, 0xCCCC")?;
        writeln!(self.out, "    movk    x9, 0xCCCC, lsl 16")?;
        writeln!(self.out, "    movk    x9, 0xCCCC, lsl 32")?;
        writeln!(self.out, "    movk    x9, 0xCCCD, lsl 48")?;
        writeln!(self.out, "    mov     w11, 10")?;
        writeln!(self.out, "    strb    w11, [x29, 47]")?;
        writeln!(self.out, "    add     x2, x29, 46")?;
        writeln!(self.out, "    mov     x5, x0")?;
        writeln!(self.out, "    cmp     x5, 0")?;
        writeln!(self.out, "    b.ne    2f")?;
        writeln!(self.out, "    mov     w0, 48")?;
        writeln!(self.out, "    strb    w0, [x2], -1")?;
        writeln!(self.out, "    b       3f")?;
        writeln!(self.out, "2:")?;
        writeln!(self.out, "    umulh   x11, x5, x9")?;
        writeln!(self.out, "    lsr     x11, x11, 3")?;
        writeln!(self.out, "    add     x12, x11, x11, lsl 2")?;
        writeln!(self.out, "    add     x12, x12, x12")?;
        writeln!(self.out, "    sub     x12, x5, x12")?;
        writeln!(self.out, "    add     w12, w12, 48")?;
        writeln!(self.out, "    strb    w12, [x2], -1")?;
        writeln!(self.out, "    mov     x5, x11")?;
        writeln!(self.out, "    cmp     x5, 0")?;
        writeln!(self.out, "    b.ne    2b")?;
        writeln!(self.out, "3:")?;
        writeln!(self.out, "    add     x1, x2, 1")?;
        writeln!(self.out, "    add     x3, x29, 48")?;
        writeln!(self.out, "    sub     x2, x3, x1")?;
        writeln!(self.out, "    mov     x0, 1")?;
        writeln!(self.out, "    mov     x16, 4")?;
        writeln!(self.out, "    svc     0")?;
        writeln!(self.out, "    ldp     x29, x30, [sp], 48")?;
        writeln!(self.out, "    ret")?;
        writeln!(self.out)?;
        Ok(())
    }

    fn write_exit(&mut self) -> io::Result<()> {
        self.c("std::exit", false)?;
        writeln!(self.out, "_exit:")?;
        writeln!(self.out, "    mov     x16, 1")?;
        writeln!(self.out, "    svc     0")?;
        writeln!(self.out)?;
        Ok(())
    }

    fn write_data_section(&mut self) -> io::Result<()> {
        self.c("data section", false)?;
        writeln!(self.out, "minus_char:")?;
        writeln!(self.out, "    .asciz \"-\"")?;

        for (i, s) in self.strings.iter().enumerate() {
            let escaped = s.replace("\n", "\\n");
            writeln!(self.out, "string.{}:", i)?;
            writeln!(self.out, "    .asciz \"{}\"", escaped)?;
        }
        Ok(())
    }

    fn c(&mut self, s: &str, padding: bool) -> io::Result<()> {
        if self.emit_comments {
            let pad = if padding { "    " } else { "" };
            writeln!(self.out, "{}; {}", pad, s)?;
        }
        Ok(())
    }
}

fn stack_size(offset: usize) -> usize {
    let offset = if offset % 2 == 1 { offset + 1 } else { offset };
    let stack_size = 16 + (offset * 8);
    stack_size
}
