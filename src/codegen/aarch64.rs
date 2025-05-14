use crate::opts::Opts;
use crate::parser::ast;

use ast::{Ast, Function, Statement, Expr, Literal, BinaryOp, UnaryOp};
use std::collections::BTreeSet;
use std::io::{self, Write};
use std::process::{exit, Command, Output};

pub struct Generator<'a> {
    out: Vec<u8>,
    ast: &'a Ast,
    // NOTE: Used BTreeSet instead of HashSet, 'cause unlike HashSet, it's ordered
    strings: Vec<String>,
    label_counter: usize,
    emit_comments: bool,
    builtins_used: BTreeSet<String>,
}

impl<'a> Generator<'a> {
    pub fn new(ast: &'a Ast, emit_comments: bool) -> Self {
        Self {
            out: Vec::new(),
            ast,
            strings: Vec::new(),
            label_counter: 0,
            emit_comments,
            builtins_used: BTreeSet::new(),
        }
    }

    pub fn into_assembly(self) -> io::Result<String> {
        String::from_utf8(self.out).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.out
    }

    fn label(&mut self) -> String {
        let l = format!("L{}", self.label_counter);
        self.label_counter += 1;
        l
    }

    pub fn generate(&mut self) -> io::Result<()> {
        self.write_preamble()?;

        for function in self.ast.functions.values() {
            if function.is_external { continue; }
            self.write_func(function)?;
        }

        for builtin_func_name in self.builtins_used.clone() {
            match builtin_func_name.as_str() {
                "write" => self.write_write()?,
                "puti" => self.write_puti()?,
                "putc" => self.write_putc()?,
                "exit" => self.write_exit()?,
                _ => unreachable!(),
            }
        }
        self.write_data_section()
    }

    fn write_preamble(&mut self) -> io::Result<()> {
        writeln!(self.out, ".global _start")?;
        writeln!(self.out, ".align 2")?;
        writeln!(self.out)?;
        writeln!(self.out, "_start:")?;
        writeln!(self.out, "    bl      _main")?;
        writeln!(self.out, "    mov     x16, 1")?;
        writeln!(self.out, "    svc     0\n")?;
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

        let mut scope: Vec<String> = Vec::new();
        for arg in func.params.clone() {
            scope.push(arg.name);
        }

        if !func.body.is_empty() {
            self.c(&format!("body: {}", func.name), true)?;
            for statement in &func.body {
                self.write_statement(statement, func, declarations_count, &mut scope)?;
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
        writeln!(self.out, "    ldp     x29, x30, [sp], {}", stack_size)?;
        writeln!(self.out, "    ret")
    }

    fn write_statement(&mut self, statement: &Statement, current_func: &Function, declarations_count: usize, scope: &mut Vec<String>) -> io::Result<()> {
        match statement {
            Statement::Ret { value } => {
                if let Some(expr) = value {
                    self.write_expression(expr, scope, current_func, 0)?;
                }
                self.write_func_epilogue(current_func.params.len() + declarations_count)?;
            }
            Statement::If { branches, otherwise } => {
                let mut labels: Vec<String> = vec![];
                let n = branches.len() + 2;

                for _ in 0..n {
                    labels.push(self.label());
                }

                let label_end = &labels[n-1];
                let label_otherwise = &labels[n-2];

                for (i, branch) in branches.iter().enumerate() {
                    let saved_scope = scope.clone();
                    writeln!(self.out, "{}:", labels[i])?;
                    self.write_expression(&branch.condition, scope, current_func, 11)?;
                    self.c(&format!("condition: {}", branch.condition), true)?;
                    writeln!(self.out, "    cmp     x11, 0")?;
                    writeln!(self.out, "    b.eq    {}", labels[i+1])?;
                    for statement in &branch.block {
                        self.write_statement(statement, current_func, declarations_count, scope)?;
                    }
                    writeln!(self.out, "    b       {}", label_end)?;
                    *scope = saved_scope;
                }

                self.c("otherwise", true)?;
                writeln!(self.out, "{}:", label_otherwise)?;
                let saved_scope = scope.clone();
                if !otherwise.is_empty() {
                    for statement in otherwise {
                        self.write_statement(statement, current_func, declarations_count, scope)?;
                    }
                }
                *scope = saved_scope;
                writeln!(self.out, "{}:", label_end)?;
            }
            Statement::While { condition, block } => {
                let start_label = self.label();
                let end_label = self.label();

                writeln!(self.out, "{}:", start_label)?;
                if let Some(expr) = condition {
                    self.write_expression(expr, scope, current_func, 12)?;
                    writeln!(self.out, "    cmp     x12, 0")?;
                    writeln!(self.out, "    b.eq    {}", end_label)?;
                } else {
                    writeln!(self.out, "    mov     x12, 1")?;
                    writeln!(self.out, "    cmp     x12, 0")?;
                    writeln!(self.out, "    b.eq    {}", end_label)?;
                }

                let saved_scope = scope.clone();
                for s in block {
                    self.write_statement(s, current_func, declarations_count, scope)?;
                }
                *scope = saved_scope;
                writeln!(self.out, "    b       {}", start_label)?;
                writeln!(self.out, "{}:", end_label)?;
            }
            Statement::Funcall { name, args, loc: _ } => {
                self.write_funcall(name, args, scope, current_func, 0)?;
            }
            Statement::Declaration { name, value, .. } => {
                scope.push(name.clone());
                self.write_expression(value, scope, current_func, 8)?;
                self.c(&format!("declaration: {} = {}", name, value), true)?;
                writeln!(self.out, "    str     x8, [x29, {}]", 16 + get_variable_position(name, scope) * 8)?;
            }
            Statement::Assignment { lhs, value } => {
                match lhs {
                    Expr::Variable { name, .. } => {
                        self.write_expression(value, scope, current_func, 8)?;
                        self.c(&format!("assignment: {} = {}", name, value), true)?;
                        writeln!(self.out, "    str     x8, [x29, {}]", 16 + get_variable_position(name, scope) * 8)?;
                    },
                    Expr::Unary { op: UnaryOp::Dereference, operand, .. } => {
                        if let Expr::Variable { name, .. } = operand.as_ref() {
                            self.write_expression(operand, scope, current_func, 8)?;
                            self.c(&format!("dereference assignment: *{} = {}", name, value), true)?;
                            self.write_expression(value, scope, current_func, 9)?;
                            writeln!(self.out, "    str     x9, [x8]")?;
                        } else {
                            eprintln!("{}: error: can only dereference variables in assignments", operand.clone().loc());
                            exit(1);
                        }
                    }
                    _ => todo!(),
                }
            }
        }
        Ok(())
    }

    fn write_expression(&mut self, expr: &Expr, scope: &mut Vec<String>, current_func: &Function, target_register_index: u8) -> io::Result<()> {
        match expr {
            Expr::Literal { lit, .. } => self.write_literal(lit, target_register_index),
            Expr::Binary { op, lhs, rhs, .. } => self.write_binop(op, lhs, rhs, scope, current_func, target_register_index),
            Expr::Unary { op, operand, .. } => self.write_unary(op, operand, scope, current_func, target_register_index),
            Expr::Funcall { name, args, .. } => self.write_funcall(name, args, scope, current_func, target_register_index),
            Expr::Variable { name, .. } => self.write_variable(name, scope, target_register_index),
            Expr::Index { collection, index, .. } => self.write_index(collection, index, scope, current_func, target_register_index),
        }
    }

    fn write_index(&mut self, collection: &Expr, index: &Expr, scope: &mut Vec<String>, current_func: &Function, target_register_index: u8) -> io::Result<()> {
        self.write_expression(collection, scope, current_func, 9)?;
        self.write_expression(index, scope, current_func, 10)?;
        writeln!(self.out, "    ldrb    w{}, [x9, x10]", target_register_index)?;
        Ok(())
    }

    fn write_literal(&mut self, lit: &Literal, target_register_index: u8) -> io::Result<()> {
        match lit {
            Literal::Number(n) => {
                self.c(&format!("number: {}", n), true)?;

                let signed = *n as i64;

                if signed >= -65536 && signed <= 65535 {
                    writeln!(self.out, "    mov     x{}, {}", target_register_index, signed)?;
                    return Ok(());
                }

                let unsigned = *n as u64;
                let mut parts: [u16; 4] = [0; 4];
                for i in 0..4 {
                    parts[i] = ((unsigned >> (16 * i)) & 0xFFFF) as u16;
                }

                let mut first = true;
                for (i, &part) in parts.iter().enumerate() {
                    if part != 0 {
                        let shift = i * 16;
                        if first {
                            writeln!(self.out, "    movz    x{}, #{}, lsl #{}", target_register_index, part, shift)?;
                            first = false;
                        } else {
                            writeln!(self.out, "    movk    x{}, #{}, lsl #{}", target_register_index, part, shift)?;
                        }
                    }
                }

                if unsigned == 0 {
                    writeln!(self.out, "    mov     x{}, #0", target_register_index)?;
                }
            },
            Literal::String(text) => {
                let idx = match self.strings.iter().position(|s| s == text) {
                    Some(idx) => idx,
                    None => {
                        self.strings.push(text.clone());
                        self.strings.len() - 1
                    },
                };

                self.c(&format!("string: \"{}\"", text.replace("\n", "\\n")), true)?;
                writeln!(self.out, "    adrp    x{}, string.{}@PAGE", target_register_index, idx)?;
                writeln!(self.out, "    add     x{}, x{}, string.{}@PAGEOFF", target_register_index, target_register_index, idx)?;
            }
            Literal::Bool(value) => {
                self.c(&format!("bool: {}", value), true)?;
                writeln!(self.out, "    mov     x{}, {}", target_register_index, *value as i8)?;
            },
            Literal::Char(ch) => {
                self.c(&format!("char: {}", ch), true)?;
                writeln!(self.out, "    mov     x{}, {}", target_register_index, *ch as u8)?;
            },
            Literal::Nil => {
                self.c("nil", true)?;
                writeln!(self.out, "    mov     x{}, 0", target_register_index)?;
            }
        }
        Ok(())
    }

    fn write_binop(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr, scope: &mut Vec<String>, current_func: &Function, target_register_index: u8) -> io::Result<()> {
        self.write_expression(lhs, scope, current_func, 9)?;

        match op {
            BinaryOp::LogicalOr => {
                let end_label = self.label();
                let true_label = self.label();

                self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

                writeln!(self.out, "    cmp     x9, 0")?;
                writeln!(self.out, "    b.ne    {}", true_label)?;

                self.write_expression(rhs, scope, current_func, 10)?;
                writeln!(self.out, "    cmp     x10, 0")?;
                writeln!(self.out, "    b.ne    {}", true_label)?;

                writeln!(self.out, "    mov     x{}, 0", target_register_index)?;
                writeln!(self.out, "    b       {}", end_label)?;
                writeln!(self.out, "{}:", true_label)?;
                writeln!(self.out, "    mov     x{}, 1", target_register_index)?;
                writeln!(self.out, "{}:", end_label)?;
                Ok(())
            }
            BinaryOp::LogicalAnd => {
                let end_label = self.label();
                let false_label = self.label();

                self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

                writeln!(self.out, "    cmp     x9, 0")?;
                writeln!(self.out, "    b.eq    {}", false_label)?;

                self.write_expression(rhs, scope, current_func, 10)?;
                writeln!(self.out, "    cmp     x10, 0")?;
                writeln!(self.out, "    b.eq    {}", false_label)?;

                writeln!(self.out, "    mov     x{}, 1", target_register_index)?;
                writeln!(self.out, "    b       {}", end_label)?;
                writeln!(self.out, "{}:", false_label)?;
                writeln!(self.out, "    mov     x{}, 0", target_register_index)?;
                writeln!(self.out, "{}:", end_label)?;
                Ok(())
            }
            _ => {
                self.write_expression(rhs, scope, current_func, 10)?;
                self.c(&format!("binop: {} {} {}", lhs, op, rhs), true)?;

                match op {
                    BinaryOp::Add => writeln!(self.out, "    add     x{}, x9, x10", target_register_index),
                    BinaryOp::Sub => writeln!(self.out, "    sub     x{}, x9, x10", target_register_index),
                    BinaryOp::Mul => writeln!(self.out, "    mul     x{}, x9, x10", target_register_index),
                    BinaryOp::Div => writeln!(self.out, "    sdiv    x{}, x9, x10", target_register_index),
                    BinaryOp::Mod => {
                        writeln!(self.out, "    sdiv    x11, x9, x10")?;
                        writeln!(self.out, "    msub    x{}, x11, x10, x9", target_register_index)
                    }
                    BinaryOp::EQ => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    x{}, eq", target_register_index)
                    }
                    BinaryOp::NE => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    x{}, ne", target_register_index)
                    }
                    BinaryOp::GT => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    x{}, gt", target_register_index)
                    }
                    BinaryOp::LT => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    x{}, lt", target_register_index)
                    }
                    BinaryOp::GE => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    x{}, ge", target_register_index)
                    }
                    BinaryOp::LE => {
                        writeln!(self.out, "    cmp     x9, x10")?;
                        writeln!(self.out, "    cset    x{}, le", target_register_index)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn write_unary(&mut self, op: &UnaryOp, operand: &Expr, scope: &mut Vec<String>, current_func: &Function, target_register_index: u8) -> io::Result<()> {
        // NOTE: to not pollute temp registers (x9, ...), we can write operand right into target register
        self.write_expression(operand, scope, current_func, target_register_index)?;

        self.c(&format!("unary: {}{}", op, operand), true)?;
        match op {
            UnaryOp::Negate => {
                writeln!(self.out, "    neg     x{}, x{}", target_register_index, target_register_index)?;
            },
            UnaryOp::AddressOf => {
                if let Expr::Variable { name, .. } = operand {
                    let var_pos = get_variable_position(name, scope);
                    writeln!(self.out, "    add     x{}, x29, #{}", target_register_index, 16 + var_pos * 8)?;
                } else {
                    eprintln!("{}: error: cannot take address of non-variable expression", operand.clone().loc());
                    exit(1);
                }
            },
            UnaryOp::Dereference => {
                writeln!(self.out, "    ldr     x{}, [x{}]", target_register_index, target_register_index)?;
            },
        }

        Ok(())
    }

    fn write_funcall(&mut self, name: &str, args: &[Expr], scope: &mut Vec<String>, current_func: &Function, target_register_index: u8) -> io::Result<()> {
        if is_builtin_func(name) {
            self.builtins_used.insert(name.to_string());
        }

        self.c("save temp registers", true)?;
        writeln!(self.out, "    stp     x9, x10, [sp, -16]!")?;
        writeln!(self.out, "    stp     x11, x12, [sp, -16]!")?;

        if !args.is_empty() {
            let n = args.len();
            let stack_size = if n % 2 == 0 { n * 8 } else { n * 8 + 8 };

            self.c(&format!("load args for {}", name), true)?;
            writeln!(self.out, "    sub     sp, sp, {}", stack_size)?;

            for (i, arg) in args.iter().enumerate() {
                self.write_expression(arg, scope, current_func, 9)?;
                writeln!(self.out, "    str     x9, [sp, {}]", 8 * i)?;
            }

            for i in 0..n {
                let pos = n - i - 1;
                writeln!(self.out, "    ldr     x{}, [sp, {}]", pos, 8 * pos)?;
            }

            writeln!(self.out, "    add     sp, sp, {}", stack_size)?;
        }

        self.c(&format!("call: {}", name), true)?;
        writeln!(self.out, "    bl      _{}", name)?;

        self.c("restore temp registers", true)?;
        writeln!(self.out, "    ldp     x11, x12, [sp], 16")?;
        writeln!(self.out, "    ldp     x9, x10, [sp], 16")?;

        if target_register_index != 0 {
            writeln!(self.out, "    mov     x{}, x0", target_register_index)?;
        }

        Ok(())
    }

    fn write_variable(&mut self, name: &str, scope: &Vec<String>, target_register_index: u8) -> io::Result<()> {
        let pos = get_variable_position(name, scope);
        self.c(&format!("var: {}", name), true)?;
        writeln!(self.out, "    ldr     x{}, [x29, {}]", target_register_index, 16 + 8 * pos)
    }

    fn write_write(&mut self) -> io::Result<()> {
        self.c("std::write", false)?;
        writeln!(self.out, "_write:")?;
        writeln!(self.out, "    mov     x16, 4")?;
        writeln!(self.out, "    svc     0")?;
        writeln!(self.out, "    ret")?;
        writeln!(self.out)?;
        Ok(())
    }

    fn write_putc(&mut self) -> io::Result<()> {
        self.c("std::putc", false)?;
        writeln!(self.out, "_putc:")?;
        writeln!(self.out, "    stp     x29, x30, [sp, -16]!")?;
        writeln!(self.out, "    mov     x29, sp")?;
        writeln!(self.out, "    sub     sp, sp, 16")?;
        writeln!(self.out, "    strb    w0, [sp, 15]")?;
        writeln!(self.out, "    mov     x0, 1")?;
        writeln!(self.out, "    add     x1, sp, 15")?;
        writeln!(self.out, "    mov     x2, 1")?;
        writeln!(self.out, "    mov     x16, 4")?;
        writeln!(self.out, "    svc     0")?;
        writeln!(self.out, "    add     sp, sp, 16")?;
        writeln!(self.out, "    ldp     x29, x30, [sp], 16")?;
        writeln!(self.out, "    ret")?;
        writeln!(self.out)?;
        Ok(())
    }

    fn write_puti(&mut self) -> io::Result<()> {
        self.c("std::puti", false)?;
        writeln!(self.out, "_puti:")?;
        writeln!(self.out, "    .cfi_startproc")?;
        writeln!(self.out, "    sub	sp, sp, #80")?;
        writeln!(self.out, "    stp	x29, x30, [sp, #64]")?;
        writeln!(self.out, "    add	x29, sp, #64")?;
        writeln!(self.out, "    .cfi_def_cfa w29, 16")?;
        writeln!(self.out, "    .cfi_offset w30, -8")?;
        writeln!(self.out, "    .cfi_offset w29, -16")?;
        writeln!(self.out, "    mov	x8, #0")?;
        writeln!(self.out, "Lloh0:")?;
        writeln!(self.out, "    adrp	x9, ___stack_chk_guard@GOTPAGE")?;
        writeln!(self.out, "Lloh1:")?;
        writeln!(self.out, "    ldr	x9, [x9, ___stack_chk_guard@GOTPAGEOFF]")?;
        writeln!(self.out, "Lloh2:")?;
        writeln!(self.out, "    ldr	x9, [x9]")?;
        writeln!(self.out, "    stur	x9, [x29, #-8]")?;
        writeln!(self.out, "    cmp	x0, #0")?;
        writeln!(self.out, "    cneg	x10, x0, mi")?;
        writeln!(self.out, "    mov	w9, #10")?;
        writeln!(self.out, "    strb	w9, [sp, #55]")?;
        writeln!(self.out, "    mov	x11, #-3689348814741910324")?;
        writeln!(self.out, "    movk	x11, #52429")?;
        writeln!(self.out, "    add	x12, sp, #24")?;
        writeln!(self.out, "LBB0_1:")?;
        writeln!(self.out, "    umulh	x13, x10, x11")?;
        writeln!(self.out, "    lsr	x13, x13, #3")?;
        writeln!(self.out, "    msub	w14, w13, w9, w10")?;
        writeln!(self.out, "    orr	w14, w14, #0x30")?;
        writeln!(self.out, "    add	x15, x12, x8")?;
        writeln!(self.out, "    strb	w14, [x15, #30]")?;
        writeln!(self.out, "    sub	x8, x8, #1")?;
        writeln!(self.out, "    cmp	x10, #9")?;
        writeln!(self.out, "    mov	x10, x13")?;
        writeln!(self.out, "    b.hi	LBB0_1")?;
        writeln!(self.out, "    tbnz	x0, #63, LBB0_4")?;
        writeln!(self.out, "    mov	w9, #1")?;
        writeln!(self.out, "    b	LBB0_5")?;
        writeln!(self.out, "LBB0_4:")?;
        writeln!(self.out, "    add	x9, sp, #24")?;
        writeln!(self.out, "    add	x9, x9, x8")?;
        writeln!(self.out, "    mov	w10, #45")?;
        writeln!(self.out, "    strb	w10, [x9, #30]")?;
        writeln!(self.out, "    mov	w9, #2")?;
        writeln!(self.out, "LBB0_5:")?;
        writeln!(self.out, "    sub	x8, x9, x8")?;
        writeln!(self.out, "    add	x9, sp, #24")?;
        writeln!(self.out, "    sub	x9, x9, x8")?;
        writeln!(self.out, "    add	x9, x9, #32")?;
        writeln!(self.out, "    stp	x9, x8, [sp, #8]")?;
        writeln!(self.out, "    mov	w8, #1")?;
        writeln!(self.out, "    str	x8, [sp]")?;
        writeln!(self.out, "    mov	w0, #4")?;
        writeln!(self.out, "    bl	_syscall")?;
        writeln!(self.out, "    ldur	x8, [x29, #-8]")?;
        writeln!(self.out, "Lloh3:")?;
        writeln!(self.out, "    adrp	x9, ___stack_chk_guard@GOTPAGE")?;
        writeln!(self.out, "Lloh4:")?;
        writeln!(self.out, "    ldr	x9, [x9, ___stack_chk_guard@GOTPAGEOFF]")?;
        writeln!(self.out, "Lloh5:")?;
        writeln!(self.out, "    ldr	x9, [x9]")?;
        writeln!(self.out, "    cmp	x9, x8")?;
        writeln!(self.out, "    b.ne	LBB0_7")?;
        writeln!(self.out, "    ldp	x29, x30, [sp, #64]")?;
        writeln!(self.out, "    add	sp, sp, #80")?;
        writeln!(self.out, "    ret")?;
        writeln!(self.out, "LBB0_7:")?;
        writeln!(self.out, "    bl	___stack_chk_fail")?;
        writeln!(self.out, "    .loh AdrpLdrGotLdr	Lloh0, Lloh1, Lloh2")?;
        writeln!(self.out, "    .loh AdrpLdrGotLdr	Lloh3, Lloh4, Lloh5")?;
        writeln!(self.out, "    .cfi_endproc")?;
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
        let puti_used = self.builtins_used.contains("puti");
        if puti_used || self.strings.len() > 0 {
            self.c("data section", false)?;
        }

        if puti_used {
            writeln!(self.out, "minus_char:")?;
            writeln!(self.out, "    .asciz \"-\"")?;
        }

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

fn get_variable_position(name: &str, scope: &Vec<String>) -> usize {
    match scope.iter().position(|n| n == name) {
        Some(pos) => pos,
        None => unreachable!(),
    }
}

pub fn generate_object_from_assembly(verbose: bool, assembly_path: &str, object_path: &str) {
    execute_command(verbose, "as",
        &["-arch", "arm64", "-o", object_path, assembly_path])
        .unwrap_or_else(|_| {
            eprintln!("error: cannot create object file");
            exit(1);
        });
}

pub fn link_object_file(opts: &Opts, object_path: &str, output_path: &str) {
    let sdk_output = execute_command(false, "xcrun", &["--show-sdk-path"]).unwrap().stdout;
    let sdk_path = String::from_utf8_lossy(&sdk_output).trim().to_string();

    let mut args = vec![
        "-o", output_path,
        object_path,
        "-lSystem",
        "-syslibroot", &sdk_path,
        "-e", "_start",
        "-arch", "arm64"
    ];

    for flag in &opts.linker_flags {
        args.push(flag.as_str());
    }

    execute_command(!opts.silent, "ld", &args).unwrap_or_else(|_| {
        eprintln!("error: cannot link object file to executable");
        exit(1);
    });
}

fn execute_command(verbose: bool, program: &str, args: &[&str]) -> Result<Output, std::io::Error> {
    if verbose {
        println!("CMD: {} {}", program, args.join(" "));
    }

    let output = Command::new(program).args(args).output();

    match output {
        Ok(ref out) if !out.status.success() => {
            eprintln!("error: command failed with status: {}", out.status);
            if !out.stdout.is_empty() {
                eprintln!("{}", String::from_utf8_lossy(&out.stdout));
            }
            if !out.stderr.is_empty() {
                eprintln!("{}", String::from_utf8_lossy(&out.stderr));
            }
        },
        Err(ref err) => {
            eprintln!("error: failed to execute command: {}", err);
        },
        _ => {}
    }

    output
}

fn is_builtin_func(name: &str) -> bool {
    match name {
        "exit" | "write" | "putc" | "puti" => true,
        _ => false
    }
}
