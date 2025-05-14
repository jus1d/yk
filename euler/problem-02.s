.global _start
.align 2

_start:
    bl      _main
    mov     x16, 1
    svc     0

_main:
    stp     x29, x30, [sp, -48]!
    mov     x29, sp
    mov     x8, 1
    str     x8, [x29, 16]
    mov     x8, 2
    str     x8, [x29, 24]
    mov     x8, 4000000
    str     x8, [x29, 32]
    mov     x8, 0
    str     x8, [x29, 40]
L0:
    ldr     x9, [x29, 16]
    ldr     x10, [x29, 32]
    cmp     x9, x10
    cset    x12, lt
    cmp     x12, 0
    b.eq    L1
L2:
    ldr     x9, [x29, 16]
    mov     x10, 2
    sdiv    x11, x9, x10
    msub    x9, x11, x10, x9
    mov     x10, 0
    cmp     x9, x10
    cset    x11, eq
    cmp     x11, 0
    b.eq    L3
    ldr     x9, [x29, 40]
    ldr     x10, [x29, 16]
    add     x8, x9, x10
    str     x8, [x29, 40]
    b       L4
L3:
L4:
    ldr     x9, [x29, 16]
    ldr     x10, [x29, 24]
    add     x8, x9, x10
    str     x8, [x29, 48]
    ldr     x8, [x29, 24]
    str     x8, [x29, 16]
    ldr     x8, [x29, 48]
    str     x8, [x29, 24]
    b       L0
L1:
    stp     x9, x10, [sp, -16]!
    stp     x11, x12, [sp, -16]!
    sub     sp, sp, 16
    ldr     x9, [x29, 40]
    str     x9, [sp, 0]
    ldr     x0, [sp, 0]
    add     sp, sp, 16
    bl      _puti
    ldp     x11, x12, [sp], 16
    ldp     x9, x10, [sp], 16
    mov     x0, 0
    ldp     x29, x30, [sp], 48
    ret

_puti:
    .cfi_startproc
    sub	sp, sp, #80
    stp	x29, x30, [sp, #64]
    add	x29, sp, #64
    .cfi_def_cfa w29, 16
    .cfi_offset w30, -8
    .cfi_offset w29, -16
    mov	x8, #0
Lloh0:
    adrp	x9, ___stack_chk_guard@GOTPAGE
Lloh1:
    ldr	x9, [x9, ___stack_chk_guard@GOTPAGEOFF]
Lloh2:
    ldr	x9, [x9]
    stur	x9, [x29, #-8]
    cmp	x0, #0
    cneg	x10, x0, mi
    mov	w9, #10
    strb	w9, [sp, #55]
    mov	x11, #-3689348814741910324
    movk	x11, #52429
    add	x12, sp, #24
LBB0_1:
    umulh	x13, x10, x11
    lsr	x13, x13, #3
    msub	w14, w13, w9, w10
    orr	w14, w14, #0x30
    add	x15, x12, x8
    strb	w14, [x15, #30]
    sub	x8, x8, #1
    cmp	x10, #9
    mov	x10, x13
    b.hi	LBB0_1
    tbnz	x0, #63, LBB0_4
    mov	w9, #1
    b	LBB0_5
LBB0_4:
    add	x9, sp, #24
    add	x9, x9, x8
    mov	w10, #45
    strb	w10, [x9, #30]
    mov	w9, #2
LBB0_5:
    sub	x8, x9, x8
    add	x9, sp, #24
    sub	x9, x9, x8
    add	x9, x9, #32
    stp	x9, x8, [sp, #8]
    mov	w8, #1
    str	x8, [sp]
    mov	w0, #4
    bl	_syscall
    ldur	x8, [x29, #-8]
Lloh3:
    adrp	x9, ___stack_chk_guard@GOTPAGE
Lloh4:
    ldr	x9, [x9, ___stack_chk_guard@GOTPAGEOFF]
Lloh5:
    ldr	x9, [x9]
    cmp	x9, x8
    b.ne	LBB0_7
    ldp	x29, x30, [sp, #64]
    add	sp, sp, #80
    ret
LBB0_7:
    bl	___stack_chk_fail
    .loh AdrpLdrGotLdr	Lloh0, Lloh1, Lloh2
    .loh AdrpLdrGotLdr	Lloh3, Lloh4, Lloh5
    .cfi_endproc

minus_char:
    .asciz "-"
