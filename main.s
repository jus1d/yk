.global _main
.align 2

; function main
_main:
; prologue
    stp     x29, x30, [sp, -16]!
    mov     x29, sp
    ; load args for puti
    ; number 0
    mov     x9, 0
    ; number 69
    mov     x10, 69
    ; binop: 0 - 69
    sub     x0, x9, x10
    ; call puti
    bl      _puti
    ; number 0
    mov     x0, 0
    ; epilogue
    mov     sp, x29
    ldp     x29, x30, [sp], 16
    ret

; std::puti
_puti:
    stp     x29, x30, [sp, -48]!
    mov     x29, sp
    cmp     x0, 0
    b.ge    1f
    neg     x0, x0
    str     x0, [sp, 16]
    mov  x0, 1
    adrp x1, minus_char@PAGE
    add  x1, x1, minus_char@PAGEOFF
    mov  x2, 1
    mov  x16, 4
    svc  0
    ldr     x0, [sp, 16]
1:
    mov     x9, 0xCCCC
    movk    x9, 0xCCCC, lsl 16
    movk    x9, 0xCCCC, lsl 32
    movk    x9, 0xCCCD, lsl 48
    mov     w11, 10
    strb    w11, [x29, 47]
    add     x2, x29, 46
    mov     x5, x0
    cmp     x5, 0
    b.ne    2f
    mov     w0, 48
    strb    w0, [x2], -1
    b       2f
2:
    umulh   x11, x5, x9
    lsr     x11, x11, 3
    add     x12, x11, x11, lsl 2
    add     x12, x12, x12
    sub     x12, x5, x12
    add     w12, w12, 48
    strb    w12, [x2], -1
    mov     x5, x11
    cmp     x5, 0
    b.ne    2b
3:
    add     x1, x2, 1
    add     x3, x29, 48
    sub     x2, x3, x1
    mov     x0, 1
    mov     x16, 4
    svc     0
    ldp     x29, x30, [sp], 48
    ret

; data section
minus_char:
    .asciz "-"
