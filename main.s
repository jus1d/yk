.global _main
.align 2
_main:
    ; println("helloooooo")
    mov     x0, 1
    adrp    x1, strings.0@PAGE
    add     x1, x1, strings.0@PAGEOFF
    mov     x2, 11
    mov     x16, 4
    svc     128
    ; exit(69)
    mov     x0, 69
    mov     x16, 1
    svc     128
strings.0:
    .ascii "helloooooo\n"
