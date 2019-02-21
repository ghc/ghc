        .text
        .globl _getAnswer

_getAnswer:
        mov     L2(%rip), %rdx
        movq    (%rdx), %rax
        ret

        .data
L1:     .quad 42
L2:     .quad L1
