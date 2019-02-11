        .text
        .globl _getAnswer

_getAnswer:
        mov     L2(%rip), %rdx
        movq    (%rdx), %rax
        ret

        .data                   # assembler generates an unsigned reloc for L2
L1:     .quad 42
L2:     .quad L1
