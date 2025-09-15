callMemcpy:
        subq
        movq  ; Move arguments into place
        movq
        movl
        xorl
        call memcpy
        addq
        jmp
