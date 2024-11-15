#include "include/ghcconfig.h"

#define WS                 4

#define RETVAL_OFF         5

// Mathces layout of struct CCallAdjustor in Nativei386.c
#define HPTR_OFF           (0*WS)
#define WPTR_OFF           (1*WS)
#define FRAME_SIZE_OFF     (2*WS)
#define ARGUMENT_WORDS_OFF (3*WS)

#if defined(LEADING_UNDERSCORE)
#define CSYM(x) _ ## x
#else
#define CSYM(x) x
#endif

#define DECLARE_CSYM(x) \
    .globl CSYM(x) ; \
    CSYM(x):

DECLARE_CSYM(ccall_adjustor)
    // At entry %eax contains a pointer to the CCallContext

    // Record a frame pointer. Paired with the `leave` below.
    pushl   %ebp
    movl    %esp, %ebp

    subl    FRAME_SIZE_OFF(%eax), %esp

    // Save %esi and %edi as we need to clobber them to perform the shuffle
    pushl   %esi
    pushl   %edi

    // Shuffle the stack down...
    leal    8(%ebp), %esi
    leal    12(%esp), %edi
    movl    ARGUMENT_WORDS_OFF(%eax), %ecx
    rep
    movsl

    // Restore %edi and %esi
    popl    %edi
    popl    %esi

    // Perform the call
    pushl   HPTR_OFF(%eax)
    call    *WPTR_OFF(%eax)

    leave
    ret

/* mark stack as nonexecutable */
#if defined(HAVE_GNU_NONEXEC_STACK)
.section .note.GNU-stack,"",@progbits
#endif
