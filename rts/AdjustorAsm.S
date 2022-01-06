#include "include/ghcconfig.h"

/* ******************************** PowerPC ******************************** */

#if defined(powerpc_HOST_ARCH) && defined(aix_HOST_OS) || defined(powerpc64_HOST_ARCH) && defined(__ELF__) && (!defined(_CALL_ELF) || _CALL_ELF == 1)
    /* The following code applies, with some differences,
       to all powerpc platforms except for powerpc32-linux,
       whose calling convention is annoyingly complex.
    */


    /* The code is "almost" the same for
       32-bit and for 64-bit
    */
#if defined(powerpc64_HOST_ARCH)
#define WS          8
#define LOAD        ld
#define STORE       std
#else
#define WS          4
#define LOAD        lwz
#define STORE       stw
#endif /* defined(powerpc64_HOST_ARCH) */

    /* Some info about stack frame layout */
#define LINK_SLOT           (2*WS)
#define LINKAGE_AREA_SIZE   (6*WS)

    /* The following defines mirror struct AdjustorStub
       from Adjustor.c. Make sure to keep these in sync.
    */
#define HEADER_WORDS   3

#define HPTR_OFF        ((HEADER_WORDS    )*WS)
#define WPTR_OFF        ((HEADER_WORDS + 1)*WS)
#define FRAMESIZE_OFF   ((HEADER_WORDS + 2)*WS)
#define EXTRA_WORDS_OFF ((HEADER_WORDS + 3)*WS)

#if defined(aix_HOST_OS)
/* IBM's assembler needs a different pseudo-op to declare a .text section */
.csect .text[PR]
#else
.text
#endif /* defined(aix_HOST_OS) */

#if LEADING_UNDERSCORE
    .globl _adjustorCode
_adjustorCode:
#else
    .globl adjustorCode
        /* Note that we don't build a function descriptor
           for AIX-derived ABIs here. This will happen at runtime
           in createAdjustor().
        */
adjustorCode:
#endif /* LEADING_UNDERSCORE */
    /* On entry, r2 will point to the AdjustorStub data structure. */

        /* save the link */
    mflr    0
    STORE   0, LINK_SLOT(1)

        /* set up stack frame */
    LOAD    12, FRAMESIZE_OFF(2)
#if defined(powerpc64_HOST_ARCH)
    stdux   1, 1, 12
#else
    stwux   1, 1, 12
#endif /* defined(powerpc64_HOST_ARCH) */

        /* Save some regs so that we can use them.
           Note that we use the "Red Zone" below the stack pointer.
        */
    STORE   31, -WS(1)
    STORE   30, -2*WS(1)

    mr      31, 1
    subf    30, 12, 31

    LOAD    12, EXTRA_WORDS_OFF(2)
    mtctr   12
    b       L2
L1:
    LOAD    0, LINKAGE_AREA_SIZE +  8*WS(30)
    STORE   0, LINKAGE_AREA_SIZE + 10*WS(31)
    addi    30, 30, WS
    addi    31, 31, WS
L2:
    bdnz    L1

        /* Restore r30 and r31 now.
        */
    LOAD    31, -WS(1)
    LOAD    30, -2*WS(1)

    STORE   10, LINKAGE_AREA_SIZE + 9*WS(1)
    STORE   9,  LINKAGE_AREA_SIZE + 8*WS(1)
    mr      10, 8
    mr      9, 7
    mr      8, 6
    mr      7, 5
    mr      6, 4
    mr      5, 3

    LOAD    3, HPTR_OFF(2)

    LOAD    12, WPTR_OFF(2)
    LOAD    0, 0(12)
        /* The function we're calling will never be a nested function,
           so we don't load r11.
        */
    mtctr   0
    LOAD    2, WS(12)
    bctrl

    LOAD    1, 0(1)
    LOAD    0, LINK_SLOT(1)
    mtlr    0
    blr
#endif

/* mark stack as nonexecutable */
#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif
