#include "../includes/ghcconfig.h"

/* ******************************** PowerPC ******************************** */

#if defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH)
#if !(defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS))
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
#endif

    /* Some info about stack frame layout */
#define LINK_SLOT           (2*WS)
#define LINKAGE_AREA_SIZE   (6*WS)

    /* The following defines mirror struct AdjustorStub
       from Adjustor.c. Make sure to keep these in sync.
    */
#if defined(powerpc_HOST_ARCH) && defined(darwin_HOST_OS)
#define HEADER_WORDS   6
#elif defined(powerpc64_HOST_ARCH) && defined(darwin_HOST_OS)
#else
#define HEADER_WORDS   3
#endif

#define HPTR_OFF        ((HEADER_WORDS    )*WS)
#define WPTR_OFF        ((HEADER_WORDS + 1)*WS)
#define FRAMESIZE_OFF   ((HEADER_WORDS + 2)*WS)
#define EXTRA_WORDS_OFF ((HEADER_WORDS + 3)*WS)

    /* Darwin insists on register names, everyone else prefers
       to use numbers. */
#if !defined(darwin_HOST_OS)
#define r0 0
#define r1 1
#define r2 2
#define r3 3
#define r4 4
#define r5 5
#define r6 6
#define r7 7
#define r8 8
#define r9 9
#define r10 10
#define r11 11
#define r12 12

#define r30 30
#define r31 31
#endif


.text
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
#endif
    /* On entry, r2 will point to the AdjustorStub data structure. */

        /* save the link */
    mflr    r0
    STORE   r0, LINK_SLOT(r1)
    
        /* set up stack frame */
    LOAD    r12, FRAMESIZE_OFF(r2)
#ifdef powerpc64_HOST_ARCH
    stdux   r1, r1, r12
#else   
    stwux   r1, r1, r12
#endif

        /* Save some regs so that we can use them.
           Note that we use the "Red Zone" below the stack pointer.
        */
    STORE   r31, -WS(r1)
    STORE   r30, -2*WS(r1)

    mr      r31, r1
    subf    r30, r12, r31

    LOAD    r12, EXTRA_WORDS_OFF(r2)
    mtctr   r12
    b       2f
1:
    LOAD    r0, LINKAGE_AREA_SIZE +  8*WS(r30)
    STORE   r0, LINKAGE_AREA_SIZE + 10*WS(r31)
    addi    r30, r30, WS
    addi    r31, r31, WS
2:
    bdnz    1b

        /* Restore r30 and r31 now.
        */
    LOAD    r31, -WS(r1)
    LOAD    r30, -2*WS(r1)

    STORE   r10, LINKAGE_AREA_SIZE + 9*WS(r1)
    STORE   r9,  LINKAGE_AREA_SIZE + 8*WS(r1)
    mr      r10, r8
    mr      r9, r7
    mr      r8, r6
    mr      r7, r5
    mr      r6, r4
    mr      r5, r3

    LOAD    r3, HPTR_OFF(r2)

    LOAD    r12, WPTR_OFF(r2)
#if defined(darwin_HOST_OS)
    mtctr   r12
#else
    LOAD    r0, 0(r12)
        /* The function we're calling will never be a nested function,
           so we don't load r11. 
        */
    mtctr   r0
    LOAD    r2, WS(r12)
#endif
    bctrl

    LOAD    r1, 0(r1)
    LOAD    r0, LINK_SLOT(r1)
    mtlr    r0
    blr
#endif

/* ********************************* i386 ********************************** */

#elif defined(i386_TARGET_ARCH) && defined(darwin_TARGET_OS)

#define WS              4
#define RETVAL_OFF      5
#define HEADER_BYTES    8

#define HPTR_OFF        HEADER_BYTES
#define WPTR_OFF        (HEADER_BYTES + 1*WS)
#define FRAMESIZE_OFF   (HEADER_BYTES + 2*WS)
#define ARGWORDS_OFF    (HEADER_BYTES + 3*WS)

    .globl _adjustorCode
_adjustorCode:
    popl    %eax
    subl    $RETVAL_OFF, %eax
    
    pushl   %ebp
    movl    %esp, %ebp
    
    subl    FRAMESIZE_OFF(%eax), %esp

    pushl   %esi
    pushl   %edi
        
    leal    8(%ebp), %esi
    leal    12(%esp), %edi
    movl    ARGWORDS_OFF(%eax), %ecx
    rep
    movsl
    
    popl    %edi
    popl    %esi
    
    pushl   HPTR_OFF(%eax)
    call    *WPTR_OFF(%eax)
    
    leave
    ret
#endif

