#include "Rts.h"

#ifdef darwin_HOST_OS
#define STG_GLOBAL ".globl "
#else
#define STG_GLOBAL ".global "
#endif

#ifdef LEADING_UNDERSCORE
#define GETESP "_getesp"
#else
#define GETESP "getesp"
#endif

void __dummy__(void)
{
        __asm__ volatile (
        STG_GLOBAL GETESP "\n"
        GETESP ":\n\t"

#if defined(i386_HOST_ARCH)
        "movl %%esp, %%eax\n\t"
#elif defined(x86_64_HOST_ARCH)
        "movq %%rsp, %%rax\n\t"
#else
#error splign.c: not implemented for this architecture
#endif
        "ret"
        : : );
}
