/* Platform-dependent cache flushing logic */

#include "Rts.h"
#include "linker/CacheFlush.h"

#if defined(powerpc_HOST_ARCH)
/*
   ocFlushInstructionCache

   Flush the data & instruction caches.
   Because the PPC has split data/instruction caches, we have to
   do that whenever we modify code at runtime.
 */

static void
ocFlushInstructionCacheFrom(void* begin, size_t length)
{
    size_t         n = (length + 3) / 4;
    unsigned long* p = begin;

    while (n--)
    {
        __asm__ volatile ( "dcbf 0,%0\n\t"
                           "sync\n\t"
                           "icbi 0,%0"
                           :
                           : "r" (p)
                         );
        p++;
    }
    __asm__ volatile ( "sync\n\t"
                       "isync"
                     );
}

void
ocFlushInstructionCache( ObjectCode *oc )
{
    /* The main object code */
    ocFlushInstructionCacheFrom(oc->image + oc->misalignment, oc->fileSize);

    /* Jump Islands */
    ocFlushInstructionCacheFrom(oc->symbol_extras, sizeof(SymbolExtra) * oc->n_symbol_extras);
}

#else

void ocFlushInstructionCache( ObjectCode *oc STG_UNUSED ) {}

#endif /* powerpc_HOST_ARCH */
