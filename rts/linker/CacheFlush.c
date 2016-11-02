/* Platform-dependent cache flushing logic */

#include "Rts.h"
#include "linker/CacheFlush.h"

#if defined(arm_HOST_ARCH)

void
ocFlushInstructionCache( ObjectCode *oc )
{
    int i;
    // Object code
    for (i=0; i < oc->n_sections; i++) {
        Section *s = &oc->sections[i];
        // This is a bit too broad but we don't have any way to determine what
        // is certainly code
        if (s->kind == SECTIONKIND_CODE_OR_RODATA)
            __clear_cache(s->start, (void*) ((uintptr_t) s->start + s->size));
    }

    // Jump islands
    // Note the (+1) to ensure that the last symbol extra is covered by the
    // flush.
    __clear_cache(oc->symbol_extras, &oc->symbol_extras[oc->n_symbol_extras+1]);
}

#elif defined(powerpc_HOST_ARCH)
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
