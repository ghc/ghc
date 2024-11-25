#include <Rts.h>
#include <assert.h>
#include "Unique.h"
#include <ghcversion.h>

// These global variables have been moved into the RTS.  It allows them to be
// shared with plugins even if two different instances of the GHC library are
// loaded at the same time (#19940)
//
// The CPP is thus about the RTS version GHC is linked against, and not the
// version of the GHC being built.
#if !MIN_VERSION_GLASGOW_HASKELL(9,8,4,0)
HsWord64 ghc_unique_counter64 = 0;
#endif
#if !MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
HsInt ghc_unique_inc     = 1;
#endif

// This function has been added to the RTS. Here we pessimistically assume
// that a threaded RTS is used. This function is only used for bootstrapping.
#if !MIN_VERSION_GLASGOW_HASKELL(9,7,0,0)
EXTERN_INLINE StgWord64
atomic_inc64(StgWord64 volatile* p, StgWord64 incr)
{
#if defined(HAVE_C11_ATOMICS)
    return __atomic_add_fetch(p, incr, __ATOMIC_SEQ_CST);
#else
    return __sync_add_and_fetch(p, incr);
#endif
}
#endif

#define UNIQUE_BITS (sizeof (HsWord64) * 8 - UNIQUE_TAG_BITS)
#define UNIQUE_MASK ((1ULL << UNIQUE_BITS) - 1)

HsWord64 genSym(void) {
    HsWord64 u = atomic_inc64((StgWord64 *)&ghc_unique_counter64, ghc_unique_inc) & UNIQUE_MASK;
    // Uh oh! We will overflow next time a unique is requested.
    ASSERT(u != UNIQUE_MASK);
    return u;
}
