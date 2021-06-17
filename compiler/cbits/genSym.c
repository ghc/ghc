#include <Rts.h>
#include <assert.h>
#include "Unique.h"
#include "ghcversion.h"

// These global variables have been moved into the RTS.  It allows them to be
// shared with plugins even if two different instances of the GHC library are
// loaded at the same time (#19940)
#if !MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
HsInt ghc_unique_counter = 0;
HsInt ghc_unique_inc     = 1;
#endif

#define UNIQUE_BITS (sizeof (HsInt) * 8 - UNIQUE_TAG_BITS)
#define UNIQUE_MASK ((1ULL << UNIQUE_BITS) - 1)

HsInt genSym(void) {
    HsInt u = atomic_inc((StgWord *)&ghc_unique_counter, ghc_unique_inc) & UNIQUE_MASK;
    // Uh oh! We will overflow next time a unique is requested.
    ASSERT(u != UNIQUE_MASK);
    return u;
}
