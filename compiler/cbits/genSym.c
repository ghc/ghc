#include <Rts.h>
#include <assert.h>
#include "Unique.h"

HsInt ghc_unique_counter = 0;
HsInt ghc_unique_inc     = 1;

#define UNIQUE_BITS (sizeof (HsInt) * 8 - UNIQUE_TAG_BITS)
#define UNIQUE_MASK ((1ULL << UNIQUE_BITS) - 1)

HsInt genSym(void) {
    HsInt u = atomic_inc((StgWord *)&ghc_unique_counter, ghc_unique_inc) & UNIQUE_MASK;
#if DEBUG
    // Uh oh! We will overflow next time a unique is requested.
    assert(u != UNIQUE_MASK);
#endif
    return u;
}
