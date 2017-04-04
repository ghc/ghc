#include <assert.h>
#include "Rts.h"
#include "Unique.h"

static HsInt GenSymCounter = 0;
static HsInt GenSymInc = 1;

#define UNIQUE_BITS (sizeof (HsInt) * 8 - UNIQUE_TAG_BITS)
#define UNIQUE_MASK ((1ULL << UNIQUE_BITS) - 1)

STATIC_INLINE void checkUniqueRange(HsInt u STG_UNUSED) {
#if DEBUG
    // Uh oh! We will overflow next time a unique is requested.
    assert(u != UNIQUE_MASK);
#endif
}

HsInt genSym(void) {
#if defined(THREADED_RTS)
    if (n_capabilities == 1) {
        GenSymCounter = (GenSymCounter + GenSymInc) & UNIQUE_MASK;
        checkUniqueRange(GenSymCounter);
        return GenSymCounter;
    } else {
        HsInt n = atomic_inc((StgWord *)&GenSymCounter, GenSymInc)
          & UNIQUE_MASK;
        checkUniqueRange(n);
        return n;
    }
#else
    GenSymCounter = (GenSymCounter + GenSymInc) & UNIQUE_MASK;
    checkUniqueRange(GenSymCounter);
    return GenSymCounter;
#endif
}

void initGenSym(HsInt NewGenSymCounter, HsInt NewGenSymInc) {
  GenSymCounter = NewGenSymCounter;
  GenSymInc = NewGenSymInc;
}
