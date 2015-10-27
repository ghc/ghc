
#include "Rts.h"

static HsInt GenSymCounter = 0;
static HsInt GenSymInc = 1;

HsInt genSym(void) {
#if defined(THREADED_RTS)
    if (n_capabilities == 1) {
        return GenSymCounter = (GenSymCounter + GenSymInc) & 0xFFFFFF;
    } else {
        return atomic_inc((StgWord *)&GenSymCounter, GenSymInc) & 0xFFFFFF;
    }
#else
    return GenSymCounter = (GenSymCounter + GenSymInc) & 0xFFFFFF;
#endif
}

void initGenSym(HsInt NewGenSymCounter, HsInt NewGenSymInc) {
  GenSymCounter = NewGenSymCounter;
  GenSymInc = NewGenSymInc;
}
