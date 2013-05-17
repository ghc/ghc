
#include "Rts.h"

static HsInt GenSymCounter = 0;

HsInt genSym(void) {
    return GenSymCounter++;
}

