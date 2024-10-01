#include "Rts.h"

#if !defined(USE_MINIINTERPRETER)

// We directly return the same BaseReg as passed to StgRun. This is
// fine on wasm which doesn't have SMP.
//
// Reading BaseReg from R1 is quite tricky; we map R1 to a wasm
// global, and reading from wasm globals from C requires calling a
// getter function implemented in assembly, which is totally not worth
// the effort.
StgRegTable * StgRun(StgFunPtr f, StgRegTable *basereg)
{
    while (f) {
        f = (StgFunPtr) (f)();
    }
    return basereg;
}

StgFunPtr StgReturn(void)
{
    return 0;
}

#endif
