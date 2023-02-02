#include "ffi023_stub.h"
#include "HsFFI.h"
#include "Rts.h"

HsInt out (HsInt x)
{
    performBlockingMajorGC();
    rts_clearMemory();
    return incall(x);
}
