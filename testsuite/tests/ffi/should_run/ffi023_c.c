#include "ffi023_stub.h"
#include "HsFFI.h"
#include "Rts.h"

HsInt out (HsInt x)
{
    performMajorGC();
    rts_clearMemory();
    return incall(x);
}
