#include "ffi023_stub.h"
#include "HsFFI.h"
#include "Rts.h"

HsInt out (HsInt x)
{
    performMajorGC();
    return incall(x);
}
