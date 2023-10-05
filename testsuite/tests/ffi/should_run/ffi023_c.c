#include "ffi023_stub.h"
#include "HsFFI.h"
#include "Rts.h"

HsInt out (HsInt x)
{
    hs_perform_gc();
    return incall(x);
}
