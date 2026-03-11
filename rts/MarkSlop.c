/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * Utilities for marking slop callable from Cmm
 *
 * N.B. If you are in C you should rather using the inlineable utilities
 * (e.g. overwritingClosure) defined in ClosureMacros.h.
 *
 * -------------------------------------------------------------------------- */

#include "Rts.h"

void stg_writeSlopMarker (StgWord *slop, StgWord n)
{
    writeSlopMarker(slop, n);
}

void stg_overwritingClosure (StgClosure *p)
{
    overwritingClosure(p);
}

void stg_overwritingClosureSize (StgClosure *p, uint32_t size /* in words */)
{
    overwritingClosureSize(p, size);
}
