/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * Utilities for zeroing slop callable from Cmm
 *
 * N.B. If you are in C you should rather using the inlineable utilities
 * (e.g. overwritingClosure) defined in ClosureMacros.h.
 *
 * -------------------------------------------------------------------------- */

#include "Rts.h"

void stg_overwritingClosure (StgClosure *p)
{
    overwritingClosure(p);
}

void stg_overwritingMutableClosureOfs (StgClosure *p, uint32_t offset)
{
    overwritingMutableClosureOfs(p, offset);
}

void stg_overwritingClosureSize (StgClosure *p, uint32_t size /* in words */)
{
    overwritingClosureSize(p, size);
}
