/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

/* For GHC 4.08, we are relying on the fact that RtsFlags has
 * compatible layout with the current version, because we're
 * #including the current version of RtsFlags.h below.  4.08 didn't
 * ship with its own RtsFlags.h, unfortunately.   For later GHC
 * versions, we #include the correct RtsFlags.h.
 */
#if __GLASGOW_HASKELL__ < 502
#include "../includes/Rts.h"
#include "../includes/RtsFlags.h"
#else
#include "Rts.h"
#include "RtsFlags.h"
#endif

#include "HsFFI.h"

#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void
defaultsHook (void)
{
    RtsFlags.GcFlags.heapSizeSuggestion = 6*1024*1024 / BLOCK_SIZE;
    RtsFlags.GcFlags.maxStkSize         = 8*1024*1024 / sizeof(W_);
#if __GLASGOW_HASKELL__ >= 411
    /* GHC < 4.11 didn't have these */
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
    RtsFlags.GcFlags.statsFile = stderr;
#endif
}

void
StackOverflowHook (unsigned long stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current limit is %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
}

