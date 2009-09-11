/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

#include "Rts.h"
#if __GLASGOW_HASKELL__ <611
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
    RtsFlags.GcFlags.maxStkSize         = 512*1024*1024 / sizeof(W_);
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
    RtsFlags.GcFlags.statsFile = stderr;

    // See #3408: the default idle GC time of 0.3s is too short on
    // Windows where we receive console events once per second or so.
    RtsFlags.GcFlags.idleGCDelayTime = 5*1000;
}

void
StackOverflowHook (unsigned long stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current limit is %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
}

