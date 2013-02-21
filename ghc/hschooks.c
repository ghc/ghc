/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

#include "../rts/PosixSource.h"
#include "Rts.h"

#include "HsFFI.h"

#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void
defaultsHook (void)
{
#if __GLASGOW_HASKELL__ >= 707
    // This helps particularly with large compiles, but didn't work
    // very well with earlier GHCs because it caused large amounts of
    // fragmentation.  See rts/sm/BlockAlloc.c:allocLargeChunk().
    RtsFlags.GcFlags.heapSizeSuggestionAuto = rtsTrue;
#else
    RtsFlags.GcFlags.heapSizeSuggestion = 6*1024*1024 / BLOCK_SIZE;
#endif

    RtsFlags.GcFlags.maxStkSize         = 512*1024*1024 / sizeof(W_);
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;

    // See #3408: the default idle GC time of 0.3s is too short on
    // Windows where we receive console events once per second or so.
#if __GLASGOW_HASKELL__ >= 703
    RtsFlags.GcFlags.idleGCDelayTime = SecondsToTime(5);
#else
    RtsFlags.GcFlags.idleGCDelayTime = 5*1000;
#endif
}

void
StackOverflowHook (StgWord stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current limit is %zu bytes.\nUse the `-K<size>' option to increase it.\n", (size_t)stack_size);
}

