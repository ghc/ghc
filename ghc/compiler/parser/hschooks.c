/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

/* For GHC 4.08, we are relying on the fact that RtsFlags has
 * compatibile layout with the current version, because we're
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
enableTimingStats( void )	/* called from the driver */
{
#if __GLASGOW_HASKELL__ >= 411
    RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
#endif
    /* ignored when bootstrapping with an older GHC */
}

void
setHeapSize( HsInt size )
{
    RtsFlags.GcFlags.heapSizeSuggestion = size / BLOCK_SIZE;
    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
	RtsFlags.GcFlags.heapSizeSuggestion > RtsFlags.GcFlags.maxHeapSize) {
	RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
    }
}

void
OutOfHeapHook (unsigned long request_size/* always zero these days */,
	       unsigned long heap_size)
    /* both in bytes */
{
    fprintf(stderr, "GHC's heap exhausted: current limit is %lu bytes;\nUse the `-M<size>' option to increase the total heap size.\n",
	heap_size);
}

void
StackOverflowHook (unsigned long stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current limit is %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
}

HsInt
ghc_strlen( HsAddr a )
{
    return (strlen((char *)a));
}

HsInt
ghc_memcmp( HsAddr a1, HsAddr a2, HsInt len )
{
    return (memcmp((char *)a1, a2, len));
}

HsInt
ghc_memcmp_off( HsAddr a1, HsInt i, HsAddr a2, HsInt len )
{
    return (memcmp((char *)a1 + i, a2, len));
}
