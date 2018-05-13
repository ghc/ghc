/*
These utility routines are used various
places in the GHC library.
*/

#include "Rts.h"

#include "HsFFI.h"

#include <string.h>

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

void
enableTimingStats( void )       /* called from the driver */
{
    RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
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
