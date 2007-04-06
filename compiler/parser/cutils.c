/*
These utility routines are used various
places in the GHC library.
*/

#include "Rts.h"
#include "RtsFlags.h"

#include "HsFFI.h"

#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*
Calling 'strlen' and 'memcpy' directly gives problems with GCC's inliner,
and causes gcc to require too many registers on x84
*/

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

void
enableTimingStats( void )	/* called from the driver */
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


