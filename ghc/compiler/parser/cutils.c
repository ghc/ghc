/*
These utility routines are used various
places in the GHC library.
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


