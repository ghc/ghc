/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

#include <string.h>

#if __GLASGOW_HASKELL__ >= 400
#include "../rts/Rts.h"
#else
#include "rtsdefs.h"
#endif

#if __GLASGOW_HASKELL__ >= 505
#include "../rts/Rts.h"
#include "../includes/RtsFlags.h"
#else
#include "Rts.h"
#include "RtsFlags.h"
#endif

#if __GLASGOW_HASKELL__ >= 502
#include "RtsFlags.h"
#endif

#if __GLASGOW_HASKELL__ >= 408
#include "HsFFI.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if __GLASGOW_HASKELL__ >= 504

char *ghc_rts_opts = "-H8m -K8m";

#else

void
defaultsHook (void)
{
#if __GLASGOW_HASKELL__ >= 408
    RtsFlags.GcFlags.heapSizeSuggestion = 6*1024*1024 / BLOCK_SIZE;
    RtsFlags.GcFlags.maxStkSize         = 8*1024*1024 / sizeof(W_);
#endif
#if __GLASGOW_HASKELL__ >= 411
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
    RtsFlags.GcFlags.statsFile = stderr;
#endif
}
#endif

void
enableTimingStats( void )	/* called from the driver */
{
#if __GLASGOW_HASKELL__ >= 505
    RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
#endif
    /* ignored when bootstrapping with an older GHC */
}

void
setHeapSize( HsInt size )
{
#if __GLASGOW_HASKELL__ >= 408
    RtsFlags.GcFlags.heapSizeSuggestion = size / BLOCK_SIZE;
    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
	RtsFlags.GcFlags.heapSizeSuggestion > RtsFlags.GcFlags.maxHeapSize) {
	RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
    }
#endif
}

void
PreTraceHook (long fd)
{
    const char msg[]="\n";
    write(fd,msg,sizeof(msg)-1);
}

void
PostTraceHook (long fd)
{
#if 0
    const char msg[]="\n";
    write(fd,msg,sizeof(msg)-1);
#endif
}

void
OutOfHeapHook (unsigned long request_size, unsigned long heap_size)
  /* both in bytes */
{
    fprintf(stderr, "GHC's heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse the `-H<size>' option to increase the total heap size.\n",
	request_size,
	heap_size);
}

void
StackOverflowHook (unsigned long stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current size %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
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
