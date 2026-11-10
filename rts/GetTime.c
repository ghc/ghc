/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Interface to the RTS time
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "GetTime.h"


/* Exported alias for the internal getMonotonicTime().
 * Used in GHC.Internal.Clock
 */
StgWord64 getMonotonicNSec(void)
{
    return TimeToNS(getMonotonicTime());
}

/* Exported alias for the internal getMonotonicTime().
 * Used in ghc-internal/cbits/inputReady.c
 */
Time getProcessElapsedTime(void)
{
    return getMonotonicTime();
}
