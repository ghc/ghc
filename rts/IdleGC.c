/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Idle GC: performing a GC during idle time.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "IdleGC.h"
#include "Timer.h"
#include "Schedule.h"

/* Flag that tracks whether we have done any execution in this time slice.
 * LOCK: currently none, perhaps we should lock (but needs to be
 * updated in the fast path of the scheduler).
 *
 * NB. must be StgWord, we use atommic operations on it.
 */
StgWord recent_activity = ACTIVITY_YES;

