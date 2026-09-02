/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Idle GC: tracking of when to perform a GC during idle time.
 *
 * Interacts closely with the scheduler and timer tick.
 *
 * ---------------------------------------------------------------------------*/

#include "BeginPrivate.h"

/* See Note [GC During Idle Time] */

#if defined(HAVE_PREEMPTION)
#define THREADED_IDLEGC
#else
/* See Note [Idle GC without preemption] */
#endif


/* Are we in a state where we are want an idle GC to occur?
 * Used in Capability globalWorkToDo() and Schedule scheduleDetectDeadlock.
 */
bool isIdleGcPending(void);

/* Called from schedule() */
void notifyIdleGcActive(void);

/* Called from schedule(), specifically scheduleDoGC() */
void notifyIdleGcDone(bool force_major);

#if defined(THREADED_IDLEGC)

/* Called from handle_tick() */
void handleIdleGcTick(void);

#else // !defined(THREADED_IDLEGC)

/* Called from I/O managers before waiting */
Time getNextIdleGcDelayTime(void);

/* Called from I/O managers after waiting */
void notifyIdleGcIdle(bool deadlocked);

#endif

#include "EndPrivate.h"
