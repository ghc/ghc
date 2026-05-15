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

/* Are we in a state where we are want an idle GC to occur?
 * Used in Capability globalWorkToDo() and Schedule scheduleDetectDeadlock.
 */
bool isIdleGcPending(void);

/* Called from schedule() */
void notifyIdleGcActive(void);

/* Called from schedule(), specifically scheduleDoGC() */
void notifyIdleGcDone(bool force_major);

/* Called from handle_tick() */
void handleIdleGcTick(void);

#include "EndPrivate.h"
