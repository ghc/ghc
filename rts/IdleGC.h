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


void initIdleGc(void);

/* Are we in a state where we are want an idle GC to occur?
 * Used in Capability globalWorkToDo() and Schedule scheduleDetectDeadlock.
 */
bool isIdleGcPending(void);

/* Used by the scheduler on a capability to notify the idle GC tracking that
 * the capability is active, i.e. about to run a Haskell thread.
 *
 * It should also inform the idle GC tracking if this is the first thread run
 * after the capability wakes up from idle. This is used as an optimisation to
 * detect transactions to active. False positives are ok, but false negatives
 * are not. That is, it is safe (but slow) to always use cap_just_awoke = true.
 */
void notifyIdleGcCapabilityIsActive(Capability *cap, bool cap_just_awoke);

/* Used by the scheduler on a capability to notify the idle GC tracking that
 * the capability is idle, i.e. about to yield or block until there is
 * something to do.
 */
void notifyIdleGcCapabilityIsIdle(Capability *cap);

/* Called from schedule(), specifically scheduleDoGC(), to notify the idle GC
 * tracking that a major GC has been completed in as requested. */
void notifyIdleGcCompleted(void);

/* Called from handle_tick() to perform regular monitoring of whether an idle
 * GC is needed. */
void handleIdleGcTick(void);

#include "EndPrivate.h"
