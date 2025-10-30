/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1995-2021
 *
 * Timeout support used by some I/O manager implementations.
 *
 * Prototypes for functions in Timeout.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

bool syncDelayTimeout(CapIOManager *iomgr, StgTSO *tso, HsInt us_delay);

void syncDelayCancelTimeout(CapIOManager *iomgr, StgTSO *tso);

/* Process the completion of any timeouts that have expired: this means
 * notifying whatever is waiting on the timeout, a thread, an MVar or TVar.
 * This is not guaranteed to unblock any threads, even if timers do actually
 * expire (since there may be no thread waiting on the MVar/TVar).
 *
 * No result is returned: callers can check if there are now any runnable
 * threads by consulting the scheduler's run queue.
 */
void processTimeoutCompletions(CapIOManager *iomgr, Time now);

/* Utility to compute the timeout wait time (in milliseconds) between now and
 * the next timer expiry (if any), or no waiting (if !wait).
 *
 * This is intended to be used with poll() which expect a
 * timeout in milliseconds, with special values of -1 for indefinite wait,
 * and 0 for no waiting.
 */
#if !(defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1)
int timeoutInMilliseconds(CapIOManager *iomgr, bool wait, Time now);
#endif

/* As above, but a timeout in nanoseconds. This is intended to be used with
 * ppoll() which expect struct timespec *, with special
 * values of NULL for indefinite wait, and 0 for no waiting.
 */
#if (defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1)
struct timespec *timeoutInNanoseconds(CapIOManager *iomgr, bool wait,
                                      Time now, struct timespec *tv);
#endif

#include "EndPrivate.h"

