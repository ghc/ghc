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

#if defined(IOMGR_ENABLED_POLL) \
 || defined(IOMGR_ENABLED_SELECTBIS)
/* Compute the timeout wait time between now and the next timer expiry (if any)
 * using the given IOManager's timeout_queue.
 *
 * Use one of the timeoutAs* functions to convert into the form expected by
 * particular platform APIs.
 *
 * It returns the wait time duration as a Time (i.e. nanoseconds), but with
 * special values 0 for no timeout and -1 for indefinite timeout.
 */
Time timeoutWaitTime(CapIOManager *iomgr, bool wait, Time now);
#endif

/* Convert the result of timeoutWaitTime into the timeout representation
 * used by poll(). This representation uses millisecond precision with special
 * values 0 and -1 for no wait and indefinite wait.
 */
int timeoutAsPollTimeout(Time waittime);

/* Convert the result of timeoutWaitTime into a 'struct timeval *' which is
 * the timeout representation used by select(). This representation uses
 * microsecond precision with NULL for indefinite wait, and 0 for no waiting.
 */
struct timeval *timeoutAsTimeval(Time waittime, struct timeval *tv);

/* Convert the result of timeoutWaitTime into a 'struct timespec *' which is
 * the timeout representation used by many modern APIs: ppoll(), pselect(),
 * epoll_wait2(), kevent() and io_uring_enter2(). This representation uses
 * nanosecond precision with NULL for indefinite wait, and 0 for no waiting.
 */
struct timespec *timeoutAsTimespec(Time waittime, struct timespec *tv);

#if !defined(THREADED_IDLEGC)
/* Utilities for handling the non-threaded idle GC variation.
 *
 * See Note [Idle GC without preemption]
 */
void adjustTimeoutForIdleGc(bool  any_pending_io,
                            Time *timeout         /* in/out */,
                            int  *idlegc_status   /* out */);

void handleIdleGcTimeout(int idlegc_status, bool *interrupt);
#endif

#include "EndPrivate.h"

