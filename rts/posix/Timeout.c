/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1995-2021
 *
 * Timeout support used by the I/O manager implementations.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Threads.h"
#include "Schedule.h"
#include "Prelude.h"

#include "Timeout.h"
#include "IOManagerInternals.h"
#include "TimeoutQueue.h"

#include <limits.h>


/* Currently only used by the poll I/O manager, but in future may be used by
   several in-RTS I/O managers.
 */
#if defined(IOMGR_ENABLED_POLL)

bool syncDelayTimeout(Capability *cap, StgTSO *tso, HsInt us_delay)
{
    Time now = getProcessElapsedTime();
    Time target;

    /* If the desired target would be larger than the maximum Time,
     * default to the maximum Time. (#7087)
     */
    if (us_delay > TimeToUS(TIME_MAX - now)) {
        target = TIME_MAX;
    } else {
        target = now + USToTime(us_delay);
    }

    /* fill in a new timeout queue entry */
    StgTimeout *timeout;
    timeout = (StgTimeout *)allocateMightFail(cap, sizeofW(StgTimeout));
    if (RTS_UNLIKELY(timeout == NULL)) { return false; }
    union NotifyCompletion notify = { .tso = tso };
    initElemTimeoutQueue(timeout, notify, NotifyTSO, cap->r.rCCCS);

    ASSERT(tso->why_blocked == NotBlocked);
    tso->why_blocked = BlockedOnDelay;
    tso->block_info.timeout = timeout;

    insertTimeoutQueue(&cap->iomgr->timeout_queue, timeout, target);

    debugTrace(DEBUG_iomanager,
               "timer for delay of %lld usec installed at time %lld ns",
               us_delay, target);
    return true;
}


void syncDelayCancelTimeout(Capability *cap, StgTSO *tso)
{
    ASSERT(tso->why_blocked == BlockedOnDelay);
    StgTimeoutQueue *timeout = tso->block_info.timeout;

    deleteTimeoutQueue(&cap->iomgr->timeout_queue, timeout);

    /* the timeout is no longer accessible from anywhere (except here) */
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushClosure(cap, (StgClosure *)timeout);
    }

    /* We don't put the TSO back on the run queue or change the why_blocked
       status, as that is done by removeFromQueues (in the throwTo* functions).
     */
}

static void notifyTimeoutCompletion(Capability *cap, StgTimeout *timeout);

/* We use the 64bit Time type from rts/Time.h so our max time (in nanosecond
 * precision) is over 290 years from the epoch of the monotonic clock.
 *
 * Previous limitations forced us to use 31 bits with millisecond precision
 * which meant we would get clock wrap around. There was a cunning formula to
 * determine if the timer had expired, even if the clock had wrapped around.
 * With 64bit Time we do not need to worry about clock wraparound and can just
 * use the simple formula.
 */
void processTimeoutCompletions(Capability *cap, Time now)
{
    CapIOManager *iomgr = cap->iomgr;

    /* Pop entries from the front of the sleeping queue that are past their
     * wake time, and unblock the corresponding MVars.
     */
    while (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time waketime = findMinWaketimeTimeoutQueue(iomgr->timeout_queue);
        if (now < waketime) {
            break;
        }
        debugTrace(DEBUG_iomanager,"timer expired at %lld ns", waketime);
        StgTimeout *timeout;
        deleteMinTimeoutQueue(&iomgr->timeout_queue, &timeout);
        notifyTimeoutCompletion(cap, timeout);

        /* the timeout is no longer accessible from anywhere (except here) */
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            updateRemembSetPushClosure(cap, (StgClosure *)timeout);
        }
    }
}


static void notifyTimeoutCompletion(Capability *cap, StgTimeout *timeout)
{
    switch (timeout->notify_type) {
        case NotifyTSO:
        {
            StgTSO *tso      = timeout->notify.tso;
            tso->why_blocked = NotBlocked;
            tso->_link       = END_TSO_QUEUE;
            pushOnRunQueue(cap, tso);
            break;
        }
        case NotifyMVar:
            performTryPutMVar(cap, timeout->notify.mvar, Unit_closure);
            break;

        case NotifyTVar:
            barf("notifyIOCompletion: TVar notification not yet supported");
            break;
    }
}


/* poll() expect a timeout in milliseconds, with special
 * values of -1 for indefinite wait, and 0 for no waiting.
 */
#if !(defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1)
int timeoutInMilliseconds(CapIOManager *iomgr, bool wait, Time now)
{
    if (!wait) {
        /* Don't wait, just poll. */
        return 0;

    } else if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time waketime = findMinWaketimeTimeoutQueue(iomgr->timeout_queue);
        Time waittime = waketime - now;

        /* Any expired timeouts should have been cleared, so we must be waiting
         * for a timeout in the future. */
        ASSERT(waittime > 0);

        /* SUSv2 allows implementations to have an implementation defined
         * maximum timeout for poll(2). It does not specify any limits on
         * this maximum however. It is safe to pick a low limit, because if
         * we wake up early, we'll just loop round again. The Linux man page
         * for epoll_wait documents a bug in older kernel versions (before
         * 2.6.37) that limits the max sleep time to just over 35 minutes. This
         * isn't a big deal: it's ok if we wake up once every 35 minutes. It
         * is unclear if this bug also applied to poll, but this seems like a
         * plausibly portable limit to pick.
         */
        const int64_t max_timeout_ms = INT_MAX / 1000;

        /* convert waittime in nanoseconds to milliseconds, rounding up */
        int64_t waittime_ms = (waittime + 999999) / 1000000;
        return (waittime_ms < max_timeout_ms) ? waittime_ms : max_timeout_ms;

    } else {
        /* No timeouts, wait forever */
        return -1;
    }
}
#endif


/* ppoll() expect a timeout in nanoseconds, using
 * struct timespec * with special values of NULL for indefinite wait,
 * and 0 for no waiting.
 */
#if (defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1)
struct timespec *timeoutInNanoseconds(CapIOManager *iomgr, bool wait,
                                      Time now, struct timespec *tv)
{
    if (!wait) {
        /* Don't wait, just poll. */
        *tv = (struct timespec) { .tv_sec = 0, .tv_nsec = 0 };
        return tv;

    } else if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time waketime = findMinWaketimeTimeoutQueue(iomgr->timeout_queue);
        Time waittime = waketime - now;

        /* Any expired timeouts should have been cleared, so we must be waiting
         * for a timeout in the future. */
        ASSERT(waittime > 0);

        /* See comments above in timeoutInMilliseconds about the max timeout */
        const int64_t max_timeout_ms = INT_MAX / 1000;

        const Time max_timeout_ns = MSToTime(max_timeout_ms);
        if (waittime < max_timeout_ns) {
            /* Time in nanoseconds to separate seconds and nanoseconds */
            *tv = (struct timespec) {
                    .tv_sec  = waittime / 1000000000,
                    .tv_nsec = waittime % 1000000000
                  };
        } else {
            *tv = (struct timespec) {
                    .tv_sec  = max_timeout_ms / 1000,
                    .tv_nsec = 0
                  };
        }
        return tv;

    } else {
        /* No timeouts, wait forever */
        return NULL;
    }
}
#endif

#endif // defined(IOMGR_ENABLED_POLL)

