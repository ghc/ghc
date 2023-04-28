/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020-2023
 *
 * An I/O manager based on the classic Unix poll() system call.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h" // needed by SET_HDR macro

#include "IOManager.h" // defines IOMGR_ENABLED_POLL

#if defined(IOMGR_ENABLED_POLL)

#include "Capability.h"
#include "Threads.h"
#include "Schedule.h"
#include "Prelude.h"
#include "RtsUtils.h"
#include "rts/Time.h"
#include "RaiseAsync.h"
#include "Trace.h"

#include "Poll.h"
#include "RtsSignals.h"

#include <limits.h>
#include <errno.h>
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
/* We prefer the ppoll() function if available since it allows more precise
 * timeouts. It is not in the posix standard however and some platforms
 * (notably glibc and freebsd) need special CPP defines to make it available:
 */
#define _GNU_SOURCE 1
#define __BSD_VISIBLE 1
#include <signal.h>
#include <poll.h>
#endif

#include "IOManagerInternals.h"
#include "Timeout.h"

/******************************************************************************

This I/O manager is based on the classic Unix poll() system call.

    int poll(struct pollfd *fds, nfds_t nfds, int timeout);

    struct pollfd {
       int   fd;         // file descriptor
       short events;     // requested events
       short revents;    // returned events
    };

The poll() function takes an array of struct pollfd, which specifies an fd and
a set of I/O events to check for. The poll() function checks each fd for the
resuested I/O events. The typical I/O events of interest are the ability to
read from the fd (POLLIN) or write to the fd (POLLOUT). The events that
occurred are returned in the revents member of the structure. Error events are
also reported.

This API is quite convenient and matches what we need to do quite nicely. In
particular it works with an array of fds rather than a set of fds (as select()
uses) which means array entries can correspond to operations by Haskell threads.
This is convenient because we can treat the operations by each Haskell thread
independently, rather than having to notice and keep track of different threads
being interested in the same fd. By contrast, the select() API is organised
around sets of fds, requiring the construction of a set of fds across all
threads, and later reversing the mapping from fds back to threads. Furthermore,
errors for each operation are reported separately, unlike with select() where,
for example, bad fds are reported as a single error result code from select(),
giving no indication of the fd involved.

So the approach we take for this I/O manager is to incrementally maintain an
array of struct pollfd, and to use it in each call to poll(). Each poll array
entry corresponds to a single active I/O operation (so no grouping by fd).

Note that this design decision to not group by fd is a performance trade off.
Not grouping by fd means that we do not have to incur the performance cost of
managing the aiop <-> fd mapping. On the other hand, if there are many threads
waiting on the same fd then we need multiple entries in the poll array, and
the kernel cost of poll() is proportional to the array length. If this happens
enough then the cost of using more poll array entries would outweigh the cost
of managing the aiop <-> fd mapping. The reason we choose not to group is that
in practice it's relatively rare for multiple threads to wait on the same fd.
Typically it's just one, or two (one for read, one for write). So it makes
sense to optimise for a small number of threads per fd. Conveniently this is
also the simpler design, which is nice.

The primary data structure for this I/O manager is a aiop_table which is
a ClosureTable of AsyncIOOps. This table tracks the active I/O operations, with
one entry per operation (corresponding to threads calling waitRead#/waitWrite#).
The aiop_poll_table is maintained as an auxiliary table to the aiop_table,
with table indexes matching the ClosureTable. So there is an entry in the
aiop_table for each operation, and a corresponding entry in the
aiop_poll_table at the same table index. The aiop_table and the aiop_poll_table
are maintained incrementally, and with dense indexes. Keeping the tables dense
makes for slightly cheaper calls to poll() (which cost O(nfds)) and cheaper
iteration over all active operations. We use a simple doubling strategy to
enlarge the tables, and never shrink.

We also use a StgTimeoutQueue to track timeouts, and use the delay to the next
timeout (if any) as the poll() timeout parameter.

The CapIOManager structure for this I/O manager contains:

    ClosureTable     aiop_table;
    struct pollfd   *aiop_poll_table;
    StgTimeoutQueue *timeout_queue;

We also support the Linux-specific ppoll API which supports higher resolution
time delays -- nanoseconds rather than milliseconds as in classic poll(). It
also allows the signal mask to be adjusted, but we do not make use of this.

   int ppoll(struct pollfd *fds, nfds_t nfds,
           const struct timespec *tmo_p, const sigset_t *sigmask);

******************************************************************************/

/* Forward declarations */
static bool enlargeTables(Capability *cap, CapIOManager *iomgr);
static void notifyIOCompletion(Capability *cap, StgAsyncIOOp *aiop);
static void ioCancel(Capability *cap, StgAsyncIOOp *aiop);
static void reportPollError(int res, nfds_t nfds) STG_NORETURN;


void initCapabilityIOManagerPoll(CapIOManager *iomgr)
{
    initClosureTable(&iomgr->aiop_table, ClosureTableCompact);
    iomgr->aiop_poll_table = NULL;
    iomgr->timeout_queue = emptyTimeoutQueue();
}


/* Used to implement syncIOWaitReady.
 * Result is true on success, or false on allocation failure. */
bool syncIOWaitReadyPoll(Capability *cap, StgTSO *tso,
                         IOReadOrWrite rw, HsInt fd)
{
    StgAsyncIOOp *aiop;
    aiop = (StgAsyncIOOp *)allocateMightFail(cap, sizeofW(StgAsyncIOOp));
    if (RTS_UNLIKELY(aiop == NULL)) return false;
    SET_HDR(aiop, &stg_ASYNCIOOP_info, cap->r.rCCCS);
    aiop->notify.tso     = tso;
    aiop->notify_type    = NotifyTSO;
    aiop->live           = &stg_ASYNCIO_LIVE0_closure;
    tso->why_blocked     = rw == IORead ? BlockedOnRead : BlockedOnWrite;
    tso->block_info.aiop = aiop;
    return asyncIOWaitReadyPoll(cap, aiop, rw, fd);
}

/* Result is true on success, or false on allocation failure. */
bool asyncIOWaitReadyPoll(Capability *cap, StgAsyncIOOp *aiop,
                         IOReadOrWrite rw, int fd)
{
    CapIOManager *iomgr = cap->iomgr;
    if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
        bool ok = enlargeTables(cap, iomgr);
        if (RTS_UNLIKELY(!ok)) return false;
    }

    int ix = insertClosureTable(cap, &iomgr->aiop_table, aiop);

    /* We use the aiop_table and aiop_poll_table densely. */
    ASSERT(ix == sizeClosureTable(&iomgr->aiop_table) - 1);

    /* The syncIO wrapper or CMM primop filled in the notify and live fields,
     * we fill the rest.
     */
    aiop->capno   = cap->no;
    aiop->index   = ix;
    aiop->outcome = IOOpOutcomeInFlight;

    /* Fill in the corresponding entry in the aiop_poll_table */
    iomgr->aiop_poll_table[ix] = (struct pollfd) {
                                   .fd      = fd,
                                   .events  = rw == IORead ? POLLIN : POLLOUT,
                                   .revents = 0
                                 };
    return true;
}


void syncIOCancelPoll(Capability *cap, StgTSO *tso)
{
    StgAsyncIOOp *aiop  = tso->block_info.aiop;
    ASSERT(aiop->notify_type == NotifyTSO);
    ASSERT(indexClosureTable(&cap->iomgr->aiop_table, aiop->index) == aiop);
    ioCancel(cap, aiop);
    /* We cannot use the normal notifyIOCompletion here. We are in the context
     * of throwTo, interrupting a thread blocked on IO via an async exception.
     * We don't put the TSO back on the run queue or change the why_blocked
     * status, as that is done by removeFromQueues (in the throwTo* functions).
     */
    tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
}


void asyncIOCancelPoll(Capability *cap, StgAsyncIOOp *aiop)
{
    /* We can reliably determine if the aiop is still in progress by checking
     * if the aiop_table still points to this aiop object. This is reliable
     * because each aiop is GC heap allocated, so cannot be recycled until it
     * is no longer retained by the application.
     */
    ASSERT(aiop->notify_type != NotifyTSO);
    if (indexClosureTable(&cap->iomgr->aiop_table, aiop->index) == aiop) {
        ioCancel(cap, aiop);
        notifyIOCompletion(cap, aiop);
    }
}


static void ioCancel(Capability *cap, StgAsyncIOOp *aiop)
{
    CapIOManager *iomgr = cap->iomgr;

    int ix = aiop->index;
    int ix_from; int ix_to;
    removeCompactClosureTable(cap, &iomgr->aiop_table, ix,
                              &ix_from, &ix_to);
    if (ix_to != ix_from) {
        StgAsyncIOOp *aiop_to = indexClosureTable(&iomgr->aiop_table, ix_to);
        aiop_to->index = ix_to;
        iomgr->aiop_poll_table[ix_to] = iomgr->aiop_poll_table[ix_from];
    }
    aiop->outcome = IOOpOutcomeCancelled;
}


bool anyPendingTimeoutsOrIOPoll(CapIOManager *iomgr)
{
    return !isEmptyTimeoutQueue(iomgr->timeout_queue)
        || !isEmptyClosureTable(&iomgr->aiop_table);
}


static void notifyIOCompletion(Capability *cap, StgAsyncIOOp *aiop)
{
    ASSERT(aiop->outcome != IOOpOutcomeInFlight);
    switch (aiop->notify_type) {
        case NotifyTSO:
        {
            if (aiop->outcome == IOOpOutcomeFailed && aiop->error == EBADF) {
                /* The fd is invalid: raise an IOError exception in the blocked
                 * thread. (See bug #4934 for what happens without this.)
                 */
                StgTSO *tso = aiop->notify.tso;
                debugTrace(DEBUG_iomanager,
                           "Raising exception in thread %" FMT_StgThreadID
                           " blocked on an invalid fd", tso->id);
                raiseAsync(cap, tso, (StgClosure *)blockedOnBadFD_closure,
                           false, NULL);
                break;
            } else {
                /* We should be guaranteed that the tso is still on the same
                 * cap because the tso was not on the run queue of any cap and
                 * so is not subject to thread migration.
                 */
                StgTSO *tso      = aiop->notify.tso;
                tso->why_blocked = NotBlocked;
                tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
                tso->_link       = END_TSO_QUEUE;
                pushOnRunQueue(cap, tso);
            }
            break;
        }
        case NotifyMVar:
            barf("poll iomgr: MVar notification not yet supported");
            break;

        case NotifyTVar:
            barf("poll iomgr: TVar notification not yet supported");
            break;
    }
}


static void processIOCompletions(Capability *cap, CapIOManager *iomgr,
                                 int ncompletions)
{
    /* The scheme we use with poll is that we have a dense poll table, and a
     * corresponding table that maps to the closure table index. The poll
     * table entries correspond to individual notification requests. It is not
     * grouped by fd. So we simply iterate over the poll table, indexing the
     * corresponding aiop table entry and notifying.
     */
    debugTrace(DEBUG_iomanager, "processIOCompletions(ncompletions = %d)",
                                ncompletions);
    struct pollfd *aiop_poll_table = iomgr->aiop_poll_table;
    int n = ncompletions;
    int i = 0;
    while (n > 0) {
        ASSERT(i < sizeClosureTable(&iomgr->aiop_table));

        /* Since each aiop_table entry is for a single (fd, rw) pair, we
         * just need to check if the aiop_poll_table[n].revents contains
         * anything. We don't need to distinguish read vs write.
         */
        if (aiop_poll_table[i].revents) {
            StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, i);

            /* We do need to handle POLLNVAL, but we do not need to do anything
             * special for POLLERR or POLLHUP. (See man poll for details).
             * The calling thread will typically try to do I/O after waiting
             * for I/O readiness. Thus for POLLERR they'll discover the error
             * when doing the I/O. And for POLLHUP, there is typically no
             * action required anyway: it's a notification and not an error.
             */
            if (aiop_poll_table[i].revents & POLLNVAL) {
                aiop->outcome = IOOpOutcomeFailed;
                aiop->error   = EBADF;
            } else {
                aiop->outcome = IOOpOutcomeSuccess;
                aiop->result  = 0;
            }

            /* Remove from the completion table, preserving compactness, and
             * apply the same compacting to the aiop_poll_table.
             */
            int ix_from; int ix_to;
            removeCompactClosureTable(cap, &iomgr->aiop_table, i,
                                      &ix_from, &ix_to);
            if (ix_to != ix_from) {
                StgAsyncIOOp *aiop_to;
                aiop_to = indexClosureTable(&iomgr->aiop_table, ix_to);
                aiop_to->index = ix_to;
                aiop_poll_table[ix_to] = aiop_poll_table[ix_from];
            }

            notifyIOCompletion(cap, aiop);
            n--;
        } else {
            /* You'd expect incrementing the poll table index to be
             * unconditional, but we don't increment the index if we did
             * process the entry, because using removeCompactClosureTable
             * means we'll move an entry from the end into the same index.
             */
            i++;
        }
    }
}


void pollCompletedTimeoutsOrIOPoll(Capability *cap)
{
    CapIOManager *iomgr = cap->iomgr;

    if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time now = getProcessElapsedTime();
        processTimeoutCompletions(cap, now);
    }

    if (!isEmptyClosureTable(&iomgr->aiop_table)) {

        nfds_t nfds = sizeClosureTable(&iomgr->aiop_table);

        /* Poll for I/O readiness, without waiting. */
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
        /* We could use poll here, since we use no timeout, but for
           consistency we use the same syscall as at the other call site. */
        struct timespec tv = (struct timespec) { .tv_sec = 0, .tv_nsec = 0 };
        int res = ppoll(iomgr->aiop_poll_table, nfds, &tv, NULL);

        debugTrace(DEBUG_iomanager,
                   "ppoll(nfds = %d, timeout.sec = 0, timeout.nsec = 0) = %d",
                   nfds, res);
#else
        int res = poll(iomgr->aiop_poll_table, nfds, 0);

        debugTrace(DEBUG_iomanager,
                   "poll(nfds = %d, timeout_ms = 0) = %d",
                   nfds, res);
#endif
        if (res == 0) {
            /* There is no I/O ready. We'll return to the scheduler. */

        } else if (res > 0) {
            int ncompletions = res;
            ASSERT(ncompletions <= (int)nfds);
            processIOCompletions(cap, iomgr, ncompletions);

        } else if (errno == EINTR) {
          /* We got interrupted by a signal. This is unlikely since we asked
           * poll() not to wait, but if so we'll return to the scheduler.
           */

        } else {
            reportPollError(res, nfds);
        }
    }
}


void awaitCompletedTimeoutsOrIOPoll(Capability *cap)
{
    CapIOManager *iomgr = cap->iomgr;

    /* Loop until we've woken up some threads. This loop is needed because the
     * poll() timing isn't accurate, we sometimes sleep for a while but not
     * long enough to wake up a thread in a threadDelay. Or we may need to
     * sleep multiple times if we need to sleep longer than the maximum timeout
     * that select() supports.
     */
    do {
        /* There is either pending I/O or pending timers. */
        ASSERT(!isEmptyTimeoutQueue(iomgr->timeout_queue) ||
               !isEmptyClosureTable(&iomgr->aiop_table));

        Time now = getProcessElapsedTime();
        processTimeoutCompletions(cap, now);

        /* If we didn't wake any threads due to expiring timeouts, then we need
         * to wait on I/O. Or to put it another way, even if we did wake some
         * threads, we'll still poll (but not wait) for I/O. This is to ensure
         * we avoid starving threads blocked on I/O.
         */
        bool wait = emptyRunQueue(cap);

        /* Decide if we are going to wait if no I/O is ready, either:
         * poll only, wait indefinitely, or wait until a timeout.
         */
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
        struct timespec ts, *timeout_ns;
        timeout_ns = timeoutInNanoseconds(iomgr, wait, now, &ts);
#else
        int timeout_ms = timeoutInMilliseconds(iomgr, wait, now);
#endif

        /* Check for I/O readiness, possibly waiting. */
        nfds_t nfds = sizeClosureTable(&iomgr->aiop_table);
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
        int res = ppoll(iomgr->aiop_poll_table, nfds, timeout_ns, NULL);

        debugTrace(DEBUG_iomanager,
                   "ppoll(nfds = %d, timeout.sec = %d, timeout.nsec = %d) = %d",
                   nfds, timeout_ns == NULL ? -1 : timeout_ns->tv_sec,
                         timeout_ns == NULL ?  0 : timeout_ns->tv_nsec,
                   res);
#else
        int res = poll(iomgr->aiop_poll_table, nfds, timeout_ms);

        debugTrace(DEBUG_iomanager,
                   "poll(nfds = %d, timeout_ms = %d) = %d",
                   nfds, timeout_ms, res);
#endif

        if (res == 0) {
            /* Success but there is no I/O ready. This can happen either if we
             * were not blocking or were in a timed wait and the timeout
             * occurred before any I/O became ready. Either way, the do-while
             * loop condition will handle it.
             */
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
            ASSERT(timeout_ns != NULL);
#else
            ASSERT(timeout_ms != -1);
#endif

        } else if (res > 0) {
            int ncompletions = res;
            ASSERT(ncompletions <= (int)nfds);
            processIOCompletions(cap, iomgr, ncompletions);

        } else if (errno == EINTR) {
            /* We got interrupted by a signal. In the non-threaded RTS, if the
             * signal is one of ours we need to return to the scheduler to let
             * it handle it. Otherwise we would loop and keep waiting for I/O
             * or timeouts, meaning we would block for a long time before the
             * signal is serviced.
             */
#if defined(RTS_USER_SIGNALS)
            if (startPendingSignalHandlers(cap)) break;
#endif

            /* We can also be interrupted by the shutdown signal handler, which
             * will set sched_state and so cause us to drop out of the loop.
             *
             * For any other interruption (e.g. timer) we will go round the
             * do-while loop again.
             */

        } else {
            reportPollError(res, nfds);
        }

    } while (emptyRunQueue(cap)
         && (getSchedState() == SCHED_RUNNING));
}

static void reportPollError(int res, nfds_t nfds)
{
    if (errno == EINVAL) {
        errorBelch("poll iomgr: exceeded resource limit for open files: %u",
                   (unsigned int) nfds);
        stg_exit(EXIT_FAILURE);
        //TODO: we could do better here. We could throw a Haskell exception
        // as soon as we get thread submitting an operation that would exceed
        // the limit.
    } else {
        sysErrorBelch("poll res = %d", res);
        stg_exit(EXIT_FAILURE);
    }
}


/* Helper function to double the size of the aiop_table and aiop_poll_table.
 */
static bool enlargeTables(Capability *cap, CapIOManager *iomgr)
{
    int oldcapacity = capacityClosureTable(&iomgr->aiop_table);
    int newcapacity = (oldcapacity == 0) ? 1 : (oldcapacity * 2);

    bool ok = enlargeClosureTable(cap, &iomgr->aiop_table, newcapacity);
    if (RTS_UNLIKELY(!ok)) return false;

    /* Update the auxiliary aiop_poll_table to match */
    struct pollfd *aiop_poll_table;
    aiop_poll_table = stgReallocBytes(iomgr->aiop_poll_table,
                                      sizeof(struct pollfd) * newcapacity,
                                      "Poll.c: enlargeTables");
    iomgr->aiop_poll_table = aiop_poll_table;
    /* Initialise the new part of the aiop_poll_table */
    for (int i = oldcapacity; i < newcapacity; i++) {
        aiop_poll_table[i] = (struct pollfd) {
                               .fd      = -1,
                               .events  = 0,
                               .revents = 0
                             };
    }
    return true;
}

#endif /* IOMGR_ENABLED_POLL */
