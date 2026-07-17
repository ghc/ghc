/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020-2026
 *
 * A second I/O manager based on the classic Unix select() system call.
 *
 * See SelectBis.h for the sad story of why this exists.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h" // needed by SET_HDR macro

#include "IOManager.h" // defines IOMGR_ENABLED_SELECTBIS

#if defined(IOMGR_ENABLED_SELECTBIS)

#include "Capability.h"
#include "Threads.h"
#include "Schedule.h"
#include "Prelude.h"
#include "RtsUtils.h"
#include "rts/Time.h"
#include "RaiseAsync.h"
#include "Trace.h"

#include "SelectBis.h"
#include "RtsSignals.h"

#include <sys/select.h>
#include <errno.h>

#include "IOManagerInternals.h"
#include "Timeout.h"
#include "FdWakeup.h"

/******************************************************************************

This I/O manager is based on the classic Unix select() system call.

       int select(int nfds, fd_set *readfds, fd_set *writefds,
                  fd_set *exceptfds, struct timeval *timeout);

The select() call has various limits, quirks and slight differences between
historical Unix variants.

The basic idea is to collect a set of fds (represented as a bitset) that we are
interested in: one for reads, one for writes. The call then queries for I/O
readiness on all the fds in the read and write sets. The result is a set of fds
that are ready to read from, and a set that are ready to write to. The same
bitset representation is used for the output. Indeed a "fun" quirk of select()
is that it mutates the fd sets it is passed, which means they either need to be
built up each time, or copied. There is also an optional timeout if no fds are
ready immediately. There is also an fd bitset for "exceptional conditions"
which we do not use.

There is of course no incremental behaviour here; this is a bulk one-off call
with no persistent state. This has obvious scaling problems. The cost each time
is O(n) in the maximum of the integer value of the fds of interest. There is
also a maximum bitset size. On Linux this is 1024. This means select() cannot
be used if the process uses more than that many open files, even if we're only
interested in a few. On OSX the default limit is also 1024 but this can be
raised or even managed dynamically, at the cost of more memory (and some
non-standard code).

That particular problem is solved by the later Unix poll() system call, which
uses an array of the fds we are interested in, which means it not limited by
the absolute value of the fds numbers (but it is still O(n) in how many fds we
are interested in).

We have some choice in how we process results. We want to find the intersection
between the requests for notification of I/O readiness (coming from the Haskell
threads) and the read and write bit sets. There's not much clever we can do to
compute this intersection efficiently: we can either iterate over the bit sets
or over the readiness requests. There is no obvious answer here. Typically
there will be few results compared to the number of requests and a bitset scan
could be fast. In practice we cannot portably scan the bitset efficiently (e.g.
word at a time). Portably, we can only probe each bit at a time using FD_ISSET.
Portability is the main reason to use select() rather than a more modern
interface, so we have to take it seriously here. Furthermore, if we iterated
over the bit sets we would have to maintain a mapping from fd to requests.

In principle we also have the choice to maintain the read and write fd bit sets
incrementally, or create them afresh each time we call select(). There is no
asymptotic bonus to maintaining them incrementally since the whole thing is
O(n) anyway. There could plausibly be some constant factor benefit. To maintain
the fd bit sets incrementally we would need to maintain a mapping between
requests and fds. This would also be an extra cost that would have to be
outweighed by any saving.

In the end we take the simple approach to constructing the bitset inputs and to
results processing. We create the bit sets afresh each time from the collection
of requests. For processing results we iterate over the requests and look up
each one to see if it is in the appropriate result bitset. Along with each
operation, we store the fd and whether we were interested in reading or writing.
We iterate over the operations and use the fd and r/w information to construct
the read and write bit sets.

A particularly frustrating feature of select() is that if any single fd in any
fd bitset is invalid (e.g. because the file was already closed) then select()
fails and tells us there is a bad fd somewhere, but it has no way to indicate
which fd was bad. This is really quite annoying as we then have to do a search
through the fds to find which one was bad.

The primary data structure for this I/O manager is a aiop_table which is a
ClosureTable of AsyncIOOps. This table tracks the active I/O operations, with
one entry per operation (corresponding to threads calling waitRead#/waitWrite#).
We also track the fd for each operation and whether the operation is waiting on
read or write readiness. This additional information is stored in the fd_table.
The fd_table is maintained as an auxiliary table to the aiop_table, with table
indexes matching the ClosureTable. So there is an entry in the aiop_table for
each operation, and a corresponding entry in the fd_table at the same table
index. The aiop_table and the fd_table are maintained incrementally, and with
dense indexes.

We also use a StgTimeoutQueue to track timeouts, and use the delay to the next
timeout (if any) as the poll() timeout parameter.

The CapIOManager structure for this I/O manager contains:

    ClosureTable     aiop_table;
    struct fd_table_entry { int fd; IOReadOrWrite rw } *fd_table;
    StgTimeoutQueue *timeout_queue;
    int interrupt_fd_r, interrupt_fd_w;

******************************************************************************/

/* Forward declarations */
static bool enlargeTables(CapIOManager *iomgr);
static void removeFromTables(CapIOManager *iomgr, int i);
static void notifyIOCompletion(CapIOManager *iomgr, StgAsyncIOOp *aiop);
static void reportSelectError(void) STG_NORETURN;
static bool checkFdRange(int fd);
static int  collectFdSets(CapIOManager *iomgr);
static void processBadFds(CapIOManager *iomgr);


void initCapabilityIOManagerSelectBis(CapIOManager *iomgr)
{
    initClosureTable(&iomgr->aiop_table, ClosureTableCompact);
    iomgr->timeout_queue = emptyTimeoutQueue();

#if defined(HAVE_PREEMPTION)
    newFdWakeup(&iomgr->interrupt_fd_r, &iomgr->interrupt_fd_w);

    /* Would never happen in a standalone process, but could plausibly happen
     * if the RTS is used within another process that already has many open fds.
     */
    if (iomgr->interrupt_fd_r < 0 || iomgr->interrupt_fd_r >= (int)FD_SETSIZE ||
        iomgr->interrupt_fd_w < 0 || iomgr->interrupt_fd_w >= (int)FD_SETSIZE) {
        barf("initCapabilityIOManagerSelectBis: fds out of select range");
    }
#endif

    iomgr->fd_table = NULL;
    iomgr->rfds = stgMallocBytes(sizeof (fd_set), "IOManagerSelectBis");
    iomgr->wfds = stgMallocBytes(sizeof (fd_set), "IOManagerSelectBis");
    iomgr->ncompletions_extra = 0;
}


void freeCapabilityIOManagerSelectBis(CapIOManager *iomgr)
{
    if (iomgr->fd_table) stgFree(iomgr->fd_table);
    stgFree(iomgr->rfds);
    stgFree(iomgr->wfds);
#if defined(HAVE_PREEMPTION)
    closeFdWakeup(iomgr->interrupt_fd_r, iomgr->interrupt_fd_w);
#endif
}


/* Result is true on success, or false on allocation failure. */
bool syncIOWaitReadySelectBis(CapIOManager *iomgr, StgTSO *tso,
                              IOReadOrWrite rw, HsInt fd)
{
    StgAsyncIOOp *aiop;
    aiop = (StgAsyncIOOp *)allocateMightFail(iomgr->cap, sizeofW(StgAsyncIOOp));
    if (RTS_UNLIKELY(aiop == NULL)) return false;
    SET_HDR(aiop, &stg_ASYNCIOOP_info, iomgr->cap->r.rCCCS);
    aiop->notify.tso     = tso;
    aiop->notify_type    = NotifyTSO;
    aiop->live           = &stg_ASYNCIO_LIVE0_closure;
    tso->why_blocked     = rw == IORead ? BlockedOnRead : BlockedOnWrite;
    tso->block_info.aiop = aiop;
    return asyncIOWaitReadySelectBis(iomgr, aiop, rw, fd);
}

/* Result is true on success, or false on allocation failure. */
bool asyncIOWaitReadySelectBis(CapIOManager *iomgr, StgAsyncIOOp *aiop,
                               IOReadOrWrite rw, int fd)
{
    if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
        bool ok = enlargeTables(iomgr);
        if (RTS_UNLIKELY(!ok)) return false;
    }

    int ix = insertClosureTable(iomgr->cap, &iomgr->aiop_table, aiop);

    /* We use the aiop_table and fd_table densely. */
    ASSERT(ix == sizeClosureTable(&iomgr->aiop_table) - 1);

    /* The syncIO wrapper or CMM primop filled in the notify and live fields,
     * we fill the rest.
     */
    aiop->capno   = iomgr->cap->no;
    aiop->index   = ix;
    aiop->outcome = IOOpOutcomeInFlight;

    /* Fill in the corresponding entry in the fd_table */
    iomgr->fd_table[ix] = (struct fd_table_entry) {
                            .fd = fd,
                            .rw = rw
                          };

    if (!checkFdRange(fd)) {
        /* We have a synchronous failure, but the primop is not set up to report
         * exceptions. We cannot report async exceptions to the caller here
         * since the thread stack is not in the right state (so we cannot use
         * notifyIOCompletion). So instead we mark the aiop as failed now, but
         * we report the failure later when we poll for completed I/O.
         */
        aiop->outcome = IOOpOutcomeFailed;
        aiop->error   = EBADF;
        /* completions for synchronous failures to report asynchronously */
        iomgr->ncompletions_extra++;
    };

    return true;
}


void syncIOCancelSelectBis(CapIOManager *iomgr, StgTSO *tso)
{
    StgAsyncIOOp *aiop  = tso->block_info.aiop;
    ASSERT(aiop->notify_type == NotifyTSO);
    ASSERT(indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop);
    removeFromTables(iomgr, aiop->index);
    aiop->outcome = IOOpOutcomeCancelled;
    /* We cannot use the normal notifyIOCompletion here. We are in the context
     * of throwTo, interrupting a thread blocked on IO via an async exception.
     * We don't put the TSO back on the run queue or change the why_blocked
     * status, as that is done by removeFromQueues (in the throwTo* functions).
     */
    tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;

    /* We are in the TSO case, where the aiop was only reachable from the TSO
     * itself, and thus it is now no longer be reachable at all.
     */
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushClosure(iomgr->cap, (StgClosure *)aiop);
    }
}


void asyncIOCancelSelectBis(CapIOManager *iomgr, StgAsyncIOOp *aiop)
{
    /* We can reliably determine if the aiop is still in progress by checking
     * if the aiop_table still points to this aiop object. This is reliable
     * because each aiop is GC heap allocated, so cannot be recycled until it
     * is no longer retained by the application.
     */
    ASSERT(aiop->notify_type != NotifyTSO);
    if (indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop) {
        removeFromTables(iomgr, aiop->index);
        aiop->outcome = IOOpOutcomeCancelled;
        notifyIOCompletion(iomgr, aiop);
    }
}


bool anyPendingTimeoutsOrIOSelectBis(CapIOManager *iomgr)
{
    return !isEmptyTimeoutQueue(iomgr->timeout_queue)
        || !isEmptyClosureTable(&iomgr->aiop_table);
}


static void notifyIOCompletion(CapIOManager *iomgr, StgAsyncIOOp *aiop)
{
    ASSERT(aiop->outcome != IOOpOutcomeInFlight);
    switch (aiop->notify_type) {
        case NotifyTSO:
        {
            /* We should be guaranteed that the tso is still on the same
             * cap because the tso was not on the run queue of any cap and
             * so is not subject to thread migration.
             */
            StgTSO *tso = aiop->notify.tso;
            ASSERT(tso->cap == iomgr->cap);
            if (aiop->outcome == IOOpOutcomeFailed && aiop->error == EBADF) {
                /* The fd is invalid: raise an IOError exception in the blocked
                 * thread. (See bug #4934 for what happens without this.)
                 */
                debugTrace(DEBUG_iomanager,
                           "Raising exception in thread %" FMT_StgThreadID
                           " blocked on an invalid fd", tso->id);
                raiseAsync(iomgr->cap, tso,
                           (StgClosure *)blockedOnBadFD_closure,
                           false, NULL);
            } else {
                tso->why_blocked = NotBlocked;
                tso->_link       = END_TSO_QUEUE;
                pushOnRunQueue(iomgr->cap, tso);
            }
            /* For the TSO case, the aiop was only reachable from the TSO
             * itself, and thus it is now no longer be reachable at all.
             */
            IF_NONMOVING_WRITE_BARRIER_ENABLED {
                updateRemembSetPushClosure(iomgr->cap, (StgClosure *)aiop);
            }
            break;
        }
        case NotifyMVar:
            barf("selectbis iomgr: MVar notification not yet supported");
            break;

        case NotifyTVar:
            barf("selectbis iomgr: TVar notification not yet supported");
            break;
    }
}


static bool processIOCompletions(CapIOManager *iomgr, int ncompletions)
{
    /* We want to find the intersection between the sets of ready fds returned
     * by select() and the aiop_table. Given how select() represents
     * things there's no particularly efficient way to do it.
     *
     * We just go through the whole aiop_table and look up each one in
     * the read or write fd_set to see if they completed. Note that here is
     * where we rely on the aiop_table being dense so we can iterate
     * over the entries. We can short-cut if we hit the ncompletions before
     * getting to the end of the table.
     */
    debugTrace(DEBUG_iomanager, "processIOCompletions(ncompletions = %d)",
                                ncompletions);

    bool interrupt = false;
#if defined(HAVE_PREEMPTION)
    /* If the interrupt_fd_r is ready, collect it */
    if (FD_ISSET(iomgr->interrupt_fd_r, iomgr->rfds)) {
        collectFdWakeup(iomgr->interrupt_fd_r);
        ncompletions--;
        interrupt = true;
        debugTrace(DEBUG_iomanager, "Received interrupt in poll I/O manager");
    }
#endif

    struct fd_table_entry *fd_table = iomgr->fd_table;
    int n = ncompletions;
    int i = 0;
    while (n > 0) {
        ASSERT(i < sizeClosureTable(&iomgr->aiop_table));

        StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, i);
        int           fd   = fd_table[i].fd;
        IOReadOrWrite rw   = fd_table[i].rw;

        if (RTS_UNLIKELY(aiop->outcome == IOOpOutcomeFailed)) {
            /* The synchronous failure case, see ncompletions_extra. */
        } else if (rw == IORead ? FD_ISSET(fd, iomgr->rfds)
                                : FD_ISSET(fd, iomgr->wfds)) {
            aiop->outcome = IOOpOutcomeSuccess;
            aiop->result  = 0;
        } else {
            /* You'd expect incrementing the table index to be unconditional,
             * but we don't increment the index if we did process the entry,
             * because using removeFromTables means we'll move an entry from
             * the end of the table into the index i.
             */
            i++;
            continue; /* skip the steps below */
        }
        removeFromTables(iomgr, i);
        notifyIOCompletion(iomgr, aiop);
        n--;
    }
    return interrupt;
}


void pollCompletedTimeoutsOrIOSelectBis(CapIOManager *iomgr)
{
    if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time now = getProcessElapsedTime();
        processTimeoutCompletions(iomgr, now);
    }

    if (!isEmptyClosureTable(&iomgr->aiop_table)) {
        /* Prepare to poll for I/O readiness: collect all of the fd's that
         * we're interested in.
         */
        int maxfd = collectFdSets(iomgr);

        /* Poll for I/O readiness, without waiting. */
        struct timeval tv = (struct timeval) { .tv_sec = 0, .tv_usec = 0 };
        int res = select(maxfd+1, iomgr->rfds, iomgr->wfds, NULL, &tv);
        if (res == 0 && iomgr->ncompletions_extra == 0) {
            /* There is no I/O ready. We'll return to the scheduler. */

        } else if (res > 0 || iomgr->ncompletions_extra > 0) {
            /* Extra completions for synchronous failures to report */
            int ncompletions = res + iomgr->ncompletions_extra;
            iomgr->ncompletions_extra = 0;

            ASSERT(ncompletions <= sizeClosureTable(&iomgr->aiop_table));
            processIOCompletions(iomgr, ncompletions);

        } else if (errno == EBADF) {
            processBadFds(iomgr);

        } else if (errno == EINTR) {
          /* We got interrupted by a signal. This is unlikely since we asked
           * select() not to wait, but if so we'll return to the scheduler.
           */

        } else {
            reportSelectError();
        }
    }

#if defined(RTS_USER_SIGNALS)
    startPendingSignalHandlers(iomgr->cap);
#endif
}


bool awaitCompletedTimeoutsOrIOSelectBis(CapIOManager *iomgr)
{
    bool interrupt = false; /* got woken up via interruptIOManager */

    /* Loop until we've woken up some threads. This loop is needed because the
     * select() timing isn't accurate, we sometimes sleep for a while but not
     * long enough to wake up a thread in a threadDelay. Or we may need to
     * sleep multiple times if we need to sleep longer than the maximum timeout
     * that select() supports.
     */
    do {
        /* We do /not/ require that there be pending I/O or pending timers.
         * If there is neither, it's because the scheduler wants us to wait
         * on signals only.
         */

        Time now = getProcessElapsedTime();
        processTimeoutCompletions(iomgr, now);

        /* If we didn't wake any threads due to expiring timeouts, then we need
         * to wait on I/O. Or to put it another way, even if we did wake some
         * threads, we'll still poll (but not wait) for I/O. This is to ensure
         * we avoid starving threads blocked on I/O.
         */
        bool wait = emptyRunQueue(iomgr->cap);

        /* If we have failures to report, we must not block. */
        if (iomgr->ncompletions_extra > 0) {
            wait = false;
        }

        /* Prepare to poll for I/O readiness: collect all of the fd's that
         * we're interested in.
         */
        int maxfd = collectFdSets(iomgr);

        /* Decide if we are going to wait if no I/O is ready, either:
         * poll only, wait indefinitely, or wait until a timeout.
         */
        struct timeval tv, *timeout_us;
        timeout_us = timeoutInMicroseconds(iomgr, wait, now, &tv);

        /* Check for I/O readiness, possibly waiting. */
        int res = select(maxfd+1, iomgr->rfds, iomgr->wfds, NULL, timeout_us);

        if (res == 0 && iomgr->ncompletions_extra == 0) {
            /* Success but there is no I/O ready. This can happen either if we
             * were not blocking or were in a timed wait and the timeout
             * occurred before any I/O became ready. Either way, the do-while
             * loop condition will handle it.
             */
            ASSERT(timeout_us != NULL);

        } else if (res > 0 || iomgr->ncompletions_extra > 0) {
            /* Extra completions for synchronous failures to report */
            int ncompletions = res + iomgr->ncompletions_extra;
            iomgr->ncompletions_extra = 0;

            ASSERT(ncompletions <= sizeClosureTable(&iomgr->aiop_table));
            interrupt = processIOCompletions(iomgr, ncompletions);
            // FIXME: do we also need to check for timeout completions now?
            // we have a non-empty queue, but if !wait then we have also moved
            // on and so we sould check for timeouts.

        } else if (errno == EINTR) {
            /* We got interrupted by a signal. */

#if defined(RTS_USER_SIGNALS)
            /* Start any corresponding user signal handlers. If any, the run
             * queue will become non-empty and we will drop out of the loop.
             */
            startPendingSignalHandlers(iomgr->cap);
#endif

            /* We can also be interrupted by the shutdown signal handler, which
             * will set sched_state and so cause us to drop out of the loop.
             *
             * For any other interruption (e.g. timer) we will go round the
             * do-while loop again.
             */

        } else if (errno == EBADF) {
            processBadFds(iomgr);

        } else {
            reportSelectError();
        }

    } while (emptyRunQueue(iomgr->cap)
         && !interrupt
         && (getSchedState() == SCHED_RUNNING));
    return !interrupt;
}


static void reportSelectError(void)
{
    sysErrorBelch("select() failed");
    stg_exit(EXIT_FAILURE);
}


static void processBadFds(CapIOManager *iomgr)
{
    /* This is extremely tiresome. The select() call fails with EBADF if any
     * fd is invalid (usually closed), but it does not tell us which one.
     * So we have to loop through them to find the offending fd.
     *
     * This will only find the first bad fd, so the caller must cope with
     * there still being bad fds after this.
     */

    fd_set rfds, wfds;
    FD_ZERO(&rfds);
    FD_ZERO(&wfds);

    struct fd_table_entry *fd_table = iomgr->fd_table;
    int  nentries = sizeClosureTable(&iomgr->aiop_table);
    for (int n = 0; n < nentries; n++) {
        int           fd = fd_table[n].fd;
        IOReadOrWrite rw = fd_table[n].rw;

        struct timeval tv = { .tv_sec = 0, .tv_usec = 0 };
        int res;
        if (rw == IORead) {
            FD_SET(fd, &rfds);
            res = select(fd+1, &rfds, NULL, NULL, &tv);
            FD_CLR(fd, &rfds);
        } else {
            FD_SET(fd, &wfds);
            res = select(fd+1, NULL, &wfds, NULL, &tv);
            FD_CLR(fd, &wfds);
        }
        if (res == 0) {
            continue;

        } else if (errno == EBADF) {
            StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, n);
            aiop->outcome = IOOpOutcomeFailed;
            aiop->error   = EBADF;
            removeFromTables(iomgr, n);
            notifyIOCompletion(iomgr, aiop);
            /* There is /probably/ only one bad fd at once, so we abort the
             * search here. If we are unlucky and there are several bad fds
             * then the caller will just loop round again.
             */

            return;

        } else if (errno == EINTR) {
            /* Unlikely, since we did a non-blocking select(). Try again. */
            n--;
            continue;

        } else {
            reportSelectError();
        }
    }
}


void interruptIOManagerSelectBis(CapIOManager *iomgr)
{
#if defined(HAVE_PREEMPTION)
    sendFdWakeup(iomgr->interrupt_fd_w);
#endif
}


/* Helper function to double the size of the aiop_table and fd_table.
 */
static bool enlargeTables(CapIOManager *iomgr)
{
    int oldcapacity = capacityClosureTable(&iomgr->aiop_table);
    int newcapacity = (oldcapacity == 0) ? 1 : (oldcapacity * 2);

    bool ok = enlargeClosureTable(iomgr->cap, &iomgr->aiop_table, newcapacity);
    if (RTS_UNLIKELY(!ok)) return false;

    /* Update the auxiliary fd_table to match */
    iomgr->fd_table =
        stgReallocBytes(iomgr->fd_table,
                        sizeof(struct fd_table_entry) * newcapacity,
                        "SelectBis.c: enlargeTables");

    /* Initialise the new part of the fd_table */
    struct fd_table_entry *fd_table = iomgr->fd_table;
    for (int i = oldcapacity; i < newcapacity; i++) {
        fd_table[i] = (struct fd_table_entry) {
                        .fd = -1,
                        .rw = 0
                      };
    }
    return true;
}


/* Remove from the completion table, preserving compactness, and apply the same
 * compacting to the fd_table.
 */
static void removeFromTables(CapIOManager *iomgr, int ix)
{
    int ix_from; int ix_to;
    removeCompactClosureTable(iomgr->cap, &iomgr->aiop_table, ix,
                              &ix_from, &ix_to);
    if (ix_to != ix_from) {
        StgAsyncIOOp *aiop_to = indexClosureTable(&iomgr->aiop_table, ix_to);
        aiop_to->index = ix_to;
        iomgr->fd_table[ix_to]   = iomgr->fd_table[ix_from];
        iomgr->fd_table[ix_from] = (struct fd_table_entry) {
                                     .fd = -1,
                                     .rw = 0
                                   };
    }
}


static int collectFdSets(CapIOManager *iomgr)
{
    int maxfd    = -1;
    int nentries = sizeClosureTable(&iomgr->aiop_table);
    struct fd_table_entry *fd_table = iomgr->fd_table;

    /* In principle we could optimise this slightly by not resetting the
     * whole of each fdset, by assuming that select() does not modify
     * entries above maxfd. This is probably not worth doing however, since
     * this I/O manager is supposed to be portable and is expected to be slow.
     */
    FD_ZERO(iomgr->rfds);
    FD_ZERO(iomgr->wfds);

#if defined(HAVE_PREEMPTION)
    /* We're always interested in our interrupt fd */
    {
        int fd = iomgr->interrupt_fd_r;
        maxfd = (fd > maxfd) ? fd : maxfd;
        FD_SET(fd, iomgr->rfds);
    }
#endif

    for (int i = 0; i < nentries; i++) {
        int           fd = fd_table[i].fd;
        IOReadOrWrite rw = fd_table[i].rw;
        ASSERT(fd != -1); // uninitialised

        // Skip aiops that we already know are failed
        StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, i);
        if (RTS_UNLIKELY(aiop->outcome == IOOpOutcomeFailed)) continue;

        if (rw == IORead) {
            FD_SET(fd, iomgr->rfds);
        } else {
            FD_SET(fd, iomgr->wfds);
        }
        maxfd = (fd > maxfd) ? fd : maxfd;
    }
    return maxfd;
}


/* Helper function to check if the fd is out of range for select().
 */
static bool checkFdRange(int fd)
{
    /* On older FreeBSDs, FD_SETSIZE is unsigned. Cast it to signed int
     * in order to switch off the 'comparison between signed and
     * unsigned error message
     * Newer versions of FreeBSD have switched to unsigned int:
     *   https://github.com/freebsd/freebsd/commit/12ae7f74a071f0439763986026525094a7032dfd
     *   http://fa.freebsd.cvs-all.narkive.com/bCWNHbaC/svn-commit-r265051-head-sys-sys
     * So the (int) cast should be removed across the code base once
     * GHC requires a version of FreeBSD that has that change in it.
     */
    return ((fd >= 0) && (fd < (int)FD_SETSIZE));
    /* TODO: on several platforms, it is possible to use a larger fd set size.
       For example on OSX:
       https://code.saghul.net/2016/05/libuv-internals-the-osx-select2-trick/
       And probably similar on other platforms. It basically amounts to looking
       through the representation abstraction of fd_set and to know that it is
       indeed a bit set, and then we can simply allocate it and manipulte it
       ourselves. We could do this, dynamically (re-)allocate the size.
     */
}

#endif /* IOMGR_ENABLED_SELECTBIS */
