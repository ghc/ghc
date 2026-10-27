/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020-2026
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
#include "Trace.h"

#include "Poll.h"

#include <unistd.h>
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
#include "IOManSignals.h"
#include "Timeout.h"
#include "FdWakeup.h"

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
    int interrupt_fd_r, interrupt_fd_w;
    int n_special_fds;

We also support the Linux-specific ppoll API which supports higher resolution
time delays -- nanoseconds rather than milliseconds as in classic poll(). It
also allows the signal mask to be adjusted, but we do not make use of this.

   int ppoll(struct pollfd *fds, nfds_t nfds,
           const struct timespec *tmo_p, const sigset_t *sigmask);

In addition to aiops, there are some special internal fds we have to monitor
and respond to. In particular there is the interrupt_fd_r used to implement
interruptIOManager. Whether this fd is used or not depends on configuration
(use of interrupt_fd_r depends on HAVE_PREEMPTION). To enable multiple such
fds and allow them to be optional we use the following scheme: we create
special internal AsyncIOOps and put them into the aiop_table. These are
treated much like other IOOpCodeWaitRead operations but with special handling
for notification. The variable n_special_fds tracks how many such entries we
have so that we can handle them specially but in a reasonably uniform manner.

******************************************************************************/

/* Forward declarations */
static bool enlargeTables(CapIOManager *iomgr);
static void notifyIOCompletion(CapIOManager *iomgr, StgAsyncIOOp *aiop);
static void removeFromTables(CapIOManager *iomgr, int i);
static void reportPollError(int res, nfds_t nfds) STG_NORETURN;

enum SpecialFd {
    InterruptFd,  /* iomgr->interrupt_fd_r for interruptIOManager */
    SignalFd      /* iomgr->shared->signal_fd_r */
};
static void insertSpecialFd(CapIOManager  *iomgr,
                            enum SpecialFd special_fd_kind,
                            int            special_fd);
static void processSpecialFd(CapIOManager *iomgr,
                             StgAsyncIOOp *aiop, bool *interrupt);


/* Called early in RTS startup _before_ the storage manager is initialised,
 * so this is not allowed to allocate on the GC heap.
 */
void initCapabilityIOManagerPoll(CapIOManager *iomgr)
{
    initClosureTable(&iomgr->aiop_table, ClosureTableCompact);
    iomgr->timeout_queue = emptyTimeoutQueue();

#if defined(HAVE_PREEMPTION)
    newFdWakeup(&iomgr->interrupt_fd_r, &iomgr->interrupt_fd_w);
#endif

    iomgr->n_special_fds = 0
#if defined(HAVE_PREEMPTION)
        + 1
#endif
#if defined(RTS_USER_SIGNALS)
        + (iomgr->cap->no == 0 ? 1 : 0)
#endif
        ;

#if defined(RTS_USER_SIGNALS)
    /* Signal handlers are shared across all caps. Only init once. */
    if (iomgr->cap->no == 0) {
        initUserSignals();
        /* See Note [Non-racy signal handling using the self-pipe trick] */
        newNonBlockingPipe(&iomgr->shared->signal_fd_r,
                           &iomgr->shared->signal_fd_w);
    }
#endif
}


/* Called late in RtsStartup, once heap allocation is ready. */
void startCapabilityIOManagerPoll(CapIOManager *iomgr)
{
    /* ClosureTable allocates on the heap, so we can do it here not in init */
    if (iomgr->n_special_fds > 0) {
        bool ok = enlargeClosureTable(iomgr->cap, &iomgr->aiop_table,
                                      iomgr->n_special_fds);
        CHECK(ok);
    }
    iomgr->aiop_poll_table =
        stgMallocBytes(sizeof(struct pollfd) * iomgr->n_special_fds,
                       "initCapabilityIOManagerPoll");

#if defined(HAVE_PREEMPTION)
    insertSpecialFd(iomgr, InterruptFd, iomgr->interrupt_fd_r);
#endif
#if defined(RTS_USER_SIGNALS)
    if (iomgr->cap->no == 0) {
        /* See Note [Non-racy signal handling using the self-pipe trick] */
        insertSpecialFd(iomgr, SignalFd, iomgr->shared->signal_fd_r);
    }
#endif
    ASSERT(iomgr->n_special_fds == sizeClosureTable(&iomgr->aiop_table));
}


/* For special fds, we add a dummy entry to the aiop_table and a
 * corresponding real entry to the aiop_poll_table.
 */
static void insertSpecialFd(CapIOManager  *iomgr,
                            enum SpecialFd special_fd_kind,
                            int            special_fd)
{
    StgAsyncIOOp *aiop  = (StgAsyncIOOp *)allocate(iomgr->cap,
                                                   sizeofW(StgAsyncIOOp));
    SET_HDR(aiop, &stg_ASYNCIOOP_info, CCS_SYSTEM);
    aiop->notify.unused = &stg_ASYNCIO_LIVE0_closure;
    aiop->live          = &stg_ASYNCIO_LIVE0_closure;
    aiop->capno         = iomgr->cap->no;
    aiop->notify_type   = NotifyInternal;
    aiop->outcome       = IOOpOutcomeInFlight;
    aiop->operation     = IOOpCodeWaitRead;
    aiop->fd            = special_fd;
    aiop->internal      = special_fd_kind;

    int ix = insertClosureTable(iomgr->cap, &iomgr->aiop_table, aiop);
    aiop->index = ix;

    iomgr->aiop_poll_table[ix] = (struct pollfd) {
                                     .fd      = special_fd,
                                     .events  = POLLIN,
                                     .revents = 0
                                 };
}


void freeCapabilityIOManagerPoll(CapIOManager *iomgr)
{
#if defined(RTS_USER_SIGNALS)
    if (iomgr->cap->no == 0) {
        finiUserSignals();
        close(iomgr->shared->signal_fd_r);
        close(iomgr->shared->signal_fd_w);
    }
#endif
    stgFree(iomgr->aiop_poll_table);
#if defined(HAVE_PREEMPTION)
    closeFdWakeup(iomgr->interrupt_fd_r, iomgr->interrupt_fd_w);
#endif
}


/* Used to implement syncIOWaitReady. */
IOSubmitResult syncIOWaitReadyPoll(CapIOManager *iomgr, StgTSO *tso,
                                   enum IOReadOrWrite rw, HsInt fd)
{
    StgAsyncIOOp *aiop;
    aiop = (StgAsyncIOOp *)allocateMightFail(iomgr->cap, sizeofW(StgAsyncIOOp));
    if (RTS_UNLIKELY(aiop == NULL)) return IOSubmitResultHeapOverflow;
    SET_HDR(aiop, &stg_ASYNCIOOP_info, iomgr->cap->r.rCCCS);
    aiop->notify.tso     = tso;
    aiop->notify_type    = NotifyTSO;
    aiop->live           = &stg_ASYNCIO_LIVE0_closure;
    tso->block_info.aiop = aiop;
    RELEASE_STORE(&tso->why_blocked, rw == IORead ? BlockedOnRead
                                                  : BlockedOnWrite);
    return asyncIOWaitReadyPoll(iomgr, aiop, rw, fd);
}

IOSubmitResult asyncIOWaitReadyPoll(CapIOManager *iomgr, StgAsyncIOOp *aiop,
                                    enum IOReadOrWrite rw, int fd)
{
    if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
        bool ok = enlargeTables(iomgr);
        if (RTS_UNLIKELY(!ok)) return IOSubmitResultHeapOverflow;
    }

    int ix = insertClosureTable(iomgr->cap, &iomgr->aiop_table, aiop);

    /* We use the aiop_table and aiop_poll_table densely. */
    ASSERT(ix == sizeClosureTable(&iomgr->aiop_table) - 1);

    /* The syncIO wrapper or CMM primop filled in the notify and live fields,
     * we fill the rest.
     */
    aiop->capno     = iomgr->cap->no;
    aiop->index     = ix;
    aiop->outcome   = IOOpOutcomeInFlight;
    aiop->operation = convIOReadOrWriteToIOOpCode(rw);
    aiop->fd        = fd;

    /* Fill in the corresponding entry in the aiop_poll_table */
    iomgr->aiop_poll_table[ix] = (struct pollfd) {
                                   .fd      = fd,
                                   .events  = rw == IORead ? POLLIN : POLLOUT,
                                   .revents = 0
                                 };
    return IOSubmitResultAsyncContinue;
}


void syncIOCancelPoll(CapIOManager *iomgr, StgTSO *tso)
{
    StgAsyncIOOp *aiop  = tso->block_info.aiop;
    ASSERT(aiop->notify_type == NotifyTSO);
    ASSERT(indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop);
    asyncIOCancelPoll(iomgr, aiop);
}


void asyncIOCancelPoll(CapIOManager *iomgr, StgAsyncIOOp *aiop)
{
    /* We can reliably determine if the aiop is still in progress by checking
     * if the aiop_table still points to this aiop object. This is reliable
     * because each aiop is GC heap allocated, so cannot be recycled until it
     * is no longer retained by the application.
     */
    if (indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop) {
        removeFromTables(iomgr, aiop->index);
        aiop->outcome = IOOpOutcomeCancelled;
        notifyIOCompletion(iomgr, aiop);
    }
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

            /* Fill in the outcome and result/error on the TSO's stack frame */
            setTsoIOOpOutcome(tso, aiop->outcome, aiop->result);
            pushOnRunQueue(iomgr->cap, tso);
            RELEASE_STORE(&tso->why_blocked, NotBlocked);

            /* For the TSO case, the aiop was only reachable from the TSO
             * itself, and thus it is now no longer be reachable at all.
             */
            IF_NONMOVING_WRITE_BARRIER_ENABLED {
                updateRemembSetPushClosure(iomgr->cap, (StgClosure *)aiop);
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


/* Called from poll/awaitCompletedTimeoutsOrIOPoll after a successful poll()
 * call to process all the I/O completions.
 *
 * We match up the I/O completion notifications from the system call against
 * the pending I/O operations from the aiop_table, and use notifyIOCompletion
 * on each completed aiop.
 *
 * Returns true if the poll() was interupted via iomgr->interrupt_fd_r.
 */
static bool processIOCompletions(CapIOManager *iomgr, int ncompletions)
{
    /* The scheme we use with poll is that we have a dense poll table, and a
     * corresponding table that maps to the closure table index. The poll
     * table entries correspond to individual notification requests. It is not
     * grouped by fd. So we simply iterate over the poll table, indexing the
     * corresponding aiop table entry and notifying.
     */
    debugTrace(DEBUG_iomanager, "processIOCompletions(ncompletions = %d)",
                                ncompletions);
    ASSERT(ncompletions > 0);

    bool interrupt = false;

    struct pollfd *aiop_poll_table = iomgr->aiop_poll_table;
    int i = 0;
    while (ncompletions > 0 && i < sizeClosureTable(&iomgr->aiop_table)) {
        /* Since each aiop_table entry is for a single (fd, rw) pair, we
         * just need to check if the aiop_poll_table[n].revents contains
         * anything. We don't need to distinguish read vs write.
         */
        if (aiop_poll_table[i].revents) {
            StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, i);

            ASSERT(aiop->operation == IOOpCodeWaitRead ||
                   aiop->operation == IOOpCodeWaitWrite);
            ASSERT(aiop->outcome == IOOpOutcomeInFlight);

            if (aiop->notify_type == NotifyInternal) {
                processSpecialFd(iomgr, aiop, &interrupt);
                ncompletions--;
                i++;
            } else {
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

                removeFromTables(iomgr, i);
                notifyIOCompletion(iomgr, aiop);
                ncompletions--;
            }
        } else {
            /* You'd expect incrementing the table index to be unconditional,
             * but we don't increment the index if we did process the entry,
             * because using removeFromTables means we'll move an entry from
             * the end of the table into the index i.
             */
            i++;
        }
    }
    ASSERT(ncompletions == 0);
    return interrupt;
}


static void processSpecialFd(CapIOManager *iomgr,
                             StgAsyncIOOp *aiop, bool *interrupt)
{
    ASSERT(aiop->notify_type == NotifyInternal);
    enum SpecialFd special_fd_kind = aiop->internal;
    int fd = aiop->fd;

    switch (special_fd_kind) {
#if defined(HAVE_PREEMPTION)
        case InterruptFd:
            ASSERT(fd == iomgr->interrupt_fd_r);
            collectFdWakeup(fd);
            *interrupt = true;
            debugTrace(DEBUG_iomanager, "Received interrupt in poll I/O manager");
            break;
#endif
#if defined(RTS_USER_SIGNALS)
        /* See Note [Non-racy signal handling using the self-pipe trick] */
        case SignalFd:
            ASSERT(fd == iomgr->shared->signal_fd_r);
            int sig; siginfo_t info;
            readSignalInfoFromPipe(&sig, &info, fd);
            startSignalHandler(iomgr->cap, sig, &info);
            debugTrace(DEBUG_iomanager,
                       "Dispatched signal %d in I/O manager", sig);
            break;
#endif
    }
}


void pollCompletedTimeoutsOrIOPoll(CapIOManager *iomgr)
{
    if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time now = getProcessElapsedTime();
        processTimeoutCompletions(iomgr, now);
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
                   "ppoll(nfds = %lu, timeout = 0) = %d",
                   (unsigned long) nfds, res);
#else
        int res = poll(iomgr->aiop_poll_table, nfds, 0);

        debugTrace(DEBUG_iomanager,
                   "poll(nfds = %lu, timeout = 0) = %d",
                   (unsigned long) nfds, res);
#endif
        if (res == 0) {
            /* There is no I/O ready. We'll return to the scheduler. */

        } else if (res > 0) {
            int ncompletions = res;
            ASSERT(ncompletions <= (int)nfds);
            processIOCompletions(iomgr, ncompletions);

        } else if (errno == EINTR) {
          /* We got interrupted by a signal. This is unlikely since we asked
           * poll() not to wait, but if so we'll return to the scheduler.
           */

        } else {
            reportPollError(res, nfds);
        }
    }
}


bool awaitCompletedTimeoutsOrIOPoll(CapIOManager *iomgr)
{
    bool interrupt = false; /* got woken up via interruptIOManager */

    /* Loop until we've woken up some threads. This loop is needed because the
     * poll() timing isn't accurate, we sometimes sleep for a while but not
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

        /* Decide if we are going to wait if no I/O is ready, either:
         * poll only, wait indefinitely, or wait until a timeout.
         */
        Time timeout = timeoutWaitTime(iomgr, wait, now);

        nfds_t nfds = sizeClosureTable(&iomgr->aiop_table);
        int n_aiops = nfds - iomgr->n_special_fds;

        if (RTS_UNLIKELY(timeout == -1 && n_aiops == 0)) {
            /* See Note [Deadlock detection without idle GC] */
            interrupt = notifyIdleGcDeadlock();
            if (interrupt) break;
        }

#if !defined(THREADED_IDLEGC)
        /* Without threaded idle GC, poll() does not get interrupted for an
         * idle GC. Instead we just limit our wait time to the idle gc delay
         * time, and if that timeout is reached then we schedule an idle GC
         * (see handleIdleGcTimeout below).
         */
        bool any_pending_io = n_aiops > 0;
        bool timeout_idlegc_delay;
        adjustTimeoutForIdleGc(any_pending_io, &timeout, &timeout_idlegc_delay);
#endif

        /* Check for I/O readiness, possibly waiting. */
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
        struct timespec ts, *timeout_ns = timeoutAsTimespec(timeout, &ts);
        int res = ppoll(iomgr->aiop_poll_table, nfds, timeout_ns, NULL);

        debugTrace(DEBUG_iomanager,
                   "ppoll(nfds = %lu, timeout = %" FMT_Int64 " ns) = %d",
                   (unsigned long) nfds, timeout, res);
#else
        int timeout_ms = timeoutAsPollTimeout(timeout);
        int res = poll(iomgr->aiop_poll_table, nfds, timeout_ms);

        debugTrace(DEBUG_iomanager,
                   "poll(nfds = %lu, timeout = %d ms) = %d",
                   (unsigned long) nfds, timeout_ms, res);
#endif

        if (res == 0) {
            /* Success but there is no I/O ready. This can happen either if we
             * were not blocking or were in a timed wait and the timeout
             * occurred before any I/O became ready. Either way, the do-while
             * loop condition will handle it.
             */
            ASSERT(timeout != -1);
#if !defined(THREADED_IDLEGC)
            handleIdleGcTimeout(timeout_idlegc_delay, &interrupt);
#endif

        } else if (res > 0) {
            int ncompletions = res;
            ASSERT(ncompletions <= (int)nfds);
            interrupt = processIOCompletions(iomgr, ncompletions);
            /* We do _not_ need to check for timeouts again here. We know that
             * poll() returned due to I/O being ready before the timeout. If we
             * were woken late by the OS then we may be past the first timer
             * wake time, but we do _not_ eagerly process the timer. This is
             * probably the desired behaviour for "timeout $ do someio".
             */

        } else if (errno == EINTR) {
            /* We got interrupted by a signal.
             *
             * Signals of note:
             *  + signals with a user handler: these will be delivered to us
             *    via the signal_fd_r pipe, which we handle by starting new
             *    threads, which will terminate the loop (non-empty run queue).
             *  + The backup ctl-c signal handler sets sched_state, and to avoid
             *    races, it also calls interruptIOManagerPoll (which causes
             *    interrupt_fd_r to become ready). We're in the EINTR case here
             *    so we know the signal arrived while we were waiting, so it is
             *    the getSchedState condition that will will terminate the loop.
             *    (If the signal arrives before the call to poll then we will
             *    notice interrupt_fd_r is ready and set interrupt=true, which
             *    will terminate the loop).
             *
             * For any other interruption we will go round the do-while loop
             * again.
             */

        } else {
            reportPollError(res, nfds);
        }

    } while (emptyRunQueue(iomgr->cap)
         && !interrupt
         && (getSchedState() == SCHED_RUNNING));
    return !interrupt;
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


void interruptIOManagerPoll(CapIOManager *iomgr USED_IF_PREEMPTION)
{
#if defined(HAVE_PREEMPTION)
    sendFdWakeup(iomgr->interrupt_fd_w);
#endif
}


#if defined(RTS_USER_SIGNALS)
static void signal_handler(int sig, siginfo_t *info, void *p STG_UNUSED)
{
    int signal_fd_w = getCapability(0)->iomgr->shared->signal_fd_w;
    writeSignalInfoToPipe(sig, info, signal_fd_w);
}


int installUserSignalHandlerPoll(int sig, int spi, sigset_t *mask,
                                 bool nocldstop)
{
    return installSignalHandler(sig, spi, mask, nocldstop, signal_handler);
}
#endif


/* Helper function to double the size of the aiop_table and aiop_poll_table.
 */
static bool enlargeTables(CapIOManager *iomgr)
{
    int oldcapacity = capacityClosureTable(&iomgr->aiop_table);
    int newcapacity = (oldcapacity == 0) ? 1 : (oldcapacity * 2);

    bool ok = enlargeClosureTable(iomgr->cap, &iomgr->aiop_table, newcapacity);
    if (RTS_UNLIKELY(!ok)) return false;

    /* Update the auxiliary aiop_poll_table to match. */
    iomgr->aiop_poll_table =
        stgReallocBytes(iomgr->aiop_poll_table,
                        sizeof(struct pollfd) * newcapacity,
                        "Poll.c: enlargeTables");

    /* Initialise the new part of the aiop_poll_table */
    struct pollfd *aiop_poll_table = iomgr->aiop_poll_table;
    for (int i = oldcapacity; i < newcapacity; i++) {
        aiop_poll_table[i] = (struct pollfd) {
                               .fd      = -1,
                               .events  = 0,
                               .revents = 0
                             };
    }
    return true;
}


/* Remove from the completion table, preserving compactness, and apply the same
 * compacting to the aiop_poll_table.
 */
static void removeFromTables(CapIOManager *iomgr, int ix)
{
    int ix_from; int ix_to;
    removeCompactClosureTable(iomgr->cap, &iomgr->aiop_table, ix,
                              &ix_from, &ix_to);
    if (ix_to != ix_from) {
        StgAsyncIOOp *aiop_to = indexClosureTable(&iomgr->aiop_table, ix_to);
        aiop_to->index = ix_to;
        iomgr->aiop_poll_table[ix_to] = iomgr->aiop_poll_table[ix_from];
    }
}

#endif /* IOMGR_ENABLED_POLL */
