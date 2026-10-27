/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#if defined(RTS_USER_SIGNALS)

#include "RtsSignals.h"
#include "IOManSignals.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#include "Libdw.h"
#include "MIO.h"

#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>

/* -----------------------------------------------------------------------------
 * The table of signal handlers
 * -------------------------------------------------------------------------- */

/* SUP: The type of handlers is a little bit, well, doubtful... */
StgInt *signal_handlers = NULL; /* Dynamically grown array of signal handlers */
static StgInt nHandlers = 0;    /* Size of handlers array */

static sigset_t userSignals;
static sigset_t savedSignals;

#if defined(THREADED_RTS)
static Mutex sig_mutex; // protects signal_handlers, nHandlers
#endif

/* -----------------------------------------------------------------------------
 * Initialisation / deinitialisation
 * -------------------------------------------------------------------------- */

void
initUserSignals(void)
{
    sigemptyset(&userSignals);
#if defined(THREADED_RTS)
    initMutex(&sig_mutex);
#endif
}

void finiUserSignals(void)
{
    if (signal_handlers != NULL) {
        stgFree(signal_handlers);
        signal_handlers = NULL;
        nHandlers = 0;
    }
#if defined(THREADED_RTS)
    closeMutex(&sig_mutex);
#endif
};

/* -----------------------------------------------------------------------------
 * Allocate/resize the table of signal handlers.
 * -------------------------------------------------------------------------- */

static void
more_handlers(int sig)
{
    StgInt i;

    if (sig < nHandlers)
        return;

    if (signal_handlers == NULL)
        signal_handlers = (StgInt *)stgMallocBytes((sig + 1) * sizeof(StgInt),
                                                   "more_handlers");
    else
        signal_handlers = (StgInt *)stgReallocBytes(signal_handlers,
                                                    (sig + 1) * sizeof(StgInt),
                                                    "more_handlers");

    for(i = nHandlers; i <= sig; i++)
        // Fill in the new slots with default actions
        signal_handlers[i] = STG_SIG_DFL;

    nHandlers = sig + 1;
}

#if !defined(THREADED_RTS)

#define N_PENDING_HANDLERS 16

siginfo_t pending_handler_buf[N_PENDING_HANDLERS];
siginfo_t *next_pending_handler = pending_handler_buf;

static inline bool signals_pending(void) {
    return (next_pending_handler != pending_handler_buf);
}

#endif /* THREADED_RTS */

/* -----------------------------------------------------------------------------
 * Low-level signal handler
 *
 * Places the requested handler on a stack of pending handlers to be
 * started up at the next context switch.
 * -------------------------------------------------------------------------- */

#if !defined(THREADED_RTS)
void
generic_handler(int sig USED_IF_THREADS,
                siginfo_t *info,
                void *p STG_UNUSED)
{

    /* Can't call allocate from here.  Probably can't call malloc
       either.  However, we have to schedule a new thread somehow.

       It's probably ok to request a context switch and allow the
       scheduler to  start the handler thread, but how do we
       communicate this to the scheduler?

       We need some kind of locking, but with low overhead (i.e. no
       blocking signals every time around the scheduler).

       Signal Handlers are atomic (i.e. they can't be interrupted), and
       we can make use of this.  We just need to make sure the
       critical section of the scheduler can't be interrupted - the
       only way to do this is to block signals.  However, we can lower
       the overhead by only blocking signals when there are any
       handlers to run, i.e. the set of pending handlers is
       non-empty.
    */

    /* We use a stack to store the pending signals.  We can't
       dynamically grow this since we can't allocate any memory from
       within a signal handler.

       Hence unfortunately we have to bomb out if the buffer
       overflows.  It might be acceptable to carry on in certain
       circumstances, depending on the signal.
    */

    memcpy(next_pending_handler, info, sizeof(siginfo_t));

    next_pending_handler++;

    // stack full?
    if (next_pending_handler == &pending_handler_buf[N_PENDING_HANDLERS]) {
        errorBelch("too many pending signals");
        stg_exit(EXIT_FAILURE);
    }

    interruptCapability(&MainCapability);

}
#endif /* THREADED_RTS */

/* -----------------------------------------------------------------------------
 * Blocking/Unblocking of the user signals
 * -------------------------------------------------------------------------- */

void
blockUserSignals(void)
{
    pthread_sigmask(SIG_BLOCK, &userSignals, &savedSignals);
}

void
unblockUserSignals(void)
{
    pthread_sigmask(SIG_SETMASK, &savedSignals, NULL);
}

/* -----------------------------------------------------------------------------
 * Install a Haskell signal handler.
 *
 * We should really do this in Haskell in GHC.Conc, and share the
 * signal_handlers array with the one there.
 *
 * -------------------------------------------------------------------------- */

int
installSignalHandler(int sig, int spi, sigset_t *mask, bool nocldstop,
                     SignalHandler signal_handler)
{
    ACQUIRE_LOCK(&sig_mutex);

    // Use sigaddset as a way to validate the signal number.
    // We do not use the resulting signal set.
    {
        sigset_t signals;
        sigemptyset(&signals);
        if (sig < 0 || sigaddset(&signals, sig)) {
            RELEASE_LOCK(&sig_mutex);
            return STG_SIG_ERR;
        }
    }

    more_handlers(sig);

    StgInt previous_spi = signal_handlers[sig];

    struct sigaction action;
    memset(&action, 0, sizeof(struct sigaction));
    action.sa_flags = 0;

    switch(spi) {
    case STG_SIG_IGN:
        action.sa_handler = SIG_IGN;
        break;

    case STG_SIG_DFL:
        action.sa_handler = SIG_DFL;
        break;

    case STG_SIG_RST:
        action.sa_flags |= SA_RESETHAND;
        /* fall through */
    case STG_SIG_HAN:
        action.sa_sigaction = signal_handler;
        action.sa_flags |= SA_SIGINFO;
        break;

    default:
        barf("stg_sig_install: bad spi");
    }

    if (mask != NULL) {
        action.sa_mask = *mask;
    } else {
        sigemptyset(&action.sa_mask);
    }

    action.sa_flags |= sig == SIGCHLD && nocldstop ? SA_NOCLDSTOP : 0;

    if (sigaction(sig, &action, NULL)) {
        errorBelch("sigaction");
        RELEASE_LOCK(&sig_mutex);
        return STG_SIG_ERR;
    }

    signal_handlers[sig] = spi;

    switch(spi) {
    case STG_SIG_RST:
    case STG_SIG_HAN:
        sigaddset(&userSignals, sig);
        break;

    default:
        sigdelset(&userSignals, sig);
        break;
    }

    RELEASE_LOCK(&sig_mutex);
    return previous_spi;
}

/* -----------------------------------------------------------------------------
 * Creating new threads for signal handlers.
 * -------------------------------------------------------------------------- */

#if !defined(THREADED_RTS)
void
startPendingSignalHandlers(Capability *cap)
{
  siginfo_t *info;
  int sig;

  if (!signals_pending()) return;

  blockUserSignals();

  while (next_pending_handler != pending_handler_buf) {

    next_pending_handler--;

    sig = next_pending_handler->si_signo;
    if (signal_handlers[sig] == STG_SIG_DFL) {
        continue; // handler has been changed.
    }

    info = next_pending_handler;
    startSignalHandler(cap, sig, info);
  }

  unblockUserSignals();
}
#endif

void startSignalHandler(Capability *cap, int sig, siginfo_t *info)
{
    /* Allocate a copy. This is freed by runHandler. */
    // TODO: this could be improved to use a GC'd pinned byte array, but this
    // would need coordination with the unix library.
    siginfo_t *info_copy = stgMallocBytes(sizeof(siginfo_t), "startSignalHandler");
    *info_copy = *info;

    /* The info structure can be zero on solaris. So at least set the signo. */
    info_copy->si_signo = sig;

    debugTrace(DEBUG_iomanager,
               "Handling pending signal, no %d", info_copy->si_signo);

    StgTSO *t =
        createIOThread(cap,
                       RtsFlags.GcFlags.initialStkSize,
                       rts_apply(cap,
                                 rts_apply(cap,
                                           runHandlersPtr_closure,
                                           rts_mkPtr(cap, info_copy)),
                                 rts_mkInt(cap, info_copy->si_signo)));
    scheduleThread(cap, t);
    setThreadLabel(cap, t, "signal handler thread");
}

/* Note [Non-racy signal handling using the self-pipe trick]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Reliably picking up unix/posix signals promptly is surprisingly tricky!
 * The standard problem is that there is a race between signal delivery and
 * waiting/blocking in something like select/poll/epoll_wait. The problem is
 * that if one checks for a signal having been delivered (e.g. by looking at a
 * shared variable set by the signal handler) just before calling select/poll
 * then there's a time window between that point and calling select/poll when
 * a signal could be delivered and then not noticed until select/poll returns.
 * Once we've called select/poll then it can return EINTR upon signal arrival,
 * but the window before remains a problem.
 *
 * One solution is to use pselect/ppoll: mask signals, then do a final check
 * for signals having been delivered, then call pselect/ppoll to unmask
 * signals (atomically) for the duration of the wait. This is fine in a single
 * threaded process. In a multi-threaded process, masking signals doesn't work.
 * See Note [pthread_sigmask and the ineffectiveness of masking signals].
 *
 * The standard solution is the "self-pipe trick" where the low level signal
 * handler writes the signal info into a pipe, and poll/select monitors that
 * pipe. This neatly eliminates the window where signals could be missed. If
 * the signal arrives before calling poll/select then the pipe is readable and
 * poll/select will return. If the signal arrives during the poll/select call
 * then they will return with EINTR.
 *
 * In the RTS context, this is done by the I/O managers: the in-RTS ones use
 * newNonBlockingPipe on startup, have their low level signal handler use
 * writeSignalInfoToPipe, and when the I/O manager finds the other end of the
 * pipe is readable then it calls readSignalInfoFromPipe, and can then use
 * startSignalHandler.
 *
 * The in-Haskell MIO I/O manager does essentially the same.
 *
 * See issue #27540.
 */

void newNonBlockingPipe(int *pipe_fd_r, int *pipe_fd_w)
{
    int pipefd[2];
#if defined(HAVE_PIPE2)
    int res = pipe2(pipefd, O_CLOEXEC | O_NONBLOCK);
#else
    int res = pipe(pipefd);
#endif
    if (RTS_UNLIKELY(res < 0)) {
        sysErrorBelch("newNonBlockingPipe");
        stg_exit(EXIT_FAILURE);
    }
#if !defined(HAVE_PIPE2)
    for (int i = 0; i < 2; i++) {
        int res1 = fcntl(pipefd[i], F_SETFD, FD_CLOEXEC);
        int res2 = fcntl(pipefd[i], F_SETFL, O_NONBLOCK);
        if (RTS_UNLIKELY(res1 < 0 || res2 < 0)) {
            sysErrorBelch("newFdWakeup fcntl()");
            stg_exit(EXIT_FAILURE);
        }
    }
#endif
    *pipe_fd_r = pipefd[0]; /* read end */
    *pipe_fd_w = pipefd[1]; /* write end */
}

/* Write the signal number and siginfo into the given fd (which is
 * typically a pipe in non-blocking mode).
 */
void writeSignalInfoToPipe(int sig, siginfo_t *info, int fd)
{
    StgWord8 buf[sizeof(siginfo_t) + 1];

    buf[0] = sig;
    if (info) {
        memcpy(buf+1, info, sizeof(siginfo_t));
    } else {
        // info may be NULL on Solaris (see #3790)
        memset(buf+1, 0, sizeof(siginfo_t));
    }

    int r = write(fd, buf, sizeof(siginfo_t)+1);
    if (r == -1 && errno == EAGAIN) {
        errorBelch("lost signal due to full pipe: %d\n", sig);
    }
}

bool readSignalInfoFromPipe(int *sig, siginfo_t *info, int fd)
{
    StgWord8 buf[sizeof(siginfo_t) + 1];

    int r = read(fd, buf, sizeof(siginfo_t)+1);

    if (r == sizeof(siginfo_t)+1) {
        *sig = (int) buf[0];
        memcpy(info, buf+1, sizeof(siginfo_t));
        return true;
    } else {
        // TODO: if r is >0 and < expected then we're kind of screwed
        // it means we're all out of sync with the protocol.
        // it _should_ never happen of course!
        return false;
    }
}

#endif /* RTS_USER_SIGNALS */
