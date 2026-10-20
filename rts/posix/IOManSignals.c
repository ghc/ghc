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
#include <string.h>

#endif

/* This curious flag is provided for the benefit of the Haskell binding
 * to POSIX.1 to control whether or not to include SA_NOCLDSTOP when
 * installing a SIGCHLD handler.
 */
HsInt nocldstop = 0;

#if defined(RTS_USER_SIGNALS)

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

static void
generic_handler(int sig USED_IF_THREADS,
                siginfo_t *info,
                void *p STG_UNUSED)
{
#if defined(THREADED_RTS)
    //TODO: This calls MIO directly. We should go via IOManager API.
    // The IOManager API should be extended to cover signals.
    timerManagerNotifySignal(sig, info);
#else /* not THREADED_RTS */

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

#endif /* THREADED_RTS */
}

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
stg_sig_install(int sig, int spi, void *mask)
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
        action.sa_sigaction = generic_handler;
        action.sa_flags |= SA_SIGINFO;
        break;

    default:
        barf("stg_sig_install: bad spi");
    }

    if (mask != NULL) {
        action.sa_mask = *(sigset_t *)mask;
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

    info = stgMallocBytes(sizeof(siginfo_t), "startPendingSignalHandlers");
           // freed by runHandler
    memcpy(info, next_pending_handler, sizeof(siginfo_t));

    debugTrace(DEBUG_iomanager,
               "Handling pending signal, no %d", info->si_signo);

    StgTSO *t =
        createIOThread(cap,
                       RtsFlags.GcFlags.initialStkSize,
                       rts_apply(cap,
                                 rts_apply(cap,
                                           runHandlersPtr_closure,
                                           rts_mkPtr(cap, info)),
                                 rts_mkInt(cap, info->si_signo)));
    scheduleThread(cap, t);
    setThreadLabel(cap, t, "signal handler thread");
  }

  unblockUserSignals();
}
#endif

#else /* !RTS_USER_SIGNALS */
StgInt
stg_sig_install(StgInt sig STG_UNUSED,
                StgInt spi STG_UNUSED,
                void* mask STG_UNUSED)
{
  //barf("User signals not supported");
  return STG_SIG_DFL;
}

#endif
