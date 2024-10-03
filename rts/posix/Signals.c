/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Schedule.h"
#include "RtsSignals.h"
#include "Signals.h"
#include "IOManager.h"
#include "RtsUtils.h"
#include "Prelude.h"
#include "Ticker.h"
#include "ThreadLabels.h"
#include "Libdw.h"

/* TODO: eliminate this include. This file should be about signals, not be
 * part of an I/O manager implementation. The code here that are really part
 * of an I/O manager should be moved into an appropriate I/O manager impl.
 */
#include "IOManagerInternals.h"

#if defined(alpha_HOST_ARCH)
# if defined(linux_HOST_OS)
#  include <fenv.h>
# else
#  include <machine/fpu.h>
# endif
#endif

#if defined(HAVE_UNISTD_H)
# include <unistd.h>
#endif

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#if defined(HAVE_ERRNO_H)
# include <errno.h>
#endif

#if defined(HAVE_EVENTFD_H)
# include <sys/eventfd.h>
#endif

#if defined(HAVE_TERMIOS_H)
#include <termios.h>
#endif

#include <stdlib.h>
#include <string.h>

/* This curious flag is provided for the benefit of the Haskell binding
 * to POSIX.1 to control whether or not to include SA_NOCLDSTOP when
 * installing a SIGCHLD handler.
 */
HsInt nocldstop = 0;

/* -----------------------------------------------------------------------------
 * The table of signal handlers
 * -------------------------------------------------------------------------- */

#if defined(RTS_USER_SIGNALS)

/* SUP: The type of handlers is a little bit, well, doubtful... */
StgInt *signal_handlers = NULL; /* Dynamically grown array of signal handlers */
static StgInt nHandlers = 0;    /* Size of handlers array */

static uint32_t n_haskell_handlers = 0;

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

void
freeSignalHandlers(void) {
    if (signal_handlers != NULL) {
        stgFree(signal_handlers);
        signal_handlers = NULL;
        nHandlers = 0;
        n_haskell_handlers = 0;
    }
#if defined(THREADED_RTS)
    closeMutex(&sig_mutex);
#endif
}

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

// Here's the pipe into which we will send our signals
static int io_manager_wakeup_fd = -1;
static int timer_manager_control_wr_fd = -1;

#define IO_MANAGER_WAKEUP 0xff
#define IO_MANAGER_DIE    0xfe
#define IO_MANAGER_SYNC   0xfd

void setTimerManagerControlFd(int fd) {
    RELAXED_STORE(&timer_manager_control_wr_fd, fd);
}

void
setIOManagerWakeupFd (int fd)
{
    // only called when THREADED_RTS, but unconditionally
    // compiled here because GHC.Event.Control depends on it.
    SEQ_CST_STORE(&io_manager_wakeup_fd, fd);
}

/* -----------------------------------------------------------------------------
 * Wake up at least one IO or timer manager HS thread.
 * -------------------------------------------------------------------------- */
void
ioManagerWakeup (void)
{
    int r;
    const int wakeup_fd = SEQ_CST_LOAD(&io_manager_wakeup_fd);
    // Wake up the IO Manager thread by sending a byte down its pipe
    if (wakeup_fd >= 0) {
#if defined(HAVE_EVENTFD)
        StgWord64 n = (StgWord64)IO_MANAGER_WAKEUP;
        r = write(wakeup_fd, (char *) &n, 8);
#else
        StgWord8 byte = (StgWord8)IO_MANAGER_WAKEUP;
        r = write(wakeup_fd, &byte, 1);
#endif
        /* N.B. If the TimerManager is shutting down as we run this
         * then there is a possibility that our first read of
         * io_manager_wakeup_fd is non-negative, but before we get to the
         * write the file is closed. If this occurs, io_manager_wakeup_fd
         * will be written into with -1 (GHC.Event.Control does this prior
         * to closing), so checking this allows us to distinguish this case.
         * To ensure we observe the correct ordering, we declare the
         * io_manager_wakeup_fd as volatile.
         * Since this is not an error condition, we do not print the error
         * message in this case.
         */
        if (r == -1 && SEQ_CST_LOAD(&io_manager_wakeup_fd) >= 0) {
            sysErrorBelch("ioManagerWakeup: write");
        }
    }
}

#if defined(THREADED_RTS)
void
ioManagerDie (void)
{
    StgWord8 byte = (StgWord8)IO_MANAGER_DIE;
    uint32_t i;
    int r;

    {
        // Shut down timer manager
        const int fd = RELAXED_LOAD(&timer_manager_control_wr_fd);
        if (0 <= fd) {
            r = write(fd, &byte, 1);
            if (r == -1) { sysErrorBelch("ioManagerDie: write"); }
            RELAXED_STORE(&timer_manager_control_wr_fd, -1);
        }
    }

    {
        // Shut down IO managers
        for (i=0; i < getNumCapabilities(); i++) {
            const int fd = RELAXED_LOAD(&getCapability(i)->iomgr->control_fd);
            if (0 <= fd) {
                r = write(fd, &byte, 1);
                if (r == -1) { sysErrorBelch("ioManagerDie: write"); }
                RELAXED_STORE(&getCapability(i)->iomgr->control_fd, -1);
            }
        }
    }
}

void
ioManagerStartCap (Capability **cap)
{
    rts_evalIO(cap,&ghczminternal_GHCziInternalziConcziIO_ensureIOManagerIsRunning_closure,NULL);
}

void
ioManagerStart (void)
{
    // Make sure the IO manager thread is running
    Capability *cap;
    if (SEQ_CST_LOAD(&timer_manager_control_wr_fd) < 0 || SEQ_CST_LOAD(&io_manager_wakeup_fd) < 0) {
        cap = rts_lock();
        ioManagerStartCap(&cap);
        rts_unlock(cap);
    }
}
#endif

#if !defined(THREADED_RTS)

#define N_PENDING_HANDLERS 16

siginfo_t pending_handler_buf[N_PENDING_HANDLERS];
siginfo_t *next_pending_handler = pending_handler_buf;

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

    StgWord8 buf[sizeof(siginfo_t) + 1];
    int r;

    buf[0] = sig;
    if (info == NULL) {
        // info may be NULL on Solaris (see #3790)
        memset(buf+1, 0, sizeof(siginfo_t));
    } else {
        memcpy(buf+1, info, sizeof(siginfo_t));
    }

    int timer_control_fd = RELAXED_LOAD(&timer_manager_control_wr_fd);
    if (0 <= timer_control_fd)
    {
        r = write(timer_control_fd, buf, sizeof(siginfo_t)+1);
        if (r == -1 && errno == EAGAIN) {
            errorBelch("lost signal due to full pipe: %d\n", sig);
        }
    }

    // If the IO manager hasn't told us what the FD of the write end
    // of its pipe is, there's not much we can do here, so just ignore
    // the signal..

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
    sigprocmask(SIG_BLOCK, &userSignals, &savedSignals);
}

void
unblockUserSignals(void)
{
    sigprocmask(SIG_SETMASK, &savedSignals, NULL);
}

bool
anyUserHandlers(void)
{
    return n_haskell_handlers != 0;
}

#if !defined(THREADED_RTS)
void
awaitUserSignals(void)
{
    while (!signals_pending() && getSchedState() == SCHED_RUNNING) {
        pause();
    }
}
#endif

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
    sigset_t signals, osignals;
    StgInt previous_spi;

    struct sigaction action;
    memset(&action, 0, sizeof(struct sigaction));

    ACQUIRE_LOCK(&sig_mutex);

    // Block the signal until we figure out what to do
    // Count on this to fail if the signal number is invalid
    if (sig < 0 ||
          sigemptyset(&signals) ||
          sigaddset(&signals, sig) ||
          sigprocmask(SIG_BLOCK, &signals, &osignals)) {
        RELEASE_LOCK(&sig_mutex);
        return STG_SIG_ERR;
    }

    more_handlers(sig);

    previous_spi = signal_handlers[sig];

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

    if (mask != NULL)
        action.sa_mask = *(sigset_t *)mask;
    else
        sigemptyset(&action.sa_mask);

    action.sa_flags |= sig == SIGCHLD && nocldstop ? SA_NOCLDSTOP : 0;

    if (sigaction(sig, &action, NULL))
    {
        errorBelch("sigaction");
        RELEASE_LOCK(&sig_mutex);
        return STG_SIG_ERR;
    }

    signal_handlers[sig] = spi;

    switch(spi) {
    case STG_SIG_RST:
    case STG_SIG_HAN:
        sigaddset(&userSignals, sig);
        if (previous_spi != STG_SIG_HAN && previous_spi != STG_SIG_RST) {
            n_haskell_handlers++;
        }
        break;

    default:
        sigdelset(&userSignals, sig);
        if (previous_spi == STG_SIG_HAN || previous_spi == STG_SIG_RST) {
            n_haskell_handlers--;
        }
        break;
    }

    if (sigprocmask(SIG_SETMASK, &osignals, NULL))
    {
        errorBelch("sigprocmask");
        RELEASE_LOCK(&sig_mutex);
        return STG_SIG_ERR;
    }

    RELEASE_LOCK(&sig_mutex);
    return previous_spi;
}

/* -----------------------------------------------------------------------------
 * Creating new threads for signal handlers.
 * -------------------------------------------------------------------------- */

#if !defined(THREADED_RTS)
void
startSignalHandlers(Capability *cap)
{
  siginfo_t *info;
  int sig;

  blockUserSignals();

  while (next_pending_handler != pending_handler_buf) {

    next_pending_handler--;

    sig = next_pending_handler->si_signo;
    if (signal_handlers[sig] == STG_SIG_DFL) {
        continue; // handler has been changed.
    }

    info = stgMallocBytes(sizeof(siginfo_t), "startSignalHandlers");
           // freed by runHandler
    memcpy(info, next_pending_handler, sizeof(siginfo_t));

    StgTSO *t =
        createIOThread(cap,
                       RtsFlags.GcFlags.initialStkSize,
                       rts_apply(cap,
                                 rts_apply(cap,
                                           &ghczminternal_GHCziInternalziConcziSignal_runHandlersPtr_closure,
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

#if defined(RTS_USER_SIGNALS)
/* -----------------------------------------------------------------------------
 * SIGINT handler.
 *
 * We like to shutdown nicely after receiving a SIGINT, write out the
 * stats, write profiling info, close open files and flush buffers etc.
 * -------------------------------------------------------------------------- */
static void
shutdown_handler(int sig STG_UNUSED)
{
    // If we're already trying to interrupt the RTS, terminate with
    // extreme prejudice.  So the first ^C tries to exit the program
    // cleanly, and the second one just kills it.
    if (getSchedState() >= SCHED_INTERRUPTING) {
        // N.B. we cannot use stg_exit() here as it calls exit() which is not
        // signal-safe. See #23417.
        _exit(EXIT_INTERRUPTED);
    } else {
        interruptStgRts();
    }
}

/* -----------------------------------------------------------------------------
 * SIGQUIT handler.
 *
 * We try to give the user an indication of what we are currently doing
 * in response to SIGQUIT.
 * -------------------------------------------------------------------------- */
static void
backtrace_handler(int sig STG_UNUSED)
{
#if USE_LIBDW
    LibdwSession *session = libdwInit();
    Backtrace *bt = libdwGetBacktrace(session);
    fprintf(stderr, "\nCaught SIGQUIT; Backtrace:\n");
    libdwPrintBacktrace(session, stderr, bt);
    backtraceFree(bt);
    libdwFree(session);
#else
    fprintf(stderr, "This build does not support backtraces.\n");
#endif
}

/* -----------------------------------------------------------------------------
 * An empty signal handler, currently used for SIGPIPE
 * -------------------------------------------------------------------------- */
static void
empty_handler (int sig STG_UNUSED)
{
    // nothing
}

/* -----------------------------------------------------------------------------
   SIGTSTP handling

   When a process is suspended with ^Z and resumed again, the shell
   makes no attempt to save and restore the terminal settings.  So on
   resume, any terminal setting modifications we made (e.g. turning off
   ICANON due to hSetBuffering NoBuffering) may well be lost.  Hence,
   we arrange to save and restore the terminal settings ourselves.

   The trick we use is:
     - catch SIGTSTP
     - in the handler,  kill(getpid(),SIGSTOP)
     - when this returns, restore the TTY settings
   This means we don't have to catch SIGCONT too.

   Note we don't re-throw SIGTSTP, we throw SIGSTOP instead.  This is
   for a few reasons:

      - re-throwing SIGTSTP would require temporarily restoring the
        default sigaction.

      - it doesn't work on certain buggy pthread implementations
        (e.g. OpenBSD).

      - throwing SIGTSTP seems slightly dodgy anyway.

   -------------------------------------------------------------------------- */

static void sigtstp_handler(int sig);
static void set_sigtstp_action (bool handle);

static void
sigtstp_handler (int sig STG_UNUSED)
{
    int fd;
    struct termios ts[3];

    // save the current TTY state for TTYs we modified
    for (fd = 0; fd <= 2; fd++) {
        if (__hscore_get_saved_termios(fd) != NULL) {
            tcgetattr(fd,&ts[fd]);
        }
    }

    // really stop the process now
    kill(getpid(), SIGSTOP);

    // on return, restore the TTY state
    for (fd = 0; fd <= 2; fd++) {
        if (__hscore_get_saved_termios(fd) != NULL) {
            tcsetattr(0,TCSANOW,&ts[fd]);
        }
    }
}

static void
set_sigtstp_action (bool handle)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof(struct sigaction));
    if (handle) {
        sa.sa_handler = sigtstp_handler;
    } else {
        sa.sa_handler = SIG_DFL;
    }
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGTSTP, &sa, NULL) != 0) {
        sysErrorBelch("warning: failed to install SIGTSTP handler");
    }
}

/* Used by ItimerTimerCreate and ItimerSetitimer implementations */
void
install_vtalrm_handler(int sig, TickProc handle_tick)
{
    struct sigaction action;
    memset(&action, 0, sizeof(struct sigaction));

    action.sa_handler = handle_tick;

    sigemptyset(&action.sa_mask);

#if defined(SA_RESTART)
    // specify SA_RESTART.  One consequence if we don't do this is
    // that readline gets confused by the -threaded RTS.  It seems
    // that if a SIGALRM handler is installed without SA_RESTART,
    // readline installs its own SIGALRM signal handler (see
    // readline's signals.c), and this somehow causes readline to go
    // wrong when the input exceeds a single line (try it).
    action.sa_flags = SA_RESTART;
#else
    action.sa_flags = 0;
#endif

    if (sigaction(sig, &action, NULL) == -1) {
        sysErrorBelch("sigaction");
        stg_exit(EXIT_FAILURE);
    }
}

/* -----------------------------------------------------------------------------
 * Install default signal handlers.
 *
 * The RTS installs a default signal handler for catching
 * SIGINT, so that we can perform an orderly shutdown.
 *
 * Haskell code may install their own SIGINT handler, which is
 * fine, provided they're so kind as to put back the old one
 * when they de-install.
 *
 * In addition to handling SIGINT, the RTS also handles SIGFPE
 * by ignoring it.  Apparently IEEE requires floating-point
 * exceptions to be ignored by default, but alpha-dec-osf3
 * doesn't seem to do so.
 * -------------------------------------------------------------------------- */
void
initDefaultHandlers(void)
{
    // N.B. We can't use initializers here as CentOS's ancient toolchain throws
    // spurious warnings. See #23577.
    struct sigaction action, oact;
    memset(&oact, 0, sizeof(struct sigaction));
    memset(&action, 0, sizeof(struct sigaction));

    // install the SIGINT handler
    action.sa_handler = shutdown_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0; // disable SA_RESTART
    if (sigaction(SIGINT, &action, &oact) != 0) {
        sysErrorBelch("warning: failed to install SIGINT handler");
    }

    // install the SIGFPE handler

    // In addition to handling SIGINT, also handle SIGFPE by ignoring it.
    // Apparently IEEE requires floating-point exceptions to be ignored by
    // default, but alpha-dec-osf3 doesn't seem to do so.

    // Commented out by SDM 2/7/2002: this causes an infinite loop on
    // some architectures when an integer division by zero occurs: we
    // don't recover from the floating point exception, and the
    // program just generates another one immediately.
#if 0
    action.sa_handler = SIG_IGN;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGFPE, &action, &oact) != 0) {
        sysErrorBelch("warning: failed to install SIGFPE handler");
    }
#endif

#if defined(alpha_HOST_ARCH)
# if defined(linux_HOST_OS)
    __ieee_set_fp_control(0);
# else
    ieee_set_fp_control(0);
# endif
#endif

    // ignore SIGPIPE; see #1619
    // actually, we use an empty signal handler rather than SIG_IGN,
    // so that SIGPIPE gets reset to its default behaviour on exec.
    action.sa_handler = empty_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGPIPE, &action, &oact) != 0) {
        sysErrorBelch("warning: failed to install SIGPIPE handler");
    }

    // Print a backtrace on SIGQUIT
    action.sa_handler = backtrace_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGQUIT, &action, &oact) != 0) {
        sysErrorBelch("warning: failed to install SIGQUIT handler");
    }

    set_sigtstp_action(true);
}

void
resetDefaultHandlers(void)
{
    struct sigaction action;

    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;

    // restore SIGINT
    if (sigaction(SIGINT, &action, NULL) != 0) {
        sysErrorBelch("warning: failed to uninstall SIGINT handler");
    }
    // restore SIGPIPE
    if (sigaction(SIGPIPE, &action, NULL) != 0) {
        sysErrorBelch("warning: failed to uninstall SIGPIPE handler");
    }

    set_sigtstp_action(false);
}

#endif /* RTS_USER_SIGNALS */
