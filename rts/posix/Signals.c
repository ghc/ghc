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
#include "RtsUtils.h"
#include "Schedule.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#include "Libdw.h"
#include "MIO.h"

#if defined(alpha_HOST_ARCH)
# if defined(linux_HOST_OS)
#  include <fenv.h>
# else
#  include <machine/fpu.h>
# endif
#endif

#include <unistd.h>
#include <signal.h>
#include <termios.h>
#include <string.h>

/* Note [pthread_sigmask and the ineffectiveness of masking signals]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Relevant posix signals facts:
 *
 *  + Every (OS) thread has a signal mask, which can be manipulated by that
 *    thread itself (though sigprocmask on macOS breaks this).
 *  + Signal masks are inherited when new threads or processes are created (or
 *    can be set as an attribute in posix spawn on some platforms).
 *  + Most signals are processes directed, while some are thread-directed.
 *  + Process-directed signals will be delivered to _any_ thread that does not
 *    have the signal masked.
 *
 * The important consequence of the "any thread will do" property is that:
 *
 *  + it is generally _effective_ to unmask a signal in a thread, because at
 *    least the calling thread has the signal unmasked; however
 *  + it is generally _ineffective_ to mask a signal in a thread, because
 *    the signal will likely still be unmasked in some other thread and thus
 *    be delivered there.
 *
 * Thus we should avoid use the masking signals as a mechanism that we rely on,
 * since it will not in general be effective. In best-effort cases where there
 * is a reasonable likelyhood that the calling thread is the only thread with
 * signals unmasked then it is ok.
 *
 * Closely related: always avoid sigprocmask, and prefer pthread_sigmask.
 * The older function is sigprocmask but it's meaning in multi-threaded
 * programs is explicitly unspecified by Posix. On Linux it modifies the
 * calling thread's signal mask, but on macOS it modifies the signal mask of
 * every thread in the process, leading to chaos. The new function is
 * pthread_sigmask which is well defined and operates only on the signal mask
 * of the calling thread.
 */

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
    if (!RtsFlags.MiscFlags.install_signal_handlers) return;

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
    if (!RtsFlags.MiscFlags.install_signal_handlers) return;

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
