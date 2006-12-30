/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

/* This is non-Posix-compliant.
   #include "PosixSource.h" 
*/
#include "Rts.h"
#include "SchedAPI.h"
#include "Schedule.h"
#include "RtsSignals.h"
#include "posix/Signals.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Prelude.h"
#include "ThrIOManager.h"

#ifdef alpha_HOST_ARCH
# if defined(linux_HOST_OS)
#  include <asm/fpu.h>
# else
#  include <machine/fpu.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#include <stdlib.h>

/* This curious flag is provided for the benefit of the Haskell binding
 * to POSIX.1 to control whether or not to include SA_NOCLDSTOP when
 * installing a SIGCHLD handler. 
 */
StgInt nocldstop = 0;

/* -----------------------------------------------------------------------------
 * The table of signal handlers
 * -------------------------------------------------------------------------- */

#if defined(RTS_USER_SIGNALS)

/* SUP: The type of handlers is a little bit, well, doubtful... */
StgInt *signal_handlers = NULL; /* Dynamically grown array of signal handlers */
static StgInt nHandlers = 0;    /* Size of handlers array */

static nat n_haskell_handlers = 0;

/* -----------------------------------------------------------------------------
 * Allocate/resize the table of signal handlers.
 * -------------------------------------------------------------------------- */

static void
more_handlers(I_ sig)
{
    StgInt i;

    if (sig < nHandlers)
	return;

    if (signal_handlers == NULL)
	signal_handlers = (StgInt *)stgMallocBytes((sig + 1) * sizeof(StgInt), "more_handlers");
    else
	signal_handlers = (StgInt *)stgReallocBytes(signal_handlers, (sig + 1) * sizeof(StgInt), "more_handlers");

    for(i = nHandlers; i <= sig; i++)
	// Fill in the new slots with default actions
	signal_handlers[i] = STG_SIG_DFL;

    nHandlers = sig + 1;
}

/* -----------------------------------------------------------------------------
 * Pending Handlers
 *
 * The mechanism for starting handlers differs between the threaded
 * (THREADED_RTS) and non-threaded versions of the RTS.
 *
 * When the RTS is single-threaded, we just write the pending signal
 * handlers into a buffer, and start a thread for each one in the
 * scheduler loop.
 *
 * When THREADED_RTS, the problem is that signals might be
 * delivered to multiple threads, so we would need to synchronise
 * access to pending_handler_buf somehow.  Using thread
 * synchronisation from a signal handler isn't possible in general
 * (some OSs support it, eg. MacOS X, but not all).  So instead:
 *
 *   - the signal handler writes the signal number into the pipe
 *     managed by the IO manager thread (see GHC.Conc).
 *   - the IO manager picks up the signal number and calls
 *     startSignalHandler() to start the thread.
 *
 * This also has the nice property that we don't need to arrange to
 * wake up a worker task to start the signal handler: the IO manager
 * wakes up when we write into the pipe.
 *
 * -------------------------------------------------------------------------- */

// Here's the pipe into which we will send our signals
static int io_manager_pipe = -1;

#define IO_MANAGER_WAKEUP 0xff
#define IO_MANAGER_DIE    0xfe

void
setIOManagerPipe (int fd)
{
    // only called when THREADED_RTS, but unconditionally
    // compiled here because GHC.Conc depends on it.
    io_manager_pipe = fd;
}

#if defined(THREADED_RTS)
void
ioManagerWakeup (void)
{
    // Wake up the IO Manager thread by sending a byte down its pipe
    if (io_manager_pipe >= 0) {
	StgWord8 byte = (StgWord8)IO_MANAGER_WAKEUP;
	write(io_manager_pipe, &byte, 1);
    }
}

void
ioManagerDie (void)
{
    // Ask the IO Manager thread to exit
    if (io_manager_pipe >= 0) {
	StgWord8 byte = (StgWord8)IO_MANAGER_DIE;
	write(io_manager_pipe, &byte, 1);
    }
}

void
ioManagerStart (void)
{
    // Make sure the IO manager thread is running
    Capability *cap;
    if (io_manager_pipe < 0) {
	cap = rts_lock();
	rts_evalIO(cap,&base_GHCziConc_ensureIOManagerIsRunning_closure,NULL);
	rts_unlock(cap);
    }
}
#endif

#if !defined(THREADED_RTS)

#define N_PENDING_HANDLERS 16

StgPtr pending_handler_buf[N_PENDING_HANDLERS];
StgPtr *next_pending_handler = pending_handler_buf;

#endif /* THREADED_RTS */

/* -----------------------------------------------------------------------------
 * SIGCONT handler
 *
 * It seems that shells tend to put stdin back into blocking mode
 * following a suspend/resume of the process.  Here we arrange to put
 * it back into non-blocking mode.  We don't do anything to
 * stdout/stderr because these handles don't get put into non-blocking
 * mode at all - see the comments on stdout/stderr in PrelHandle.hsc.
 * -------------------------------------------------------------------------- */

static void
cont_handler(int sig STG_UNUSED)
{
    setNonBlockingFd(0);
}

/* -----------------------------------------------------------------------------
 * Low-level signal handler
 *
 * Places the requested handler on a stack of pending handlers to be
 * started up at the next context switch.
 * -------------------------------------------------------------------------- */

static void
generic_handler(int sig)
{
    sigset_t signals;

#if defined(THREADED_RTS)

    if (io_manager_pipe != -1)
    {
	// Write the signal number into the pipe as a single byte.  We
	// hope that signals fit into a byte...
	StgWord8 csig = (StgWord8)sig;
	write(io_manager_pipe, &csig, 1);
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

    *next_pending_handler++ = deRefStablePtr((StgStablePtr)signal_handlers[sig]);

    // stack full?
    if (next_pending_handler == &pending_handler_buf[N_PENDING_HANDLERS]) {
	errorBelch("too many pending signals");
	stg_exit(EXIT_FAILURE);
    }
    
#endif /* THREADED_RTS */

    // re-establish the signal handler, and carry on
    sigemptyset(&signals);
    sigaddset(&signals, sig);
    sigprocmask(SIG_UNBLOCK, &signals, NULL);

    // *always* do the SIGCONT handler, even if the user overrides it.
    if (sig == SIGCONT) {
	cont_handler(sig);
    }

    context_switch = 1;
}

/* -----------------------------------------------------------------------------
 * Blocking/Unblocking of the user signals
 * -------------------------------------------------------------------------- */

static sigset_t userSignals;
static sigset_t savedSignals;

void
initUserSignals(void)
{
    sigemptyset(&userSignals);
}

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

rtsBool
anyUserHandlers(void)
{
    return n_haskell_handlers != 0;
}

#if !defined(THREADED_RTS)
void
awaitUserSignals(void)
{
    while (!signals_pending() && sched_state == SCHED_RUNNING) {
	pause();
    }
}
#endif

/* -----------------------------------------------------------------------------
 * Install a Haskell signal handler.
 * -------------------------------------------------------------------------- */

int
stg_sig_install(int sig, int spi, StgStablePtr *handler, void *mask)
{
    sigset_t signals, osignals;
    struct sigaction action;
    StgInt previous_spi;

    // Block the signal until we figure out what to do
    // Count on this to fail if the signal number is invalid
    if (sig < 0 || sigemptyset(&signals) ||
	sigaddset(&signals, sig) || sigprocmask(SIG_BLOCK, &signals, &osignals)) {
	return STG_SIG_ERR;
    }
    
    more_handlers(sig);

    previous_spi = signal_handlers[sig];

    action.sa_flags = 0;
    
    switch(spi) {
    case STG_SIG_IGN:
    	signal_handlers[sig] = STG_SIG_IGN;
	sigdelset(&userSignals, sig);
        action.sa_handler = SIG_IGN;
    	break;
    	
    case STG_SIG_DFL:
    	signal_handlers[sig] = STG_SIG_DFL;
	sigdelset(&userSignals, sig);
        action.sa_handler = SIG_DFL;
    	break;

    case STG_SIG_HAN:
    case STG_SIG_RST:
    	signal_handlers[sig] = (StgInt)*handler;
	sigaddset(&userSignals, sig);
    	action.sa_handler = generic_handler;
	if (spi == STG_SIG_RST) {
	    action.sa_flags = SA_RESETHAND;
	}
	n_haskell_handlers++;
    	break;

    default:
        barf("stg_sig_install: bad spi");
    }

    if (mask != NULL)
        action.sa_mask = *(sigset_t *)mask;
    else
	sigemptyset(&action.sa_mask);

    action.sa_flags |= sig == SIGCHLD && nocldstop ? SA_NOCLDSTOP : 0;

    if (sigaction(sig, &action, NULL) || 
	sigprocmask(SIG_SETMASK, &osignals, NULL)) 
    {
	// need to return an error code, so avoid a stable pointer leak
	// by freeing the previous handler if there was one.
	if (previous_spi >= 0) {
	    freeStablePtr(stgCast(StgStablePtr,signal_handlers[sig]));
	    n_haskell_handlers--;
	}
	return STG_SIG_ERR;
    }

    if (previous_spi == STG_SIG_DFL || previous_spi == STG_SIG_IGN
	|| previous_spi == STG_SIG_ERR) {
	return previous_spi;
    } else {
	*handler = (StgStablePtr)previous_spi;
	return STG_SIG_HAN;
    }
}

/* -----------------------------------------------------------------------------
 * Creating new threads for signal handlers.
 * -------------------------------------------------------------------------- */

#if !defined(THREADED_RTS)
void
startSignalHandlers(Capability *cap)
{
  blockUserSignals();
  
  while (next_pending_handler != pending_handler_buf) {

    next_pending_handler--;

    scheduleThread (cap,
	createIOThread(cap,
		       RtsFlags.GcFlags.initialStkSize, 
		       (StgClosure *) *next_pending_handler));
  }

  unblockUserSignals();
}
#endif

/* ----------------------------------------------------------------------------
 * Mark signal handlers during GC.
 *
 * We do this rather than trying to start all the signal handlers
 * prior to GC, because that requires extra heap for the new threads.
 * Signals must be blocked (see blockUserSignals() above) during GC to
 * avoid race conditions.
 * -------------------------------------------------------------------------- */

#if !defined(THREADED_RTS)
void
markSignalHandlers (evac_fn evac)
{
    StgPtr *p;

    p = next_pending_handler;
    while (p != pending_handler_buf) {
	p--;
	evac((StgClosure **)p);
    }
}
#else
void
markSignalHandlers (evac_fn evac STG_UNUSED)
{
}
#endif

#else /* !RTS_USER_SIGNALS */
StgInt 
stg_sig_install(StgInt sig STG_UNUSED,
		StgInt spi STG_UNUSED,
		StgStablePtr* handler STG_UNUSED,
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
#ifdef SMP
pthread_t startup_guy;
#endif

static void
shutdown_handler(int sig STG_UNUSED)
{
#ifdef SMP
    // if I'm a worker thread, send this signal to the guy who
    // originally called startupHaskell().  Since we're handling
    // the signal, it won't be a "send to all threads" type of signal
    // (according to the POSIX threads spec).
    if (pthread_self() != startup_guy) {
	pthread_kill(startup_guy, sig);
	return;
    }
#endif

    // If we're already trying to interrupt the RTS, terminate with
    // extreme prejudice.  So the first ^C tries to exit the program
    // cleanly, and the second one just kills it.
    if (sched_state >= SCHED_INTERRUPTING) {
	stg_exit(EXIT_INTERRUPTED);
    } else {
	interruptStgRts();
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
initDefaultHandlers()
{
    struct sigaction action,oact;

#ifdef SMP
    startup_guy = pthread_self();
#endif

    // install the SIGINT handler
    action.sa_handler = shutdown_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGINT, &action, &oact) != 0) {
	sysErrorBelch("warning: failed to install SIGINT handler");
    }

#if defined(HAVE_SIGINTERRUPT)
    siginterrupt(SIGINT, 1);	// isn't this the default? --SDM
#endif

    // install the SIGCONT handler
    action.sa_handler = cont_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGCONT, &action, &oact) != 0) {
	sysErrorBelch("warning: failed to install SIGCONT handler");
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

#ifdef alpha_HOST_ARCH
    ieee_set_fp_control(0);
#endif
}

void
freeSignalHandlers(void) {
    if (signal_handlers != NULL) {
        stgFree(signal_handlers);
    }
}

#endif /* RTS_USER_SIGNALS */
