/* -----------------------------------------------------------------------------
 * $Id: Signals.c,v 1.22 2001/10/31 10:34:29 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
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
#include "Signals.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "StablePriv.h"

#ifdef alpha_TARGET_ARCH
#include <machine/fpu.h>
#endif

#ifndef mingw32_TARGET_OS

#ifndef PAR

/* SUP: The type of handlers is a little bit, well, doubtful... */
static StgInt *handlers = NULL; /* Dynamically grown array of signal handlers */
static StgInt nHandlers = 0;    /* Size of handlers array */

#define N_PENDING_HANDLERS 16

StgPtr pending_handler_buf[N_PENDING_HANDLERS];
StgPtr *next_pending_handler = pending_handler_buf;

StgInt nocldstop = 0;

/* -----------------------------------------------------------------------------
 * Allocate/resize the table of signal handlers.
 * -------------------------------------------------------------------------- */

static void
more_handlers(I_ sig)
{
    StgInt i;

    if (sig < nHandlers)
	return;

    if (handlers == NULL)
	handlers = (StgInt *) malloc((sig + 1) * sizeof(StgInt));
    else
	handlers = (StgInt *) realloc(handlers, (sig + 1) * sizeof(StgInt));

    if (handlers == NULL) {
	// don't fflush(stdout); WORKAROUND bug in Linux glibc
	barf("VM exhausted (in more_handlers)");
    }
    for(i = nHandlers; i <= sig; i++)
	// Fill in the new slots with default actions
	handlers[i] = STG_SIG_DFL;

    nHandlers = sig + 1;
}

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

    *next_pending_handler++ = deRefStablePtr((StgStablePtr)handlers[sig]);

    // stack full?
    if (next_pending_handler == &pending_handler_buf[N_PENDING_HANDLERS]) {
	barf("too many pending signals");
    }
    
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
    sigprocmask(SIG_SETMASK, &userSignals, &savedSignals);
}

void
unblockUserSignals(void)
{
    sigprocmask(SIG_SETMASK, &savedSignals, NULL);
}


/* -----------------------------------------------------------------------------
 * Install a Haskell signal handler.
 * -------------------------------------------------------------------------- */

StgInt 
stg_sig_install(StgInt sig, StgInt spi, StgStablePtr handler, sigset_t *mask)
{
    sigset_t signals;
    struct sigaction action;
    StgInt previous_spi;

    // Block the signal until we figure out what to do
    // Count on this to fail if the signal number is invalid
    if (sig < 0 || sigemptyset(&signals) ||
	sigaddset(&signals, sig) || sigprocmask(SIG_BLOCK, &signals, NULL)) {
	return STG_SIG_ERR;
    }
    
    more_handlers(sig);

    previous_spi = handlers[sig];

    switch(spi) {
    case STG_SIG_IGN:
    	handlers[sig] = STG_SIG_IGN;
	sigdelset(&userSignals, sig);
        action.sa_handler = SIG_IGN;
    	break;
    	
    case STG_SIG_DFL:
    	handlers[sig] = STG_SIG_DFL;
	sigdelset(&userSignals, sig);
        action.sa_handler = SIG_DFL;
    	break;

    case STG_SIG_HAN:
    	handlers[sig] = (StgInt)handler;
	sigaddset(&userSignals, sig);
    	action.sa_handler = generic_handler;
    	break;

    default:
        barf("stg_sig_install: bad spi");
    }

    if (mask != 0)
        action.sa_mask = *mask;
    else
	sigemptyset(&action.sa_mask);

    action.sa_flags = sig == SIGCHLD && nocldstop ? SA_NOCLDSTOP : 0;

    if (sigaction(sig, &action, NULL) || 
	sigprocmask(SIG_UNBLOCK, &signals, NULL)) 
    {
	// need to return an error code, so avoid a stable pointer leak
	// by freeing the previous handler if there was one.
	if (previous_spi >= 0) {
	    freeStablePtr(stgCast(StgStablePtr,handlers[sig]));
	}
	return STG_SIG_ERR;
    }
    
    return previous_spi;
}

/* -----------------------------------------------------------------------------
 * Creating new threads for the pending signal handlers.
 * -------------------------------------------------------------------------- */
void
startSignalHandlers(void)
{
  blockUserSignals();
  
  while (next_pending_handler != pending_handler_buf) {

    next_pending_handler--;

    scheduleThread(
       createIOThread(RtsFlags.GcFlags.initialStkSize, 
		      (StgClosure *) *next_pending_handler));
  }

  unblockUserSignals();
}

#else // PAR
StgInt 
stg_sig_install(StgInt sig, StgInt spi, StgStablePtr handler, sigset_t *mask)
{
    // don't fflush(stdout); WORKAROUND bug in Linux glibc
    barf("no signal handling support in a parallel implementation");
}

void
startSignalHandlers(void)
{
}
#endif

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
    if (interrupted) {
	exit(EXIT_INTERRUPTED);
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
	prog_belch("warning: failed to install SIGINT handler");
    }

    siginterrupt(SIGINT, 1);	// isn't this the default? --SDM

    // install the SIGCONT handler
    action.sa_handler = cont_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGCONT, &action, &oact) != 0) {
	prog_belch("warning: failed to install SIGCONT handler");
    }

    // install the SIGFPE handler
    action.sa_handler = SIG_IGN;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGFPE, &action, &oact) != 0) {
	prog_belch("warning: failed to install SIGFPE handler");
    }
#ifdef alpha_TARGET_ARCH
    ieee_set_fp_control(0);
#endif
}

#endif /*! mingw32_TARGET_OS */
