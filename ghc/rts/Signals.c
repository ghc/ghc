/* -----------------------------------------------------------------------------
 * $Id: Signals.c,v 1.10 1999/11/09 15:46:57 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "SchedAPI.h"
#include "Schedule.h"
#include "Signals.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "StablePriv.h"

#ifndef mingw32_TARGET_OS

#ifndef PAR

static StgInt *handlers = NULL; /* Dynamically grown array of signal handlers */
static StgInt nHandlers = 0;    /* Size of handlers array */

#define N_PENDING_HANDLERS 16

StgPtr pending_handler_buf[N_PENDING_HANDLERS];
StgPtr *next_pending_handler = pending_handler_buf;

StgInt nocldstop = 0;

/* -----------------------------------------------------------------------------
   Allocate/resize the table of signal handlers.
   -------------------------------------------------------------------------- */

static void
more_handlers(I_ sig)
{
    I_ i;

    if (sig < nHandlers)
      return;

    if (handlers == NULL)
      handlers = (I_ *) malloc((sig + 1) * sizeof(I_));
    else
      handlers = (I_ *) realloc(handlers, (sig + 1) * sizeof(I_));

    if (handlers == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      fprintf(stderr, "VM exhausted (in more_handlers)\n");
      exit(EXIT_FAILURE);
    }
    for(i = nHandlers; i <= sig; i++)
      /* Fill in the new slots with default actions */
      handlers[i] = STG_SIG_DFL;

    nHandlers = sig + 1;
}

/* -----------------------------------------------------------------------------
   Low-level signal handler

   Places the requested handler on a stack of pending handlers to be
   started up at the next context switch.
   -------------------------------------------------------------------------- */

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

    *next_pending_handler++ = deRefStablePtr(handlers[sig]);

    /* stack full? */
    if (next_pending_handler == &pending_handler_buf[N_PENDING_HANDLERS]) {
      barf("too many pending signals");
    }
    
    /* re-establish the signal handler, and carry on */
    sigemptyset(&signals);
    sigaddset(&signals, sig);
    sigprocmask(SIG_UNBLOCK, &signals, NULL);

    context_switch = 1;
}

/* -----------------------------------------------------------------------------
   Blocking/Unblocking of the user signals
   -------------------------------------------------------------------------- */

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
   Install a Haskell signal handler.
   -------------------------------------------------------------------------- */

StgInt 
sig_install(StgInt sig, StgInt spi, StgStablePtr handler, sigset_t *mask)
{
    sigset_t signals;
    struct sigaction action;
    StgInt previous_spi;

    /* Block the signal until we figure out what to do */
    /* Count on this to fail if the signal number is invalid */
    if(sig < 0 || sigemptyset(&signals) || sigaddset(&signals, sig) ||
       sigprocmask(SIG_BLOCK, &signals, NULL))
      return STG_SIG_ERR;

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
    	handlers[sig] = (I_)handler;
	sigaddset(&userSignals, sig);
    	action.sa_handler = generic_handler;
    	break;

    default:
        barf("sig_install: bad spi");
    }

    if (mask != 0)
        action.sa_mask = *mask;
    else
	sigemptyset(&action.sa_mask);

    action.sa_flags = sig == SIGCHLD && nocldstop ? SA_NOCLDSTOP : 0;

    if (sigaction(sig, &action, NULL) || 
	sigprocmask(SIG_UNBLOCK, &signals, NULL)) 
    {
      /* need to return an error code, so avoid a stable pointer leak
       * by freeing the previous handler if there was one.
       */	 
      if (previous_spi >= 0) {
	  freeStablePtr(handlers[sig]);
      }
      return STG_SIG_ERR;
    }

    return previous_spi;
}

/* -----------------------------------------------------------------------------
   Creating new threads for the pending signal handlers.
   -------------------------------------------------------------------------- */

void
start_signal_handlers(void)
{
  blockUserSignals();
  
  while (next_pending_handler != pending_handler_buf) {

    next_pending_handler--;

    /* create*Thread  puts the thread on the head of the runnable
     * queue, hence it will be run next.  Poor man's priority
     * scheduling.
     */
    createIOThread(RtsFlags.GcFlags.initialStkSize, 
		   (StgClosure *) *next_pending_handler);
  }

  unblockUserSignals();
}

#else /* PAR */
StgInt 
sig_install(StgInt sig, StgInt spi, StgStablePtr handler, sigset_t *mask)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  fprintf(stderr,
	  "No signal handling support in a parallel implementation.\n");
  exit(EXIT_FAILURE);
}

void
start_signal_handlers(void)
{
}
#endif

/* -----------------------------------------------------------------------------
   SIGINT handler.

   We like to shutdown nicely after receiving a SIGINT, write out the
   stats, write profiling info, close open files and flush buffers etc.
   -------------------------------------------------------------------------- */

#ifdef SMP
pthread_t startup_guy;
#endif

static void
shutdown_handler(int sig STG_UNUSED)
{
#ifdef SMP
  /* if I'm a worker thread, send this signal to the guy who
   * originally called startupHaskell().  Since we're handling
   * the signal, it won't be a "send to all threads" type of signal
   * (according to the POSIX threads spec).
   */
  if (pthread_self() != startup_guy) {
    pthread_kill(startup_guy, sig);
  } else
#endif

  shutdownHaskellAndExit(EXIT_FAILURE);
}

/*
 * The RTS installs a default signal handler for catching
 * SIGINT, so that we can perform an orderly shutdown.
 *
 * Haskell code may install their own SIGINT handler, which is
 * fine, provided they're so kind as to put back the old one
 * when they de-install.
 */
void
init_shutdown_handler()
{
    struct sigaction action,oact;

#ifdef SMP
    startup_guy = pthread_self();
#endif
    action.sa_handler = shutdown_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGINT, &action, &oact) != 0) {
      /* Oh well, at least we tried. */
#ifdef DEBUG
      fprintf(stderr, "init_shutdown_handler: failed to reg SIGINT handler");
#endif
    }
}




#endif /*! mingw32_TARGET_OS */
