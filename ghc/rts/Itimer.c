/* -----------------------------------------------------------------------------
 * $Id: Itimer.c,v 1.2 1998/12/02 13:28:27 simonm Exp $
 *
 * (c) The GHC Team, 1995-1998
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * The interval timer is used for profiling and for context switching in the
 * threaded build.  Though POSIX 1003.1b includes a standard interface for
 * such things, no one really seems to be implementing them yet.  Even 
 * Solaris 2.3 only seems to provide support for @CLOCK_REAL@, whereas we're
 * keen on getting access to @CLOCK_VIRTUAL@.
 * 
 * Hence, we use the old-fashioned @setitimer@ that just about everyone seems
 * to support.  So much for standards.
 */

#if !defined(_AIX)
# define NON_POSIX_SOURCE
#endif

#include "Rts.h"
#include "Itimer.h"

/* As recommended in the autoconf manual */
# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif
 
/*
 * Handling timer events under cygwin32 is not done with signal/setitimer.
 * Instead of the two steps of first registering a signal handler to handle
 * \tr{SIGVTALRM} and then start generating them via @setitimer()@, we use
 * the Multimedia API (MM) and its @timeSetEvent@. (Internally, the MM API
 * creates a separate thread that will notify the main thread of timer
 * expiry). -- SOF 7/96
 *
 * 11/98: if the cygwin DLL supports setitimer(), then use it instead.
 */

#if defined(cygwin32_TARGET_OS) && !defined(HAVE_SETITIMER)

#include <windows.h>  /* OK, bring it all in... */

/*
  vtalrm_handler is assigned and set up in
  main/Signals.lc.

  vtalrm_id (defined in main/Signals.lc) holds
  the system id for the current timer (used to 
  later block/kill the timer)
*/
extern nat vtalrm_id;
extern TIMECALLBACK *vtalrm_cback;
 
nat
initialize_virtual_timer(nat ms)
{
  /* VTALRM is currently not supported by  cygwin32, 
     so we use the Timer support provided by the
     MultiMedia API that is part of Win32. The
     parameters to timeSetEvent may require some tweaking.
  */
  unsigned int delay,vtalrm_id;
 
  delay = timeBeginPeriod(1);
  if (delay == TIMERR_NOCANDO) { /* error of some sort. */
     return delay;
  }
  vtalrm_id =
    timeSetEvent(ms,     /* event every `delay' milliseconds. */
 	        1,       /* precision is within 5 millisecs. */
 	        (LPTIMECALLBACK)vtalrm_cback,
 		0,
 		TIME_PERIODIC);
  return 0;
}
 
#else

nat
initialize_virtual_timer(nat ms)
{
# ifndef HAVE_SETITIMER
    fprintf(stderr, "No virtual timer on this system\n");
    return -1;
# else
    struct itimerval it;

    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_usec = 1000 * (ms - (1000 * it.it_value.tv_sec));
    it.it_interval = it.it_value;
    return (setitimer(ITIMER_VIRTUAL, &it, NULL));
# endif
}

#endif /* !cygwin32_TARGET_OS */

# if 0
/* This is a potential POSIX version */
nat
initialize_virtual_timer(nat ms)
{
    struct sigevent se;
    struct itimerspec it;
    timer_t tid;

    se.sigev_notify = SIGEV_SIGNAL;
    se.sigev_signo = SIGVTALRM;
    se.sigev_value.sival_int = SIGVTALRM;
    if (timer_create(CLOCK_VIRTUAL, &se, &tid)) {
	fprintf(stderr, "Can't create virtual timer.\n");
	EXIT(EXIT_FAILURE);
    }
    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_nsec = 1000000 * (ms - 1000 * it.it_value.tv_sec);
    it.it_interval = it.it_value;
    timer_settime(tid, TIMER_RELTIME, &it, NULL);
}
# endif

int
install_vtalrm_handler(void (*handler)(int))
{
    struct sigaction action;

    action.sa_handler = handler;

    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;

    return sigaction(SIGVTALRM, &action, NULL);
}

void
block_vtalrm_signal(void)
{
    sigset_t signals;
    
    sigemptyset(&signals);
    sigaddset(&signals, SIGVTALRM);

    (void) sigprocmask(SIG_BLOCK, &signals, NULL);
}

void
unblock_vtalrm_signal(void)
{
    sigset_t signals;
    
    sigemptyset(&signals);
    sigaddset(&signals, SIGVTALRM);

    (void) sigprocmask(SIG_UNBLOCK, &signals, NULL);
}
