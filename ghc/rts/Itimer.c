/* -----------------------------------------------------------------------------
 * $Id: Itimer.c,v 1.11 2000/03/20 09:42:49 andy Exp $
 *
 * (c) The GHC Team, 1995-1999
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
#include "Schedule.h"

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

#if HAVE_WINDOWS_H
# include <windows.h>
#endif
 
lnat total_ticks = 0;
rtsBool do_prof_ticks = rtsFalse;

static
void
#if defined(mingw32_TARGET_OS) || (defined(cygwin32_TARGET_OS) && !defined(HAVE_SETITIMER))
CALLBACK
#endif
handle_tick(int unused STG_UNUSED);

/* -----------------------------------------------------------------------------
   Tick handler

   We use the ticker for two things: supporting threadDelay, and time
   profiling.

   SMP note: this signal could be delivered to *any* thread.  We have
   to ensure that it doesn't matter which thread actually runs the
   signal handler.
   -------------------------------------------------------------------------- */

static
void
#if defined(mingw32_TARGET_OS) || (defined(cygwin32_TARGET_OS) && !defined(HAVE_SETITIMER))
CALLBACK
#endif
handle_tick(int unused STG_UNUSED)
{
  total_ticks++;

#ifdef PROFILING
  if (do_prof_ticks == rtsTrue) {
    CCS_TICK(CCCS);
  }
#endif

  /* For threadDelay etc., see Select.c */
  ticks_since_select++;
}


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

#if defined(mingw32_TARGET_OS) || (defined(cygwin32_TARGET_OS) && !defined(HAVE_SETITIMER))

/* 
 * Sigh - to avoid requiring anyone that wants to build ghc to have
 * to augment the Win32 header files that comes with cygwinb20.1,
 * include the missing MM API decls here inline.
 *
 * ToDo: check and remove these once the next version of cygwin is
 * released.
 */
#define TIMERR_NOERROR   0
#define TIMERR_NOCANDO   97
#define TIME_PERIODIC    1

typedef UINT MMRESULT;
typedef void CALLBACK (*TIMECALLBACK) (UINT, UINT, DWORD, DWORD, DWORD);
typedef TIMECALLBACK *LPTIMECALLBACK;
MMRESULT STDCALL  timeSetEvent(UINT, UINT, LPTIMECALLBACK, DWORD, UINT);
/*
  vtalrm_handler is assigned and set up in Signals.c

  vtalrm_id (defined in Signals.c) holds
  the system id for the current timer (used to 
  later block/kill it.)
*/
extern nat vtalrm_id;
TIMECALLBACK *vtalrm_cback;
 
nat
initialize_virtual_timer(nat ms)
{
# ifdef PROFILING
  /* On Win32 setups that don't have support for
     setitimer(), we use the MultiMedia API's timer
     support.
     
     As the delivery of ticks isn't free, we only
     enable it if we really needed, i.e., when profiling.
     (the RTS now also needs timer ticks to implement
     threadDelay in non-profiling mode, but the pure
     Win32 port doesn't support that.....yet.)
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
# endif
  return 0;
}
 
#else

nat
initialize_virtual_timer(nat ms)
{
# ifndef HAVE_SETITIMER
  /*    fprintf(stderr, "No virtual timer on this system\n"); */
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
	barf("can't create virtual timer");
    }
    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_nsec = 1000000 * (ms - 1000 * it.it_value.tv_sec);
    it.it_interval = it.it_value;
    timer_settime(tid, TIMER_RELTIME, &it, NULL);
}
# endif

#if defined(mingw32_TARGET_OS) || (defined(cygwin32_TARGET_OS) && !defined(HAVE_SETITIMER))
int
install_vtalrm_handler(void)
{
  vtalrm_cback = handle_tick;
  return 0;
}

#else
int
install_vtalrm_handler(void)
{
    struct sigaction action;

    action.sa_handler = handle_tick;

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
#endif

unsigned int 
getourtimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, (struct timezone *) NULL);
  return (tv.tv_sec * 1000000 + tv.tv_usec);
}
