/* -----------------------------------------------------------------------------
 * $Id: Itimer.c,v 1.24 2001/11/13 13:38:02 simonmar Exp $
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

/* This is not posix compliant. */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "RtsFlags.h"
#include "Itimer.h"
#include "Proftimer.h"
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

/* ticks left before next pre-emptive context switch */
int ticks_to_ctxt_switch = 0;

/* -----------------------------------------------------------------------------
   Tick handler

   We use the ticker for time profiling.

   SMP note: this signal could be delivered to *any* thread.  We have
   to ensure that it doesn't matter which thread actually runs the
   signal handler.
   -------------------------------------------------------------------------- */

static
void
#if defined(mingw32_TARGET_OS) || (defined(cygwin32_TARGET_OS) && !defined(HAVE_SETITIMER))
CALLBACK
handle_tick(UINT uID STG_UNUSED, UINT uMsg STG_UNUSED, DWORD dwUser STG_UNUSED,
	    DWORD dw1 STG_UNUSED, DWORD d STG_UNUSED)
#else
handle_tick(int unused STG_UNUSED)
#endif
{
  total_ticks++;

#ifdef PROFILING
  handleProfTick();
#endif

  if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0) {
      ticks_to_ctxt_switch--;
      if (ticks_to_ctxt_switch <= 0) {
	  ticks_to_ctxt_switch = RtsFlags.ConcFlags.ctxtSwitchTicks;
	  context_switch = 1;	/* schedule a context switch */
      }
  }
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

LPTIMECALLBACK vtalrm_cback;

nat
initialize_virtual_timer(nat ms)
{
  /* On Win32 setups that don't have support for
     setitimer(), we use the MultiMedia API's timer
     support.
     
     The delivery of ticks isn't free; the performance hit should be checked.
  */
  unsigned int delay;
  static unsigned int vtalrm_id;
 
  if (ms) {
    delay = timeBeginPeriod(1);
    if (delay == TIMERR_NOCANDO) { /* error of some sort. */
      return delay;
    }
    vtalrm_id =
      timeSetEvent(ms,      /* event every `delay' milliseconds. */
		   1,       /* precision is within 1 ms */
		   vtalrm_cback,
		   TIME_CALLBACK_FUNCTION,     /* ordinary callback */
		   TIME_PERIODIC);
  } else {
    timeKillEvent(vtalrm_id);
    timeEndPeriod(1);
  }

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

    timestamp = getourtimeofday();

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

    timestamp = getourtimeofday();

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

/* gettimeofday() takes around 1us on our 500MHz PIII.  Since we're
 * only calling it 50 times/s, it shouldn't have any great impact.
 */
#if !defined(mingw32_TARGET_OS)
unsigned int 
getourtimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, (struct timezone *) NULL);
  return (tv.tv_sec * TICK_FREQUENCY +
	  tv.tv_usec * TICK_FREQUENCY / 1000000);
}
#else
unsigned int
getourtimeofday(void)
{
  return ((unsigned int)GetTickCount() * TICK_FREQUENCY) / 1000;
}
#endif
