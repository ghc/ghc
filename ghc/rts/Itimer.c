/* -----------------------------------------------------------------------------
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
#include "Rts.h"
#include "RtsFlags.h"
#include "Timer.h"
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

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

/* Major bogosity:
 * 
 * In the threaded RTS, we can't set the virtual timer because the
 * thread which has the virtual timer might be sitting waiting for a
 * capability, and the virtual timer only ticks in CPU time.
 *
 * So, possible solutions:
 *
 * (1) tick in realtime.  Not very good, because this ticker is used for
 *     profiling, and this will give us unreliable time profiling
 *     results.  Furthermore, this requires picking a single OS thread
 *     to be the timekeeper, which is a bad idea because the thread in
 *     question might just be making a temporary call into Haskell land.
 *
 * (2) save/restore the virtual timer around excursions into STG land.
 *     Sounds great, but I tried it and the resolution of the virtual timer
 *     isn't good enough (on Linux) - most of our excursions fall
 *     within the timer's resolution and we never make any progress.
 *   
 * (3) have a virtual timer in every OS thread.  Might be reasonable,
 *     because most of the time there is only ever one of these
 *     threads running, so it approximates a single virtual timer.
 *     But still quite bogus (and I got crashes when I tried this).
 *
 * For now, we're using (1), but this needs a better solution. --SDM
 */
#ifdef RTS_SUPPORTS_THREADS
#define ITIMER_FLAVOUR  ITIMER_REAL
#define ITIMER_SIGNAL   SIGALRM
#else
#define ITIMER_FLAVOUR  ITIMER_VIRTUAL
#define ITIMER_SIGNAL   SIGVTALRM
#endif

static
int
install_vtalrm_handler(TickProc handle_tick)
{
    struct sigaction action;

    action.sa_handler = handle_tick;

    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;

    return sigaction(ITIMER_SIGNAL, &action, NULL);
}

int
startTicker(nat ms, TickProc handle_tick)
{
# ifndef HAVE_SETITIMER
  /*    debugBelch("No virtual timer on this system\n"); */
    return -1;
# else
    struct itimerval it;

    install_vtalrm_handler(handle_tick);

    timestamp = getourtimeofday();

    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_usec = 1000 * (ms - (1000 * it.it_value.tv_sec));
    it.it_interval = it.it_value;
    return (setitimer(ITIMER_FLAVOUR, &it, NULL));
# endif
}

int
stopTicker()
{
# ifndef HAVE_SETITIMER
  /*    debugBelch("No virtual timer on this system\n"); */
    return -1;
# else
    struct itimerval it;
  
    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    it.it_interval = it.it_value;
    return (setitimer(ITIMER_FLAVOUR, &it, NULL));
# endif
}

# if 0
/* This is a potential POSIX version */
int
startTicker(nat ms)
{
    struct sigevent se;
    struct itimerspec it;
    timer_t tid;

    timestamp = getourtimeofday();

    se.sigev_notify = SIGEV_SIGNAL;
    se.sigev_signo = ITIMER_SIGNAL;
    se.sigev_value.sival_int = ITIMER_SIGNAL;
    if (timer_create(CLOCK_VIRTUAL, &se, &tid)) {
	barf("can't create virtual timer");
    }
    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_nsec = 1000000 * (ms - 1000 * it.it_value.tv_sec);
    it.it_interval = it.it_value;
    return timer_settime(tid, TIMER_RELTIME, &it, NULL);
}

int
stopTicker()
{
    struct sigevent se;
    struct itimerspec it;
    timer_t tid;

    timestamp = getourtimeofday();

    se.sigev_notify = SIGEV_SIGNAL;
    se.sigev_signo = ITIMER_SIGNAL;
    se.sigev_value.sival_int = ITIMER_SIGNAL;
    if (timer_create(CLOCK_VIRTUAL, &se, &tid)) {
	barf("can't create virtual timer");
    }
    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 0;
    it.it_interval = it.it_value;
    return timer_settime(tid, TIMER_RELTIME, &it, NULL);
}
# endif

#if 0
/* Currently unused */
void
block_vtalrm_signal(void)
{
    sigset_t signals;
    
    sigemptyset(&signals);
    sigaddset(&signals, ITIMER_SIGNAL);

    (void) sigprocmask(SIG_BLOCK, &signals, NULL);
}

void
unblock_vtalrm_signal(void)
{
    sigset_t signals;
    
    sigemptyset(&signals);
    sigaddset(&signals, ITIMER_SIGNAL);

    (void) sigprocmask(SIG_UNBLOCK, &signals, NULL);
}
#endif

/* gettimeofday() takes around 1us on our 500MHz PIII.  Since we're
 * only calling it 50 times/s, it shouldn't have any great impact.
 */
lnat
getourtimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, (struct timezone *) NULL);
  	// cast to lnat because nat may be 64 bit when int is only 32 bit
  return ((lnat)tv.tv_sec * TICK_FREQUENCY +
	  (lnat)tv.tv_usec * TICK_FREQUENCY / 1000000);
}
