/* -----------------------------------------------------------------------------
 * $Id: Itimer.c,v 1.34 2003/03/29 00:00:41 sof Exp $
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

static
int
install_vtalrm_handler(void)
{
    struct sigaction action;

    action.sa_handler = handle_tick;

    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;

    return sigaction(SIGVTALRM, &action, NULL);
}

int
startTicker(nat ms)
{
# ifndef HAVE_SETITIMER
  /*    fprintf(stderr, "No virtual timer on this system\n"); */
    return -1;
# else
    struct itimerval it;

    install_vtalrm_handler();

    timestamp = getourtimeofday();

    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_usec = 1000 * (ms - (1000 * it.it_value.tv_sec));
    it.it_interval = it.it_value;
    return (setitimer(ITIMER_VIRTUAL, &it, NULL));
# endif
}

int
stopTicker()
{
# ifndef HAVE_SETITIMER
  /*    fprintf(stderr, "No virtual timer on this system\n"); */
    return -1;
# else
    struct itimerval it;
  
    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    it.it_interval = it.it_value;
    return (setitimer(ITIMER_VIRTUAL, &it, NULL));
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
    se.sigev_signo = SIGVTALRM;
    se.sigev_value.sival_int = SIGVTALRM;
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
    se.sigev_signo = SIGVTALRM;
    se.sigev_value.sival_int = SIGVTALRM;
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
unsigned int 
getourtimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, (struct timezone *) NULL);
  return (tv.tv_sec * TICK_FREQUENCY +
	  tv.tv_usec * TICK_FREQUENCY / 1000000);
}

