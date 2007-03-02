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
#include "Ticker.h"
#include "posix/Itimer.h"
#include "Proftimer.h"
#include "Storage.h"
#include "Schedule.h"
#include "posix/Select.h"

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
 *     results.
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

#if defined(HAVE_TIMER_CREATE) && defined(HAVE_TIMER_SETTIME)

#  define USE_TIMER_CREATE
#  define ITIMER_SIGNAL SIGVTALRM
#  ifdef THREADED_RTS
#    define TIMER_FLAVOUR CLOCK_REALTIME
#  else
#    define TIMER_FLAVOUR CLOCK_PROCESS_CPUTIME_ID
#  endif

#elif defined(HAVE_SETITIMER)

#  define USE_ITIMER
#  ifdef THREADED_RTS
//   Oh dear, we have to use SIGALRM if there's no timer_create and
//   we're using the THREADED_RTS.  This leads to problems, see bug #850.
#    define ITIMER_SIGNAL  SIGALRM
#    define ITIMER_FLAVOUR ITIMER_REAL
#  else
#    define ITIMER_SIGNAL  SIGVTALRM
#    define ITIMER_FLAVOUR ITIMER_VIRTUAL
#  endif

#else

#  error No way to set an interval timer.

#endif

#if defined(USE_TIMER_CREATE)
timer_t timer;
#endif

static
void
install_vtalrm_handler(TickProc handle_tick)
{
    struct sigaction action;

    action.sa_handler = handle_tick;

    sigemptyset(&action.sa_mask);

#ifdef SA_RESTART
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

    if (sigaction(ITIMER_SIGNAL, &action, NULL) == -1) {
        sysErrorBelch("sigaction");
        stg_exit(EXIT_FAILURE);
    }
}

void
startTicker(nat ms, TickProc handle_tick)
{
    install_vtalrm_handler(handle_tick);

#if !defined(THREADED_RTS)
    timestamp = getourtimeofday();
#endif

#if defined(USE_TIMER_CREATE)
    {
        struct itimerspec it;
        struct sigevent ev;

        ev.sigev_notify = SIGEV_SIGNAL;
        ev.sigev_signo  = ITIMER_SIGNAL;
        
        it.it_value.tv_sec = ms / 1000;
        it.it_value.tv_nsec = (ms % 1000) * 1000000;
        it.it_interval = it.it_value;
        
        if (timer_create(TIMER_FLAVOUR, &ev, &timer) != 0) {
            sysErrorBelch("timer_create");
            stg_exit(EXIT_FAILURE);
        }

        if (timer_settime(timer, 0, &it, NULL) != 0) {
            sysErrorBelch("timer_settime");
            stg_exit(EXIT_FAILURE);
        }
    }
#else
    {
        struct itimerval it;

        it.it_value.tv_sec = ms / 1000;
        it.it_value.tv_usec = (ms % 1000) * 1000;
        it.it_interval = it.it_value;
        
        if (setitimer(ITIMER_FLAVOUR, &it, NULL) != 0) {
            sysErrorBelch("setitimer");
            stg_exit(EXIT_FAILURE);
        }
    }
#endif
}

void
stopTicker(void)
{
#if defined(USE_TIMER_CREATE)
    struct itimerspec it;

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 0;
    it.it_interval = it.it_value;

    if (timer_settime(timer, 0, &it, NULL) != 0) {
        sysErrorBelch("timer_settime");
        stg_exit(EXIT_FAILURE);
    }
#else
    struct itimerval it;

    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    it.it_interval = it.it_value;

    if (setitimer(ITIMER_FLAVOUR, &it, NULL) != 0) {
        sysErrorBelch("setitimer");
        stg_exit(EXIT_FAILURE);
    }
#endif
}

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
  return ((lnat)tv.tv_sec * 1000 / RtsFlags.MiscFlags.tickInterval +
	  (lnat)tv.tv_usec / (RtsFlags.MiscFlags.tickInterval * 1000));
}
