/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2007
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Ticker.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "posix/Clock.h"
#include "posix/Signals.h"

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

#include <string.h>

static Time itimer_interval = DEFAULT_TICK_INTERVAL;

void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;
    install_vtalrm_handler(SIGALRM, handle_tick);
}

void
startTicker(void)
{
    struct itimerval it;

    it.it_value.tv_sec = TimeToSeconds(itimer_interval);
    it.it_value.tv_usec = TimeToUS(itimer_interval) % 1000000;
    it.it_interval = it.it_value;

    if (setitimer(ITIMER_REAL, &it, NULL) != 0) {
        sysErrorBelch("setitimer");
        stg_exit(EXIT_FAILURE);
    }
}

void
stopTicker(void)
{
    struct itimerval it;

    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    it.it_interval = it.it_value;

    if (setitimer(ITIMER_REAL, &it, NULL) != 0) {
        sysErrorBelch("setitimer");
        stg_exit(EXIT_FAILURE);
    }
}

void
exitTicker (rtsBool wait STG_UNUSED)
{
    return;
}

int
rtsTimerSignal(void)
{
    return SIGALRM;
    // Using SIGALRM can leads to problems, see #850.  But we have no
    // option if timer_create() is not available.
}
