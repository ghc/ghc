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

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#include <string.h>

static Time itimer_interval = DEFAULT_TICK_INTERVAL;
static timer_t timer;

void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;

    struct sigevent ev;

    // Keep programs like valgrind happy
    memset(&ev, 0, sizeof(ev));

    ev.sigev_notify = SIGEV_SIGNAL;
    ev.sigev_signo  = SIGVTALRM;

    if (timer_create(CLOCK_ID, &ev, &timer) != 0) {
        sysErrorBelch("timer_create");
        stg_exit(EXIT_FAILURE);
    }

    install_vtalrm_handler(SIGVTALRM, handle_tick);
}

void
startTicker(void)
{
    struct itimerspec it;

    it.it_value.tv_sec  = TimeToSeconds(itimer_interval);
    it.it_value.tv_nsec = TimeToNS(itimer_interval) % 1000000000;
    it.it_interval = it.it_value;

    if (timer_settime(timer, 0, &it, NULL) != 0) {
        sysErrorBelch("timer_settime");
        stg_exit(EXIT_FAILURE);
    }
}

void
stopTicker(void)
{
    struct itimerspec it;

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 0;
    it.it_interval = it.it_value;

    if (timer_settime(timer, 0, &it, NULL) != 0) {
        sysErrorBelch("timer_settime");
        stg_exit(EXIT_FAILURE);
    }
}

void
exitTicker (rtsBool wait STG_UNUSED)
{
    // Before deleting the timer set the signal to ignore to avoid the
    // possibility of the signal being delivered after the timer is deleted.
    signal(SIGVTALRM, SIG_IGN);
    timer_delete(timer);
    // ignore errors - we don't really care if it fails.
}

int
rtsTimerSignal(void)
{
    return SIGVTALRM;
}
