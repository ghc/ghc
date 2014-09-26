/*
 * RTS periodic timers.
 *
 */
#define _WIN32_WINNT 0x0501

#include "Rts.h"
#include "Ticker.h"
#include <windows.h>
#include <stdio.h>
#include <process.h>

static TickProc tick_proc = NULL;
static HANDLE timer_queue = NULL;
static HANDLE timer       = NULL;
static Time tick_interval = 0;

static VOID CALLBACK tick_callback(
  PVOID lpParameter STG_UNUSED,
  BOOLEAN TimerOrWaitFired STG_UNUSED
  )
{
    tick_proc(0);
}

// We use the CreateTimerQueue() API which has been around since
// Windows 2000.  Apparently it gives bad results before Windows 7,
// though: http://www.virtualdub.org/blog/pivot/entry.php?id=272
//
// Even with the improvements in Windows 7, this timer isn't going to
// be very useful for profiling with a max usable resolution of
// 15ms. Unfortunately we don't have anything better.

void
initTicker (Time interval, TickProc handle_tick)
{
    tick_interval = interval;
    tick_proc = handle_tick;

    timer_queue = CreateTimerQueue();
    if (timer_queue == NULL) {
        sysErrorBelch("CreateTimerQueue");
        stg_exit(EXIT_FAILURE);
    }
}

void
startTicker(void)
{
    BOOL r;

    r = CreateTimerQueueTimer(&timer,
                              timer_queue,
                              tick_callback,
                              0,
                              0,
                              TimeToUS(tick_interval) / 1000, // ms
                              WT_EXECUTEINTIMERTHREAD);
    if (r == 0) {
        sysErrorBelch("CreateTimerQueueTimer");
        stg_exit(EXIT_FAILURE);
    }
}

void
stopTicker(void)
{
    if (timer_queue != NULL && timer != NULL) {
        DeleteTimerQueueTimer(timer_queue, timer, NULL);
        timer = NULL;
    }
}

void
exitTicker (rtsBool wait)
{
    if (timer_queue != NULL) {
        DeleteTimerQueueEx(timer_queue, wait ? INVALID_HANDLE_VALUE : NULL);
        timer_queue = NULL;
    }
}
