/*
 * RTS periodic timers.
 *
 */

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

// Update as of 2020-04-02:
// It seems we can get somewhat reliable resolution even for intervals
// at 1ms which had an average error of <5%.
// This seems to be the case starting at some point during the
// Windows 7 lifetime and any newer versions of windows.

// Forward decls
static void startTicker(void);
static void stopTicker(bool synchronous);

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
unpauseTicker(void)
{
    startTicker();
}

void
pauseTicker(void)
{
    if (timer_queue != NULL && timer != NULL) {
        /* pauseTicker is called from within the handle_tick, so stopping
         * the ticker here /must/ be asynchronous or we will deadlock! */
        stopTicker(false /* asynchronous */);
    }
}

void
exitTicker(void)
{
    if (timer != NULL) {
        stopTicker(true /* synchronous */);
    }
    if (timer_queue != NULL) {
        // From the docs for DeleteTimerQueueEx:
        //   If this parameter is INVALID_HANDLE_VALUE, the function waits
        //   for all callback functions to complete before returning.
        HANDLE completion = INVALID_HANDLE_VALUE;
        DeleteTimerQueueEx(timer_queue, completion);
        timer_queue = NULL;
    }
}

static void startTicker(void) {
    ASSERT(timer_queue != NULL && timer == NULL);
    BOOL r = CreateTimerQueueTimer(&timer,
                                   timer_queue,
                                   tick_callback,
                                   0,
                                   0,
                                   TimeToMS(tick_interval), // ms
                                   WT_EXECUTEINTIMERTHREAD);
    if (r == 0) {
        sysErrorBelch("CreateTimerQueueTimer");
        stg_exit(EXIT_FAILURE);
    }
    ASSERT(timer != NULL);
}

static void stopTicker(bool synchronous) {
    ASSERT(timer_queue != NULL && timer != NULL);
    // From the docs for DeleteTimerQueueTimer:
    // If this parameter is INVALID_HANDLE_VALUE, the function waits for any
    // running timer callback functions to complete before returning.
    HANDLE completion = synchronous ? INVALID_HANDLE_VALUE : NULL;
    DeleteTimerQueueTimer(timer_queue, timer, completion);
    timer = NULL;
}
