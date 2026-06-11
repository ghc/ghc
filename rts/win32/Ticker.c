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

static Mutex lock; // To protect the timer and paused var below
static HANDLE timer       = NULL;
static bool paused;

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
    ASSERT(timer_queue == NULL);
    tick_interval = interval;
    tick_proc = handle_tick;

    OS_INIT_LOCK(&lock);
    paused = true; // starts paused
    timer_queue = CreateTimerQueue();
    if (timer_queue == NULL) {
        sysErrorBelch("CreateTimerQueue");
        stg_exit(EXIT_FAILURE);
    }
}

// Asynchronous. Idempotent.
void
unpauseTicker(void)
{
    OS_ACQUIRE_LOCK(&lock);
    if (paused) {
        startTicker();
    }
    paused = false;
    OS_RELEASE_LOCK(&lock);
}

// Asynchronous. Idempotent.
void
pauseTicker(void)
{
    OS_ACQUIRE_LOCK(&lock);
    if (!paused) {
        /* pauseTicker is called from within the handle_tick, so stopping
         * the ticker here /must/ be asynchronous or we will deadlock! */
        stopTicker(false /* asynchronous */);
    }
    paused = true;
    OS_RELEASE_LOCK(&lock);
}

void
exitTicker(void)
{
    ASSERT(timer_queue != NULL);

    OS_ACQUIRE_LOCK(&lock);
    if (!paused) {
        stopTicker(true /* synchronous */);
    }
    OS_RELEASE_LOCK(&lock);

    // From the docs for DeleteTimerQueueEx:
    //   If this parameter is INVALID_HANDLE_VALUE, the function waits
    //   for all callback functions to complete before returning.
    // This is a belt-and-braces approach to ensuring exitTicker is synchronous,
    // since stopTicker(true) is already synchronous and there's only one timer.
    HANDLE completion = INVALID_HANDLE_VALUE;
    DeleteTimerQueueEx(timer_queue, completion);
    timer_queue = NULL;
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
