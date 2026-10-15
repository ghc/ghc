/*
 * RTS periodic timers.
 *
 */

#include "Rts.h"
#include "Timer.h"
#include <windows.h>
#include <stdio.h>
#include <process.h>

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

struct Timer {
    Mutex    lock;
    HANDLE   timer;
    TimerHandler handler;
    void        *handler_arg;
    DWORD    interval; // milliseconds
    bool     paused;
    bool     running;
    enum TimerHandlerHint hint;
};

static Mutex  timer_queue_lock = INIT_MUTEX;
static HANDLE timer_queue      = NULL;
static int    timer_count     = 0;

// Forward decls
static void startTimer_(Timer *timer);
static void stopTimer_(Timer *timer, bool synchronous);

Ticker *newPeriodicTimer(TimerHandler handler, void *handler_arg,
                         enum TimerHandlerHint hint,
                         Time interval)
{
    OS_ACQUIRE_LOCK(timer_queue_lock);
    timer_count++;
    if (count == 1) {
        ASSERT(timer_queue == NULL);
        timer_queue = CreateTimerQueue();
        if (timer_queue == NULL) {
           sysErrorBelch("CreateTimerQueue");
           stg_exit(EXIT_FAILURE);
        }
    } else {
        ASSERT(timer_queue);
    }
    OS_RELEASE_LOCK(timer_queue_lock);

    Timer *timer = stgMallocBytes(sizeof(struct Timer), "newPeriodicTimer");
    OS_INIT_LOCK(&timer->lock);
    timer->timer       = NULL;
    timer->handler     = handler;
    timer->handler_arg = handler_arg;
    timer->interval    = stg_min((DWORD) 1, TimeToMS(interval));
    timer->paused      = true; // starts paused
    timer->hint        = hint;
}

// Asynchronous. Idempotent.
void
unpauseTimer(Timer *timer)
{
    OS_ACQUIRE_LOCK(&timer->lock);
    if (timer->running && timer->paused) {
        startTimer_(timer);
    }
    timer->paused = false;
    OS_RELEASE_LOCK(&timer->lock);
}

// Asynchronous. Idempotent.
void
pauseTimer(Timer *timer)
{
    OS_ACQUIRE_LOCK(&timer->lock);
    if (timer->running && !timer->paused) {
        /* pauseTimer is called from within the handle_tick, so stopping
         * the timer here /must/ be asynchronous or we will deadlock! */
        stopTimer_(timer, false /* asynchronous */);
    }
    timer->paused = true;
    OS_RELEASE_LOCK(&timer->lock);
}

void
runTimer(Timer *timer)
{
    OS_ACQUIRE_LOCK(&timer->lock);
    if (!timer->running && !timer->paused) {
        startTimer_(timer);
    }
    timer->running = true;
    OS_RELEASE_LOCK(&timer->lock);
}

void
haltTimer(Timer *timer)
{
    OS_ACQUIRE_LOCK(&timer->lock);
    //FIXME: there is a danger here that if the timer was very recently paused
    // then it may still be stopping asynchronously, but we cannot now stop it
    // synchronously.
    if (timer->running && !timer->paused) {
        stopTimer_(timer, true /* synchronous */);
    }
    timer->running = false;
    OS_RELEASE_LOCK(&timer->lock);
}

void freeTimer(Timer timer)
{
    stgFree(timer);

    OS_ACQUIRE_LOCK(timer_queue_lock);
    timer_count--;
    if (timer_count == 0) {
        // From the docs for DeleteTimerQueueEx:
        //   If this parameter is INVALID_HANDLE_VALUE, the function waits
        //   for all callback functions to complete before returning.
        // This is a belt-and-braces approach to ensuring haltTimer is
        // synchronous, since stopTimer_(timer, true) is already synchronous.
        HANDLE completion = INVALID_HANDLE_VALUE;
        DeleteTimerQueueEx(timer_queue, completion);
        timer_queue = NULL;
    }
    OS_RELEASE_LOCK(timer_queue_lock);
}

static VOID CALLBACK tick_callback (PVOID lpParameter,
                                    BOOLEAN TimerOrWaitFired STG_UNUSED)
{
    Timer *timer = (Timer *) lpParameter;
    timer->handler(timer->handler_arg);
}

static void startTimer_(Timer *timer) {
    ASSERT(timer_queue != NULL && timer->timer == NULL);
    ULONG flags = timer->hint == TimerHandlerShortRunning
                ? WT_EXECUTEINTIMERTHREAD
                : WT_EXECUTEDEFAULT;
    BOOL r = CreateTimerQueueTimer(&timer->timer,
                                   timer_queue,
                                   tick_callback,
                                   timer,   // callback param
                                   timer->interval, // inital interval
                                   timer->interval, // recurrant interval
                                   flags);
    if (r == 0) {
        sysErrorBelch("CreateTimerQueueTimer");
        stg_exit(EXIT_FAILURE);
    }
    ASSERT(timer->timer != NULL);
}

static void stopTimer_(Timer *timer, bool synchronous) {
    ASSERT(timer_queue != NULL && timer->timer != NULL);
    // From the docs for DeleteTimerQueueTimer:
    // If this parameter is INVALID_HANDLE_VALUE, the function waits for any
    // running timer callback functions to complete before returning.
    HANDLE completion = synchronous ? INVALID_HANDLE_VALUE : NULL;
    DeleteTimerQueueTimer(timer_queue, timer->timer, completion);
    timer->timer = NULL;
}
