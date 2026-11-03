/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2026
 *
 * The posix implementation of the interval timer, used for pre-emptive
 * scheduling of Haskell threads, and for sample based profiling.
 *
 * See rts/Timers.c for uses of timers.
 *
 * ---------------------------------------------------------------------------*/

/* This implementation uses a posix thread which repeatedly blocks on a timeout
 * using either the ppoll() or select() API. This lets it also block on a file
 * descriptor for early wakeup.
 *
 * The design for periodic timers uses a simple relative time delay with no
 * catchup. That is, time spent by the timer handler (e.g. flushing eventlog
 * buffers) is not accounted for, and the next timer tick is delayed by that
 * much (modulo wakeup jitter). This is probably the right thing to do:
 * generally in realtime systems one does not want to try to catch up when
 * behind, since that tends towards oversubscribing resources. Graceful
 * degredation is usually preferable.
 *
 * Experimental results (on Linux 6.18 on x86-64) to measure the typical
 * difference between the requested wakeup time and actual wakeup time for
 * different delay intervals:
 *
 *  interval   typical actual wakeup time after due time
 *   10000us   340 -- 400us      (this is the default interval)
 *    1000us    55 -- 100us
 *     100us    55us
 *      10us    55us
 *
 * While there's quite a bit of variance to these numbers, the results do not
 * vary significantly between using select, ppoll or nanosleep.
 *
 * On Linux at least, for longer delays the kernel allows itself lower wakeup
 * accuracy (which allows it to save power by coalescing multiple wakeups).
 * Similarly, the reason for 55us on the low end is that the default thread
 * timer slack on Linux is 50us, and context switch time accounts for the
 * remainder.
 *
 * In conclusion, on Linux at least, the accuracy is fine, both for the
 * default interval (10ms, 10000us) and for shorter intervals used during
 * profiling.
 *
 * Historically we had timer implementations using signals. This was always a
 * rather shakey thing to do but we originally had few alternatives.
 * - One problem with using signals is that there are severe limits on what
 *   code can be called from signal handlers. In particular it's not possible
 *   to take locks in a signal handler contex. This was enough for contex
 *   switching, but it's no good for things like flushing the eventlog, or
 *   waking up rts tasks.
 * - We also want to avoid using alarm signals, as these can interrupt system
 *   calls (#10840) or can be overwritten by user code.
 */

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Timer.h"
#include "RtsUtils.h"
#include "Proftimer.h"

#include "Schedule.h"
#include "posix/Clock.h"
#include "posix/FdWakeup.h"
#include "posix/OSThreads.h"

#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
/* We prefer the ppoll() function if available since it allows sanely waiting
 * on a single fd with precise timeouts (nanosecond precision). It is not in
 * the posix standard however and some platforms (notably glibc and freebsd)
 * need special CPP defines to make it available:
 */
#define _GNU_SOURCE 1
#define __BSD_VISIBLE 1
#include <signal.h>
#include <poll.h>
#else
/* Otherwise we use the classic select(), which does have microsecond
 * precision, but requires we build three whole 1024 bit (128 byte) fd sets
 * just to wait on one fd.
 */
#include <sys/select.h>
#endif

#include <time.h>
#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#include <string.h>

#include <pthread.h>
#if defined(HAVE_PTHREAD_NP_H)
#include <pthread_np.h>
#endif
#include <unistd.h>
#include <fcntl.h>


struct Timer {
    /* Mutex to protect the thread id and other shared variables below. */
    Mutex mutex;

    /* Condition variable for notifying changtes to the shared variables. */
    Condition cond;

    /* When running (i.e. not stopped) each timer runs in its own thread.
     * The thread id is used to wait for the timer thread to terminate after
     * asking it politely to stop.
     */
    OSThreadId thread_id;

    /* The timer handler function and argument. */
    TimerHandler handler;
    void        *handler_arg;

    enum TimerHandlerHint hint;

    /* For periodic timers this is the tick interval. */
    Time interval;

    /* Shared variable used to track if the timer is running or stopped.
     */
    bool running;

    /* Shared variable used by client threads to communicate that they
     * want the timer thread to pause. This communication is one-way, with no
     * acknowledgement.
     */
    bool pause_request;

    /* Shared variable used by other threads to communicate that they
     * want the timer thread to stop.
     */
    bool stop_request;
};

static void *timer_thread_func(void *_timer)
{
    Timer *timer = _timer;

    /* We release the lock during waitCondition (which is of course most of the
     * time) and when running the timer handler.
     */
    ACQUIRE_LOCK_ALWAYS(&timer->mutex);
    while (!timer->stop_request) {
        if (timer->pause_request) {
            waitCondition(&timer->cond, &timer->mutex);
        } else {
            if (!timedWaitCondition(&timer->cond, &timer->mutex, timer->interval)) {
                /* The time expired */

                if (timer->hint == TimerHandlerLongRunning) {
                    /* Long-running handlers are allowed to take locks, and in
                     * particular it may need pause the timer, which requires
                     * taking the timer lock! Thus we must release the lock
                     * while running the handler,
                     */
                    RELEASE_LOCK_ALWAYS(&timer->mutex);
                    timer->handler(timer->handler_arg);
                    ACQUIRE_LOCK_ALWAYS(&timer->mutex);
                } else {
                    /* Short-running handlers on the other hand are not allowed
                     * to block, which means they're not allowed to take locks,
                     * and hence we don't need to drop the lock here.
                     */
                    timer->handler(timer->handler_arg);
                }
            }
        }
    }
    RELEASE_LOCK_ALWAYS(&timer->mutex);

    return NULL;
}

/* Create a new periodic timer.
 *
 * The timer is created in the stopped state. Use runTimer to continue.
 */
Timer *newPeriodicTimer(TimerHandler handler, void *handler_arg,
                        enum TimerHandlerHint hint,
                        Time interval)
{
    ASSERT(interval > 0);
    Timer *timer = stgMallocBytes(sizeof(struct Timer), "newPeriodicTimer");

    timer->interval      = interval;
    timer->handler       = handler;
    timer->handler_arg   = handler_arg;
    timer->hint          = hint;
    reinitTimer(timer);
    return timer;
}


/* Called after fork() in the child process. The mutex is in an unknown state
 * but we can re-init, as we do elsewhere in the RTS (though POSIX appears
 * unclear on the matter).
 */
void reinitTimer(Timer *timer)
{
    timer->pause_request = false;
    timer->stop_request  = false;
    timer->running       = false;
    initMutex(&timer->mutex);
    initCondition(&timer->cond);
}


/* Idempotent. */
void runTimer(Timer *timer)
{
    ACQUIRE_LOCK_ALWAYS(&timer->mutex);
    if (timer->running) {
        RELEASE_LOCK_ALWAYS(&timer->mutex);
        return;
    }

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    /* We will use pthread_join on this thread later */
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

    /* Save resources for timers that do little by using a smaller stack. */
    if (timer->hint == TimerHandlerShortRunning) {
        pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN);
    }

    /*
     * Create the thread with all blockable signals blocked, leaving signal
     * handling to the main and/or other threads.  This is especially useful in
     * the non-threaded runtime, where applications might expect sigprocmask(2)
     * or pthread_sigmask(3) to effectively block signals.
     */
#if defined(HAVE_PTHREAD_SETSIGMASK_NP)
    /* Set the signal mask as an attribute of the new thread, if available. */
    sigset_t mask;
    sigfillset(&mask);
    pthread_attr_setsigmask_np(&attr, &mask);
#elif defined(HAVE_SIGNAL_H)
    /* Otherwise set (and later unset) our own thread's signal mask and have
     * that be inherited by the new thread.
     */
    sigset_t mask, omask;
    sigfillset(&mask);
    int sigret = pthread_sigmask(SIG_SETMASK, &mask, &omask);
#endif

    int ret = createPThread(&timer->thread_id, "ghc_timer",
                            timer_thread_func, (void*)timer,
                            &attr);

#if defined(HAVE_SIGNAL_H) && !defined(HAVE_PTHREAD_SETSIGMASK_NP)
    if (sigret == 0)
        pthread_sigmask(SIG_SETMASK, &omask, NULL);
#endif

    if (ret != 0) {
        barf("Timer: Failed to spawn thread: %s", strerror(errno));
    }
    timer->running = true;
    RELEASE_LOCK_ALWAYS(&timer->mutex);
}


/* Synchronous. Idempotent.
 * The timer is guaranteed stopped after this.
 */
void haltTimer(Timer *timer)
{
    ACQUIRE_LOCK_ALWAYS(&timer->mutex);
    if (!timer->running) {
        RELEASE_LOCK_ALWAYS(&timer->mutex);
        return;
    }

    timer->stop_request = true;
    signalCondition(&timer->cond);
    timer->running = false;
    RELEASE_LOCK_ALWAYS(&timer->mutex);

    // wait for the timer thread to terminate
    if (pthread_join(timer->thread_id, NULL)) {
        sysErrorBelch("Timer: Failed to join: %s", strerror(errno));
    }
}


/* Asynchronous. Idempotent. */
void unpauseTimer(Timer *timer)
{
    ACQUIRE_LOCK_ALWAYS(&timer->mutex);
    timer->pause_request = false;
    signalCondition(&timer->cond);
    RELEASE_LOCK_ALWAYS(&timer->mutex);
}


/* Asynchronous. Idempotent.
 * There may be at additional ticks fired after a call to this, but it will
 * usually stop quickly.
 */
void pauseTimer(Timer *timer)
{
    ACQUIRE_LOCK_ALWAYS(&timer->mutex);
    timer->pause_request = true;
    signalCondition(&timer->cond);
    RELEASE_LOCK_ALWAYS(&timer->mutex);
}


void freeTimer(Timer *timer)
{
    closeMutex(&timer->mutex);
    closeCondition(&timer->cond);
    stgFree(timer);
}
