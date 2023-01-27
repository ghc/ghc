/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2007
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * We use a realtime timer by default.  I found this much more
 * reliable than a CPU timer:
 *
 * Experiments with different frequencies: using
 * CLOCK_REALTIME/CLOCK_MONOTONIC on Linux 2.6.32,
 *     1000us has  <1% impact on runtime
 *      100us has  ~2% impact on runtime
 *       10us has ~40% impact on runtime
 *
 * using CLOCK_PROCESS_CPUTIME_ID on Linux 2.6.32,
 *     I cannot get it to tick faster than 10ms (10000us)
 *     which isn't great for profiling.
 *
 * In the threaded RTS, we can't tick in CPU time because the thread
 * which has the virtual timer might be idle, so the tick would never
 * fire.  Therefore we used to tick in realtime in the threaded RTS and
 * in CPU time otherwise, but now we always tick in realtime, for
 * several reasons:
 *
 *   - resolution (see above)
 *   - consistency (-threaded is the same as normal)
 *   - more consistency: Windows only has a realtime timer
 *
 * Note we want to use CLOCK_MONOTONIC rather than CLOCK_REALTIME,
 * because the latter may jump around (NTP adjustments, leap seconds
 * etc.).
 */

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Ticker.h"
#include "RtsUtils.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "posix/Clock.h"
#include <poll.h>

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

/*
 * TFD_CLOEXEC has been added in Linux 2.6.26.
 * If it is not available, we use fcntl(F_SETFD).
 */
#if !defined(TFD_CLOEXEC)
#define TFD_CLOEXEC 0
#endif

static Time itimer_interval = DEFAULT_TICK_INTERVAL;

// Should we be firing ticks?
// Writers to this must hold the mutex below.
static bool stopped = false;

// should the ticker thread exit?
// This can be set without holding the mutex.
static bool exited = true;

// Signaled when we want to (re)start the timer
static Condition start_cond;
static Mutex mutex;
static OSThreadId thread;

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;

    // Relaxed is sufficient: If we don't see that exited was set in one iteration we will
    // see it next time.
    TSAN_ANNOTATE_BENIGN_RACE(&exited, "itimer_thread_func");
    while (!RELAXED_LOAD(&exited)) {
        if (rtsSleep(itimer_interval) != 0) {
            sysErrorBelch("Ticker: sleep failed: %s", strerror(errno));
        }

        // first try a cheap test
        TSAN_ANNOTATE_BENIGN_RACE(&stopped, "itimer_thread_func");
        if (RELAXED_LOAD(&stopped)) {
            OS_ACQUIRE_LOCK(&mutex);
            // should we really stop?
            if (stopped) {
                waitCondition(&start_cond, &mutex);
            }
            OS_RELEASE_LOCK(&mutex);
        } else {
            handle_tick(0);
        }
    }

    return NULL;
}

void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;
    stopped = true;
    exited = false;
#if defined(HAVE_SIGNAL_H)
    sigset_t mask, omask;
    int sigret;
#endif
    int ret;

    initCondition(&start_cond);
    initMutex(&mutex);

    /*
     * Create the thread with all blockable signals blocked, leaving signal
     * handling to the main and/or other threads.  This is especially useful in
     * the non-threaded runtime, where applications might expect sigprocmask(2)
     * to effectively block signals.
     */
#if defined(HAVE_SIGNAL_H)
    sigfillset(&mask);
    sigret = pthread_sigmask(SIG_SETMASK, &mask, &omask);
#endif
    ret = createAttachedOSThread(&thread, "ghc_ticker", itimer_thread_func, (void*)handle_tick);
#if defined(HAVE_SIGNAL_H)
    if (sigret == 0)
        pthread_sigmask(SIG_SETMASK, &omask, NULL);
#endif

    if (ret != 0) {
        barf("Ticker: Failed to spawn thread: %s", strerror(errno));
    }
}

void
startTicker(void)
{
    OS_ACQUIRE_LOCK(&mutex);
    RELAXED_STORE(&stopped, false);
    signalCondition(&start_cond);
    OS_RELEASE_LOCK(&mutex);
}

/* There may be at most one additional tick fired after a call to this */
void
stopTicker(void)
{
    OS_ACQUIRE_LOCK(&mutex);
    RELAXED_STORE(&stopped, true);
    OS_RELEASE_LOCK(&mutex);
}

/* There may be at most one additional tick fired after a call to this */
void
exitTicker (bool wait)
{
    ASSERT(!SEQ_CST_LOAD(&exited));
    SEQ_CST_STORE(&exited, true);
    // ensure that ticker wakes up if stopped
    startTicker();

    // wait for ticker to terminate if necessary
    if (wait) {
        if (pthread_join(thread, NULL)) {
            sysErrorBelch("Ticker: Failed to join: %s", strerror(errno));
        }
        closeMutex(&mutex);
        closeCondition(&start_cond);
    } else {
        pthread_detach(thread);
    }
}

int
rtsTimerSignal(void)
{
    return SIGALRM;
}
