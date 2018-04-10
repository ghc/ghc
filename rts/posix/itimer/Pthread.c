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

#include "PosixSource.h"
#include "Rts.h"

#include "Ticker.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "posix/Clock.h"

/* As recommended in the autoconf manual */
# if defined(TIME_WITH_SYS_TIME)
#  include <sys/time.h>
#  include <time.h>
# else
#  if defined(HAVE_SYS_TIME_H)
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#include <string.h>

#include <pthread.h>
#include <unistd.h>
#include <fcntl.h>

#if defined(HAVE_SYS_TIMERFD_H)
#include <sys/timerfd.h>
#define USE_TIMERFD_FOR_ITIMER 1
#else
#define USE_TIMERFD_FOR_ITIMER 0
#endif

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
static volatile HsBool stopped = 0;

// should the ticker thread exit?
// This can be set without holding the mutex.
static volatile HsBool exited = 1;

// Signaled when we want to (re)start the timer
static Condition start_cond;
static Mutex mutex;
static OSThreadId thread;

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;
    uint64_t nticks;
    int timerfd = -1;

#if defined(USE_TIMERFD_FOR_ITIMER) && USE_TIMERFD_FOR_ITIMER
    struct itimerspec it;
    it.it_value.tv_sec  = TimeToSeconds(itimer_interval);
    it.it_value.tv_nsec = TimeToNS(itimer_interval) % 1000000000;
    it.it_interval = it.it_value;

    timerfd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (timerfd == -1) {
        sysErrorBelch("timerfd_create");
        stg_exit(EXIT_FAILURE);
    }
    if (!TFD_CLOEXEC) {
      fcntl(timerfd, F_SETFD, FD_CLOEXEC);
    }
    if (timerfd_settime(timerfd, 0, &it, NULL)) {
        sysErrorBelch("timerfd_settime");
        stg_exit(EXIT_FAILURE);
    }
#endif

    while (!exited) {
        if (USE_TIMERFD_FOR_ITIMER) {
            if (read(timerfd, &nticks, sizeof(nticks)) != sizeof(nticks)) {
                if (errno != EINTR) {
                    sysErrorBelch("Itimer: read(timerfd) failed");
                }
            }
        } else {
            if (usleep(TimeToUS(itimer_interval)) != 0 && errno != EINTR) {
                sysErrorBelch("usleep(TimeToUS(itimer_interval) failed");
            }
        }

        // first try a cheap test
        if (stopped) {
            ACQUIRE_LOCK(&mutex);
            // should we really stop?
            if (stopped) {
                waitCondition(&start_cond, &mutex);
            }
            RELEASE_LOCK(&mutex);
        } else {
            handle_tick(0);
        }
    }

    if (USE_TIMERFD_FOR_ITIMER)
        close(timerfd);
    closeMutex(&mutex);
    closeCondition(&start_cond);
    return NULL;
}

void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;
    stopped = 0;
    exited = 0;

    initCondition(&start_cond);
    initMutex(&mutex);

    /*
     * We can't use the RTS's createOSThread here as we need to remain attached
     * to the thread we create so we can later join to it if requested
     */
    if (! pthread_create(&thread, NULL, itimer_thread_func, (void*)handle_tick)) {
#if defined(HAVE_PTHREAD_SETNAME_NP)
        pthread_setname_np(thread, "ghc_ticker");
#endif
    } else {
        sysErrorBelch("Itimer: Failed to spawn thread");
        stg_exit(EXIT_FAILURE);
    }
}

void
startTicker(void)
{
    ACQUIRE_LOCK(&mutex);
    stopped = 0;
    signalCondition(&start_cond);
    RELEASE_LOCK(&mutex);
}

/* There may be at most one additional tick fired after a call to this */
void
stopTicker(void)
{
    ACQUIRE_LOCK(&mutex);
    stopped = 1;
    RELEASE_LOCK(&mutex);
}

/* There may be at most one additional tick fired after a call to this */
void
exitTicker (bool wait)
{
    ASSERT(!exited);
    exited = 1;
    // ensure that ticker wakes up if stopped
    startTicker();

    // wait for ticker to terminate if necessary
    if (wait) {
        if (pthread_join(thread, NULL)) {
            sysErrorBelch("Itimer: Failed to join");
        }
    } else {
        pthread_detach(thread);
    }
}

int
rtsTimerSignal(void)
{
    return SIGALRM;
}
