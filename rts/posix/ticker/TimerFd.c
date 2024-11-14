/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2023
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

#include <sys/timerfd.h>


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

// file descriptor for the timer (Linux only)
static int timerfd = -1;

// pipe for signaling exit
static int pipefds[2];

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;
    uint64_t nticks;
    ssize_t r = 0;
    struct pollfd pollfds[2];

    pollfds[0].fd = pipefds[0];
    pollfds[0].events = POLLIN;
    pollfds[1].fd = timerfd;
    pollfds[1].events = POLLIN;

    // Relaxed is sufficient: If we don't see that exited was set in one iteration we will
    // see it next time.
    TSAN_ANNOTATE_BENIGN_RACE(&exited, "itimer_thread_func");
    while (!RELAXED_LOAD(&exited)) {
        if (poll(pollfds, 2, -1) == -1) {
            // While the RTS attempts to mask signals, some foreign libraries
            // may rely on signal delivery may unmask them. Consequently we may
            // see EINTR. See #24610.
            if (errno != EINTR) {
                sysErrorBelch("Ticker: poll failed: %s", strerror(errno));
            }
        }

        // We check the pipe first, even though the timerfd may also have triggered.
        if (pollfds[0].revents & POLLIN) {
            // the pipe is ready for reading, the only possible reason is that we're exiting
            exited = true; // set this again to make sure even RELAXED_LOAD will read the proper value
            // no further action needed, skip ahead to handling the final tick and then stopping
        }
        else if (pollfds[1].revents & POLLIN) { // the timerfd is ready for reading
            r = read(timerfd, &nticks, sizeof(nticks)); // this should never block now

            if ((r == 0) && (errno == 0)) {
               /* r == 0 is expected only for non-blocking fd (in which case
                * errno should be EAGAIN) but we use a blocking fd.
                *
                * Due to a kernel bug (cf https://lkml.org/lkml/2019/8/16/335)
                * on some platforms we could see r == 0 and errno == 0.
                */
               IF_DEBUG(scheduler, debugBelch("read(timerfd) returned 0 with errno=0. This is a known kernel bug. We just ignore it."));
            }
            else if (r != sizeof(nticks) && errno != EINTR) {
               barf("Ticker: read(timerfd) failed with %s and returned %zd", strerror(errno), r);
            }
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

    close(timerfd);
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

    /* Open the file descriptor for the timer synchronously.
     *
     * We used to do it in itimer_thread_func (i.e. in the timer thread) but it
     * meant that some user code could run before it and get confused by the
     * allocation of the timerfd.
     *
     * See hClose002 which unsafely closes a file descriptor twice expecting an
     * exception the second time: it sometimes failed when the second call to
     * "close" closed our own timerfd which inadvertently reused the same file
     * descriptor closed by the first call! (see #20618)
     */
    struct itimerspec it;
    it.it_value.tv_sec  = TimeToSeconds(itimer_interval);
    it.it_value.tv_nsec = TimeToNS(itimer_interval) % 1000000000;
    it.it_interval = it.it_value;

    if (timerfd != -1) {
        // don't leak the old file descriptors after a fork (#25280)
        close(timerfd);
        close(pipefds[0]);
        close(pipefds[1]);
        timerfd = -1;
    }

    timerfd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (timerfd == -1) {
        barf("timerfd_create: %s", strerror(errno));
    }
    if (!TFD_CLOEXEC) {
        fcntl(timerfd, F_SETFD, FD_CLOEXEC);
    }
    if (timerfd_settime(timerfd, 0, &it, NULL)) {
        barf("timerfd_settime: %s", strerror(errno));
    }

    if (pipe(pipefds) < 0) {
        barf("pipe: %s", strerror(errno));
    }

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
        // write anything to the pipe to trigger poll() in the ticker thread
        if (write(pipefds[1], "stop", 5) < 0) {
            sysErrorBelch("Ticker: Failed to write to pipe: %s", strerror(errno));
        }

        if (pthread_join(thread, NULL)) {
            sysErrorBelch("Ticker: Failed to join: %s", strerror(errno));
        }

        // These need to happen AFTER the ticker thread has finished to prevent a race condition
        // where the ticker thread closes the read end of the pipe before we're done writing to it.
        close(pipefds[0]);
        close(pipefds[1]);

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
