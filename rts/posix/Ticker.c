/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2026
 *
 * The posix implementation of the interval timer, used for pre-emptive
 * scheduling of Haskell threads, and for sample based profiling.
 *
 * This file defines the "ticker": the platform-specific service to install and
 * run the timer. See rts/Timer.c for the platform-dependent view of interval
 * timing.
 *
 * ---------------------------------------------------------------------------*/

/* This implementation uses a posix thread which repeatedly blocks on a timeout
 * using either the ppoll() or select() API. This lets it also block on a file
 * descriptor for early wakeup.
 *
 * The design uses a simple relative time delay with no catchup. That is, time
 * spent by the ticker thread itself (e.g. flushing eventlog buffers) is not
 * accounted for, and the next tick is delayed by that much (modulo wakeup
 * jitter). This is probably the right thing to do: generally in realtime
 * systems one does not want to try to catch up when behind, since that tends
 * towards oversubscribing resources. Graceful degredation is usually
 * preferable.
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
 * Historically we had ticker implementations using signals. This was always a
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

#include "Ticker.h"
#include "RtsUtils.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "posix/Clock.h"
#include "posix/FdWakeup.h"

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

// fds for interrupting the ticker
static int interruptfd_r = -1, interruptfd_w = -1;

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;

#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
    struct pollfd pollfds[1];

    pollfds[0].fd = interruptfd_r;
    pollfds[0].events = POLLIN;

    struct timespec ts = { .tv_sec  = TimeToSeconds(itimer_interval)
                         , .tv_nsec = TimeToNS(itimer_interval) % 1000000000
                         };
#else
    fd_set selectfds;
    FD_ZERO(&selectfds);
    FD_SET(interruptfd_r, &selectfds);

    struct timeval tv = { .tv_sec  = TimeToSeconds(itimer_interval)
                                     /* convert remainder time in nanoseconds
                                        to microseconds, rounding up: */
                        , .tv_usec = ((TimeToNS(itimer_interval) % 1000000000)
                                     + 999) / 1000
                        };
#endif

    // Relaxed is sufficient: If we don't see that exited was set in one iteration we will
    // see it next time.
    while (!RELAXED_LOAD_ALWAYS(&exited)) {

#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
        int nfds   = 1;
        int nready = ppoll(pollfds, nfds, &ts, NULL);
#else
        struct timeval tv_tmp = tv; // copy since select may change this value.
        int nfds   = interruptfd_r+1;
        int nready = select(nfds, &selectfds, NULL, NULL, &tv_tmp);
#endif
        // In either case (ppoll or select), the result nready is the number
        // of fds that are ready.
        if (RTS_LIKELY(nready == 0)) {
            // Timer expired, not interrupted, continue.
        } else if (nready > 0) {
            // We only monitor one fd (the interruptfd_r), so we know
            // it is that fd that is ready without any further checks.
            collectFdWakeup(interruptfd_r);
            // No further action needed, continue on to handling the final tick
            // and then stop.

            // Note that we rely on sendFdWakeup and select/poll to provide the
            // happens-before relation. So if 'exited' was set before calling
            // sendFdWakeup, then we should be able to reliably read it after.
            // And thus reading 'exited' in the while loop guard is ok.
        } else {
            // While the RTS attempts to mask signals, some foreign libraries
            // that rely on signal delivery may unmask them. Consequently we
            // may see EINTR. See #24610.
            if (errno != EINTR) {
                sysErrorBelch("Ticker: poll failed: %s", strerror(errno));
            }
        }

        // first try a cheap test
        if (RELAXED_LOAD_ALWAYS(&stopped)) {
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

    /* Open the interrupt fd synchronously.
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

    if (interruptfd_r != -1) {
        // don't leak the old file descriptors after a fork (#25280)
        closeFdWakeup(interruptfd_r, interruptfd_w);
    }
    newFdWakeup(&interruptfd_r, &interruptfd_w);

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
    sendFdWakeup(interruptfd_w);

    // wait for ticker to terminate if necessary
    if (wait) {
        if (pthread_join(thread, NULL)) {
            sysErrorBelch("Ticker: Failed to join: %s", strerror(errno));
        }
        closeFdWakeup(interruptfd_r, interruptfd_w);
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
