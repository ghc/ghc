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


// Forward declarations of local types and helper functions to hide the
// difference between ppoll() and select()
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
typedef struct timespec timeout;  // for ppoll()
typedef struct { struct pollfd pollfds[1]; } fdset;
#else
typedef struct timeval timeout;   // for select()
typedef struct { int fd; fd_set selectfds; } fdset; // need to stash fd
#endif
static void poll_init_timeout(timeout *tv, Time t);
static void poll_init_fdset(fdset *fds, int fd); // single fd only
// poll_*_timeout returns >0 if fd ready, ==0 if timeout, <0 if error
static int poll_no_timeout(fdset *fdset);
static int poll_with_timeout(fdset *fdset, timeout *t);


static Time itimer_interval = DEFAULT_TICK_INTERVAL;

// Atomic variable used by client threads to communicate that they want the
// ticker thread to pause. This communication is one-way, with no
// acknowledgement.
static bool pause_request;

// Atomic variable used by other threads to communicate that they want the
// ticker thread to exit.
static bool exit_request;

// Used to wait for the ticker thread to terminate after asking it to exit.
static OSThreadId thread;

// fds for interrupting the ticker
static int interruptfd_r = -1, interruptfd_w = -1;

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;

    // Thread-local view of our state. We compare these with the corresponding
    // atomic shared variables used to request state changes.
    bool paused  = true;  // updated from atomic shared var pause_request
    bool exit    = false; // updated from atomic shared var exit_request
    // Note that we start paused.

    timeout timeout;
    fdset fdset;
    poll_init_timeout(&timeout, itimer_interval);
    poll_init_fdset(&fdset, interruptfd_r);

    while (!exit) {

        int notify;
        if (paused) {
            notify = poll_no_timeout(&fdset);
        } else {
            notify = poll_with_timeout(&fdset, &timeout);
        }

        if (RTS_LIKELY(notify == 0)) {
            // The time expired, no state change notification.
            handle_tick(0);

        } else if (notify > 0) {
            // State change notification, check the request variables.

            // We rely on sendFdWakeup and select/poll to provide the
            // happens-before relation. So if the request variables are set
            // before calling sendFdWakeup, then we should be able to reliably
            // read them here afterwards.
            collectFdWakeup(interruptfd_r);

            paused = ACQUIRE_LOAD_ALWAYS(&pause_request);
            exit   = RELAXED_LOAD_ALWAYS(&exit_request);
        } else if (errno != EINTR) {
            // While the RTS attempts to mask signals, some foreign libraries
            // that rely on signal delivery may unmask them. Consequently we
            // may see EINTR. See #24610.
            sysErrorBelch("Ticker: poll failed: %s", strerror(errno));
        }
    }

    return NULL;
}

/* Initialise the ticker on startup or re-initialise the ticker after a fork().
 * In the fork case, the thread will not be present, but fds are inherited.
 *
 * The ticker is started in the paused state. Use unpauseTicker to continue.
 */
void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;
    pause_request = true;
    exit_request = false;
#if defined(HAVE_SIGNAL_H)
    sigset_t mask, omask;
    int sigret;
#endif
    int ret;

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

/* Asynchronous. Idempotent. */
void unpauseTicker(void)
{
    RELEASE_STORE_ALWAYS(&pause_request, false);
    sendFdWakeup(interruptfd_w);
}

/* Asynchronous. Idempotent.
 * There may be at additional ticks fired after a call to this, but it will
 * usually stop quickly.
 */
void pauseTicker(void)
{
    RELEASE_STORE_ALWAYS(&pause_request, true);
    sendFdWakeup(interruptfd_w);
}

/* Synchronous. Not idempotent.
 * The ticker is guaranteed stopped after this.
 */
void exitTicker(void)
{
    ASSERT(!RELAXED_LOAD_ALWAYS(&exit_request));
    RELEASE_STORE_ALWAYS(&exit_request, true);
    sendFdWakeup(interruptfd_w);

    // wait for ticker to terminate
    if (pthread_join(thread, NULL)) {
        sysErrorBelch("Ticker: Failed to join: %s", strerror(errno));
    }
    closeFdWakeup(interruptfd_r, interruptfd_w);
}

/* Implementation of the local helpers, to hide the difference between ppoll()
 * and select().
 */
#if defined(HAVE_DECL_PPOLL) && HAVE_DECL_PPOLL == 1
static void poll_init_timeout(timeout *tv, Time t)
{
    tv->tv_sec  = TimeToSeconds(t);
    tv->tv_nsec = TimeToNS(t) % 1000000000;
}

static void poll_init_fdset(fdset *fds, int fd)
{
    fds->pollfds[0].fd = fd;
    fds->pollfds[0].events = POLLIN;
}

static int poll_no_timeout(fdset *fds)
{
    int nfds = 1;
    return ppoll(fds->pollfds, nfds, NULL, NULL);
}

static int poll_with_timeout(fdset *fds, timeout *ts)
{
    int nfds = 1;
    return ppoll(fds->pollfds, nfds, ts, NULL);
}

#else // select()

static void poll_init_timeout(timeout *tv, Time t)
{
    tv->tv_sec  = TimeToSeconds(t);
    /* convert remainder time in nanoseconds to microseconds, rounding up: */
    tv->tv_usec = ((TimeToNS(t) % 1000000000) + 999) / 1000;
}

static void poll_init_fdset(fdset *fds, int fd)
{
    /* select() modifies the fd_set: it uses the same fd_set for reporting as
     * for input. Thus we must rebuild it every time. We can optimise this
     * rebuilding somewhat however if we rely on select() not modifying the
     * bits that we didn't ask it to look at. So we can zero the fd_set just
     * once, and then only reset the single bit for the single fd, before each
     * call to selct().
     */
    fds->fd = fd;
    FD_ZERO(&fds->selectfds);
}

static int poll_no_timeout(fdset *fds)
{
    /* select() modifies the fd_set so we must set it every time, but we rely
     * on it not touching other bits to avoid having to FD_ZERO it every time
     */
    FD_SET(fds->fd, &fds->selectfds);
    int nfds = fds->fd+1;
    return select(nfds, &fds->selectfds, NULL, NULL, NULL);
}

static int poll_with_timeout(fdset *fds, timeout *tv)
{
    struct timeval tv_tmp = *tv; // copy since select may change this value.
    /* select() modifies the fd_set so we must set it every time, but we rely
     * on it not touching other bits to avoid having to FD_ZERO it every time
     */
    FD_SET(fds->fd, &fds->selectfds);
    int nfds = fds->fd+1;
    return select(nfds, &fds->selectfds, NULL, NULL, &tv_tmp);
}
#endif

/* This is obsolete, but is used in the unix package for now */
int
rtsTimerSignal(void)
{
    return SIGALRM;
}
