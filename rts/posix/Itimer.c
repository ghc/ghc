/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2007
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * The interval timer is used for profiling and for context switching in the
 * threaded build.  Though POSIX 1003.1b includes a standard interface for
 * such things, no one really seems to be implementing them yet.  Even
 * Solaris 2.3 only seems to provide support for @CLOCK_REAL@, whereas we're
 * keen on getting access to @CLOCK_VIRTUAL@.
 *
 * Hence, we use the old-fashioned @setitimer@ that just about everyone seems
 * to support.  So much for standards.
 */

#include "PosixSource.h"
#include "Rts.h"

#include "Ticker.h"
#include "Itimer.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "Clock.h"

/* As recommended in the autoconf manual */
# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#include <string.h>

/*
 * timer_create doesn't exist and setitimer doesn't fire on iOS, so we're using
 * a pthreads-based implementation. It may be to do with interference with the
 * signals of the debugger. Revisit. See #7723.
 */
#if defined(ios_HOST_OS)
#define USE_PTHREAD_FOR_ITIMER
#endif

/*
 * On Linux in the threaded RTS we can use timerfd_* (introduced in Linux
 * 2.6.25) and a thread instead of alarm signals. It avoids the risk of
 * interrupting syscalls (see #10840) and the risk of being accidentally
 * modified in user code using signals.
 */
#if defined(linux_HOST_OS) && defined(THREADED_RTS) && HAVE_SYS_TIMERFD_H
#include <sys/timerfd.h>
#include <fcntl.h>
#define USE_PTHREAD_FOR_ITIMER
#define USE_TIMERFD_FOR_ITIMER 1
#undef USE_TIMER_CREATE
#else
#define USE_TIMERFD_FOR_ITIMER 0
#endif

/*
 * TFD_CLOEXEC has been added in Linux 2.6.26.
 * If it is not available, we use fcntl(F_SETFD).
 */
#ifndef TFD_CLOEXEC
#define TFD_CLOEXEC 0
#endif


#if defined(USE_PTHREAD_FOR_ITIMER)
#include <pthread.h>
#include <unistd.h>
#endif

/*
 * We use a realtime timer by default.  I found this much more
 * reliable than a CPU timer:
 *
 * Experiments with different frequences: using
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
 * fire.  Therfore we used to tick in realtime in the threaded RTS and
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

#if defined(solaris2_HOST_OS)
/* USE_TIMER_CREATE is usually disabled for Solaris. In fact it is
   supported well on this OS, but requires additional privilege. When
   user does not have it, then the testing configure program fails
   which results in USE_TIMER_CREATE not defined.
   On the other hand when we cross-compile, then we optimistically
   assume usage of timer_create function. The problem is that if we
   cross compile for example from i386-solaris2 to x86_64-solaris2,
   then the build fails with error like this:

ghc-stage2: timer_create: Not owner

   which happens on first ghc-stage2 invocation. So to support
   cross-compilation to Solaris we manually undefine USE_TIMER_CREATE
   here */
#undef USE_TIMER_CREATE
#endif /* solaris2_HOST_OS */

#if defined(USE_TIMER_CREATE)
#  define ITIMER_SIGNAL SIGVTALRM
#elif defined(HAVE_SETITIMER)
#  define ITIMER_SIGNAL  SIGALRM
   // Using SIGALRM can leads to problems, see #850.  But we have no
   // option if timer_create() is not available.
#else
#  error No way to set an interval timer.
#endif

#if defined(USE_TIMER_CREATE)
static timer_t timer;
#endif

static Time itimer_interval = DEFAULT_TICK_INTERVAL;

#if !defined(USE_PTHREAD_FOR_ITIMER)
static void install_vtalrm_handler(TickProc handle_tick)
{
    struct sigaction action;

    action.sa_handler = handle_tick;

    sigemptyset(&action.sa_mask);

#ifdef SA_RESTART
    // specify SA_RESTART.  One consequence if we don't do this is
    // that readline gets confused by the -threaded RTS.  It seems
    // that if a SIGALRM handler is installed without SA_RESTART,
    // readline installs its own SIGALRM signal handler (see
    // readline's signals.c), and this somehow causes readline to go
    // wrong when the input exceeds a single line (try it).
    action.sa_flags = SA_RESTART;
#else
    action.sa_flags = 0;
#endif

    if (sigaction(ITIMER_SIGNAL, &action, NULL) == -1) {
        sysErrorBelch("sigaction");
        stg_exit(EXIT_FAILURE);
    }
}
#endif

#if defined(USE_PTHREAD_FOR_ITIMER)
enum ItimerState {STOPPED, RUNNING, STOPPING, EXITED};
static volatile enum ItimerState itimer_state = STOPPED;

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;
    uint64_t nticks;
    int timerfd = -1;

#if USE_TIMERFD_FOR_ITIMER
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
    int ret = timerfd_settime(timerfd, 0, &it, NULL);
#endif

    while (1) {
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
        switch (itimer_state) {
            case RUNNING:
                handle_tick(0);
                break;
            case STOPPED:
                break;
            case STOPPING:
                itimer_state = STOPPED;
                break;
            case EXITED:
                if (USE_TIMERFD_FOR_ITIMER)
                    close(timerfd);
                return NULL;
        }
    }
    return NULL; // Never reached.
}
#endif

void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;

#if defined(USE_PTHREAD_FOR_ITIMER)
    pthread_t tid;
    int r = pthread_create(&tid, NULL, itimer_thread_func, (void*)handle_tick);
    if (!r) {
        pthread_detach(tid);
#if HAVE_PTHREAD_SETNAME_NP
        pthread_setname_np(tid, "ghc_ticker");
#endif
    }
#elif defined(USE_TIMER_CREATE)
    {
        struct sigevent ev;

        // Keep programs like valgrind happy
        memset(&ev, 0, sizeof(ev));

        ev.sigev_notify = SIGEV_SIGNAL;
        ev.sigev_signo  = ITIMER_SIGNAL;

        if (timer_create(CLOCK_ID, &ev, &timer) != 0) {
            sysErrorBelch("timer_create");
            stg_exit(EXIT_FAILURE);
        }
    }
    install_vtalrm_handler(handle_tick);
#else
    install_vtalrm_handler(handle_tick);
#endif
}

void
startTicker(void)
{
#if defined(USE_PTHREAD_FOR_ITIMER)
    // sanity check
    if (itimer_state == EXITED) {
        sysErrorBelch("ITimer: Tried to start a dead timer!\n");
        stg_exit(EXIT_FAILURE);
    }
    itimer_state = RUNNING;
#elif defined(USE_TIMER_CREATE)
    {
        struct itimerspec it;

        it.it_value.tv_sec  = TimeToSeconds(itimer_interval);
        it.it_value.tv_nsec = TimeToNS(itimer_interval) % 1000000000;
        it.it_interval = it.it_value;

        if (timer_settime(timer, 0, &it, NULL) != 0) {
            sysErrorBelch("timer_settime");
            stg_exit(EXIT_FAILURE);
        }
    }
#else
    {
        struct itimerval it;

        it.it_value.tv_sec = TimeToSeconds(itimer_interval);
        it.it_value.tv_usec = TimeToUS(itimer_interval) % 1000000;
        it.it_interval = it.it_value;

        if (setitimer(ITIMER_REAL, &it, NULL) != 0) {
            sysErrorBelch("setitimer");
            stg_exit(EXIT_FAILURE);
        }
    }
#endif
}

void
stopTicker(void)
{
#if defined(USE_PTHREAD_FOR_ITIMER)
    if (itimer_state == RUNNING) {
        itimer_state = STOPPING;
        // Note that the timer may fire once more, but that's okay;
        // handle_tick is only called when itimer_state == RUNNING
    }
#elif defined(USE_TIMER_CREATE)
    struct itimerspec it;

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 0;
    it.it_interval = it.it_value;

    if (timer_settime(timer, 0, &it, NULL) != 0) {
        sysErrorBelch("timer_settime");
        stg_exit(EXIT_FAILURE);
    }
#else
    struct itimerval it;

    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    it.it_interval = it.it_value;

    if (setitimer(ITIMER_REAL, &it, NULL) != 0) {
        sysErrorBelch("setitimer");
        stg_exit(EXIT_FAILURE);
    }
#endif
}

void
exitTicker (rtsBool wait STG_UNUSED)
{
#if defined(USE_PTHREAD_FOR_ITIMER)
    itimer_state = EXITED;
#elif defined(USE_TIMER_CREATE)
    // Before deleting the timer set the signal to ignore to avoid the
    // possibility of the signal being delivered after the timer is deleted.
    signal(ITIMER_SIGNAL, SIG_IGN);
    timer_delete(timer);
    // ignore errors - we don't really care if it fails.
#endif
}

int
rtsTimerSignal(void)
{
    return ITIMER_SIGNAL;
}
