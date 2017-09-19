/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1995-2002
 *
 * Support for concurrent non-blocking I/O and thread waiting in the
 * non-threaded RTS.  In the threded RTS, this file is not used at
 * all, instead we use the IO manager thread implemented in Haskell in
 * the base package.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Signals.h"
#include "Schedule.h"
#include "Prelude.h"
#include "RaiseAsync.h"
#include "RtsUtils.h"
#include "Capability.h"
#include "Select.h"
#include "AwaitEvent.h"
#include "Stats.h"
#include "GetTime.h"

# ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
# endif

# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

#include <errno.h>
#include <string.h>

#include "Clock.h"

#if !defined(THREADED_RTS)

// The target time for a threadDelay is stored in a one-word quantity
// in the TSO (tso->block_info.target).  On a 32-bit machine we
// therefore can't afford to use nanosecond resolution because it
// would overflow too quickly, so instead we use millisecond
// resolution.

#if SIZEOF_VOID_P == 4
#define LowResTimeToTime(t)          (USToTime((t) * 1000))
#define TimeToLowResTimeRoundDown(t) ((LowResTime)(TimeToUS(t) / 1000))
#define TimeToLowResTimeRoundUp(t)   ((TimeToUS(t) + 1000-1) / 1000)
#else
#define LowResTimeToTime(t) (t)
#define TimeToLowResTimeRoundDown(t) (t)
#define TimeToLowResTimeRoundUp(t)   (t)
#endif

/*
 * Return the time since the program started, in LowResTime,
 * rounded down.
 */
static LowResTime getLowResTimeOfDay(void)
{
    return TimeToLowResTimeRoundDown(getProcessElapsedTime());
}

/*
 * For a given microsecond delay, return the target time in LowResTime.
 */
LowResTime getDelayTarget (HsInt us)
{
    Time elapsed;
    elapsed = getProcessElapsedTime();

    // If the desired target would be larger than the maximum Time,
    // default to the maximum Time. (#7087)
    if (us > TimeToUS(TIME_MAX - elapsed)) {
        return TimeToLowResTimeRoundDown(TIME_MAX);
    } else {
        // round up the target time, because we never want to sleep *less*
        // than the desired amount.
        return TimeToLowResTimeRoundUp(elapsed + USToTime(us));
    }
}

/* There's a clever trick here to avoid problems when the time wraps
 * around.  Since our maximum delay is smaller than 31 bits of ticks
 * (it's actually 31 bits of microseconds), we can safely check
 * whether a timer has expired even if our timer will wrap around
 * before the target is reached, using the following formula:
 *
 *        (int)((uint)current_time - (uint)target_time) < 0
 *
 * if this is true, then our time has expired.
 * (idea due to Andy Gill).
 */
static bool wakeUpSleepingThreads (LowResTime now)
{
    StgTSO *tso;
    bool flag = false;

    while (sleeping_queue != END_TSO_QUEUE) {
        tso = sleeping_queue;
        if (((long)now - (long)tso->block_info.target) < 0) {
            break;
        }
        sleeping_queue = tso->_link;
        tso->why_blocked = NotBlocked;
        tso->_link = END_TSO_QUEUE;
        IF_DEBUG(scheduler, debugBelch("Waking up sleeping thread %lu\n",
                                       (unsigned long)tso->id));
        // MainCapability: this code is !THREADED_RTS
        pushOnRunQueue(&MainCapability,tso);
        flag = true;
    }
    return flag;
}

static void GNUC3_ATTRIBUTE(__noreturn__)
fdOutOfRange (int fd)
{
    errorBelch("file descriptor %d out of range for select (0--%d).\n"
               "Recompile with -threaded to work around this.",
               fd, (int)FD_SETSIZE);
    stg_exit(EXIT_FAILURE);
}

/*
 * State of individual file descriptor after a 'select()' poll.
 */
enum FdState {
    RTS_FD_IS_READY = 0,
    RTS_FD_IS_BLOCKING,
    RTS_FD_IS_INVALID,
};

static enum FdState fdPollReadState (int fd)
{
    int r;
    fd_set rfd;
    struct timeval now;

    FD_ZERO(&rfd);
    FD_SET(fd, &rfd);

    /* only poll */
    now.tv_sec  = 0;
    now.tv_usec = 0;
    for (;;)
    {
        r = select(fd+1, &rfd, NULL, NULL, &now);
        /* the descriptor is sane */
        if (r != -1)
            break;

        switch (errno)
        {
            case EBADF: return RTS_FD_IS_INVALID;
            case EINTR: continue;
            default:
                sysErrorBelch("select");
                stg_exit(EXIT_FAILURE);
        }
    }

    if (r == 0)
        return RTS_FD_IS_BLOCKING;
    else
        return RTS_FD_IS_READY;
}

static enum FdState fdPollWriteState (int fd)
{
    int r;
    fd_set wfd;
    struct timeval now;

    FD_ZERO(&wfd);
    FD_SET(fd, &wfd);

    /* only poll */
    now.tv_sec  = 0;
    now.tv_usec = 0;
    for (;;)
    {
        r = select(fd+1, NULL, &wfd, NULL, &now);
        /* the descriptor is sane */
        if (r != -1)
            break;

        switch (errno)
        {
            case EBADF: return RTS_FD_IS_INVALID;
            case EINTR: continue;
            default:
                sysErrorBelch("select");
                stg_exit(EXIT_FAILURE);
        }
    }

    if (r == 0)
        return RTS_FD_IS_BLOCKING;
    else
        return RTS_FD_IS_READY;
}

/* Argument 'wait' says whether to wait for I/O to become available,
 * or whether to just check and return immediately.  If there are
 * other threads ready to run, we normally do the non-waiting variety,
 * otherwise we wait (see Schedule.c).
 *
 * SMP note: must be called with sched_mutex locked.
 *
 * Windows: select only works on sockets, so this doesn't really work,
 * though it makes things better than before. MsgWaitForMultipleObjects
 * should really be used, though it only seems to work for read handles,
 * not write handles.
 *
 */
void
awaitEvent(bool wait)
{
    StgTSO *tso, *prev, *next;
    fd_set rfd,wfd;
    int numFound;
    int maxfd = -1;
    bool seen_bad_fd = false;
    struct timeval tv, *ptv;
    LowResTime now;

    IF_DEBUG(scheduler,
             debugBelch("scheduler: checking for threads blocked on I/O");
             if (wait) {
                 debugBelch(" (waiting)");
             }
             debugBelch("\n");
             );

    /* loop until we've woken up some threads.  This loop is needed
     * because the select timing isn't accurate, we sometimes sleep
     * for a while but not long enough to wake up a thread in
     * a threadDelay.
     */
    do {

      now = getLowResTimeOfDay();
      if (wakeUpSleepingThreads(now)) {
          return;
      }

      /*
       * Collect all of the fd's that we're interested in
       */
      FD_ZERO(&rfd);
      FD_ZERO(&wfd);

      for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
        next = tso->_link;

      /* On older FreeBSDs, FD_SETSIZE is unsigned. Cast it to signed int
       * in order to switch off the 'comparison between signed and
       * unsigned error message
       * Newer versions of FreeBSD have switched to unsigned int:
       *   https://github.com/freebsd/freebsd/commit/12ae7f74a071f0439763986026525094a7032dfd
       *   http://fa.freebsd.cvs-all.narkive.com/bCWNHbaC/svn-commit-r265051-head-sys-sys
       * So the (int) cast should be removed across the code base once
       * GHC requires a version of FreeBSD that has that change in it.
       */
        switch (tso->why_blocked) {
        case BlockedOnRead:
          {
            int fd = tso->block_info.fd;
            if ((fd >= (int)FD_SETSIZE) || (fd < 0)) {
                fdOutOfRange(fd);
            }
            maxfd = (fd > maxfd) ? fd : maxfd;
            FD_SET(fd, &rfd);
            continue;
          }

        case BlockedOnWrite:
          {
            int fd = tso->block_info.fd;
            if ((fd >= (int)FD_SETSIZE) || (fd < 0)) {
                fdOutOfRange(fd);
            }
            maxfd = (fd > maxfd) ? fd : maxfd;
            FD_SET(fd, &wfd);
            continue;
          }

        default:
          barf("AwaitEvent");
        }
      }

      if (!wait) {
          // just poll
          tv.tv_sec  = 0;
          tv.tv_usec = 0;
          ptv = &tv;
      } else if (sleeping_queue != END_TSO_QUEUE) {
          /* SUSv2 allows implementations to have an implementation defined
           * maximum timeout for select(2). The standard requires
           * implementations to silently truncate values exceeding this maximum
           * to the maximum. Unfortunately, OSX and the BSD don't comply with
           * SUSv2, instead opting to return EINVAL for values exceeding a
           * timeout of 1e8.
           *
           * Select returning an error crashes the runtime in a bad way. To
           * play it safe we truncate any timeout to 31 days, as SUSv2 requires
           * any implementations maximum timeout to be larger than this.
           *
           * Truncating the timeout is not an issue, because if nothing
           * interesting happens when the timeout expires, we'll see that the
           * thread still wants to be blocked longer and simply block on a new
           * iteration of select(2).
           */
          const time_t max_seconds = 2678400; // 31 * 24 * 60 * 60

          Time min = LowResTimeToTime(sleeping_queue->block_info.target - now);
          tv.tv_sec  = TimeToSeconds(min);
          if (tv.tv_sec < max_seconds) {
              tv.tv_usec = TimeToUS(min) % 1000000;
          } else {
              tv.tv_sec = max_seconds;
              tv.tv_usec = 0;
          }
          ptv = &tv;
      } else {
          ptv = NULL;
      }

      /* Check for any interesting events */

      while ((numFound = select(maxfd+1, &rfd, &wfd, NULL, ptv)) < 0) {
          if (errno != EINTR) {
            if ( errno == EBADF ) {
                seen_bad_fd = true;
                break;
            } else {
                sysErrorBelch("select");
                stg_exit(EXIT_FAILURE);
            }
          }

          /* We got a signal; could be one of ours.  If so, we need
           * to start up the signal handler straight away, otherwise
           * we could block for a long time before the signal is
           * serviced.
           */
#if defined(RTS_USER_SIGNALS)
          if (RtsFlags.MiscFlags.install_signal_handlers && signals_pending()) {
              startSignalHandlers(&MainCapability);
              return; /* still hold the lock */
          }
#endif

          /* we were interrupted, return to the scheduler immediately.
           */
          if (sched_state >= SCHED_INTERRUPTING) {
              return; /* still hold the lock */
          }

          /* check for threads that need waking up
           */
          wakeUpSleepingThreads(getLowResTimeOfDay());

          /* If new runnable threads have arrived, stop waiting for
           * I/O and run them.
           */
          if (!emptyRunQueue(&MainCapability)) {
              return; /* still hold the lock */
          }
      }

      /* Step through the waiting queue, unblocking every thread that now has
       * a file descriptor in a ready state.
       */

      prev = NULL;
      {
          /*
           * The queue is being rebuilt in this loop:
           * 'blocked_queue_hd' will contain already
           * traversed blocked TSOs. As a result you
           * can't use functions accessing 'blocked_queue_hd'.
           */
          for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
              next = tso->_link;
              int fd;
              enum FdState fd_state = RTS_FD_IS_BLOCKING;

              switch (tso->why_blocked) {
              case BlockedOnRead:
                  fd = tso->block_info.fd;

                  if (seen_bad_fd) {
                      fd_state = fdPollReadState (fd);
                  } else if (FD_ISSET(fd, &rfd)) {
                      fd_state = RTS_FD_IS_READY;
                  }
                  break;
              case BlockedOnWrite:
                  fd = tso->block_info.fd;

                  if (seen_bad_fd) {
                      fd_state = fdPollWriteState (fd);
                  } else if (FD_ISSET(fd, &wfd)) {
                      fd_state = RTS_FD_IS_READY;
                  }
                  break;
              default:
                  barf("awaitEvent");
              }

              switch (fd_state) {
              case RTS_FD_IS_INVALID:
                  /*
                   * Don't let RTS loop on such descriptors,
                   * pass an IOError to blocked threads (Trac #4934)
                   */
                  IF_DEBUG(scheduler,
                      debugBelch("Killing blocked thread %lu on bad fd=%i\n",
                                 (unsigned long)tso->id, fd));
                  raiseAsync(&MainCapability, tso,
                      (StgClosure *)blockedOnBadFD_closure, false, NULL);
                  break;
              case RTS_FD_IS_READY:
                  IF_DEBUG(scheduler,
                      debugBelch("Waking up blocked thread %lu\n",
                                 (unsigned long)tso->id));
                  tso->why_blocked = NotBlocked;
                  tso->_link = END_TSO_QUEUE;
                  pushOnRunQueue(&MainCapability,tso);
                  break;
              case RTS_FD_IS_BLOCKING:
                  if (prev == NULL)
                      blocked_queue_hd = tso;
                  else
                      setTSOLink(&MainCapability, prev, tso);
                  prev = tso;
                  break;
              }
          }

          if (prev == NULL)
              blocked_queue_hd = blocked_queue_tl = END_TSO_QUEUE;
          else {
              prev->_link = END_TSO_QUEUE;
              blocked_queue_tl = prev;
          }
      }

    } while (wait && sched_state == SCHED_RUNNING
             && emptyRunQueue(&MainCapability));
}

#endif /* THREADED_RTS */
