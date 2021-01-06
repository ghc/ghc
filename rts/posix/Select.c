/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1995-2002
 *
 * Support for concurrent non-blocking I/O and thread waiting in the
 * non-threaded RTS.  In the threaded RTS, this file is not used at
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
#include "IOManager.h"
#include "Stats.h"
#include "GetTime.h"
#include "Threads.h"

# if defined(HAVE_SYS_SELECT_H)
#  include <sys/select.h>
# endif

# if defined(HAVE_SYS_TYPES_H)
#  include <sys/types.h>
# endif

#include <errno.h>
#include <string.h>

#include "Clock.h"

#if !defined(THREADED_RTS)

#define END_TIMEOUT_QUEUE ((StgTimeoutQueue *)END_TSO_QUEUE)

void registerDelay(Capability *cap, StgMVar *mvar, HsInt usecs)
{
    CapIOManager *iomgr = cap->iomgr;

    Time elapsed = getProcessElapsedTime();
    Time target;

    // If the desired target would be larger than the maximum Time,
    // default to the maximum Time. (#7087)
    if (usecs > TimeToUS(TIME_MAX - elapsed)) {
        target = TIME_MAX;
    } else {
        target = elapsed + USToTime(usecs);
    }

    IF_DEBUG(scheduler,
             debugBelch("scheduler: timer for delay of %llu usec installed at %llu\n",
                       (unsigned long long)usecs, (unsigned long long)target));

    /* Allocate and fill in a new sleep list entry */
    StgTimeoutQueue *p = (StgTimeoutQueue *)allocate(cap, sizeofW(StgTimeoutQueue));
    SET_HDR(p, &stg_TIMEOUT_QUEUE_info, CCS_SYSTEM);
    p->mvar     = mvar;
    p->waketime = target;

    /* Insert the request into the right place in the sorted list.
     * This is not efficient of course, being linear in the worst case.
     * TODO: replace this data structure with a heap or timer wheel.
     */
    StgTimeoutQueue *prev = NULL;
    StgTimeoutQueue *q = iomgr->timeout_queue;
    while (q != END_TIMEOUT_QUEUE && q->waketime < target) {
        prev = q;
        q = q->next;
    }
    p->next = q;
    if (prev == NULL) {
        iomgr->timeout_queue = p;
    } else {
        prev->next = p;
    }
}

/* We use the 64bit Time type from rts/Time.h so our max time (in nanosecond
 * precision) is over 290 years from the epoch of the monotonic clock.
 *
 * Previous limitations forced us to use 31 bits with millisecond precision
 * which meant we would get clock wrap around. There was a cunning formula to
 * determine if the timer had expired, even if the clock had wrapped around.
 * With 64bit Time we do not need to worry about clock wraparound and can just
 * use the simple formula.
 */
static bool wakeUpSleepingThreads (Capability *cap, Time now)
{
    CapIOManager *iomgr = cap->iomgr;

    /* Pop entries from the front of the sleeping queue that are past their
     * wake time, and unblock the corresponding MVars.
     */
    while (iomgr->timeout_queue != END_TIMEOUT_QUEUE) {
        StgTimeoutQueue *q = iomgr->timeout_queue;
        if (now < q->waketime) {
            break;
        }
        iomgr->timeout_queue = q->next;
        IF_DEBUG(scheduler,
                 debugBelch("scheduler: timer expired at %llu, writing to MVar\n",
                           (unsigned long long)q->waketime));
        performTryPutMVar(cap, q->mvar, Unit_closure);
    }
    /* Filling any of these MVars is not guaranteed to wake any threads, since
     * threads many not be still waiting on the MVars. We don't actually need
     * to know if we woke any threads. We need to know if there are now any
     * runnable threads. We can find out simply by checking the run queue.
     */
    return (!emptyRunQueue(cap));
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
awaitEvent(Capability *cap, bool wait)
{
    CapIOManager *iomgr = cap->iomgr;
    StgTSO *tso, *prev, *next;
    fd_set rfd,wfd;
    int numFound;
    int maxfd = -1;
    bool seen_bad_fd = false;
    struct timeval tv, *ptv;
    Time now;

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

      now = getProcessElapsedTime();
      if (wakeUpSleepingThreads(cap, now)) {
          /* If we woke any sleeping threads,
           * return to the scheduler to run them */
          return;
      }

      /*
       * Collect all of the fd's that we're interested in
       */
      FD_ZERO(&rfd);
      FD_ZERO(&wfd);

      for(tso = iomgr->blocked_queue_hd;
          tso != END_TSO_QUEUE;
          tso = next) {
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
      } else if (iomgr->timeout_queue != END_TIMEOUT_QUEUE) {
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

          Time min = iomgr->timeout_queue->waketime - now;
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
              startSignalHandlers(cap);
              return; /* still hold the lock */
          }
#endif

          /* we were interrupted, return to the scheduler immediately.
           */
          if (sched_state >= SCHED_INTERRUPTING) {
              return; /* still hold the lock */
          }

          /* Check for threads that need waking up. If new runnable threads
           * have arrived, stop waiting for I/O and run them.
           */
          if (wakeUpSleepingThreads(cap, getLowResTimeOfDay())) {
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
          for(tso = iomgr->blocked_queue_hd;
              tso != END_TSO_QUEUE;
              tso = next) {
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
                   * pass an IOError to blocked threads (#4934)
                   */
                  IF_DEBUG(scheduler,
                      debugBelch("Killing blocked thread %lu on bad fd=%i\n",
                                 (unsigned long)tso->id, fd));
                  raiseAsync(cap, tso,
                      (StgClosure *)blockedOnBadFD_closure, false, NULL);
                  break;
              case RTS_FD_IS_READY:
                  IF_DEBUG(scheduler,
                      debugBelch("Waking up blocked thread %lu\n",
                                 (unsigned long)tso->id));
                  tso->why_blocked = NotBlocked;
                  tso->_link = END_TSO_QUEUE;
                  pushOnRunQueue(cap,tso);
                  break;
              case RTS_FD_IS_BLOCKING:
                  if (prev == NULL)
                      iomgr->blocked_queue_hd = tso;
                  else
                      setTSOLink(cap, prev, tso);
                  prev = tso;
                  break;
              }
          }

          if (prev == NULL)
              iomgr->blocked_queue_hd =
                iomgr->blocked_queue_tl = END_TSO_QUEUE;
          else {
              prev->_link = END_TSO_QUEUE;
              iomgr->blocked_queue_tl = prev;
          }
      }

    } while (wait && sched_state == SCHED_RUNNING
                  && emptyRunQueue(cap));
}

#endif /* THREADED_RTS */
