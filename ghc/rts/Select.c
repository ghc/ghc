/* -----------------------------------------------------------------------------
 * $Id: Select.c,v 1.18 2001/10/31 10:34:29 simonmar Exp $
 *
 * (c) The GHC Team 1995-1999
 *
 * Support for concurrent non-blocking I/O and thread waiting.
 *
 * ---------------------------------------------------------------------------*/

/* we're outside the realms of POSIX here... */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Itimer.h"
#include "Signals.h"

# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif

# ifdef mingw32_TARGET_OS
#  include <windows.h>
# endif

/* last timestamp */
nat timestamp = 0;

/* keep track of the number of ticks since we last called
 * gettimeofday(), to avoid having to call it every time we need
 * a timestamp.
 */
nat ticks_since_timestamp = 0;

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
rtsBool
wakeUpSleepingThreads(nat ticks)
{
    StgTSO *tso;
    rtsBool flag = rtsFalse;

    while (sleeping_queue != END_TSO_QUEUE &&
	   (int)(ticks - sleeping_queue->block_info.target) > 0) {
	tso = sleeping_queue;
	sleeping_queue = tso->link;
	tso->why_blocked = NotBlocked;
	tso->link = END_TSO_QUEUE;
	IF_DEBUG(scheduler,belch("Waking up sleeping thread %d\n", tso->id));
	PUSH_ON_RUN_QUEUE(tso);
	flag = rtsTrue;
    }
    return flag;
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
awaitEvent(rtsBool wait)
{
    StgTSO *tso, *prev, *next;
    rtsBool ready;
    fd_set rfd,wfd;
#ifndef mingw32_TARGET_OS
    int numFound;
    int maxfd = -1;
#endif
    rtsBool select_succeeded = rtsTrue;
    struct timeval tv;
    lnat min, ticks;

    tv.tv_sec  = 0;
    tv.tv_usec = 0;

    IF_DEBUG(scheduler,
	     belch("scheduler: checking for threads blocked on I/O");
	     if (wait) {
		 belch(" (waiting)");
	     }
	     belch("\n");
	     );

    /* loop until we've woken up some threads.  This loop is needed
     * because the select timing isn't accurate, we sometimes sleep
     * for a while but not long enough to wake up a thread in
     * a threadDelay.
     */
    do {

      ticks = timestamp = getourtimeofday();
      ticks_since_timestamp = 0;
      if (wakeUpSleepingThreads(ticks)) { 
	  return;
      }

      if (!wait) {
	  min = 0;
      } else if (sleeping_queue != END_TSO_QUEUE) {
	  min = (sleeping_queue->block_info.target - ticks) 
	      * TICK_MILLISECS * 1000;
      } else {
	  min = 0x7ffffff;
      }

#ifndef mingw32_TARGET_OS
      /* 
       * Collect all of the fd's that we're interested in
       */
      FD_ZERO(&rfd);
      FD_ZERO(&wfd);

      for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
	next = tso->link;

	switch (tso->why_blocked) {
	case BlockedOnRead:
	  { 
	    int fd = tso->block_info.fd;
	    maxfd = (fd > maxfd) ? fd : maxfd;
	    FD_SET(fd, &rfd);
	    continue;
	  }

	case BlockedOnWrite:
	  { 
	    int fd = tso->block_info.fd;
	    maxfd = (fd > maxfd) ? fd : maxfd;
	    FD_SET(fd, &wfd);
	    continue;
	  }

	default:
	  barf("AwaitEvent");
	}
      }

      /* Release the scheduler lock while we do the poll.
       * this means that someone might muck with the blocked_queue
       * while we do this, but it shouldn't matter:
       *
       *   - another task might poll for I/O and remove one
       *     or more threads from the blocked_queue.
       *   - more I/O threads may be added to blocked_queue.
       *   - more delayed threads may be added to blocked_queue. We'll
       *     just subtract delta from their delays after the poll.
       *
       * I believe none of these cases lead to trouble --SDM.
       */
      RELEASE_LOCK(&sched_mutex);

      /* Check for any interesting events */
      
      tv.tv_sec  = min / 1000000;
      tv.tv_usec = min % 1000000;

      while ((numFound = select(maxfd+1, &rfd, &wfd, NULL, &tv)) < 0) {
	  if (errno != EINTR) {

	      printf("%d\n", errno);
	      fflush(stdout);
	      perror("select");
	      barf("select failed");
	  }
#else /* on mingwin */
      while (1) {
	  Sleep(0); /* don't busy wait */
#endif /* mingw32_TARGET_OS */
	  ACQUIRE_LOCK(&sched_mutex);

#ifndef mingw32_TARGET_OS
	  /* We got a signal; could be one of ours.  If so, we need
	   * to start up the signal handler straight away, otherwise
	   * we could block for a long time before the signal is
	   * serviced.
	   */
	  if (signals_pending()) {
	      RELEASE_LOCK(&sched_mutex); /* ToDo: kill */
	      startSignalHandlers();
	      ACQUIRE_LOCK(&sched_mutex);
	      return; /* still hold the lock */
	  }
#endif

	  /* we were interrupted, return to the scheduler immediately.
	   */
	  if (interrupted) {
	      return; /* still hold the lock */
	  }
	  
	  /* check for threads that need waking up 
	   */
	  wakeUpSleepingThreads(getourtimeofday());
	  
	  /* If new runnable threads have arrived, stop waiting for
	   * I/O and run them.
	   */
	  if (run_queue_hd != END_TSO_QUEUE) {
	      return; /* still hold the lock */
	  }
	  
	  RELEASE_LOCK(&sched_mutex);
      }

      ACQUIRE_LOCK(&sched_mutex);

      /* Step through the waiting queue, unblocking every thread that now has
       * a file descriptor in a ready state.
       */

      prev = NULL;
      if (select_succeeded) {
	  for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
	      next = tso->link;
	      switch (tso->why_blocked) {
	      case BlockedOnRead:
		  ready = FD_ISSET(tso->block_info.fd, &rfd);
		  break;
	      case BlockedOnWrite:
		  ready = FD_ISSET(tso->block_info.fd, &wfd);
		  break;
	      default:
		  barf("awaitEvent");
	      }
      
	      if (ready) {
		  IF_DEBUG(scheduler,belch("Waking up blocked thread %d\n", tso->id));
		  tso->why_blocked = NotBlocked;
		  tso->link = END_TSO_QUEUE;
		  PUSH_ON_RUN_QUEUE(tso);
	      } else {
		  if (prev == NULL)
		      blocked_queue_hd = tso;
		  else
		      prev->link = tso;
		  prev = tso;
	      }
	  }

	  if (prev == NULL)
	      blocked_queue_hd = blocked_queue_tl = END_TSO_QUEUE;
	  else {
	      prev->link = END_TSO_QUEUE;
	      blocked_queue_tl = prev;
	  }
      }

    } while (wait && !interrupted && run_queue_hd == END_TSO_QUEUE);
}
