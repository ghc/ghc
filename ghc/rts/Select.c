/* -----------------------------------------------------------------------------
 * $Id: Select.c,v 1.12 2000/04/03 15:24:21 rrt Exp $
 *
 * (c) The GHC Team 1995-1999
 *
 * Support for concurrent non-blocking I/O and thread waiting.
 *
 * ---------------------------------------------------------------------------*/

/* we're outside the realms of POSIX here... */
#define NON_POSIX_SOURCE

#include "Rts.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Itimer.h"
#include "Signals.h"

# if defined(HAVE_SYS_TYPES_H)
#  include <sys/types.h>
# endif

# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif

nat ticks_since_select = 0;

/* Argument 'wait' says whether to wait for I/O to become available,
 * or whether to just check and return immediately.  If there are
 * other threads ready to run, we normally do the non-waiting variety,
 * otherwise we wait (see Schedule.c).
 *
 * SMP note: must be called with sched_mutex locked.
 */
void
awaitEvent(rtsBool wait)
{
#ifdef mingw32_TARGET_OS
/*
 * Win32 doesn't support select(). ToDo: use MsgWaitForMultipleObjects()
 * to achieve (similar) effect.
 *
 */
    return;
#else

    StgTSO *tso, *prev, *next;
    rtsBool ready;
    fd_set rfd,wfd;
    int numFound;
    nat min, delta;
    int maxfd = -1;
    rtsBool select_succeeded = rtsTrue;
   
    struct timeval tv;
#ifndef linux_TARGET_OS
    struct timeval tv_before,tv_after;
#endif

    IF_DEBUG(scheduler,belch("Checking for threads blocked on I/O...\n"));

    /* loop until we've woken up some threads.  This loop is needed
     * because the select timing isn't accurate, we sometimes sleep
     * for a while but not long enough to wake up a thread in
     * a threadDelay.
     */
    do {

      /* see how long it's been since we last checked the blocked queue.
       * ToDo: make this check atomic, so we don't lose any ticks.
       */
      delta = ticks_since_select;
      ticks_since_select = 0;
      delta = delta * TICK_MILLISECS * 1000;

      min = wait == rtsTrue ? 0x7fffffff : 0;

      /* 
       * Collect all of the fd's that we're interested in, and capture
       * the minimum waiting time (in microseconds) for the delayed threads.
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

	case BlockedOnDelay:
	  {
	    int candidate; /* signed int is intentional */
#if defined(HAVE_SETITIMER) || defined(mingw32_TARGET_OS)
	    candidate = tso->block_info.delay;
#else
	    candidate = tso->block_info.target - getourtimeofday();
	    if (candidate < 0) {
	      candidate = 0;
	    }
#endif
	    if ((nat)candidate < min) {
	      min = candidate;
	    }
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

      tv.tv_sec = min / 1000000;
      tv.tv_usec = min % 1000000;

#ifndef linux_TARGET_OS
      gettimeofday(&tv_before, (struct timezone *) NULL);
#endif

      while (!interrupted &&
	     (numFound = select(maxfd+1, &rfd, &wfd, NULL, &tv)) < 0) {
	if (errno != EINTR) {
	  /* fflush(stdout); */
	  perror("select");
	  barf("select failed");
	}
	ACQUIRE_LOCK(&sched_mutex);

	/* We got a signal; could be one of ours.  If so, we need
	 * to start up the signal handler straight away, otherwise
	 * we could block for a long time before the signal is
	 * serviced.
	 */
	if (signals_pending()) {
	  RELEASE_LOCK(&sched_mutex);
	  start_signal_handlers();
	  /* Don't wake up any other threads that were waiting on I/O */
	  select_succeeded = rtsFalse;
	  break;
	}

	/* If new runnable threads have arrived, stop waiting for
	 * I/O and run them.
	 */
	if (run_queue_hd != END_TSO_QUEUE) {
	  RELEASE_LOCK(&sched_mutex);
	  select_succeeded = rtsFalse;
	  break;
	}
	
	RELEASE_LOCK(&sched_mutex);
      }	

#ifdef linux_TARGET_OS
      /* on Linux, tv is set to indicate the amount of time not
       * slept, so we don't need to gettimeofday() to find out.
       */
      delta += min - (tv.tv_sec * 1000000 + tv.tv_usec);
#else
      gettimeofday(&tv_after, (struct timezone *) NULL);
      delta += (tv_after.tv_sec - tv_before.tv_sec) * 1000000 +
	tv_after.tv_usec - tv_before.tv_usec;
#endif

#if 0
      if (delta != 0) { fprintf(stderr,"waited: %d %d %d\n", min, delta,
				interrupted); }
#endif

      ACQUIRE_LOCK(&sched_mutex);

      /* Step through the waiting queue, unblocking every thread that now has
       * a file descriptor in a ready state.
	
       * For the delayed threads, decrement the number of microsecs
       * we've been blocked for. Unblock the threads that have thusly expired.
       */

      prev = NULL;
      for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
	next = tso->link;
	switch (tso->why_blocked) {
	case BlockedOnRead:
	  ready = select_succeeded && FD_ISSET(tso->block_info.fd, &rfd);
	  break;
	
	case BlockedOnWrite:
	  ready = select_succeeded && FD_ISSET(tso->block_info.fd, &wfd);
	  break;
	
	case BlockedOnDelay:
	  {
#if defined(HAVE_SETITIMER) || defined(mingw32_TARGET_OS)
	    if (tso->block_info.delay > delta) {
	      tso->block_info.delay -= delta;
	      ready = 0;
	    } else {
	      tso->block_info.delay = 0;
	      ready = 1;
	    }
#else
	    int candidate; /* signed int is intentional */
	    candidate = tso->block_info.target - getourtimeofday();
	    if (candidate < 0) {
	      candidate = 0;
	    }
	    if ((nat)candidate > delta) {
	      ready = 0;
	    } else {
	      ready = 1;
	    }
#endif
	    break;
	  }
	
	default:
	  barf("awaitEvent");
	}
      
	if (ready) {
	  IF_DEBUG(scheduler,belch("Waking up thread %d\n", tso->id));
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

    } while (wait && run_queue_hd == END_TSO_QUEUE);
#endif
}
