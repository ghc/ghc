/* -----------------------------------------------------------------------------
 * $Id: Select.c,v 1.2 1999/09/13 08:28:45 sof Exp $
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
    int min, numFound, delta;
    int maxfd = -1;
   
    struct timeval tv,tv_before,tv_after;

    IF_DEBUG(scheduler,belch("Checking for threads blocked on I/O...\n"));

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
	  if ((int)tso->block_info.delay < min)
	    min = tso->block_info.delay;
	  continue;
	}

      default:
	barf("AwaitEvent");
      }
    }

    /* Check for any interesting events */

    tv.tv_sec = min / 1000000;
    tv.tv_usec = min % 1000000;

    gettimeofday(&tv_before, (struct timezone *) NULL);

    while ((numFound = select(maxfd+1, &rfd, &wfd, NULL, &tv)) < 0) {
      if (errno != EINTR) {
	/* fflush(stdout); */
	fprintf(stderr, "awaitEvent: select failed\n");
	stg_exit(EXIT_FAILURE);
      }
    }	

    if (numFound != 0) { 
      /* 
	File descriptors ready, but we don't know how much time was spent
	in the select(). To interpolate, we compare the time before
	and after the select(). 
      */

      gettimeofday(&tv_after, (struct timezone *) NULL);
      delta += (tv_after.tv_sec - tv_before.tv_sec) * 1000000 +
	        tv_after.tv_usec - tv_before.tv_usec;

    } else {
      delta += min;
    }

    /*
      Step through the waiting queue, unblocking every thread that now has
      a file descriptor in a ready state.

      For the delayed threads, decrement the number of microsecs
      we've been blocked for. Unblock the threads that have thusly expired.
     */

    prev = NULL;
    for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
      next = tso->link;
      switch (tso->why_blocked) {
      case BlockedOnRead:
	ready = FD_ISSET(tso->block_info.fd, &rfd);
	break;
	
      case BlockedOnWrite:
	ready = FD_ISSET(tso->block_info.fd, &wfd);
	break;
	
      case BlockedOnDelay:
	tso->block_info.delay -= delta;
	ready = (tso->block_info.delay <= 0);
	break;
	
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
#endif
}
