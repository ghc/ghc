/* -----------------------------------------------------------------------------
 * $Id: Select.c,v 1.27 2003/03/29 00:00:41 sof Exp $
 *
 * (c) The GHC Team 1995-2002
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
#include "Timer.h"
#include "Itimer.h"
#include "Signals.h"
#include "Capability.h"

# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif

#include <errno.h>
#include <string.h>

/* last timestamp */
nat timestamp = 0;

#ifdef RTS_SUPPORTS_THREADS
static rtsBool isWorkerBlockedInAwaitEvent = rtsFalse;
static rtsBool workerWakeupPending = rtsFalse;
static int workerWakeupPipe[2];
static rtsBool workerWakeupInited = rtsFalse;
#endif

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
    int numFound;
    int maxfd = -1;
    rtsBool select_succeeded = rtsTrue;
    rtsBool unblock_all = rtsFalse;
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

#ifdef RTS_SUPPORTS_THREADS
      if(!workerWakeupInited) {
          pipe(workerWakeupPipe);
          workerWakeupInited = rtsTrue;
      }
      FD_SET(workerWakeupPipe[0], &rfd);
      maxfd = workerWakeupPipe[0] > maxfd ? workerWakeupPipe[0] : maxfd;
#endif
      
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
      
#ifdef RTS_SUPPORTS_THREADS
      isWorkerBlockedInAwaitEvent = rtsTrue;
      workerWakeupPending = rtsFalse;
#endif
      RELEASE_LOCK(&sched_mutex);

      /* Check for any interesting events */
      
      tv.tv_sec  = min / 1000000;
      tv.tv_usec = min % 1000000;

      while ((numFound = select(maxfd+1, &rfd, &wfd, NULL, &tv)) < 0) {
	  if (errno != EINTR) {
	    /* Handle bad file descriptors by unblocking all the
	       waiting threads. Why? Because a thread might have been
	       a bit naughty and closed a file descriptor while another
	       was blocked waiting. This is less-than-good programming
	       practice, but having the RTS as a result fall over isn't
	       acceptable, so we simply unblock all the waiting threads
	       should we see a bad file descriptor & give the threads
	       a chance to clean up their act. 
	       
	       Note: assume here that threads becoming unblocked
	       will try to read/write the file descriptor before trying
	       to issue a threadWaitRead/threadWaitWrite again (==> an
	       IOError will result for the thread that's got the bad
	       file descriptor.) Hence, there's no danger of a bad
	       file descriptor being repeatedly select()'ed on, so
	       the RTS won't loop.
	    */
	    if ( errno == EBADF ) {
	      unblock_all = rtsTrue;
	      break;
	    } else {
 	      fprintf(stderr,"%d\n", errno);
 	      fflush(stderr);
 	      perror("select");
	      barf("select failed");
	    }
	  }
	  ACQUIRE_LOCK(&sched_mutex);
#ifdef RTS_SUPPORTS_THREADS
          isWorkerBlockedInAwaitEvent = rtsFalse;
#endif

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
	  
#ifdef RTS_SUPPORTS_THREADS
	  /* If another worker thread wants to take over,
	   * return to the scheduler
	   */
	  if (needToYieldToReturningWorker()) {
	      return; /* still hold the lock */
	  }
#endif
	  
#ifdef RTS_SUPPORTS_THREADS
          isWorkerBlockedInAwaitEvent = rtsTrue;
#endif
	  RELEASE_LOCK(&sched_mutex);
      }

      ACQUIRE_LOCK(&sched_mutex);

      /* Step through the waiting queue, unblocking every thread that now has
       * a file descriptor in a ready state.
       */

      prev = NULL;
      if (select_succeeded || unblock_all) {
	  for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
	      next = tso->link;
	      switch (tso->why_blocked) {
	      case BlockedOnRead:
		  ready = unblock_all || FD_ISSET(tso->block_info.fd, &rfd);
		  break;
	      case BlockedOnWrite:
		  ready = unblock_all || FD_ISSET(tso->block_info.fd, &wfd);
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
      
#if defined(RTS_SUPPORTS_THREADS)
      	// if we were woken up by wakeBlockedWorkerThread,
      	// read the dummy byte from the pipe
      if(select_succeeded && FD_ISSET(workerWakeupPipe[0], &rfd)) {
          unsigned char dummy;
          wait = rtsFalse;
          read(workerWakeupPipe[0],&dummy,1);
      }
#endif
    } while (wait && !interrupted && run_queue_hd == END_TSO_QUEUE);
}


#ifdef RTS_SUPPORTS_THREADS
/* wakeBlockedWorkerThread
 *
 * If a worker thread is currently blocked within awaitEvent,
 * wake it.
 * Must be called with sched_mutex held.
 */
void
wakeBlockedWorkerThread()
{
    if(isWorkerBlockedInAwaitEvent && !workerWakeupPending) {
    	unsigned char dummy = 42;	// Any value will do here
    	
			// write something so that select() wakes up
    	write(workerWakeupPipe[1],&dummy,1);
    	workerWakeupPending = rtsTrue;
    }
}
#endif
