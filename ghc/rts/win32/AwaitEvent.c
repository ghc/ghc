/*
 * Wait/check for external events. Periodically, the
 * Scheduler checks for the completion of external operations,
 * like the expiration of timers, completion of I/O requests
 * issued by Haskell threads.
 *
 * If the Scheduler is otherwise out of work, it'll block
 * herein waiting for external events to occur.
 *
 * This file mirrors the select()-based functionality 
 * for POSIX / Unix platforms in rts/Select.c, but for
 * Win32.
 *
 */
#include "Rts.h"
#include "Schedule.h"
#include <windows.h>
#include "win32/AsyncIO.h"
#if defined(THREADED_RTS)
#include "Capability.h"
#endif

// Used to avoid calling abandonRequestWait() if we don't need to.
// Protected by sched_mutex.
static nat workerWaitingForRequests = 0;

void
awaitEvent(rtsBool wait)
{
  int ret;

#ifdef THREADED_RTS
  // Small optimisation: we don't want the waiting thread to wake
  // up straight away just because a previous returning worker has
  // called abandonRequestWait().  If the event is no longer needed,
  // reset it.  We must do this inside the sched_mutex.
  if (!needToYieldToReturningWorker()) {
      resetAbandonRequestWait();
  }
#endif

  do {
    /* Try to de-queue completed IO requests
     */
    workerWaitingForRequests = 1;
    RELEASE_LOCK(&sched_mutex);
    ret = awaitRequests(wait);
    ACQUIRE_LOCK(&sched_mutex);
    workerWaitingForRequests = 0;
    if (!ret) { 
      return; /* still hold the lock */
    }

    // Return to the scheduler if:
    //
    //  - we were interrupted
    //  - new threads have arrived
    //  - another worker wants to take over (THREADED_RTS)

  } while (wait
	   && !interrupted
	   && run_queue_hd == END_TSO_QUEUE
#ifdef THREADED_RTS
	   && !needToYieldToReturningWorker()
#endif
      );
}

#ifdef THREADED_RTS
void
wakeBlockedWorkerThread()
{
    if (workerWaitingForRequests) {
	abandonRequestWait();
    }
}
#endif
