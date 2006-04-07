#if !defined(THREADED_RTS) /* to the end */
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
#include "AwaitEvent.h"
#include <windows.h>
#include "win32/AsyncIO.h"

// Used to avoid calling abandonRequestWait() if we don't need to.
// Protected by sched_mutex.
static nat workerWaitingForRequests = 0;

void
awaitEvent(rtsBool wait)
{
  int ret;

  do {
    /* Try to de-queue completed IO requests
     */
    workerWaitingForRequests = 1;
    ret = awaitRequests(wait);
    workerWaitingForRequests = 0;
    if (!ret) { 
      return; /* still hold the lock */
    }

    // Return to the scheduler if:
    //
    //  - we were interrupted
    //  - new threads have arrived

  } while (wait
	   && sched_state == SCHED_RUNNING
	   && emptyRunQueue(&MainCapability)
      );
}
#endif
