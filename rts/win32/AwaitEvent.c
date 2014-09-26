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
#include "win32/ConsoleHandler.h"

// Used to avoid calling abandonRequestWait() if we don't need to.
// Protected by sched_mutex.
static nat workerWaitingForRequests = 0;

void
awaitEvent(rtsBool wait)
{
  do {
    /* Try to de-queue completed IO requests
     */
    workerWaitingForRequests = 1;
    awaitRequests(wait);
    workerWaitingForRequests = 0;

    // If a signal was raised, we need to service it
    // XXX the scheduler loop really should be calling
    // startSignalHandlers(), but this is the way that posix/Select.c
    // does it and I'm feeling too paranoid to refactor it today --SDM
    if (stg_pending_events != 0) {
        startSignalHandlers(&MainCapability);
        return;
    }

    // The return value from awaitRequests() is a red herring: ignore
    // it.  Return to the scheduler if !wait, or
    //
    //  - we were interrupted
    //  - the run-queue is now non- empty

  } while (wait
           && sched_state == SCHED_RUNNING
           && emptyRunQueue(&MainCapability)
      );
}
#endif
