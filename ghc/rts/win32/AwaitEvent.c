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

void
awaitEvent(rtsBool wait)
{
  RELEASE_LOCK(&sched_mutex);
  do {
    /* Try to de-queue completed IO requests */
    if (!awaitRequests(wait)) {
      return;
    }
    ACQUIRE_LOCK(&sched_mutex);
    /* we were interrupted, return to the scheduler immediately.
     */
    if (interrupted) {
      return; /* still hold the lock */
    }

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
    RELEASE_LOCK(&sched_mutex);
  } while (wait && !interrupted && run_queue_hd == END_TSO_QUEUE);
}

#ifdef RTS_SUPPORTS_THREADS
void
wakeBlockedWorkerThread()
{
  abandonRequestWait();
}
#endif

