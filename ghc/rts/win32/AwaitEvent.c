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
#if defined(RTS_SUPPORTS_THREADS)
#include "Capability.h"
#endif

void
awaitEvent(rtsBool wait)
{
  int ret;

  do {
    /* Try to de-queue completed IO requests
     */
    RELEASE_LOCK(&sched_mutex);
    ret = awaitRequests(wait);
    ACQUIRE_LOCK(&sched_mutex);
    if (!ret) { 
      return; /* still hold the lock */
    }

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
  } while (wait && !interrupted && run_queue_hd == END_TSO_QUEUE);
}

#ifdef RTS_SUPPORTS_THREADS
void
wakeBlockedWorkerThread()
{
  abandonRequestWait();
}
#endif

