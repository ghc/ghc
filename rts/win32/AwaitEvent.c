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
#include "RtsFlags.h"
#include "Schedule.h"
#include "IOManager.h"
#include <windows.h>
#include "win32/AwaitEvent.h"
#include "win32/AsyncMIO.h"
#include "win32/AsyncWinIO.h"
#include "win32/ConsoleHandler.h"
#include <stdbool.h>

// Used to avoid calling abandonRequestWait() if we don't need to.
// Protected by sched_mutex.
static bool workerWaitingForRequests = false;

bool
awaitCompletedTimeoutsOrIOWin32(Capability *cap, bool wait)
{
  bool interrupt = false;
  do {
    /* Try to de-queue completed IO requests
     */
    workerWaitingForRequests = true;
    if (is_io_mng_native_p())
      awaitAsyncRequests(wait);
      /* FIXME: no support yet for interrupting in WinIO I/O manager
       * See issue #27403
       */
    else
      interrupt = !awaitRequests(wait);
    workerWaitingForRequests = false;

    // If a signal was raised, we need to service it
    // XXX the scheduler loop really should be calling
    // startSignalHandlers(), but this is the way that posix/Select.c
    // does it and I'm feeling too paranoid to refactor it today --SDM
    if (stg_pending_events != 0) {
        startSignalHandlers(cap);
        // This will normally cause emptyRunQueue to become false and
        // thus we will drop out of the loop.
    }

    // The return value from awaitRequests() reports if it was interrupted by
    // abandonRequestWait(). Return to the scheduler if !wait, or
    //
    //  - we were interrupted
    //  - the run-queue is now non- empty

  } while (wait
           && getSchedState() == SCHED_RUNNING
           && emptyRunQueue(cap)
           && !interrupt
      );
  return !interrupt;
}
#endif
