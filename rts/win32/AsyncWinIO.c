/* AsyncIO.h
 *
 * Integrating Win32 asynchronous IOCP with the GHC RTS.
 *
 * (c) Tamar Christina, 2018 - 2019
 *
 * NOTE: This is the WinIO manager, only used for --io-manager=native.
 *       For the MIO manager see AsyncIO.h.
 */

#include "Rts.h"
#include <rts/IOManager.h>
#include "AsyncWinIO.h"
#include "Prelude.h"
#include "Capability.h"
#include "Schedule.h"

#include <stdbool.h>
#include <windows.h>
#include <stdint.h>
#include <stdio.h>

/* Note [Non-Threaded WINIO design]
   Compared to Async MIO, Async WINIO does all of the heavy processing at the
   Haskell side of things.  The same code as the threaded WINIO is re-used for
   the Non-threaded version.  Of course since we are in a non-threaded rts we
   can't block on foreign calls without hanging the application.

   This file thus serves as a back-end service that continuously reads pending
   events from the given I/O completion port and notifies the Haskell I/O manager
   of work that has been completed.  This does incur a slight cost in that the
   rts has to actually schedule the Haskell thread to do the work, however this
   shouldn't be a problem for performance.

   It is however a problem for the workload buffer we use as we are not allowed
   to service new requests until the old ones have actually been read and
   processes by the Haskell I/O side.

   To account for this the I/O manager works in two stages.

   1) Like the threaded version, any long wait we do, we prefer to do it in an
   alterable state so that we can respond immediately to new requests.  Note
   that once we know which completion port handle we are bound to we no longer
   need the Haskell side to tell us of new work.  We can simply handle any new
   work pre-emptively.

   2) We block in a non-alertable state whenever
     a) The Completion port handle is yet unknown.
     b) The RTS requested the I/O manager be shutdown via an event --TODO: Remove?
     c) We are waiting on the Haskell I/O manager to service a previous
     request as to allow us to re-use the buffer.

   We would ideally like to spend as little time as possible in 2).

   The workflow for this I/O manager is as follows:

                          +------------------------+
                          | Worker thread creation |
                          +-----------+------------+
                                      |
                                      |
                        +-------------v---------------+
                 +------>  Block in unalertable wait  +-----+
                 |      +-------------+---------------+     |
                 |                    |                     |
                 |                    |                     |
                 |        +-----------v------------+        |
                 |        |Init by Haskell I/O call|        | If init already
   wait for I/O  |        +-----------+------------+        |
   processing in |                    |                     |
   Haskell side  |                    |                     |
                 |           +--------v---------+           |
   Also process  |           |  alertable wait  <-----------+
   events like   |           +--------+---------+
   shutdown      |                    |
                 |                    |
                 |            +-------v--------+
                 +------------+process response|
                              +----------------+

   The non-alertable wait itself is split into two phases during regular
   execution:
    1.) canQueueIOThread == true
    2.) canQueueIOThread == false, outstanding_service_requests == true

   `notifyScheduler` puts us into the first phase. During which we wait
   for the scheduler to call `queueIOThread`.
   During the second phase we wait for the queued haskell thread to run.

   The alertable wait is done by calling into GetQueuedCompletionStatusEx.
   After we return from the call we notify the haskell side of new events
   via `notifyScheduler`.

   notifyScheduler set's flags to indicate to the scheduler that new IO work
   needs to be processed. At this point the next call to `schedule` will
   check the flag and schedule execution of a haskell thread executing
   processRemoteCompletion.

    `processRemoteCompletion` will process IO results invoking call backs and
   processing timer events. Once done it resets `outstanding_service_requests`
   and wakes up the IOManager thread. Which at this point becomes unblocked
   and reenters the altertable wait state.

   As a design decision to keep this side as light as possible no bookkeeping
   is done here to track requests.  That is, this file has no way of knowing
   of the remaining outstanding I/O requests, how many it actually completed
   in the last call as that list may contain spurious events.

   It works around this by having the Haskell side tell it how much work it
   still has left to do.

   Unlike the Threaded version we use a single worker thread to handle
   completions and so it won't scale as well.  But if high scalability is needed
   then use the threaded runtime.  This would have to become threadsafe
   in order to use multiple threads, but this is non-trivial as the non-threaded
   rts has no locks around any of the key parts.

   See also Note [WINIO Manager design].


  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Note [Notifying the RTS/Haskell of completed events]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  notifyRtsOfFinishedCall can't directly create a haskell thread.
  With the current API of the haskell runtime this would be terrible
  unsound. In particular the GC assumes no heap objects are generated,
  and no heap memory is requested while it is running.

  To work around this the scheduler invokes <insertName> which checks
  if a new thread should be created. Since we only use this code path
  in the non-threaded runtime this is safe. The scheduler is never
  running concurrently with the GC or Mutator.

   */

/* The IOCP Handle all I/O requests are associated with for this RTS.  */
static HANDLE completionPortHandle = INVALID_HANDLE_VALUE;
/* Number of currently still outstanding I/O requests.  */
uint64_t outstanding_requests = 0;
/* Boolean controlling if the I/O manager is/should still be running.  */
bool running = false;
/* Boolean to indicate whether we have outstanding I/O requests that still need
   to be processed by the I/O manager on the Haskell side.
    Set by:
      notifyRtsOfFinishedCall (true)
      servicedIOEntries (false)
    Read by:
      runner
   */
volatile bool outstanding_service_requests = false;
/* Indicates wether we have hit one case where we serviced as much requests as
   we could because the buffer got full.  In such cases for the next requests
   we expand the buffers so we have room to process requests in bigger
   batches.
    Set by:
      runner
    Read by:
      servicedIOEntries
*/
bool queue_full = false;

/* Timeout to use for the next alertable wait.  INFINITE means never timeout.
   Also see note [WINIO Timer management].  */
DWORD timeout = INFINITE;
DWORD WINAPI runner (LPVOID lpParam);
HANDLE workerThread = NULL;
DWORD workerThreadId = 0;

/* Synchronization mutex for modifying the above state variables in a thread
   safe way.  */
SRWLOCK lock;
/* Conditional variable to wake the I/O manager up from a non-alertable waiting
   state.  */
CONDITION_VARIABLE wakeEvent;
/* Conditional variable to force the system thread to wait for a request to
   complete.  */
CONDITION_VARIABLE threadIOWait;

/* Number of callbacks to reserve slots for in ENTRIES.  This is also the
   total number of concurrent I/O requests we can handle in one go.  */
uint32_t num_callbacks = 32;
/* Buffer for I/O request information.  */
OVERLAPPED_ENTRY *entries;
/* Number of I/O calls verified to have completed in the last round by the
   Haskell I/O Manager.  */
uint32_t num_last_completed;

/* Notify the Haskell side of this many new finished requests */
uint32_t num_notify;

/* Indicates to the scheduler that new work is available for processing.
    Set by:
      runner
      queueIOThread
    Read by
      queueIOThread
*/
static volatile bool canQueueIOThread;

static void notifyScheduler(uint32_t num);

// static void notifyRtsOfFinishedCall (uint32_t num);

/* Create and initialize the non-threaded I/O manager.  */
bool startupAsyncWinIO(void)
{
  running = true;
  outstanding_service_requests = false;
  completionPortHandle = INVALID_HANDLE_VALUE;
  outstanding_requests = 0;

  InitializeSRWLock (&lock);
  InitializeConditionVariable (&wakeEvent);
  InitializeConditionVariable (&threadIOWait);

  entries = calloc (sizeof (OVERLAPPED_ENTRY), num_callbacks);

  /* Start the I/O manager before creating the worker thread to prevent a busy
     wait or spin-lock, this will call registerNewIOCPHandle allowing us to
     skip the initial un-alertable wait.  */
  ioManagerStart ();

  workerThread = CreateThread (NULL, 0, runner, NULL, 0, &workerThreadId);
  if (!workerThread)
    {
      barf ("could not create I/O manager thread.");
      return false;
    }

  return true;
}

/* Terminate the I/O manager, if WAIT_THREADS then the call will block until
   all helper threads are finished.  */
void shutdownAsyncWinIO(bool wait_threads)
{
  if (workerThread != NULL)
    {
      if (wait_threads)
        {
          AcquireSRWLockExclusive (&lock);

          running = false;
          ioManagerWakeup ();
          PostQueuedCompletionStatus (completionPortHandle, 0, 0, NULL);
          WakeConditionVariable (&wakeEvent);
          WakeConditionVariable (&threadIOWait);

          ReleaseSRWLockExclusive (&lock);

          /* Now wait for the thread to actually finish.  */
          WaitForSingleObject (workerThread, INFINITE);
        }
      completionPortHandle = INVALID_HANDLE_VALUE;
      workerThread = NULL;
      workerThreadId = 0;
      free (entries);
      entries = NULL;
    }

  /* Call back into the Haskell side to terminate things there too.  */
  ioManagerDie ();
}

/* Register the I/O completetion port handle PORT that the I/O manager will be
   monitoring.  All handles are expected to be associated with this handle.  */
void registerNewIOCPHandle (HANDLE port)
{
  AcquireSRWLockExclusive (&lock);

  completionPortHandle = port;

  ReleaseSRWLockExclusive (&lock);
}

/* Callback hook so the Haskell part of the I/O manager can notify this manager
   that a request someone is waiting on was completed synchronously.  This means
   we need to wake up the scheduler as there is work to be done.   */

void completeSynchronousRequest (void)
{
  AcquireSRWLockExclusive (&lock);

  WakeConditionVariable (&threadIOWait);

  ReleaseSRWLockExclusive (&lock);
}


/* Register a new I/O request that the I/O manager should handle. PORT is the
   completion port handle that the request is associated with, MSSEC is the
   maximum amount of time in milliseconds that an alertable wait should be done
   for before the RTS requested to be notified of progress and NUM_REQ is the
   total overall number of outstanding I/O requests.  */

void registerAlertableWait (HANDLE port, DWORD mssec, uint64_t num_req)
{
  bool interrupt = false;
  bool wakeup = false;
  AcquireSRWLockExclusive (&lock);

  /* Decide if we may have to wake up the I/O manager.  */
  wakeup = outstanding_requests == 0 && num_req > 0;

  outstanding_requests = num_req;
  /* If the new timeout is earlier than the old one we have to reschedule the
     wait.  Do this by interrupting the current operation and setting the new
     timeout, since it must be the shortest one in the queue.  */
  if (timeout > mssec)
    {
      timeout = mssec;
      interrupt = true;
    }

  ReleaseSRWLockExclusive (&lock);

  if (wakeup)
    WakeConditionVariable (&wakeEvent);
  else if (interrupt)
    PostQueuedCompletionStatus (port, 0, 0, NULL);
}

/* Exported callback function that will be called by the RTS to collect the
   finished overlapped entried belonging to the completed I/O requests.  The
   number of read entries will be returned in NUM.

   NOTE: This function isn't thread safe, but is intended to be called only
         when requested to by the I/O manager via notifyRtsOfFinishedCall.  In
         that context it is thread safe as we're guaranteeing that the I/O
         manager is blocked waiting for the read to happen followed by a
         servicedIOEntries call.   */
OVERLAPPED_ENTRY* getOverlappedEntries (uint32_t *num)
{
  *num = num_last_completed;
  return entries;
}

/* Called by the scheduler when we have ran out of work to do and we have at
   least one thread blocked on an I/O Port.  When WAIT then if this function
   returns you will have at least one action to service, though this may be a
   wake-up action.  */

void awaitAsyncRequests (bool wait)
{
  if(queueIOThread()) {
    return;
  }
  AcquireSRWLockExclusive (&lock);
  /* We don't deal with spurious requests here, that's left up to AwaitEvent.c
     because in principle we need to check if the capability work queue is now
     not empty but we can't do that here.  Also these locks don't guarantee
     fairness, as such a request may have completed without us seeing a
     timeslice in between.  */
  if (wait && outstanding_service_requests)
    SleepConditionVariableSRW (&threadIOWait, &lock, INFINITE, 0);

  ReleaseSRWLockExclusive (&lock);
}


/* Exported function that will be called by the RTS in order to indicate that
   the RTS has successfully finished servicing I/O request returned with
   getOverlappedEntries.  Some of the overlapped requests may not have been
   an I/O request so the RTS also returns the amount of REMAINING overlapped
   entried it still expects to be processed.  */

void servicedIOEntries (uint64_t remaining)
{
  AcquireSRWLockExclusive (&lock);

  outstanding_requests = remaining;
  if (outstanding_requests <= 0)
    timeout = INFINITE;
  outstanding_service_requests = false;

  if (queue_full)
  {
    num_callbacks *= 2;
    OVERLAPPED_ENTRY *new
      = realloc (entries,
                  sizeof (OVERLAPPED_ENTRY) * num_callbacks);
    if (new)
      entries = new;
  }

  ReleaseSRWLockExclusive (&lock);

  WakeConditionVariable (&wakeEvent);
}

/* Sets `canQueueIOThread` to indicate to the scheduler that it should
   queue a new haskell thread to process IO events. */
static void notifyScheduler(uint32_t num) {
  AcquireSRWLockExclusive (&lock);
  ASSERT(!canQueueIOThread);
  canQueueIOThread = true;
  num_notify = num;
  WakeConditionVariable(&threadIOWait);
  ReleaseSRWLockExclusive (&lock);
}

/* Queues a new haskell thread to process IO events
   if there is work to do.
   Return true if work was queued.

   Precond:
    Not already waiting on service requests.
   Postcond:
    outstanding_service_requests = true
    processRemoteCompletion queued.
    IOThread blocked until processRemoteCompletion has run.
    */
bool queueIOThread()
{
  bool result = false;
#if !defined(THREADED_RTS)
  AcquireSRWLockExclusive (&lock);
  if(canQueueIOThread)
  {
      ASSERT(!outstanding_service_requests);
      num_last_completed = num_notify;
      outstanding_service_requests = true;
      canQueueIOThread = false;
      Capability *cap = &MainCapability;
      StgTSO * tso = createStrictIOThread (cap, RtsFlags.GcFlags.initialStkSize,
                                          processRemoteCompletion_closure);
      ASSERT(tso);
      scheduleThreadNow (cap, tso);
      result = true;
  }
  ReleaseSRWLockExclusive (&lock);
#endif
  return result;
}

/* Main thread runner for the non-threaded I/O Manager.  */

DWORD WINAPI runner (LPVOID lpParam STG_UNUSED)
{
  /* The last event that was sent to the I/O manager.  */
  HsWord32 lastEvent = 0;
  while (running)
    {
      AcquireSRWLockExclusive (&lock);

      lastEvent = readIOManagerEvent ();
      /* Non-alertable wait.  While here we can't server any I/O requests so we
         would ideally like to spent as little time here as possible.  As such
         there are only 3 reasons to enter this state:

         1) I/O manager hasn't been fully initialized yet.
         2) I/O manager was told to shutdown, instead of doing that we just
            block indefinitely so we don't have to recreate the thread to start
            back up.
         3) We are waiting for the RTS to service the last round of requests.  */
      while (completionPortHandle == INVALID_HANDLE_VALUE
             || lastEvent == IO_MANAGER_DIE
             || outstanding_service_requests
             || canQueueIOThread)
        {
          SleepConditionVariableSRW (&wakeEvent, &lock, INFINITE, 0);
          HsWord32 nextEvent = readIOManagerEvent ();
          lastEvent = nextEvent ? nextEvent : lastEvent;
        }

      ReleaseSRWLockExclusive (&lock);

      ULONG num_removed = -1;
      ZeroMemory (entries, sizeof (entries[0]) * num_callbacks);
      if (GetQueuedCompletionStatusEx (completionPortHandle, entries,
                                       num_callbacks, &num_removed, timeout,
                                       false))
        {
          if (outstanding_requests == 0)
            num_removed = 0;

          if (num_removed > 0)
            {
              queue_full = num_removed == num_callbacks;
              notifyScheduler (num_removed);
            }
        }
      else if (WAIT_TIMEOUT == GetLastError ())
        {
          num_removed = 0;
          notifyScheduler (num_removed);
        }

      AcquireSRWLockExclusive (&lock);

      if (!running)
        ExitThread (0);

      ReleaseSRWLockExclusive (&lock);
    }
    return 0;
}
