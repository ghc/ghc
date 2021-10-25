/*
 * A fixed-size queue; MT-friendly.
 *
 * (c) sof, 2002-2003.
 */
#include "Rts.h"
#include "RtsUtils.h"
#include "WorkQueue.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

static void queue_error_rc( char* loc, DWORD err);
static void queue_error( char* loc, char* reason);


/* Wrapper around OS call to create semaphore */
static Semaphore
newSemaphore(int initCount, int max)
{
  Semaphore s;
  s = CreateSemaphore ( NULL,       /* LPSECURITY_ATTRIBUTES (default) */
                        initCount,  /* LONG lInitialCount */
                        max,        /* LONG lMaxCount */
                        NULL);      /* LPCTSTR (anonymous / no object name) */
  if ( NULL == s) {
    queue_error_rc("newSemaphore", GetLastError());
    return NULL;
  }
  return s;
}

/*
 * Function: NewWorkQueue
 *
 * The queue constructor - semaphores are initialised to match
 * max number of queue entries.
 *
 */
WorkQueue*
NewWorkQueue()
{
  WorkQueue* wq = (WorkQueue*)stgMallocBytes(sizeof(WorkQueue), "NewWorkQueue");

  memset(wq, 0, sizeof *wq);

  OS_INIT_LOCK(&wq->queueLock);
  wq->workAvailable = newSemaphore(0, WORKQUEUE_SIZE);
  wq->roomAvailable = newSemaphore(WORKQUEUE_SIZE, WORKQUEUE_SIZE);

  /* Fail if we were unable to create any of the sync objects. */
  if ( NULL == wq->workAvailable ||
       NULL == wq->roomAvailable ) {
    FreeWorkQueue(wq);
    return NULL;
  }

  return wq;
}

void
FreeWorkQueue ( WorkQueue* pq )
{
  int i;

  /* Free any remaining work items. */
  for (i = 0; i < WORKQUEUE_SIZE; i++) {
    if (pq->items[i] != NULL) {
      stgFree(pq->items[i]);
    }
  }

  /* Close the semaphores; any threads blocked waiting
   * on either will as a result be woken up.
   */
  if ( pq->workAvailable ) {
    CloseHandle(pq->workAvailable);
  }
  if ( pq->roomAvailable ) {
    CloseHandle(pq->roomAvailable);
  }
  OS_CLOSE_LOCK(&pq->queueLock);
  stgFree(pq);
  return;
}

HANDLE
GetWorkQueueHandle ( WorkQueue* pq )
{
  if (!pq) return NULL;

  return pq->workAvailable;
}

/*
 * Function: GetWork
 *
 * Fetch a work item from the queue, blocking if none available.
 * Return value indicates of false indicates error/fatal condition.
 */
BOOL
GetWork ( WorkQueue* pq, void** ppw )
{
  DWORD rc;

  if (!pq) {
    queue_error("GetWork", "NULL WorkQueue object");
    return false;
  }
  if (!ppw) {
    queue_error("GetWork", "NULL WorkItem object");
    return false;
  }

  /* Block waiting for work item to become available */
  if ( (rc = WaitForSingleObject( pq->workAvailable, INFINITE))
         != WAIT_OBJECT_0 ) {
    queue_error_rc("GetWork.WaitForSingleObject(workAvailable)",
                   ( (WAIT_FAILED == rc) ? GetLastError() : rc));
    return false;
  }

  return FetchWork(pq,ppw);
}

/*
 * Function: FetchWork
 *
 * Fetch a work item from the queue, blocking if none available.
 * Return value indicates of false indicates error/fatal condition.
 */
BOOL
FetchWork ( WorkQueue* pq, void** ppw )
{
  DWORD rc;

  if (!pq) {
    queue_error("FetchWork", "NULL WorkQueue object");
    return false;
  }
  if (!ppw) {
    queue_error("FetchWork", "NULL WorkItem object");
    return false;
  }

  OS_ACQUIRE_LOCK(&pq->queueLock);
  *ppw = pq->items[pq->head];
  /* For sanity's sake, zero out the pointer. */
  pq->items[pq->head] = NULL;
  pq->head = (pq->head + 1) % WORKQUEUE_SIZE;
  rc = ReleaseSemaphore(pq->roomAvailable,1, NULL);
  OS_RELEASE_LOCK(&pq->queueLock);
  if ( 0 == rc ) {
    queue_error_rc("FetchWork.ReleaseSemaphore()", GetLastError());
    return false;
  }

  return true;
}

/*
 * Function: SubmitWork
 *
 * Add work item to the queue, blocking if no room available.
 * Return value indicates of false indicates error/fatal condition.
 */
BOOL
SubmitWork ( WorkQueue* pq, void* pw )
{
  DWORD rc;

  if (!pq) {
    queue_error("SubmitWork", "NULL WorkQueue object");
    return false;
  }
  if (!pw) {
    queue_error("SubmitWork", "NULL WorkItem object");
    return false;
  }

  /* Block waiting for work item to become available */
  if ( (rc = WaitForSingleObject( pq->roomAvailable, INFINITE))
         != WAIT_OBJECT_0 ) {
    queue_error_rc("SubmitWork.WaitForSingleObject(workAvailable)",
                   ( (WAIT_FAILED == rc) ? GetLastError() : rc));

    return false;
  }

  OS_ACQUIRE_LOCK(&pq->queueLock);
  pq->items[pq->tail] = pw;
  pq->tail = (pq->tail + 1) % WORKQUEUE_SIZE;
  rc = ReleaseSemaphore(pq->workAvailable,1, NULL);
  OS_RELEASE_LOCK(&pq->queueLock);
  if ( 0 == rc ) {
    queue_error_rc("SubmitWork.ReleaseSemaphore()", GetLastError());
    return false;
  }

  return true;
}

/* Error handling */

static void
queue_error_rc( char* loc,
                DWORD err)
{
  fprintf(stderr, "%s failed: return code = 0x%lx\n", loc, err);
  fflush(stderr);
  return;
}


static void
queue_error( char* loc,
             char* reason)
{
  fprintf(stderr, "%s failed: %s\n", loc, reason);
  fflush(stderr);
  return;
}
