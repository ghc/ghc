/* AsyncIO.c
 *
 * Integrating Win32 asynchronous I/O with the GHC RTS.
 *
 * (c) sof, 2002-2003.
 */
#include "Rts.h"
#include <windows.h>
#include <stdio.h>
#include "Schedule.h"
#include "win32/AsyncIO.h"
#include "win32/IOManager.h"

/*
 * Overview:
 *
 * Haskell code issue asynchronous I/O requests via the 
 * asyncRead# and asyncWrite# primops. These cause addIORequest()
 * to be invoked, which forwards the request to the underlying
 * asynchronous I/O subsystem. Each request is tagged with a unique
 * ID.
 *
 * addIORequest() returns this ID, so that when the blocked CH
 * thread is added onto blocked_queue, its TSO is annotated with
 * it. Upon completion of an I/O request, the async I/O handling
 * code makes a back-call to signal its completion; the local
 * onIOComplete() routine. It adds the IO request ID (along with
 * its result data) to a queue of completed requests before returning. 
 *
 * The queue of completed IO request is read by the thread operating
 * the RTS scheduler. It de-queues the CH threads corresponding
 * to the request IDs, making them runnable again.
 *
 */

typedef struct CompletedReq {
  unsigned int   reqID;
  int            len;
  int            errCode;
} CompletedReq;

#define MAX_REQUESTS 200

static CRITICAL_SECTION queue_lock;
static HANDLE           completed_req_event;
static HANDLE           abandon_req_wait;
static HANDLE           wait_handles[2];
static CompletedReq     completedTable[MAX_REQUESTS];
static int              completed_hw;
static int              issued_reqs;

static void
onIOComplete(unsigned int reqID,
	     void* param STG_UNUSED,
	     int   fd STG_UNUSED,
	     int   len,
	     char* buf STG_UNUSED,
	     int   errCode)
{
  /* Deposit result of request in queue/table */
  EnterCriticalSection(&queue_lock);
  if (completed_hw == MAX_REQUESTS) {
    /* Not likely */
    fprintf(stderr, "Request table overflow (%d); dropping.\n", reqID);
    fflush(stderr);
  } else {
#if 0
    fprintf(stderr, "onCompl: %d %d %d %d %d\n", reqID, len, errCode, issued_reqs, completed_hw); fflush(stderr);
#endif
    completedTable[completed_hw].reqID   = reqID;
    completedTable[completed_hw].len     = len;
    completedTable[completed_hw].errCode = errCode;
    completed_hw++;
    issued_reqs--;
    if (completed_hw == 1) {
      /* The event is used to wake up the scheduler thread should it
       * be blocked waiting for requests to complete. It reset once
       * that thread has cleared out the request queue/table.
       */
      SetEvent(completed_req_event);
    }
  }
  LeaveCriticalSection(&queue_lock);
}

unsigned int
addIORequest(int   fd,
	     int   forWriting,
	     int   isSock,
	     int   len,
	     char* buf)
{
  EnterCriticalSection(&queue_lock);
  issued_reqs++;
  LeaveCriticalSection(&queue_lock);
#if 0
  fprintf(stderr, "addIOReq: %d %d %d\n", fd, forWriting, len); fflush(stderr);
#endif
  return AddIORequest(fd,forWriting,isSock,len,buf,0,onIOComplete);
}

unsigned int
addDelayRequest(int   msecs)
{
  EnterCriticalSection(&queue_lock);
  issued_reqs++;
  LeaveCriticalSection(&queue_lock);
#if 0
  fprintf(stderr, "addDelayReq: %d %d %d\n", msecs); fflush(stderr);
#endif
  return AddDelayRequest(msecs,0,onIOComplete);
}

int
startupAsyncIO()
{
  if (!StartIOManager()) {
    return 0;
  }
  InitializeCriticalSection(&queue_lock);
  /* Create a pair of events:
   *
   *    - completed_req_event  -- signals the deposit of request result; manual reset.
   *    - abandon_req_wait     -- external OS thread tells current RTS/Scheduler
   *                              thread to abandon wait for IO request completion.
   *                              Auto reset.
   */
  completed_req_event = CreateEvent (NULL, TRUE,  FALSE, NULL);
  abandon_req_wait    = CreateEvent (NULL, FALSE, FALSE, NULL);
  wait_handles[0] = completed_req_event;
  wait_handles[1] = abandon_req_wait;
  completed_hw = 0;
  return ( completed_req_event != INVALID_HANDLE_VALUE &&
	   abandon_req_wait    != INVALID_HANDLE_VALUE );
}

void
shutdownAsyncIO()
{
  CloseHandle(completed_req_event);
  ShutdownIOManager();
}

int
awaitRequests(rtsBool wait)
{
start:
#if 0
  fprintf(stderr, "awaitRequests: %d %d %d\n", issued_reqs, completed_hw, wait); fflush(stderr);
#endif
  EnterCriticalSection(&queue_lock);
  /* Nothing immediately available & we won't wait */
  if ((!wait && completed_hw == 0) || 
      (issued_reqs == 0 && completed_hw == 0)) {
    LeaveCriticalSection(&queue_lock);
    return 0;
  }
  if (completed_hw == 0) {
    /* empty table, drop lock and wait */
    LeaveCriticalSection(&queue_lock);
    if (wait) {
      DWORD dwRes = WaitForMultipleObjects(2, wait_handles, FALSE, INFINITE);
      switch (dwRes) {
      case WAIT_OBJECT_0:
	break;
      case WAIT_OBJECT_0 + 1:
      case WAIT_TIMEOUT:
	return 0;
      default:
	fprintf(stderr, "awaitRequests: unexpected wait return code %lu\n", dwRes); fflush(stderr);
	return 0;
      }
    } else {
      return 0; /* cannot happen */
    }
    goto start;
  } else {
    int i;
    StgTSO *tso, *prev;
    
    for (i=0; i < completed_hw; i++) {
      unsigned int rID = completedTable[i].reqID;
      prev = NULL;
      for(tso = blocked_queue_hd ; tso != END_TSO_QUEUE; tso = tso->link) {
	switch(tso->why_blocked) {
	case BlockedOnDelay:
	case BlockedOnRead:
	case BlockedOnWrite:
	  if (tso->block_info.async_result->reqID == rID) {
	    /* Found the thread blocked waiting on request; stodgily fill 
	     * in its result block. 
	     */
	    if (tso->why_blocked != BlockedOnDelay) {
	      tso->block_info.async_result->len = completedTable[i].len;
	      tso->block_info.async_result->errCode = completedTable[i].errCode;
	    }

	    /* Drop the matched TSO from blocked_queue */
	    if (prev) {
	      prev->link = tso->link;
	    } else {
	      blocked_queue_hd = tso->link;
	    }
	    if (blocked_queue_tl == tso) {
	      blocked_queue_tl = prev;
	    }
	    /* Terminates the run queue + this inner for-loop. */
	    tso->link = END_TSO_QUEUE;
	    tso->why_blocked = NotBlocked;
	    PUSH_ON_RUN_QUEUE(tso);
	    break;
	  }
	  break;
	default:
	  break;
	}
	prev = tso;
      }
    }
    completed_hw = 0;
    ResetEvent(completed_req_event);
    LeaveCriticalSection(&queue_lock);
    return 1;
  }
}

void
abandonRequestWait()
{
  /* the event is auto-reset, but in case there's no thread
   * already waiting on the event, we want to return it to
   * a non-signalled state.
   */
  PulseEvent(abandon_req_wait);
}
