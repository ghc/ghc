/* AsyncIO.c
 *
 * Integrating Win32 asynchronous I/O with the GHC RTS.
 *
 * (c) sof, 2002-2003.
 */
#include "Rts.h"
#include "RtsUtils.h"
#include <windows.h>
#include <stdio.h>
#include "Schedule.h"
#include "RtsFlags.h"
#include "Capability.h"
#include "win32/AsyncIO.h"
#include "win32/IOManager.h"

/*
 * Overview:
 *
 * Haskell code issue asynchronous I/O requests via the 
 * async{Read,Write,DoOp}# primops. These cause addIORequest()
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
static HANDLE           completed_req_event = INVALID_HANDLE_VALUE;
static HANDLE           abandon_req_wait = INVALID_HANDLE_VALUE;
static HANDLE           wait_handles[2];
static CompletedReq     completedTable[MAX_REQUESTS];
static int              completed_hw;
static HANDLE           completed_table_sema;
static int              issued_reqs;

static void
onIOComplete(unsigned int reqID,
	     int   fd STG_UNUSED,
	     int   len,
	     void* buf STG_UNUSED,
	     int   errCode)
{
    DWORD dwRes;
    /* Deposit result of request in queue/table..when there's room. */
    dwRes = WaitForSingleObject(completed_table_sema, INFINITE);
    switch (dwRes) {
    case WAIT_OBJECT_0:
	break;
    default:
	/* Not likely */
	fprintf(stderr, "onIOComplete: failed to grab table semaphore, dropping request 0x%x\n", reqID);
	fflush(stderr);
	return;
    }
    EnterCriticalSection(&queue_lock);
    if (completed_hw == MAX_REQUESTS) {
	/* Shouldn't happen */
	fprintf(stderr, "onIOComplete: ERROR -- Request table overflow (%d); dropping.\n", reqID);
	fflush(stderr);
    } else {
#if 0
	fprintf(stderr, "onCompl: %d %d %d %d %d\n", 
		reqID, len, errCode, issued_reqs, completed_hw); 
	fflush(stderr);
#endif
	completedTable[completed_hw].reqID   = reqID;
	completedTable[completed_hw].len     = len;
	completedTable[completed_hw].errCode = errCode;
	completed_hw++;
	issued_reqs--;
	if (completed_hw == 1) {
	    /* The event is used to wake up the scheduler thread should it
	     * be blocked waiting for requests to complete. The event resets once
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
    return AddIORequest(fd,forWriting,isSock,len,buf,onIOComplete);
}

unsigned int
addDelayRequest(int msecs)
{
    EnterCriticalSection(&queue_lock);
    issued_reqs++;
    LeaveCriticalSection(&queue_lock);
#if 0
    fprintf(stderr, "addDelayReq: %d\n", msecs); fflush(stderr);
#endif
    return AddDelayRequest(msecs,onIOComplete);
}

unsigned int
addDoProcRequest(void* proc, void* param)
{
    EnterCriticalSection(&queue_lock);
    issued_reqs++;
    LeaveCriticalSection(&queue_lock);
#if 0
    fprintf(stderr, "addProcReq: %p %p\n", proc, param); fflush(stderr);
#endif
    return AddProcRequest(proc,param,onIOComplete);
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
    if ( !(completed_table_sema = CreateSemaphore (NULL, MAX_REQUESTS, MAX_REQUESTS, NULL)) ) {
	DWORD rc = GetLastError();
	fprintf(stderr, "startupAsyncIO: CreateSemaphore failed 0x%x\n", (int)rc);
	fflush(stderr);
    }

    return ( completed_req_event  != INVALID_HANDLE_VALUE &&
	     abandon_req_wait     != INVALID_HANDLE_VALUE &&
	     completed_table_sema != NULL );
}

void
shutdownAsyncIO()
{
    if (completed_req_event != INVALID_HANDLE_VALUE) {
        CloseHandle(completed_req_event);
	completed_req_event = INVALID_HANDLE_VALUE;
    }
    if (abandon_req_wait != INVALID_HANDLE_VALUE) {
        CloseHandle(abandon_req_wait);
	abandon_req_wait = INVALID_HANDLE_VALUE;
    }
    if (completed_table_sema != NULL) {
        CloseHandle(completed_table_sema);
	completed_table_sema = NULL;
    }
    ShutdownIOManager();
}

/*
 * Function: awaitRequests(wait)
 *
 * Check for the completion of external IO work requests. Worker
 * threads signal completion of IO requests by depositing them
 * in a table (completedTable). awaitRequests() matches up 
 * requests in that table with threads on the blocked_queue, 
 * making the threads whose IO requests have completed runnable
 * again.
 * 
 * awaitRequests() is called by the scheduler periodically _or_ if
 * it is out of work, and need to wait for the completion of IO
 * requests to make further progress. In the latter scenario, 
 * awaitRequests() will simply block waiting for worker threads 
 * to complete if the 'completedTable' is empty.
 */
int
awaitRequests(rtsBool wait)
{
#ifndef THREADED_RTS
  // none of this is actually used in the threaded RTS

start:
#if 0
    fprintf(stderr, "awaitRequests(): %d %d %d\n", issued_reqs, completed_hw, wait);
    fflush(stderr);
#endif
    EnterCriticalSection(&queue_lock);
    /* Nothing immediately available & we won't wait */
    if ((!wait && completed_hw == 0)
#if 0
	// If we just return when wait==rtsFalse, we'll go into a busy
	// wait loop, so I disabled this condition --SDM 18/12/2003
	(issued_reqs == 0 && completed_hw == 0)
#endif
	) {
	LeaveCriticalSection(&queue_lock);
	return 0;
    }
    if (completed_hw == 0) {
	/* empty table, drop lock and wait */
	LeaveCriticalSection(&queue_lock);
	if ( wait && sched_state == SCHED_RUNNING ) {
	    DWORD dwRes = WaitForMultipleObjects(2, wait_handles, FALSE, INFINITE);
	    switch (dwRes) {
	    case WAIT_OBJECT_0:
		/* a request was completed */
		break;
	    case WAIT_OBJECT_0 + 1:
	    case WAIT_TIMEOUT:
		/* timeout (unlikely) or told to abandon waiting */
		return 0;
	    case WAIT_FAILED: {
		DWORD dw = GetLastError();
		fprintf(stderr, "awaitRequests: wait failed -- error code: %lu\n", dw); fflush(stderr);
		return 0;
	    }
	    default:
		fprintf(stderr, "awaitRequests: unexpected wait return code %lu\n", dwRes); fflush(stderr);
		return 0;
	    }
	} else {
	    return 0;
	}
	goto start;
    } else {
	int i;
	StgTSO *tso, *prev;
	
	for (i=0; i < completed_hw; i++) {
	    /* For each of the completed requests, match up their Ids
	     * with those of the threads on the blocked_queue. If the
	     * thread that made the IO request has been subsequently
	     * killed (and removed from blocked_queue), no match will
	     * be found for that request Id. 
	     *
	     * i.e., killing a Haskell thread doesn't attempt to cancel
	     * the IO request it is blocked on.
	     *
	     */
	    unsigned int rID = completedTable[i].reqID;
	    
	    prev = NULL;
	    for(tso = blocked_queue_hd ; tso != END_TSO_QUEUE; prev = tso, tso = tso->link) {
	
		switch(tso->why_blocked) {
		case BlockedOnRead:
		case BlockedOnWrite:
		case BlockedOnDoProc:
		    if (tso->block_info.async_result->reqID == rID) {
			/* Found the thread blocked waiting on request; stodgily fill 
			 * in its result block. 
			 */
			tso->block_info.async_result->len = completedTable[i].len;
			tso->block_info.async_result->errCode = completedTable[i].errCode;
			
			/* Drop the matched TSO from blocked_queue */
			if (prev) {
			    prev->link = tso->link;
			} else {
			    blocked_queue_hd = tso->link;
			}
			if (blocked_queue_tl == tso) {
			    blocked_queue_tl = prev ? prev : END_TSO_QUEUE;
			}
		    
			/* Terminates the run queue + this inner for-loop. */
			tso->link = END_TSO_QUEUE;
			tso->why_blocked = NotBlocked;
			pushOnRunQueue(&MainCapability, tso);
			break;
		    }
		    break;
		default:
		    if (tso->why_blocked != NotBlocked) {
			barf("awaitRequests: odd thread state");
		    }
		    break;
		}
	    }
	    /* Signal that there's completed table slots available */
	    if ( !ReleaseSemaphore(completed_table_sema, 1, NULL) ) {
		DWORD dw = GetLastError();
		fprintf(stderr, "awaitRequests: failed to signal semaphore (error code=0x%x)\n", (int)dw);
		fflush(stderr);
	    }
	}
	completed_hw = 0;
	ResetEvent(completed_req_event);
	LeaveCriticalSection(&queue_lock);
	return 1;
    }
#endif /* !THREADED_RTS */
}

/*
 * Function: abandonRequestWait()
 *
 * Wake up a thread that's blocked waiting for new IO requests
 * to complete (via awaitRequests().)
 */
void
abandonRequestWait( void )
{
    /* the event is auto-reset, but in case there's no thread
     * already waiting on the event, we want to return it to
     * a non-signalled state.
     *
     * Careful!  There is no synchronisation between
     * abandonRequestWait and awaitRequest, which means that
     * abandonRequestWait might be called just before a thread
     * goes into a wait, and we miss the abandon signal.  So we
     * must SetEvent() here rather than PulseEvent() to ensure
     * that the event isn't lost.  We can re-optimise by resetting
     * the event somewhere safe if we know the event has been
     * properly serviced (see resetAbandon() below).  --SDM 18/12/2003
     */
    SetEvent(abandon_req_wait);
}

void
resetAbandonRequestWait( void )
{
    ResetEvent(abandon_req_wait);
}

