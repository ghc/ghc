/* IOManager.c
 *
 * Non-blocking / asynchronous I/O for Win32.
 *
 * (c) sof, 2002-2003.
 */
#include "IOManager.h"
#include "WorkQueue.h"
#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <winsock.h>
#include <process.h>

/*
 * Internal state maintained by the IO manager.
 */
typedef struct IOManagerState {
    CritSection      manLock;
    WorkQueue*       workQueue;
    int              queueSize;
    int              numWorkers;
    int              workersIdle;
    HANDLE           hExitEvent;
    unsigned int     requestID;
} IOManagerState;

/* ToDo: wrap up this state via a IOManager handle instead? */
static IOManagerState* ioMan;

/*
 * The routine executed by each worker thread.
 */
static
unsigned
WINAPI
IOWorkerProc(PVOID param)
{
    HANDLE  hWaits[2];
    DWORD   rc;
    IOManagerState* iom = (IOManagerState*)param;
    WorkQueue* pq = iom->workQueue;
    WorkItem*  work;
    int        len = 0, fd = 0;
    DWORD      errCode;
    void*      complData;

    hWaits[0] = (HANDLE)iom->hExitEvent;
    hWaits[1] = GetWorkQueueHandle(pq);
  
    while (1) {
	/* The error code is communicated back on completion of request; reset. */
	errCode = 0;
	
	EnterCriticalSection(&iom->manLock);
	/* Signal that the worker is idle.
	 *
	 * 'workersIdle' is used when determining whether or not to
	 * increase the worker thread pool when adding a new request.
	 * (see addIORequest().)
	 */
	iom->workersIdle++;
	LeaveCriticalSection(&iom->manLock);
	
	/*
	 * A possible future refinement is to make long-term idle threads
	 * wake up and decide to shut down should the number of idle threads
	 * be above some threshold.
	 *
	 */
	rc = WaitForMultipleObjects( 2, hWaits, FALSE, INFINITE );

	EnterCriticalSection(&iom->manLock);
	/* Signal that the thread is 'non-idle' and about to consume 
	 * a work item.
	 */
	iom->workersIdle--;
	iom->queueSize--;
	LeaveCriticalSection(&iom->manLock);
    
	if ( WAIT_OBJECT_0 == rc ) {
	    /* shutdown */
	    return 0;
	} else if ( (WAIT_OBJECT_0 + 1) == rc ) {
	    /* work item available, fetch it. */
	    if (FetchWork(pq,(void**)&work)) {
		if ( work->workKind & WORKER_READ ) {
		    if ( work->workKind & WORKER_FOR_SOCKET ) {
			len = recv(work->workData.ioData.fd, 
				   work->workData.ioData.buf,
				   work->workData.ioData.len,
				   0);
			if (len == SOCKET_ERROR) {
			    errCode = WSAGetLastError();
			}
		    } else {
			len = read(work->workData.ioData.fd,
				   work->workData.ioData.buf,
				   work->workData.ioData.len);
			if (len == -1) { errCode = errno; }
		    }
		    complData = work->workData.ioData.buf;
		    fd = work->workData.ioData.fd;
		} else if ( work->workKind & WORKER_WRITE ) {
		    if ( work->workKind & WORKER_FOR_SOCKET ) {
			len = send(work->workData.ioData.fd,
				   work->workData.ioData.buf,
				   work->workData.ioData.len,
				   0);
			if (len == SOCKET_ERROR) {
			    errCode = WSAGetLastError();
			}
		    } else {
			len = write(work->workData.ioData.fd,
				    work->workData.ioData.buf,
				    work->workData.ioData.len);
			if (len == -1) { errCode = errno; }
		    }
		    complData = work->workData.ioData.buf;
		    fd = work->workData.ioData.fd;
		} else if ( work->workKind & WORKER_DELAY ) {
		    /* Approximate implementation of threadDelay;
		     * 
		     * Note: Sleep() is in milliseconds, not micros.
		     */
		    Sleep(work->workData.delayData.msecs / 1000);
		    len = work->workData.delayData.msecs;
		    complData = NULL;
		    fd = 0;
		    errCode = 0;
		} else if ( work->workKind & WORKER_DO_PROC ) {
		    /* perform operation/proc on behalf of Haskell thread. */
		    if (work->workData.procData.proc) {
			/* The procedure is assumed to encode result + success/failure
			 * via its param.
			 */
			errCode=work->workData.procData.proc(work->workData.procData.param);
		    } else {
			errCode=1;
		    }
		    complData = work->workData.procData.param;
		} else {
		    fprintf(stderr, "unknown work request type (%d) , ignoring.\n", work->workKind);
		    fflush(stderr);
		    continue;
		}
		work->onCompletion(work->requestID,
				   fd,
				   len,
				   complData,
				   errCode);
		/* Free the WorkItem */
		free(work);
	    } else {
		fprintf(stderr, "unable to fetch work; fatal.\n"); fflush(stderr);
		return 1;
	    }
	} else {
	    fprintf(stderr, "waiting failed; fatal.\n"); fflush(stderr);
	    return 1;
	}
    }
    return 0;
}

static 
BOOL
NewIOWorkerThread(IOManagerState* iom)
{
    unsigned threadId;
    return ( 0 != _beginthreadex(NULL,
				 0,
				 IOWorkerProc,
				 (LPVOID)iom,
				 0,
				 &threadId) );
}

BOOL
StartIOManager(void)
{
    HANDLE hExit;
    WorkQueue* wq;

    wq = NewWorkQueue();
    if ( !wq ) return FALSE;  
  
    ioMan = (IOManagerState*)malloc(sizeof(IOManagerState));
  
    if (!ioMan) {
	FreeWorkQueue(wq);
	return FALSE;
    }

    /* A manual-reset event */
    hExit = CreateEvent ( NULL, TRUE, FALSE, NULL );
    if ( !hExit ) {
	FreeWorkQueue(wq);
	free(ioMan);
	return FALSE;
    }
  
    ioMan->hExitEvent = hExit;
    InitializeCriticalSection(&ioMan->manLock);
    ioMan->workQueue   = wq;
    ioMan->numWorkers  = 0;
    ioMan->workersIdle = 0;
    ioMan->queueSize   = 0;
    ioMan->requestID   = 1;
 
    return TRUE;
}

/*
 * Function: depositWorkItem()
 *
 * Local function which deposits a WorkItem onto a work queue,
 * deciding in the process whether or not the thread pool needs
 * to be augmented with another thread to handle the new request.
 *
 */
static
int
depositWorkItem( unsigned int reqID,
		 WorkItem* wItem )
{
    EnterCriticalSection(&ioMan->manLock);

#if 0
    fprintf(stderr, "depositWorkItem: %d/%d\n", ioMan->workersIdle, ioMan->numWorkers); 
    fflush(stderr);
#endif
    /* A new worker thread is created when there are fewer idle threads
     * than non-consumed queue requests. This ensures that requests will
     * be dealt with in a timely manner.
     *
     * [Long explanation of why the previous thread pool policy lead to 
     * trouble]
     *
     * Previously, the thread pool was augmented iff no idle worker threads
     * were available. That strategy runs the risk of repeatedly adding to
     * the request queue without expanding the thread pool to handle this
     * sudden spike in queued requests. 
     * [How? Assume workersIdle is 1, and addIORequest() is called. No new 
     * thread is created and the request is simply queued. If addIORequest()
     * is called again _before the OS schedules a worker thread to pull the
     * request off the queue_, workersIdle is still 1 and another request is 
     * simply added to the queue. Once the worker thread is run, only one
     * request is de-queued, leaving the 2nd request in the queue]
     * 
     * Assuming none of the queued requests take an inordinate amount of to 
     * complete, the request queue would eventually be drained. But if that's 
     * not the case, the later requests will end up languishing in the queue 
     * indefinitely. The non-timely handling of requests may cause CH applications
     * to misbehave / hang; bad.
     *
     */
    ioMan->queueSize++;
    if ( (ioMan->workersIdle < ioMan->queueSize) ) {
	/* see if giving up our quantum ferrets out some idle threads.
	 */
	LeaveCriticalSection(&ioMan->manLock);
	Sleep(0);
	EnterCriticalSection(&ioMan->manLock);
	if ( (ioMan->workersIdle < ioMan->queueSize) ) {
	    /* No, go ahead and create another. */
	    ioMan->numWorkers++;
	    LeaveCriticalSection(&ioMan->manLock);
	    NewIOWorkerThread(ioMan);
	} else {
	    LeaveCriticalSection(&ioMan->manLock);
	}
    } else {
	LeaveCriticalSection(&ioMan->manLock);
    }
  
    if (SubmitWork(ioMan->workQueue,wItem)) {
	/* Note: the work item has potentially been consumed by a worker thread
	 *       (and freed) at this point, so we cannot use wItem's requestID.
	 */
	return reqID;
    } else {
	return 0;
    }
}

/*
 * Function: AddIORequest()
 *
 * Conduit to underlying WorkQueue's SubmitWork(); adds IO
 * request to work queue, deciding whether or not to augment
 * the thread pool in the process. 
 */
int
AddIORequest ( int   fd,
	       BOOL  forWriting,
	       BOOL  isSocket,
	       int   len,
	       char* buffer,
	       CompletionProc onCompletion)
{
    WorkItem* wItem    = (WorkItem*)malloc(sizeof(WorkItem));
    unsigned int reqID = ioMan->requestID++;
    if (!ioMan || !wItem) return 0;
  
    /* Fill in the blanks */
    wItem->workKind     = ( isSocket   ? WORKER_FOR_SOCKET : 0 ) | 
	                  ( forWriting ? WORKER_WRITE : WORKER_READ );
    wItem->workData.ioData.fd  = fd;
    wItem->workData.ioData.len = len;
    wItem->workData.ioData.buf = buffer;

    wItem->onCompletion        = onCompletion;
    wItem->requestID           = reqID;
  
    return depositWorkItem(reqID, wItem);
}       

/*
 * Function: AddDelayRequest()
 *
 * Like AddIORequest(), but this time adding a delay request to
 * the request queue.
 */
BOOL
AddDelayRequest ( unsigned int   msecs,
		  CompletionProc onCompletion)
{
    WorkItem* wItem = (WorkItem*)malloc(sizeof(WorkItem));
    unsigned int reqID = ioMan->requestID++;
    if (!ioMan || !wItem) return FALSE;
  
    /* Fill in the blanks */
    wItem->workKind     = WORKER_DELAY;
    wItem->workData.delayData.msecs = msecs;
    wItem->onCompletion = onCompletion;
    wItem->requestID    = reqID;

    return depositWorkItem(reqID, wItem);
}

/*
 * Function: AddProcRequest()
 *
 * Add an asynchronous procedure request.
 */
BOOL
AddProcRequest ( void* proc,
		 void* param,
		 CompletionProc onCompletion)
{
    WorkItem* wItem = (WorkItem*)malloc(sizeof(WorkItem));
    unsigned int reqID = ioMan->requestID++;
    if (!ioMan || !wItem) return FALSE;
  
    /* Fill in the blanks */
    wItem->workKind     = WORKER_DO_PROC;
    wItem->workData.procData.proc  = proc;
    wItem->workData.procData.param = param;
    wItem->onCompletion = onCompletion;
    wItem->requestID    = reqID;

    return depositWorkItem(reqID, wItem);
}

void ShutdownIOManager()
{
  SetEvent(ioMan->hExitEvent);
  free(ioMan);
  ioMan = NULL;
}
