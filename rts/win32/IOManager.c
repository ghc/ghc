/* IOManager.c
 *
 * Non-blocking / asynchronous I/O for Win32.
 *
 * (c) sof, 2002-2003.
 */
#include "Rts.h"
#include "IOManager.h"
#include "WorkQueue.h"
#include "ConsoleHandler.h"
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
    /* fields for keeping track of active WorkItems */
    CritSection      active_work_lock;
    WorkItem*        active_work_items;
} IOManagerState;

/* ToDo: wrap up this state via a IOManager handle instead? */
static IOManagerState* ioMan;

static void RegisterWorkItem  ( IOManagerState* iom, WorkItem* wi);
static void DeregisterWorkItem( IOManagerState* iom, WorkItem* wi);

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
    DWORD      errCode = 0;
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

	if (rc == WAIT_OBJECT_0) {
	    // we received the exit event
	    EnterCriticalSection(&iom->manLock);
	    ioMan->numWorkers--;
	    LeaveCriticalSection(&iom->manLock);
	    return 0;
	}

	EnterCriticalSection(&iom->manLock);
	/* Signal that the thread is 'non-idle' and about to consume 
	 * a work item.
	 */
	iom->workersIdle--;
	iom->queueSize--;
	LeaveCriticalSection(&iom->manLock);
    
	if ( rc == (WAIT_OBJECT_0 + 1) ) {
	    /* work item available, fetch it. */
	    if (FetchWork(pq,(void**)&work)) {
		work->abandonOp = 0;
		RegisterWorkItem(iom,work);
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
			while (1) {
			/* Do the read(), with extra-special handling for Ctrl+C */
			len = read(work->workData.ioData.fd,
				   work->workData.ioData.buf,
				   work->workData.ioData.len);
			if ( len == 0 && work->workData.ioData.len != 0 ) {
			    /* Given the following scenario:
			     *     - a console handler has been registered that handles Ctrl+C
			     *       events.
			     *     - we've not tweaked the 'console mode' settings to turn on
			     *       ENABLE_PROCESSED_INPUT.
			     *     - we're blocked waiting on input from standard input.
			     *     - the user hits Ctrl+C.
			     *
			     * The OS will invoke the console handler (in a separate OS thread),
			     * and the above read() (i.e., under the hood, a ReadFile() op) returns
			     * 0, with the error set to ERROR_OPERATION_ABORTED. We don't
			     * want to percolate this error condition back to the Haskell user.
			     * Do this by waiting for the completion of the Haskell console handler.
			     * If upon completion of the console handler routine, the Haskell thread 
			     * that issued the request is found to have been thrown an exception, 
			     * the worker abandons the request (since that's what the Haskell thread 
			     * has done.) If the Haskell thread hasn't been interrupted, the worker 
			     * retries the read request as if nothing happened.
			     */
			    if ( (GetLastError()) == ERROR_OPERATION_ABORTED ) {
				/* For now, only abort when dealing with the standard input handle.
				 * i.e., for all others, an error is raised.
				 */
				HANDLE h  = (HANDLE)GetStdHandle(STD_INPUT_HANDLE);
				if ( _get_osfhandle(work->workData.ioData.fd) == (long)h ) {
				    if (rts_waitConsoleHandlerCompletion()) {
					/* If the Scheduler has set work->abandonOp, the Haskell thread has 
					 * been thrown an exception (=> the worker must abandon this request.)
					 * We test for this below before invoking the on-completion routine.
					 */
					if (work->abandonOp) {
					    break;
					} else {
					    continue;
					}
				    } 
				} else { 
				    break; /* Treat it like an error */
				}
			    } else {
				break;
			    }
			} else {
			    break;
			}
			}
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
		if (!work->abandonOp) {
		    work->onCompletion(work->requestID,
				       fd,
				       len,
				       complData,
				       errCode);
		}
		/* Free the WorkItem */
		DeregisterWorkItem(iom,work);
		free(work);
	    } else {
		fprintf(stderr, "unable to fetch work; fatal.\n"); fflush(stderr);
		return 1;
	    }
	} else {
	    fprintf(stderr, "waiting failed (%lu); fatal.\n", rc); fflush(stderr);
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
    InitializeCriticalSection(&ioMan->active_work_lock);
    ioMan->active_work_items = NULL;
 
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
    wItem->link = NULL;

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
    wItem->link         = NULL;

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
    wItem->abandonOp    = 0;
    wItem->link         = NULL;

    return depositWorkItem(reqID, wItem);
}

void ShutdownIOManager ( void )
{
    int num;

    SetEvent(ioMan->hExitEvent);
  
    /* Wait for all worker threads to die. */
    for (;;) {
        EnterCriticalSection(&ioMan->manLock);
	num = ioMan->numWorkers;
	LeaveCriticalSection(&ioMan->manLock);
	if (num == 0)
	    break;
	Sleep(10);
    }
    FreeWorkQueue(ioMan->workQueue);
    CloseHandle(ioMan->hExitEvent);
    free(ioMan);
    ioMan = NULL;
}

/* Keep track of WorkItems currently being serviced. */
static 
void
RegisterWorkItem(IOManagerState* ioMan, 
		 WorkItem* wi)
{
    EnterCriticalSection(&ioMan->active_work_lock);
    wi->link = ioMan->active_work_items;
    ioMan->active_work_items = wi;
    LeaveCriticalSection(&ioMan->active_work_lock);
}

static 
void
DeregisterWorkItem(IOManagerState* ioMan, 
		   WorkItem* wi)
{
    WorkItem *ptr, *prev;
    
    EnterCriticalSection(&ioMan->active_work_lock);
    for(prev=NULL,ptr=ioMan->active_work_items;ptr;prev=ptr,ptr=ptr->link) {
	if (wi->requestID == ptr->requestID) {
	    if (prev==NULL) {
		ioMan->active_work_items = ptr->link;
	    } else {
		prev->link = ptr->link;
	    }
	    LeaveCriticalSection(&ioMan->active_work_lock);
	    return;
	}
    }
    fprintf(stderr, "DeregisterWorkItem: unable to locate work item %d\n", wi->requestID);
    LeaveCriticalSection(&ioMan->active_work_lock);
}


/*
 * Function: abandonWorkRequest()
 *
 * Signal that a work request isn't of interest. Called by the Scheduler
 * if a blocked Haskell thread has an exception thrown to it.
 *
 * Note: we're not aborting the system call that a worker might be blocked on
 * here, just disabling the propagation of its result once its finished. We
 * may have to go the whole hog here and switch to overlapped I/O so that we
 * can abort blocked system calls.
 */
void
abandonWorkRequest ( int reqID )
{
    WorkItem *ptr;
    EnterCriticalSection(&ioMan->active_work_lock);
    for(ptr=ioMan->active_work_items;ptr;ptr=ptr->link) {
	if (ptr->requestID == (unsigned int)reqID ) {
	    ptr->abandonOp = 1;
	    LeaveCriticalSection(&ioMan->active_work_lock);
	    return;
	}
    }
    /* Note: if the request ID isn't present, the worker will have
     * finished sometime since awaitRequests() last drained the completed
     * request table; i.e., not an error.
     */
    LeaveCriticalSection(&ioMan->active_work_lock);
}
