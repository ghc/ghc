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
    iom->workersIdle++;
    LeaveCriticalSection(&iom->manLock);

    rc = WaitForMultipleObjects( 2, hWaits, FALSE, INFINITE );

    EnterCriticalSection(&iom->manLock);
    iom->workersIdle--;
    LeaveCriticalSection(&iom->manLock);
    
    if ( WAIT_OBJECT_0 == rc ) {
      /* shutdown */
#if 0
      fprintf(stderr, "shutting down...\n"); fflush(stderr);
#endif
      return 0;
    } else if ( (WAIT_OBJECT_0 + 1) == rc ) {
      /* work item available, fetch it. */
#if 0
      fprintf(stderr, "work available...\n"); fflush(stderr);
#endif
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
	  /* very approximate implementation of threadDelay */
	  Sleep(work->workData.delayData.msecs);
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
		work->workData.procData.proc(work->workData.procData.param);
		errCode=0;
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
  ioMan->requestID   = 1;
 
  return TRUE;
}

/*
 * Function: AddIORequest()
 *
 * Conduit to underlying WorkQueue's SubmitWork(); adds IO
 * request to work queue, returning without blocking.
 */
int
AddIORequest ( int   fd,
	       BOOL  forWriting,
	       BOOL  isSocket,
	       int   len,
	       char* buffer,
	       CompletionProc onCompletion)
{
  WorkItem* wItem = (WorkItem*)malloc(sizeof(WorkItem));
  if (!ioMan || !wItem) return 0;
  
  /* Fill in the blanks */
  wItem->workKind     = ( isSocket   ? WORKER_FOR_SOCKET : 0 ) | 
                        ( forWriting ? WORKER_WRITE : WORKER_READ );
  wItem->workData.ioData.fd  = fd;
  wItem->workData.ioData.len = len;
  wItem->workData.ioData.buf = buffer;

  wItem->onCompletion        = onCompletion;
  wItem->requestID           = ioMan->requestID++;
  
  EnterCriticalSection(&ioMan->manLock);
  /* If there are no worker threads available, create one.
   *
   * If this turns out to be too aggressive a policy, refine.
   */
#if 0
  fprintf(stderr, "AddIORequest: %d\n", ioMan->workersIdle); fflush(stderr);
#endif
  if ( ioMan->workersIdle == 0 ) {
    ioMan->numWorkers++;
    NewIOWorkerThread(ioMan);
  }
  LeaveCriticalSection(&ioMan->manLock);
  
  if (SubmitWork(ioMan->workQueue,wItem)) {
    return wItem->requestID;
  } else {
    return 0;
  }
}	       

/*
 * Function: AddDelayRequest()
 *
 */
BOOL
AddDelayRequest ( unsigned int   msecs,
		  CompletionProc onCompletion)
{
  WorkItem* wItem = (WorkItem*)malloc(sizeof(WorkItem));
  if (!ioMan || !wItem) return FALSE;
  
  /* Fill in the blanks */
  wItem->workKind     = WORKER_DELAY;
  wItem->workData.delayData.msecs = msecs;
  wItem->onCompletion = onCompletion;
  wItem->requestID    = ioMan->requestID++;

  EnterCriticalSection(&ioMan->manLock);
  if ( ioMan->workersIdle == 0 ) {
    ioMan->numWorkers++;
    NewIOWorkerThread(ioMan);
  }
  LeaveCriticalSection(&ioMan->manLock);
  
  if (SubmitWork(ioMan->workQueue,wItem)) {
    return wItem->requestID;
  } else {
    return 0;
  }
}

/*
 * Function: AddDelayRequest()
 *
 */
BOOL
AddProcRequest ( void* proc,
		 void* param,
		 CompletionProc onCompletion)
{
  WorkItem* wItem = (WorkItem*)malloc(sizeof(WorkItem));
  if (!ioMan || !wItem) return FALSE;
  
  /* Fill in the blanks */
  wItem->workKind     = WORKER_DO_PROC;
  wItem->workData.procData.proc  = proc;
  wItem->workData.procData.param = param;
  wItem->onCompletion = onCompletion;
  wItem->requestID    = ioMan->requestID++;

  EnterCriticalSection(&ioMan->manLock);
  if ( ioMan->workersIdle == 0 ) {
    ioMan->numWorkers++;
    NewIOWorkerThread(ioMan);
  }
  LeaveCriticalSection(&ioMan->manLock);
  
  if (SubmitWork(ioMan->workQueue,wItem)) {
    return wItem->requestID;
  } else {
    return 0;
  }
}

void ShutdownIOManager()
{
  SetEvent(ioMan->hExitEvent);
  free(ioMan);
  ioMan = NULL;
}
