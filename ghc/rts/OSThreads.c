/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner. 
 *
 * 
 * --------------------------------------------------------------------------*/
#include "Rts.h"
#if defined(RTS_SUPPORTS_THREADS)
#include "OSThreads.h"
#include "RtsUtils.h"

#if defined(HAVE_PTHREAD_H) && !defined(WANT_NATIVE_WIN32_THREADS)
/*
 * This (allegedly) OS threads independent layer was initially
 * abstracted away from code that used Pthreads, so the functions
 * provided here are mostly just wrappers to the Pthreads API.
 *
 */

void initCondVar( CondVar* pCond )
{
  pthread_cond_init(pCond, NULL);
  return;
}

void closeCondVar( CondVar* pCond )
{
  pthread_cond_destroy(pCond);
  return;
}

rtsBool
broadcastCondVar ( CondVar* pCond )
{
  return (pthread_cond_broadcast(pCond) == 0);
}

rtsBool
signalCondVar ( CondVar* pCond )
{
  return (pthread_cond_signal(pCond) == 0);
}

rtsBool
waitCondVar ( CondVar* pCond, MutexVar* pMut )
{
  return (pthread_cond_wait(pCond,pMut) == 0);
}

void shutdownThread()
{
  pthread_exit(NULL);
}

int createOSThread ( OSThreadId* pId, void *(*startProc)(void*))
{
  return pthread_create(pId, NULL, startProc, NULL);
}

OSThreadId osThreadId()
{
  return pthread_self();
}

void initMutexVar (MutexVar* pMut)
{
  pthread_mutex_init(pMut,NULL);
  return;
}

#elif defined(HAVE_WINDOWS_H)
#include <process.h>

/* Win32 threads and synchronisation objects */

/* A CondVar is represented by a Win32 Event object,
 * a MutexVar by a Mutex kernel object.
 *
 * ToDo: go through the defn and usage of these to
 * make sure the semantics match up with that of 
 * the (assumed) pthreads behaviour. This is really
 * just a first pass at getting something compilable.
 */

void initCondVar( CondVar* pCond )
{
  HANDLE h =  CreateEvent(NULL, 
			  TRUE,  /* manual reset */
			  TRUE,  /* initially signalled */
			  NULL); /* unnamed => process-local. */
  
  if ( h == NULL ) {
    belch("initCondVar: unable to create");
  }
  *pCond = h;
  return;
}

void closeCondVar( CondVar* pCond )
{
  if ( CloseHandle(*pCond) == 0 ) {
    belch("closeCondVar: failed to close");
  }
  return;
}

rtsBool
broadcastCondVar ( CondVar* pCond )
{
  PulseEvent(*pCond);
  return rtsTrue;
}

rtsBool
signalCondVar ( CondVar* pCond )
{
  SetEvent(*pCond);
  return rtsTrue;
}

rtsBool
waitCondVar ( CondVar* pCond, MutexVar* pMut )
{
  ReleaseMutex(*pMut);
  WaitForSingleObject(*pCond, INFINITE);
  /* Hmm..use WaitForMultipleObjects() ? */
  WaitForSingleObject(*pMut, INFINITE);
  return rtsTrue;
}

void shutdownThread()
{
  _endthreadex(0);
}

static unsigned __stdcall startProcWrapper(void* pReal);
static unsigned __stdcall startProcWrapper(void* pReal)
{
  ((void (*)(void*))pReal)(NULL);
  return 0;
}

int createOSThread ( OSThreadId* pId, void *(*startProc)(void*))
{
  
  return _beginthreadex ( NULL,  /* default security attributes */
			  0,
			  startProcWrapper,
			  (void*)startProc,
			  0,
			  (unsigned*)pId);
}

OSThreadId osThreadId()
{
  return GetCurrentThreadId();
}

void initMutexVar (MutexVar* pMut)
{
  HANDLE h = CreateMutex ( NULL,  /* default sec. attributes */
			   FALSE, /* not owned => initially signalled */
			   NULL
			   );
  *pMut = h;
  return;
}

#endif /* defined(HAVE_PTHREAD_H) */

#endif /* defined(RTS_SUPPORTS_THREADS) */
