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

void
initCondition( Condition* pCond )
{
  pthread_cond_init(pCond, NULL);
  return;
}

void
closeCondition( Condition* pCond )
{
  pthread_cond_destroy(pCond);
  return;
}

rtsBool
broadcastCondition ( Condition* pCond )
{
  return (pthread_cond_broadcast(pCond) == 0);
}

rtsBool
signalCondition ( Condition* pCond )
{
  return (pthread_cond_signal(pCond) == 0);
}

rtsBool
waitCondition ( Condition* pCond, Mutex* pMut )
{
  return (pthread_cond_wait(pCond,pMut) == 0);
}

void
yieldThread()
{
  sched_yield();
  return;
}

void
shutdownThread()
{
  pthread_exit(NULL);
}

/* Don't need the argument nor the result, at least not yet. */
static void* startProcWrapper(void* pProc);
static void*
startProcWrapper(void* pProc)
{
  ((void (*)(void))pProc)();
  return NULL;
}


int
createOSThread ( OSThreadId* pId, void (*startProc)(void))
{
  return pthread_create(pId, NULL, startProcWrapper, (void*)startProc);
}

OSThreadId
osThreadId()
{
  return pthread_self();
}

void
initMutex(Mutex* pMut)
{
  pthread_mutex_init(pMut,NULL);
  return;
}

#elif defined(HAVE_WINDOWS_H)
#include <process.h>

/* Win32 threads and synchronisation objects */

/* A Condition is represented by a Win32 Event object;
 * a Mutex by a Mutex kernel object.
 *
 * ToDo: go through the defn and usage of these to
 * make sure the semantics match up with that of 
 * the (assumed) pthreads behaviour. This is really
 * just a first pass at getting something compilable.
 */

void
initCondition( Condition* pCond )
{
  HANDLE h =  CreateEvent(NULL, 
			  FALSE,  /* auto reset */
			  FALSE,  /* initially not signalled */
			  NULL); /* unnamed => process-local. */
  
  if ( h == NULL ) {
    belch("initCondition: unable to create");
  }
  *pCond = h;
  return;
}

void
closeCondition( Condition* pCond )
{
  if ( CloseHandle(*pCond) == 0 ) {
    belch("closeCondition: failed to close");
  }
  return;
}

rtsBool
broadcastCondition ( Condition* pCond )
{
  PulseEvent(*pCond);
  return rtsTrue;
}

rtsBool
signalCondition ( Condition* pCond )
{
  SetEvent(*pCond);
  return rtsTrue;
}

rtsBool
waitCondition ( Condition* pCond, Mutex* pMut )
{
  ReleaseMutex(*pMut);
  WaitForSingleObject(*pCond, INFINITE);
  /* Hmm..use WaitForMultipleObjects() ? */
  WaitForSingleObject(*pMut, INFINITE);
  return rtsTrue;
}

void
yieldThread()
{
  Sleep(0);
  return;
}

void
shutdownThread()
{
  _endthreadex(0);
}

static unsigned __stdcall startProcWrapper(void* pReal);
static unsigned __stdcall
startProcWrapper(void* pReal)
{
  ((void (*)(void))pReal)();
  return 0;
}

int
createOSThread ( OSThreadId* pId, void (*startProc)(void))
{
  
  return (_beginthreadex ( NULL,  /* default security attributes */
			   0,
			   startProcWrapper,
			   (void*)startProc,
			   0,
			   (unsigned*)pId) == 0);
}

OSThreadId
osThreadId()
{
  return GetCurrentThreadId();
}

void
initMutex (Mutex* pMut)
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
