/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner. 
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#if defined(THREADED_RTS)
#include "OSThreads.h"
#include "RtsUtils.h"

/* For reasons not yet clear, the entire contents of process.h is protected 
 * by __STRICT_ANSI__ not being defined.
 */
#undef __STRICT_ANSI__
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
    errorBelch("initCondition: unable to create");
  }
  *pCond = h;
  return;
}

void
closeCondition( Condition* pCond )
{
  if ( CloseHandle(*pCond) == 0 ) {
    errorBelch("closeCondition: failed to close");
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
    if (SetEvent(*pCond) == 0) {
	barf("SetEvent: %d", GetLastError());
    }
    return rtsTrue;
}

rtsBool
waitCondition ( Condition* pCond, Mutex* pMut )
{
  RELEASE_LOCK(pMut);
  WaitForSingleObject(*pCond, INFINITE);
  /* Hmm..use WaitForMultipleObjects() ? */
  ACQUIRE_LOCK(pMut);
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

int
createOSThread (OSThreadId* pId, OSThreadProc *startProc, void *param)
{
  
  return (_beginthreadex ( NULL,  /* default security attributes */
			   0,
			   startProc,
			   param,
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

void
newThreadLocalKey (ThreadLocalKey *key)
{
    DWORD r;
    r = TlsAlloc();
    if (r == TLS_OUT_OF_INDEXES) {
	barf("newThreadLocalKey: out of keys");
    }
    *key = r;
}

void *
getThreadLocalVar (ThreadLocalKey *key)
{
    void *r;
    r = TlsGetValue(*key);
    if (r == NULL) {
	barf("getThreadLocalVar: key not found");
    }
    return r;
}

void
setThreadLocalVar (ThreadLocalKey *key, void *value)
{
    BOOL b;
    b = TlsSetValue(*key, value);
    if (!b) {
	barf("setThreadLocalVar: %d", GetLastError());
    }
}


static unsigned __stdcall
forkOS_createThreadWrapper ( void * entry )
{
    Capability *cap;
    cap = rts_lock();
    rts_evalStableIO(cap, (HsStablePtr) entry, NULL);
    rts_unlock(cap);
    return 0;
}

int
forkOS_createThread ( HsStablePtr entry )
{
    unsigned long pId;
    return (_beginthreadex ( NULL,  /* default security attributes */
			   0,
			   forkOS_createThreadWrapper,
			   (void*)entry,
			   0,
			   (unsigned*)&pId) == 0);
}

#endif /* defined(HAVE_PTHREAD_H) */

#else /* !defined(THREADED_RTS) */

int
forkOS_createThread ( HsStablePtr entry STG_UNUSED )
{
    return -1;
}

#endif /* !defined(THREADED_RTS) */
