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
/* Win32 threads and synchronisation objects */

/* A CondVar is represented by a Win32 Event object,
 * a MutexVar by a Mutex kernel object.
 */

void initCondVar( CondVar* pCond )
{
  HANDLE h =  CreateEvent(NULL, 
			  TRUE,  /* manual reset */
			  TRUE,  /* initially signalled */
			  NULL); /* unnamed => process-local. */
  


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
  pthread_mutex_init(pMut);
  return;
}

#endif /* defined(HAVE_PTHREAD_H) */

#endif /* defined(RTS_SUPPORTS_THREADS) */
