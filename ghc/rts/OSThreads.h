/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner. 
 *
 * 
 * --------------------------------------------------------------------------*/
#ifndef __OSTHREADS_H__
#define __OSTHREADS_H__
#if defined(RTS_SUPPORTS_THREADS) /*to the end */

#if defined(HAVE_PTHREAD_H) && !defined(WANT_NATIVE_WIN32_THREADS)
#include <pthread.h>
typedef pthread_cond_t  CondVar;
typedef pthread_mutex_t MutexVar;
typedef pthread_t       OSThreadId;

#define INIT_MUTEX_VAR  PTHREAD_MUTEX_INITIALIZER
#define INIT_COND_VAR   PTHREAD_COND_INITIALIZER
#elif defined(HAVE_WINDOWS_H)
#include <windows.h>
typedef HANDLE CondVar;
typedef HANDLE MutexVar;
typedef HANDLE OSThreadId;

#define INIT_MUTEX_VAR 0
#define INIT_COND_VAR  0
#else
#error "Threads not supported"
#endif

extern void initCondVar ( CondVar* pCond );
extern void closeCondVar ( CondVar* pCond );
extern rtsBool broadcastCondVar (CondVar* pCond );
extern rtsBool signalCondVar ( CondVar* pCond );
extern rtsBool waitCondVar   ( CondVar* pCond, MutexVar* pMut);

extern OSThreadId osThreadId(void);
extern void shutdownThread(void);
extern int  createOSThread ( OSThreadId* tid, void *(*startProc)(void*));

extern void initMutexVar ( MutexVar* pMut );

#endif /* defined(RTS_SUPPORTS_THREADS) */

#endif /* __OSTHREADS_H__ */
