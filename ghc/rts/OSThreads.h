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
#if defined(RTS_SUPPORTS_THREADS) /* to the end */

# if defined(HAVE_PTHREAD_H) && !defined(WANT_NATIVE_WIN32_THREADS)
#  include <pthread.h>
typedef pthread_cond_t  Condition;
typedef pthread_mutex_t Mutex;
typedef pthread_t       OSThreadId;

#define INIT_MUTEX_VAR      PTHREAD_MUTEX_INITIALIZER
#define INIT_COND_VAR       PTHREAD_COND_INITIALIZER

#ifdef LOCK_DEBUG
#define ACQUIRE_LOCK(mutex) fprintf(stderr, "ACQUIRE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); fflush(stderr); pthread_mutex_lock(mutex)
#define RELEASE_LOCK(mutex) fprintf(stderr, "RELEASE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); fflush(stderr); pthread_mutex_unlock(mutex)
#else
#define ACQUIRE_LOCK(mutex) pthread_mutex_lock(mutex)
#define RELEASE_LOCK(mutex) pthread_mutex_unlock(mutex)
#endif

# elif defined(HAVE_WINDOWS_H)
#include <windows.h>

#include "RtsUtils.h"

typedef HANDLE Condition;
typedef HANDLE Mutex;
typedef DWORD OSThreadId;

#define INIT_MUTEX_VAR 0
#define INIT_COND_VAR  0

static inline void
ACQUIRE_LOCK(Mutex *mutex)
{
    if (WaitForSingleObject(*mutex,INFINITE) == WAIT_FAILED) {
	barf("WaitForSingleObject: %d", GetLastError());
    }
}

static inline void
RELEASE_LOCK(Mutex *mutex)
{
    if (ReleaseMutex(*mutex) == 0) {
	barf("ReleaseMutex: %d", GetLastError());
    }
}

# else
#  error "Threads not supported"
# endif

extern void initCondition         ( Condition* pCond );
extern void closeCondition        ( Condition* pCond );
extern rtsBool broadcastCondition ( Condition* pCond );
extern rtsBool signalCondition    ( Condition* pCond );
extern rtsBool waitCondition      ( Condition* pCond, 
				    Mutex* pMut );

extern void initMutex             ( Mutex* pMut );

extern OSThreadId osThreadId      ( void );
extern void shutdownThread        ( void );
extern void yieldThread           ( void );
extern int  createOSThread        ( OSThreadId* tid,
				    void (*startProc)(void) );
#else

#define ACQUIRE_LOCK(l)
#define RELEASE_LOCK(l)

#endif /* defined(RTS_SUPPORTS_THREADS) */

#endif /* __OSTHREADS_H__ */
