/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner. 
 * 
 * --------------------------------------------------------------------------*/

#ifndef __OSTHREADS_H__
#define __OSTHREADS_H__

#if defined(THREADED_RTS) /* to the end */

# if defined(HAVE_PTHREAD_H) && !defined(WANT_NATIVE_WIN32_THREADS)

#include <pthread.h>

typedef pthread_cond_t  Condition;
typedef pthread_mutex_t Mutex;
typedef pthread_t       OSThreadId;
typedef pthread_key_t   ThreadLocalKey;

#define OSThreadProcAttr /* nothing */

#define INIT_MUTEX_VAR      PTHREAD_MUTEX_INITIALIZER
#define INIT_COND_VAR       PTHREAD_COND_INITIALIZER

#ifdef LOCK_DEBUG

#define ACQUIRE_LOCK(mutex) \
  debugBelch("ACQUIRE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); \
  pthread_mutex_lock(mutex)
#define RELEASE_LOCK(mutex) \
  debugBelch("RELEASE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); \
  pthread_mutex_unlock(mutex)
#define ASSERT_LOCK_HELD(mutex) /* nothing */

#elif defined(DEBUG) && defined(linux_HOST_OS)
#include <errno.h>
/* 
 * On Linux, we can use extensions to determine whether we already
 * hold a lock or not, which is useful for debugging.
 */
#define ACQUIRE_LOCK(mutex) \
  if (pthread_mutex_lock(mutex) == EDEADLK) { \
    barf("multiple ACQUIRE_LOCK: %s %d", __FILE__,__LINE__); \
  }
#define RELEASE_LOCK(mutex) \
  if (pthread_mutex_unlock(mutex) != 0) { \
    barf("RELEASE_LOCK: I do not own this lock: %s %d", __FILE__,__LINE__); \
  }

#define ASSERT_LOCK_HELD(mutex) ASSERT(pthread_mutex_lock(mutex) == EDEADLK)

#define ASSERT_LOCK_NOTHELD(mutex)		\
  if (pthread_mutex_lock(mutex) != EDEADLK) {	\
     pthread_mutex_unlock(mutex);		\
  } else {					\
    ASSERT(0);					\
  }


#else

#define ACQUIRE_LOCK(mutex) pthread_mutex_lock(mutex)
#define RELEASE_LOCK(mutex) pthread_mutex_unlock(mutex)
#define ASSERT_LOCK_HELD(mutex) /* nothing */

#endif

# elif defined(HAVE_WINDOWS_H)
#include <windows.h>

typedef HANDLE Condition;
typedef HANDLE Mutex;
typedef DWORD OSThreadId;
typedef DWORD ThreadLocalKey;

#define OSThreadProcAttr __stdcall

#define INIT_MUTEX_VAR 0
#define INIT_COND_VAR  0

INLINE_HEADER void
ACQUIRE_LOCK(Mutex *mutex)
{
    if (WaitForSingleObject(*mutex,INFINITE) == WAIT_FAILED) {
	barf("WaitForSingleObject: %d", GetLastError());
    }
}

INLINE_HEADER void
RELEASE_LOCK(Mutex *mutex)
{
    if (ReleaseMutex(*mutex) == 0) {
	barf("ReleaseMutex: %d", GetLastError());
    }
}

#define ASSERT_LOCK_HELD(mutex) /* nothing */

# else
#  error "Threads not supported"
# endif

//
// General thread operations
//
extern OSThreadId osThreadId      ( void );
extern void shutdownThread        ( void );
extern void yieldThread           ( void );

typedef void OSThreadProcAttr OSThreadProc(void *);

extern int  createOSThread        ( OSThreadId* tid, 
				    OSThreadProc *startProc, void *param);

//
// Condition Variables
//
extern void initCondition         ( Condition* pCond );
extern void closeCondition        ( Condition* pCond );
extern rtsBool broadcastCondition ( Condition* pCond );
extern rtsBool signalCondition    ( Condition* pCond );
extern rtsBool waitCondition      ( Condition* pCond, 
				    Mutex* pMut );

//
// Mutexes
//
extern void initMutex             ( Mutex* pMut );

//
// Thread-local storage
//
void  newThreadLocalKey (ThreadLocalKey *key);
void *getThreadLocalVar (ThreadLocalKey *key);
void  setThreadLocalVar (ThreadLocalKey *key, void *value);

#else

#define ACQUIRE_LOCK(l)
#define RELEASE_LOCK(l)
#define ASSERT_LOCK_HELD(l)

#endif /* defined(RTS_SUPPORTS_THREADS) */

#endif /* __OSTHREADS_H__ */
