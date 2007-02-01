/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
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
typedef DWORD OSThreadId;
// don't be tempted to use HANDLE as the OSThreadId: there can be 
// many HANDLES to a given thread, so comparison would not work.
typedef DWORD ThreadLocalKey;

#define OSThreadProcAttr __stdcall

#define INIT_COND_VAR  0

// We have a choice for implementing Mutexes on Windows.  Standard
// Mutexes are kernel objects that require kernel calls to
// acquire/release, whereas CriticalSections are spin-locks that block
// in the kernel after spinning for a configurable number of times.
// CriticalSections are *much* faster, so we use those.  The Mutex
// implementation is left here for posterity.
#define USE_CRITICAL_SECTIONS 1

#if USE_CRITICAL_SECTIONS

typedef CRITICAL_SECTION Mutex;

#ifdef LOCK_DEBUG

#define ACQUIRE_LOCK(mutex) \
  debugBelch("ACQUIRE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); \
  EnterCriticalSection(mutex)
#define RELEASE_LOCK(mutex) \
  debugBelch("RELEASE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); \
  LeaveCriticalSection(mutex)
#define ASSERT_LOCK_HELD(mutex) /* nothing */

#else

#define ACQUIRE_LOCK(mutex)  EnterCriticalSection(mutex)
#define RELEASE_LOCK(mutex)  LeaveCriticalSection(mutex)

// I don't know how to do this.  TryEnterCriticalSection() doesn't do
// the right thing.
#define ASSERT_LOCK_HELD(mutex) /* nothing */

#endif

#else

typedef HANDLE Mutex;

// casting to (Mutex *) here required due to use in .cmm files where
// the argument has (void *) type.
#define ACQUIRE_LOCK(mutex)					\
    if (WaitForSingleObject(*((Mutex *)mutex),INFINITE) == WAIT_FAILED) { \
	barf("WaitForSingleObject: %d", GetLastError());	\
    }

#define RELEASE_LOCK(mutex)				\
    if (ReleaseMutex(*((Mutex *)mutex)) == 0) {		\
	barf("ReleaseMutex: %d", GetLastError());	\
    }

#define ASSERT_LOCK_HELD(mutex) /* nothing */
#endif

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
extern rtsBool osThreadIsAlive    ( OSThreadId id );

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
extern void closeMutex            ( Mutex* pMut );

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

#endif /* defined(THREADED_RTS) */

#endif /* __OSTHREADS_H__ */
