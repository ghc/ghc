/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2009
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner. 
 * 
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * --------------------------------------------------------------------------*/

#ifndef RTS_OSTHREADS_H
#define RTS_OSTHREADS_H

#if defined(THREADED_RTS) /* to near the end */

#if defined(HAVE_PTHREAD_H) && !defined(mingw32_HOST_OS)

#if CMINUSMINUS

#define ACQUIRE_LOCK(mutex) foreign "C" pthread_mutex_lock(mutex)
#define RELEASE_LOCK(mutex) foreign "C" pthread_mutex_unlock(mutex)
#define ASSERT_LOCK_HELD(mutex) /* nothing */

#else

#include <pthread.h>
#include <errno.h>

typedef pthread_cond_t  Condition;
typedef pthread_mutex_t Mutex;
typedef pthread_t       OSThreadId;
typedef pthread_key_t   ThreadLocalKey;

#define OSThreadProcAttr /* nothing */

#define INIT_COND_VAR       PTHREAD_COND_INITIALIZER

#ifdef LOCK_DEBUG
#define LOCK_DEBUG_BELCH(what, mutex) \
  debugBelch("%s(0x%p) %s %d\n", what, mutex, __FILE__, __LINE__)
#else
#define LOCK_DEBUG_BELCH(what, mutex) /* nothing */
#endif

/* Always check the result of lock and unlock. */
#define ACQUIRE_LOCK(mutex) \
  LOCK_DEBUG_BELCH("ACQUIRE_LOCK", mutex); \
  if (pthread_mutex_lock(mutex) == EDEADLK) { \
    barf("multiple ACQUIRE_LOCK: %s %d", __FILE__,__LINE__); \
  }

// Returns zero if the lock was acquired.
EXTERN_INLINE int TRY_ACQUIRE_LOCK(pthread_mutex_t *mutex);
EXTERN_INLINE int TRY_ACQUIRE_LOCK(pthread_mutex_t *mutex)
{
    LOCK_DEBUG_BELCH("TRY_ACQUIRE_LOCK", mutex);
    return pthread_mutex_trylock(mutex);
}

#define RELEASE_LOCK(mutex) \
  LOCK_DEBUG_BELCH("RELEASE_LOCK", mutex); \
  if (pthread_mutex_unlock(mutex) != 0) { \
    barf("RELEASE_LOCK: I do not own this lock: %s %d", __FILE__,__LINE__); \
  }

// Note: this assertion calls pthread_mutex_lock() on a mutex that
// is already held by the calling thread.  The mutex should therefore
// have been created with PTHREAD_MUTEX_ERRORCHECK, otherwise this
// assertion will hang.  We always initialise mutexes with
// PTHREAD_MUTEX_ERRORCHECK when DEBUG is on (see rts/posix/OSThreads.h).
#define ASSERT_LOCK_HELD(mutex) ASSERT(pthread_mutex_lock(mutex) == EDEADLK)

#endif // CMINUSMINUS

# elif defined(HAVE_WINDOWS_H)

#if CMINUSMINUS

/* We jump through a hoop here to get a CCall EnterCriticalSection
   and LeaveCriticalSection, as that's what C-- wants. */

#define ACQUIRE_LOCK(mutex) foreign "stdcall" EnterCriticalSection(mutex)
#define RELEASE_LOCK(mutex) foreign "stdcall" LeaveCriticalSection(mutex)
#define ASSERT_LOCK_HELD(mutex) /* nothing */

#else

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

#define ACQUIRE_LOCK(mutex)      EnterCriticalSection(mutex)
#define TRY_ACQUIRE_LOCK(mutex)  (TryEnterCriticalSection(mutex) == 0)
#define RELEASE_LOCK(mutex)      LeaveCriticalSection(mutex)

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

#endif // CMINUSMINUS

# else
#  error "Threads not supported"
# endif


#ifndef CMINUSMINUS
//
// General thread operations
//
extern OSThreadId osThreadId      ( void );
extern void shutdownThread        ( void )   GNUC3_ATTRIBUTE(__noreturn__);
extern void yieldThread           ( void );

typedef void OSThreadProcAttr OSThreadProc(void *);

extern int  createOSThread        ( OSThreadId* tid, 
				    OSThreadProc *startProc, void *param);
extern rtsBool osThreadIsAlive    ( OSThreadId id );
extern void interruptOSThread (OSThreadId id);

//
// Condition Variables
//
extern void initCondition         ( Condition* pCond );
extern void closeCondition        ( Condition* pCond );
extern rtsBool broadcastCondition ( Condition* pCond );
extern rtsBool signalCondition    ( Condition* pCond );
extern rtsBool waitCondition      ( Condition* pCond, Mutex* pMut );

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
void  freeThreadLocalKey (ThreadLocalKey *key);

// Processors and affinity
void setThreadAffinity     (nat n, nat m);
#endif // !CMINUSMINUS

#else

#define ACQUIRE_LOCK(l)
#define RELEASE_LOCK(l)
#define ASSERT_LOCK_HELD(l)

#endif /* defined(THREADED_RTS) */

#ifndef CMINUSMINUS
//
// Support for forkOS (defined regardless of THREADED_RTS, but does
// nothing when !THREADED_RTS).
//
int forkOS_createThread ( HsStablePtr entry );

//
// Returns the number of processor cores in the machine
//
nat getNumberOfProcessors (void);

//
// Support for getting at the kernel thread Id for tracing/profiling.
//
// This stuff is optional and only used for tracing/profiling purposes, to
// match up thread ids recorded by other tools. For example, on Linux and OSX
// the pthread_t type is not the same as the kernel thread id, and system
// profiling tools like Linux perf, and OSX's DTrace use the kernel thread Id.
// So if we want to match up RTS tasks with kernel threads recorded by these
// tools then we need to know the kernel thread Id, and this must be a separate
// type from the OSThreadId.
//
// If the feature cannot be supported on an OS, it is OK to always return 0.
// In particular it would almost certaily be meaningless on systems not using
// a 1:1 threading model.

// We use a common serialisable representation on all OSs
// This is ok for Windows, OSX and Linux.
typedef StgWord64 KernelThreadId;

// Get the current kernel thread id
KernelThreadId kernelThreadId (void);

#endif /* CMINUSMINUS */

#endif /* RTS_OSTHREADS_H */
