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
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * --------------------------------------------------------------------------*/

#pragma once

#if defined(HAVE_PTHREAD_H) && !defined(mingw32_HOST_OS)

#if defined(CMINUSMINUS)

#define OS_ACQUIRE_LOCK(mutex) foreign "C" pthread_mutex_lock(mutex)
#define OS_RELEASE_LOCK(mutex) foreign "C" pthread_mutex_unlock(mutex)
#define OS_ASSERT_LOCK_HELD(mutex) /* nothing */

#else

#include <pthread.h>
#include <errno.h>

typedef struct {
    pthread_cond_t cond;

    // Which clock are pthread_cond_timedwait calls referenced against?
    // N.B. Some older Darwin releases don't support clock_gettime. However, we
    // do want to reference to CLOCK_MONOTONIC whenever possible as it is more
    // robust against system time changes and is likely cheaper to query.
#if defined(HAVE_CLOCK_GETTIME) && defined(HAVE_PTHREAD_CONDATTR_SETCLOCK)
    clockid_t timeout_clk;
#endif
} Condition;
typedef pthread_mutex_t Mutex;
typedef pthread_t       OSThreadId;

#define OSThreadProcAttr /* nothing */

#define INIT_COND_VAR       PTHREAD_COND_INITIALIZER

#if defined(LOCK_DEBUG)
#define LOCK_DEBUG_BELCH(what, mutex) \
  debugBelch("%s(0x%p) %s %d\n", what, mutex, __FILE__, __LINE__)
#else
#define LOCK_DEBUG_BELCH(what, mutex) /* nothing */
#endif

/* Always check the result of lock and unlock. */
#define OS_ACQUIRE_LOCK(mutex) { \
  LOCK_DEBUG_BELCH("ACQUIRE_LOCK", mutex); \
  int __r = pthread_mutex_lock(mutex); \
  if (__r != 0) { \
    barf("ACQUIRE_LOCK failed (%s:%d): %d", __FILE__, __LINE__, __r); \
  } }

// Returns zero if the lock was acquired.
EXTERN_INLINE int OS_TRY_ACQUIRE_LOCK(pthread_mutex_t *mutex);
EXTERN_INLINE int OS_TRY_ACQUIRE_LOCK(pthread_mutex_t *mutex)
{
    LOCK_DEBUG_BELCH("TRY_ACQUIRE_LOCK", mutex);
    return pthread_mutex_trylock(mutex);
}

#define OS_RELEASE_LOCK(mutex) \
  LOCK_DEBUG_BELCH("RELEASE_LOCK", mutex); \
  if (pthread_mutex_unlock(mutex) != 0) { \
    barf("RELEASE_LOCK: I do not own this lock: %s %d", __FILE__,__LINE__); \
  }

// Note: this assertion calls pthread_mutex_lock() on a mutex that
// is already held by the calling thread.  The mutex should therefore
// have been created with PTHREAD_MUTEX_ERRORCHECK, otherwise this
// assertion will hang.  We always initialise mutexes with
// PTHREAD_MUTEX_ERRORCHECK when DEBUG is on (see rts/posix/OSThreads.h).
#define OS_ASSERT_LOCK_HELD(mutex) ASSERT(pthread_mutex_lock(mutex) == EDEADLK)

#endif // CMINUSMINUS

# elif defined(HAVE_WINDOWS_H)

#if defined(CMINUSMINUS)

/* We jump through a hoop here to get a CCall AcquireSRWLockExclusive
   and ReleaseSRWLockExclusive, as that's what C-- wants. */

#define OS_ACQUIRE_LOCK(mutex) ccall AcquireSRWLockExclusive((mutex) "ptr")
#define OS_RELEASE_LOCK(mutex) ccall ReleaseSRWLockExclusive((mutex) "ptr")
#define OS_ASSERT_LOCK_HELD(mutex) /* nothing */

#else // CMINUSMINUS

#include <windows.h>
#include <synchapi.h>

/* Use native conditional variables coupled with SRW locks, these are more
   efficient and occur a smaller overhead then emulating them with events.
   See Note [SRW locks].  */
typedef CONDITION_VARIABLE Condition;
typedef DWORD OSThreadId;
// don't be tempted to use HANDLE as the OSThreadId: there can be
// many HANDLES to a given thread, so comparison would not work.

#define OSThreadProcAttr

#define INIT_COND_VAR  0

/* Note [SRW locks]
   ~~~~~~~~~~~~~~~~
   We have a choice for implementing Mutexes on Windows.  Standard
   Mutexes are kernel objects that require kernel calls to
   acquire/release, whereas CriticalSections are spin-locks that block
   in the kernel after spinning for a configurable number of times.
   CriticalSections are *much* faster than Mutexes, however not as fast as
   slim reader/writer locks.  CriticalSections also require a 48 byte structure
   to provide lock re-entrancy.  We don't need that because the other primitives
   used for other platforms don't have this, as such locks are used defensively
   in the RTS in a way that we don't need re-entrancy.  This means that SRW's
   8 byte size is much more appropriate.  With an 8 byte payload there's a
   higher chance of it being in your cache line.  They're also a lot faster than
   CriticalSections when multiple threads are involved.  CS requires setup and
   teardown via kernel calls while SRWL is zero-initialized via
   SRWLOCK_INIT assignment. */

typedef SRWLOCK Mutex;

#if defined(LOCK_DEBUG)

#define OS_ACQUIRE_LOCK(mutex) \
  debugBelch("ACQUIRE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); \
  AcquireSRWLockExclusive(mutex)
#define OS_RELEASE_LOCK(mutex) \
  debugBelch("RELEASE_LOCK(0x%p) %s %d\n", mutex,__FILE__,__LINE__); \
  ReleaseSRWLockExclusive(mutex)
#define OS_ASSERT_LOCK_HELD(mutex) /* nothing */

#else

#define OS_ACQUIRE_LOCK(mutex)      AcquireSRWLockExclusive(mutex)
#define OS_TRY_ACQUIRE_LOCK(mutex)  (TryAcquireSRWLockExclusive(mutex) == 0)
#define OS_RELEASE_LOCK(mutex)      ReleaseSRWLockExclusive(mutex)
#define OS_INIT_LOCK(mutex)         InitializeSRWLock(mutex)
#define OS_CLOSE_LOCK(mutex)

// I don't know how to do this.  TryEnterCriticalSection() doesn't do
// the right thing.
#define OS_ASSERT_LOCK_HELD(mutex) /* nothing */

#endif // LOCK_DEBUG

#endif // CMINUSMINUS

# elif defined(wasm32_HOST_ARCH)

#if defined(CMINUSMINUS)
#else // CMINUSMINUS

#include <errno.h>

typedef void* Condition;
typedef void* Mutex;
typedef void* OSThreadId;

#define OSThreadProcAttr

#endif // CMINUSMINUS

# elif defined(THREADED_RTS)
#  error "Threads not supported"
# endif


#if !defined(CMINUSMINUS)
//
// General thread operations
//
extern OSThreadId osThreadId      ( void );
extern void shutdownThread        ( void )   STG_NORETURN;
extern void yieldThread           ( void );

typedef void* OSThreadProcAttr OSThreadProc(void *);

extern int  createOSThread        ( OSThreadId* tid, const char *name,
                                    OSThreadProc *startProc, void *param);
#if !defined(mingw32_HOST_OS)
extern int  createAttachedOSThread( OSThreadId *tid, const char *name,
                                    OSThreadProc *startProc, void *param);
#endif
extern bool osThreadIsAlive       ( OSThreadId id );
extern void interruptOSThread     ( OSThreadId id );
extern void joinOSThread          ( OSThreadId id );

//
// Condition Variables
//
extern void initCondition         ( Condition* pCond );
extern void closeCondition        ( Condition* pCond );
extern void broadcastCondition    ( Condition* pCond );
extern void signalCondition       ( Condition* pCond );
extern void waitCondition         ( Condition* pCond, Mutex* pMut );
// Returns false on timeout, true otherwise.
extern bool timedWaitCondition    ( Condition* pCond, Mutex* pMut, Time timeout);

//
// Mutexes
//
extern void initMutex             ( Mutex* pMut );
extern void closeMutex            ( Mutex* pMut );

// Processors and affinity
void setThreadAffinity (uint32_t n, uint32_t m);
void setThreadNode (uint32_t node);
void releaseThreadNode (void);
#endif // !CMINUSMINUS

#if defined(THREADED_RTS)

#define ACQUIRE_LOCK(l) OS_ACQUIRE_LOCK(l)
#define TRY_ACQUIRE_LOCK(l) OS_TRY_ACQUIRE_LOCK(l)
#define RELEASE_LOCK(l) OS_RELEASE_LOCK(l)
#define ASSERT_LOCK_HELD(l) OS_ASSERT_LOCK_HELD(l)

#else

#define ACQUIRE_LOCK(l)
#define TRY_ACQUIRE_LOCK(l) 0
#define RELEASE_LOCK(l)
#define ASSERT_LOCK_HELD(l)

#endif /* defined(THREADED_RTS) */

#if !defined(CMINUSMINUS)
//
// Support for forkOS (defined regardless of THREADED_RTS, but does
// nothing when !THREADED_RTS).
//
int forkOS_createThread ( HsStablePtr entry );

//
// Free any global resources created in OSThreads.
//
void freeThreadingResources(void);

//
// Returns the number of processor cores in the machine
//
uint32_t getNumberOfProcessors (void);

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
// In particular it would almost certainly be meaningless on systems not using
// a 1:1 threading model.

// We use a common serialisable representation on all OSs
// This is ok for Windows, OSX and Linux.
typedef StgWord64 KernelThreadId;

// Get the current kernel thread id
KernelThreadId kernelThreadId (void);

#endif /* CMINUSMINUS */
