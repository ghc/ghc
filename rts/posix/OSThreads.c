/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner.
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"

#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS)
/* Inclusion of system headers usually requires __BSD_VISIBLE on FreeBSD and
 * DragonflyBSD, because of some specific types, like u_char, u_int, etc. */
#define __BSD_VISIBLE   1
#endif
#if defined(darwin_HOST_OS)
/* Inclusion of system headers usually requires _DARWIN_C_SOURCE on Mac OS X
 * because of some specific types like u_char, u_int, etc. */
#define _DARWIN_C_SOURCE 1
#endif

#include "Rts.h"

#if defined(linux_HOST_OS)
#include <unistd.h>
#include <sys/types.h>
#include <sys/syscall.h>
#endif

#if defined(HAVE_PTHREAD_H)
#include <pthread.h>
#if defined(freebsd_HOST_OS)
#include <pthread_np.h>
#endif
#endif

#include "RtsUtils.h"
#include "Task.h"

#if HAVE_STRING_H
#include <string.h>
#endif

#if defined(darwin_HOST_OS) || defined(freebsd_HOST_OS)
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#if !defined(HAVE_PTHREAD_H)
#error pthreads.h is required for the threaded RTS on Posix platforms
#endif

#if defined(HAVE_SCHED_H)
#include <sched.h>
#endif

#if defined(HAVE_SYS_PARAM_H)
#include <sys/param.h>
#endif
#if defined(HAVE_SYS_CPUSET_H)
#include <sys/cpuset.h>
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(darwin_HOST_OS)
#include <mach/mach.h>
#endif

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#if defined(HAVE_NUMA_H)
#include <numa.h>
#endif

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

bool
broadcastCondition ( Condition* pCond )
{
  return (pthread_cond_broadcast(pCond) == 0);
}

bool
signalCondition ( Condition* pCond )
{
  return (pthread_cond_signal(pCond) == 0);
}

bool
waitCondition ( Condition* pCond, Mutex* pMut )
{
  return (pthread_cond_wait(pCond,pMut) == 0);
}

void
yieldThread(void)
{
  sched_yield();
  return;
}

void
shutdownThread(void)
{
  pthread_exit(NULL);
}

int
createOSThread (OSThreadId* pId, char *name STG_UNUSED,
                OSThreadProc *startProc, void *param)
{
  int result = pthread_create(pId, NULL, (void *(*)(void *))startProc, param);
  if (!result) {
    pthread_detach(*pId);
#if defined(HAVE_PTHREAD_SETNAME_NP)
    pthread_setname_np(*pId, name);
#endif
  }
  return result;
}

OSThreadId
osThreadId(void)
{
  return pthread_self();
}

bool
osThreadIsAlive(OSThreadId id STG_UNUSED)
{
    // no good way to implement this on POSIX, AFAICT.  Returning true
    // is safe.
    return true;
}

void
initMutex(Mutex* pMut)
{
#if defined(DEBUG)
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_ERRORCHECK);
    pthread_mutex_init(pMut,&attr);
#else
    pthread_mutex_init(pMut,NULL);
#endif
    return;
}
void
closeMutex(Mutex* pMut)
{
    pthread_mutex_destroy(pMut);
}

void
newThreadLocalKey (ThreadLocalKey *key)
{
    int r;
    if ((r = pthread_key_create(key, NULL)) != 0) {
        barf("newThreadLocalKey: %s", strerror(r));
    }
}

void *
getThreadLocalVar (ThreadLocalKey *key)
{
    return pthread_getspecific(*key);
    // Note: a return value of NULL can indicate that either the key
    // is not valid, or the key is valid and the data value has not
    // yet been set.  We need to use the latter case, so we cannot
    // detect errors here.
}

void
setThreadLocalVar (ThreadLocalKey *key, void *value)
{
    int r;
    if ((r = pthread_setspecific(*key,value)) != 0) {
        barf("setThreadLocalVar: %s", strerror(r));
    }
}

void
freeThreadLocalKey (ThreadLocalKey *key)
{
    int r;
    if ((r = pthread_key_delete(*key)) != 0) {
        barf("freeThreadLocalKey: %s", strerror(r));
    }
}

#if defined(THREADED_RTS)

static void *
forkOS_createThreadWrapper ( void * entry )
{
    Capability *cap;
    cap = rts_lock();
    rts_evalStableIO(&cap, (HsStablePtr) entry, NULL);
    rts_unlock(cap);
    rts_done();
    return NULL;
}

int
forkOS_createThread ( HsStablePtr entry )
{
    pthread_t tid;
    int result = pthread_create(&tid, NULL,
                                forkOS_createThreadWrapper, (void*)entry);
    if(!result)
        pthread_detach(tid);
    return result;
}

void freeThreadingResources (void) { /* nothing */ }

uint32_t
getNumberOfProcessors (void)
{
    static uint32_t nproc = 0;

    if (nproc == 0) {
#if defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN)
        nproc = sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_CONF)
        nproc = sysconf(_SC_NPROCESSORS_CONF);
#elif defined(darwin_HOST_OS)
        size_t size = sizeof(uint32_t);
        if(sysctlbyname("hw.logicalcpu",&nproc,&size,NULL,0) != 0) {
            if(sysctlbyname("hw.ncpu",&nproc,&size,NULL,0) != 0)
                nproc = 1;
        }
#elif defined(freebsd_HOST_OS)
        size_t size = sizeof(uint32_t);
        if(sysctlbyname("hw.ncpu",&nproc,&size,NULL,0) != 0)
            nproc = 1;
#else
        nproc = 1;
#endif
    }

    return nproc;
}

#else /* !defined(THREADED_RTS) */

int
forkOS_createThread ( HsStablePtr entry STG_UNUSED )
{
    return -1;
}

void freeThreadingResources (void) { /* nothing */ }

uint32_t getNumberOfProcessors (void)
{
    return 1;
}

#endif /* defined(THREADED_RTS) */

#if defined(HAVE_SCHED_H) && defined(HAVE_SCHED_SETAFFINITY)
// Schedules the thread to run on CPU n of m.  m may be less than the
// number of physical CPUs, in which case, the thread will be allowed
// to run on CPU n, n+m, n+2m etc.
void
setThreadAffinity (uint32_t n, uint32_t m)
{
    uint32_t nproc;
    cpu_set_t cs;
    uint32_t i;

    nproc = getNumberOfProcessors();
    CPU_ZERO(&cs);
    for (i = n; i < nproc; i+=m) {
        CPU_SET(i, &cs);
    }
    sched_setaffinity(0, sizeof(cpu_set_t), &cs);
}

#elif defined(darwin_HOST_OS) && defined(THREAD_AFFINITY_POLICY)
// Schedules the current thread in the affinity set identified by tag n.
void
setThreadAffinity (uint32_t n, uint32_t m GNUC3_ATTRIBUTE(__unused__))
{
    thread_affinity_policy_data_t policy;

    policy.affinity_tag = n;
    thread_policy_set(mach_thread_self(),
                      THREAD_AFFINITY_POLICY,
                      (thread_policy_t) &policy,
                      THREAD_AFFINITY_POLICY_COUNT);
}

#elif defined(HAVE_SYS_CPUSET_H) /* FreeBSD 7.1+ */
void
setThreadAffinity(uint32_t n, uint32_t m)
{
        uint32_t nproc;
        cpuset_t cs;
        uint32_t i;

        nproc = getNumberOfProcessors();
        CPU_ZERO(&cs);

        for (i = n; i < nproc; i += m)
                CPU_SET(i, &cs);

        cpuset_setaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID,
                           -1, sizeof(cpuset_t), &cs);
}

#else
void
setThreadAffinity (uint32_t n STG_UNUSED,
                   uint32_t m STG_UNUSED)
{
}
#endif

#if HAVE_LIBNUMA
void setThreadNode (uint32_t node)
{
    if (numa_run_on_node(node) == -1) {
        sysErrorBelch("numa_run_on_node");
        stg_exit(1);
    }
}

void releaseThreadNode (void)
{
    if (numa_run_on_node(-1) == -1) {
        sysErrorBelch("numa_run_on_node");
        stg_exit(1);
    }
}

#else
void setThreadNode (uint32_t node STG_UNUSED) { /* nothing */ }
void releaseThreadNode (void) { /* nothing */ }
#endif

void
interruptOSThread (OSThreadId id)
{
    pthread_kill(id, SIGPIPE);
}

KernelThreadId kernelThreadId (void)
{
#if defined(linux_HOST_OS)
    pid_t tid = syscall(SYS_gettid); // no really, see man gettid
    return (KernelThreadId) tid;

/* FreeBSD 9.0+ */
#elif defined(freebsd_HOST_OS) && (__FreeBSD_version >= 900031)
    return pthread_getthreadid_np();

// Check for OS X >= 10.6 (see #7356)
#elif defined(darwin_HOST_OS) && \
       !(defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && \
         __MAC_OS_X_VERSION_MIN_REQUIRED < 1060)
    uint64_t ktid;
    pthread_threadid_np(NULL, &ktid);
    return ktid;

#else
    // unsupported
    return 0;
#endif
}
