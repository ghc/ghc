/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner.
 *
 * --------------------------------------------------------------------------*/

#define _WIN32_WINNT 0x0501

#include "Rts.h"
#include <windows.h>
#if defined(THREADED_RTS)
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
      sysErrorBelch("initCondition: unable to create");
      stg_exit(EXIT_FAILURE);
  }
  *pCond = h;
  return;
}

void
closeCondition( Condition* pCond )
{
  if ( CloseHandle(*pCond) == 0 ) {
      sysErrorBelch("closeCondition: failed to close");
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
        sysErrorBelch("SetEvent");
        stg_exit(EXIT_FAILURE);
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
  SwitchToThread();
  return;
}

void
shutdownThread()
{
    ExitThread(0);
    barf("ExitThread() returned"); // avoid gcc warning
}

int
createOSThread (OSThreadId* pId, OSThreadProc *startProc, void *param)
{
    HANDLE h;
    h = CreateThread ( NULL,  /* default security attributes */
                       0,
                       (LPTHREAD_START_ROUTINE)startProc,
                       param,
                       0,
                       pId);

    if (h == 0) {
        return 1;
    } else {
        // This handle leaks if we don't close it here.  Perhaps we
        // should try to keep it around to avoid needing OpenThread()
        // later.
        CloseHandle(h);
        return 0;
    }
}

OSThreadId
osThreadId()
{
  return GetCurrentThreadId();
}

rtsBool
osThreadIsAlive(OSThreadId id)
{
    DWORD exit_code;
    HANDLE hdl;
    if (!(hdl = OpenThread(THREAD_QUERY_INFORMATION,FALSE,id))) {
        sysErrorBelch("osThreadIsAlive: OpenThread");
        stg_exit(EXIT_FAILURE);
    }
    if (!GetExitCodeThread(hdl, &exit_code)) {
        sysErrorBelch("osThreadIsAlive: GetExitCodeThread");
        stg_exit(EXIT_FAILURE);
    }
    CloseHandle(hdl);
    return (exit_code == STILL_ACTIVE);
}

#ifdef USE_CRITICAL_SECTIONS
void
initMutex (Mutex* pMut)
{
    InitializeCriticalSectionAndSpinCount(pMut,4000);
}
void
closeMutex (Mutex* pMut)
{
    DeleteCriticalSection(pMut);
}
#else
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
closeMutex (Mutex* pMut)
{
    CloseHandle(*pMut);
}
#endif

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
#ifdef DEBUG
    // r is allowed to be NULL - it can mean that either there was an
    // error or the stored value is in fact NULL.
    if (GetLastError() != NO_ERROR) {
        sysErrorBelch("getThreadLocalVar");
        stg_exit(EXIT_FAILURE);
    }
#endif
    return r;
}

void
setThreadLocalVar (ThreadLocalKey *key, void *value)
{
    BOOL b;
    b = TlsSetValue(*key, value);
    if (!b) {
        sysErrorBelch("setThreadLocalVar");
        stg_exit(EXIT_FAILURE);
    }
}

void
freeThreadLocalKey (ThreadLocalKey *key)
{
    BOOL r;
    r = TlsFree(*key);
    if (r == 0) {
        DWORD dw = GetLastError();
        barf("freeThreadLocalKey failed: %lu", dw);
    }
}


static unsigned __stdcall
forkOS_createThreadWrapper ( void * entry )
{
    Capability *cap;
    cap = rts_lock();
    rts_evalStableIO(&cap, (HsStablePtr) entry, NULL);
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

nat
getNumberOfProcessors (void)
{
    static nat nproc = 0;

    if (nproc == 0) {
        SYSTEM_INFO si;
        GetSystemInfo(&si);
        nproc = si.dwNumberOfProcessors;
    }

    return nproc;
}

void
setThreadAffinity (nat n, nat m) // cap N of M
{
    HANDLE hThread;
    DWORD_PTR mask, r;  // 64-bit win is required to handle more than 32 procs
    nat nproc, i;

    hThread = GetCurrentThread();

    nproc = getNumberOfProcessors();

    mask = 0;
    for (i = n; i < nproc; i+=m) {
        mask |= 1 << i;
    }

    r = SetThreadAffinityMask(hThread, mask);
    if (r == 0) {
        sysErrorBelch("SetThreadAffinity");
        stg_exit(EXIT_FAILURE);
    }
}

typedef BOOL (WINAPI *PCSIO)(HANDLE);

void
interruptOSThread (OSThreadId id)
{
    HANDLE hdl;
    PCSIO pCSIO;
    if (!(hdl = OpenThread(THREAD_TERMINATE,FALSE,id))) {
        sysErrorBelch("interruptOSThread: OpenThread");
        stg_exit(EXIT_FAILURE);
    }
    pCSIO = (PCSIO) GetProcAddress(GetModuleHandle(TEXT("Kernel32.dll")), "CancelSynchronousIo");
    if ( NULL != pCSIO ) {
        pCSIO(hdl);
    } else {
        // Nothing to do, unfortunately
    }
    CloseHandle(hdl);
}

#else /* !defined(THREADED_RTS) */

int
forkOS_createThread ( HsStablePtr entry STG_UNUSED )
{
    return -1;
}

nat getNumberOfProcessors (void)
{
    return 1;
}

#endif /* !defined(THREADED_RTS) */

KernelThreadId kernelThreadId (void)
{
    DWORD tid = GetCurrentThreadId();
    return tid;
}
