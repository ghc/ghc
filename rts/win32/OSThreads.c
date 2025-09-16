/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner.
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include <windows.h>
#include "sm/OSMem.h"
#if defined(THREADED_RTS)
#include "RtsUtils.h"

/* For reasons not yet clear, the entire contents of process.h is protected
 * by __STRICT_ANSI__ not being defined.
 */
#undef __STRICT_ANSI__
#include <process.h>


/* Processor group info cache.  */
static uint8_t* cpuGroupCache = NULL;
/* Processor group cumulative summary cache.  */
static uint32_t* cpuGroupCumulativeCache = NULL;
/* Processor group dist cache.  */
static uint8_t* cpuGroupDistCache = NULL;

void
yieldThread(void)
{
  SwitchToThread();
  return;
}

void
shutdownThread(void)
{
    ExitThread(0);
    barf("ExitThread() returned"); // avoid gcc warning
}

int
createOSThread (OSThreadId* pId, const char *name STG_UNUSED,
                OSThreadProc *startProc, void *param)
{
    HANDLE h;
    h = CreateThread ( NULL,  /* default security attributes */
                       0,
                       (LPTHREAD_START_ROUTINE)(void*)startProc,
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
osThreadId(void)
{
  return GetCurrentThreadId();
}

bool
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

static unsigned
forkOS_createThreadWrapper ( void * entry )
{
    Capability *cap;
    cap = rts_lock();
    rts_evalStableIO(&cap, (HsStablePtr) entry, NULL);
    rts_unlock(cap);
    rts_done();
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

#if defined(x86_64_HOST_ARCH)

#if !defined(ALL_PROCESSOR_GROUPS)
#define ALL_PROCESSOR_GROUPS 0xffff
#endif
#endif

void freeThreadingResources (void)
{
    if (cpuGroupCache)
    {
        stgFree(cpuGroupCache);
        cpuGroupCache = NULL;
    }

    if (cpuGroupCumulativeCache)
    {
        stgFree(cpuGroupCumulativeCache);
        cpuGroupCumulativeCache = NULL;
    }

    if (cpuGroupDistCache)
    {
        stgFree(cpuGroupDistCache);
        cpuGroupDistCache = NULL;
    }
}

/* Processor groups are not guaranteed to be uniformly distributed
   nor guaranteed to be filled before a next group is needed.
   The OS will assign processors to groups based on physical proximity
   and will never partially assign cores from one physical cpu to more
   than one group. If one has two 48 core CPUs then you'd end up with
   two groups of 48 logical cpus. Now add a 3rd CPU with 10 cores and
   the group it is assigned to depends where the socket is on the board.

   So we need to make a map of where the CPUs reside and how the groups are filled.
   Since groups are created at boot time by the kernel, we can cache this information.

   NOTE: This code does not support hot-swapping cores as it's caching the information.
   If you activate a new core you have to restart the program. This builds a
   simple lookup array for cpu -> group indexes. This gives O(1) lookup against
   O(n) space. But n is < 256 so we'll only use 256 bytes of extra memory. */

static uint8_t
getNumberOfProcessorsGroups (void)
{
    /* Group count cache.  */
    static uint8_t n_groups = 0;


#if defined(x86_64_HOST_ARCH)
    if (!n_groups)
    {
        n_groups = GetActiveProcessorGroupCount();

        IF_DEBUG(scheduler, debugBelch("[*] Number of processor groups detected: %u\n", n_groups));
    }
#endif

    if (!n_groups)
    {
        n_groups = 1;
    }

    return n_groups;
}

#if defined(x86_64_HOST_ARCH)
static uint8_t*
getProcessorsDistribution (void)
{
    if (cpuGroupDistCache)
    {
        return cpuGroupDistCache;
    }

    if (!cpuGroupDistCache)
    {
        uint8_t n_groups = getNumberOfProcessorsGroups();
        cpuGroupDistCache = stgMallocBytes(n_groups * sizeof(uint8_t), "getProcessorsDistribution");
        memset(cpuGroupDistCache, MAXIMUM_PROCESSORS, n_groups * sizeof(uint8_t));

        for (int i = 0; i < n_groups; i++)
        {
            cpuGroupDistCache[i] = GetActiveProcessorCount(i);
            IF_DEBUG(scheduler, debugBelch("[*] Number of active processors in group %u detected: %u\n", i, cpuGroupDistCache[i]));
        }
    }

    return cpuGroupDistCache;
}
#endif

static uint32_t*
getProcessorsCumulativeSum(void)
{
    if (cpuGroupCumulativeCache)
    {
        return cpuGroupCumulativeCache;
    }

    if (!cpuGroupCumulativeCache)
    {
        uint8_t n_groups = getNumberOfProcessorsGroups();
        cpuGroupCumulativeCache = stgMallocBytes(n_groups * sizeof(uint32_t), "getProcessorsCumulativeSum");
        memset(cpuGroupCumulativeCache, 0, n_groups * sizeof(uint32_t));

#if defined(x86_64_HOST_ARCH)
        uint8_t* proc_dist = getProcessorsDistribution();
        uint32_t cum_num_proc = 0;
        for (int i = 0; i < n_groups; i++)
        {
            cpuGroupCumulativeCache[i] = cum_num_proc;
            cum_num_proc += proc_dist[i];
            IF_DEBUG(scheduler, debugBelch("[*] Cumulative active processors for group %u: %u\n", i, cpuGroupCumulativeCache[i]));
        }
#endif
    }

    return cpuGroupCumulativeCache;
}

/*
 Because processors can be distributed rather unpredictably inside
 processor groups, we need to keep track of which processors are in
 which group to be able to determine which mask to set and which bit
 in the mask to set.

 This can either be done by the typical trade-off: speed or
 memory usage. In this case I prioritize speed.

 This function will generate an array where each index is a processor
 and the value of the array the group it belongs to. This allows us to
 in constant time determine where a processor is.
 */
static uint8_t*
createProcessorGroupMap (void)
{
    if (cpuGroupCache)
    {
        return cpuGroupCache;
    }

    uint32_t numProcs = getNumberOfProcessors();

    cpuGroupCache = stgMallocBytes(numProcs * sizeof(uint8_t), "createProcessorGroupMap");
    /* For 32bit Windows and 64bit older than Windows 7, create a default mapping. */
    memset(cpuGroupCache, 0, numProcs * sizeof(uint8_t));

#if defined(x86_64_HOST_ARCH)
    uint8_t* proc_dist = getProcessorsDistribution();

    int totalProcs = 0;
    uint8_t nGroups = getNumberOfProcessorsGroups();
    int group;
    for (group = 0; group < nGroups; group++)
    {
        uint8_t nProc = proc_dist[group];
        memset(cpuGroupCache + totalProcs, group, nProc * sizeof(uint8_t));
        totalProcs += nProc;
    }

    IF_DEBUG(scheduler, debugBelch("[*] Processor group map created\n"));
#endif

    return cpuGroupCache;
}

uint32_t
getNumberOfProcessors (void)
{
    static uint32_t nproc = 0;

#if defined(x86_64_HOST_ARCH)
    if (!nproc)
    {
        nproc = GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);

        if (nproc)
        {
            IF_DEBUG(scheduler, debugBelch("[*] Total number of active "
                                           "processors detected: %u\n", nproc));
            return nproc;
        }

        IF_DEBUG(scheduler, debugBelch("Could not determine Max number of "
                                       "logical processors.\n"
                                       "Falling back to old code which limits "
                                       "to 64 logical processors.\n"));
    }
#endif

    /* This will return the maximum number of processes
       within one processor group. It's also slower
       so use it only when needed.  */
    if (nproc == 0) {
        SYSTEM_INFO si;
        GetSystemInfo(&si);
        nproc = si.dwNumberOfProcessors;
    }

    return nproc;
}

void
setThreadAffinity (uint32_t n, uint32_t m) // cap N of M
{
    ASSERT(n <= m);

    HANDLE hThread;
    DWORD_PTR *mask, r;  // 64-bit win is required to handle more than 32 procs
                         // and Windows 7+ required for more than 64 procs
    uint32_t n_proc, i, ix;
    uint8_t* proc_map      = createProcessorGroupMap();
    uint32_t n_groups      = getNumberOfProcessorsGroups();
    uint32_t* proc_cum     = getProcessorsCumulativeSum();
    n_proc                 = getNumberOfProcessors();
    hThread                = GetCurrentThread();

    ASSERT(proc_map         );
    ASSERT(proc_cum         );
    ASSERT(hThread          );
    ASSERT(n_groups      > 0);
    ASSERT(n_proc        > 0);

    mask = stgMallocBytes(n_groups * sizeof(DWORD_PTR), "setThreadAffinity");
    memset(mask, 0, n_groups * sizeof(DWORD_PTR));

    /* The mask for the individual groups are all 0 based
       so we need different masks for every group.  */
    int group;
    for (i = n; i < n_proc; i += m)
    {
        group = proc_map[i];
        ix = i - proc_cum[group];
        mask[group] |= 1 << ix;
    }

    for (i = 0; i < n_groups; i++)
    {
#if defined(x86_64_HOST_ARCH)
        if (mask[i] > 0)
        {
            GROUP_AFFINITY hGroup;
            ZeroMemory(&hGroup, sizeof(hGroup));
            hGroup.Mask = mask[i];
            hGroup.Group = i;

            if (!SetThreadGroupAffinity(hThread, &hGroup, NULL))
            {
                sysErrorBelch("SetThreadGroupAffinity");
            }

            continue;
        }
#endif
        // Fall-back methods. Only do it if there's a mask to set
        if (mask[i] > 0)
        {
            r = SetThreadAffinityMask(hThread, mask[i]);
            if (r == 0) {
                stgFree(mask);
                sysErrorBelch("SetThreadAffinity");
                stg_exit(EXIT_FAILURE);
            }
        }
    }

    stgFree(mask);
}

void
interruptOSThread (OSThreadId id)
{
    HANDLE hdl;
    if (!(hdl = OpenThread(THREAD_TERMINATE,FALSE,id))) {
        sysErrorBelch("interruptOSThread: OpenThread");
        stg_exit(EXIT_FAILURE);
    }
    CancelSynchronousIo(hdl);
    CloseHandle(hdl);
}

void
joinOSThread (OSThreadId id)
{
    HANDLE hdl;
    if (!(hdl = OpenThread(SYNCHRONIZE,FALSE,id))) {
        sysErrorBelch("interruptOSThread: OpenThread");
        stg_exit(EXIT_FAILURE);
    }
    int ret = WaitForSingleObject(hdl, INFINITE);
    if (ret != WAIT_OBJECT_0) {
        sysErrorBelch("joinOSThread: error %d", ret);
    }
}

void setThreadNode (uint32_t node)
{
    if (osNumaAvailable())
    {
        uint64_t mask = 0;
        if (!GetNumaNodeProcessorMask(node, &mask) && !SetThreadAffinityMask(GetCurrentThread(), mask))
        {
            sysErrorBelch(
                "setThreadNode: Error setting affinity of thread to NUMA node `%u': %lu.",
                node, GetLastError());
            stg_exit(EXIT_FAILURE);
        }
    }
}

void releaseThreadNode (void)
{
    if (osNumaAvailable())
    {
        PDWORD_PTR processMask = NULL;
        PDWORD_PTR systemMask = NULL;
        if (!GetProcessAffinityMask(GetCurrentProcess(),
                                    processMask,
                                    systemMask))
        {
            sysErrorBelch(
                "releaseThreadNode: Error resetting affinity of thread: %lu",
                GetLastError());
            stg_exit(EXIT_FAILURE);
        }

        if (!SetThreadAffinityMask(GetCurrentThread(), *processMask))
        {
            sysErrorBelch(
                "releaseThreadNode: Error reseting NUMA affinity mask of thread: %lu.",
                GetLastError());
            stg_exit(EXIT_FAILURE);
        }

    }
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

#endif /* !defined(THREADED_RTS) */

KernelThreadId kernelThreadId (void)
{
    DWORD tid = GetCurrentThreadId();
    return tid;
}

/* Win32 threads and synchronisation objects */

/* A Condition is represented by a Win32 Conditional variable which is a
 * user-mode object and so incurs no context switching overhead.
 * a Mutex by a Mutex kernel object.
 */

void
initCondition( Condition* pCond )
{
  InitializeConditionVariable(pCond);
  return;
}

void
closeCondition( Condition* pCond STG_UNUSED)
{
  return;
}

void
broadcastCondition ( Condition* pCond )
{
  WakeAllConditionVariable(pCond);
}

void
signalCondition ( Condition* pCond )
{
  WakeConditionVariable(pCond);
}

void
waitCondition ( Condition* pCond, Mutex* pMut )
{
  CHECK(SleepConditionVariableSRW(pCond, pMut, INFINITE, 0));
}

bool
timedWaitCondition ( Condition* pCond, Mutex* pMut, Time timeout )
{
  // If we pass a timeout of 0 SleepConditionVariableSRW will return immediately
  // https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleepconditionvariablesrw
  DWORD ms = (DWORD)stg_min(1, TimeToMS(timeout));
  BOOL res = SleepConditionVariableSRW(pCond, pMut, ms, 0);
  if (res) {
    return true; // success
  } else if (GetLastError() == ERROR_TIMEOUT) {
    return false; // timeout
  } else {
    barf("timedWaitCondition: error %" FMT_Word, (StgWord) GetLastError());
  }
}

void
initMutex (Mutex* pMut)
{
  InitializeSRWLock(pMut);
}
void
closeMutex (Mutex* pMut)
{
  (void)pMut;
}
