/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Hooks for the I/O subsystem(s) that are called from other parts of the RTS.
 *
 * There are several different I/O subsystem implementations, for different
 * platforms (notably Windows vs others), and for the threaded vs non-threaded
 * RTS. These implementations all need hooks into other parts of the RTS, such
 * as startup/shutdown, the scheduler and other special features.
 *
 * To keep things comprehensible, all the hooks used by all the different I/O
 * subsystem implementations are centralised here. Not all implementations use
 * all hooks.
 *
 * -------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "HsFFI.h"

#include "IO.h"
#include "Capability.h"
#include "Schedule.h"

/* Some things declared in rts/IOManager.h are defined here */
#include "rts/IOManager.h"

#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
#include "win32/AsyncMIO.h"
#include "win32/AsyncWinIO.h"
#endif

/* Called in the RTS initialisation
 */
void
initIOManager(void)
{

#if defined(THREADED_RTS)
    /* Posix implementation in posix/Signals.c
     * Win32 implementation in win32/ThrIOManager.c
     */
    ioManagerStart();

#elif defined(mingw32_HOST_OS)
    /* Non-threaded Win32 implementation, either the WinIO IOCP implementation,
     * or the classic implementation. */
    if (is_io_mng_native_p()) {
        startupAsyncWinIO();
    } else {
        startupAsyncIO();
    }

#else
    /* The other implementation is the non-threaded Posix select() one.
     * It does not need any initialisation.
     */
#endif

}

/* Called in the RTS shutdown before the scheduler exits
 */
void
stopIOManager(void)
{

#if defined(THREADED_RTS)
    /* Posix implementation in posix/Signals.c
     * Win32 implementation in win32/ThrIOManager.c
     */
    ioManagerDie();
#endif

}

/* Called in the RTS shutdown after the scheduler exits
 */
void
exitIOManager(bool wait_threads USED_IF_NOT_THREADS_AND_MINGW32)
{

#if !defined(THREADED_RTS) && defined(mingw32_HOST_OS)
    if (is_io_mng_native_p()) {
        shutdownAsyncWinIO(wait_threads);
    } else {
        shutdownAsyncIO(wait_threads);
    }
#endif

}

void initCapabilityIOManager(CapIOManager *iomgr USED_IF_THREADS_AND_NOT_MINGW32)
{

#if defined(THREADED_RTS)
#if !defined(mingw32_HOST_OS)
    iomgr->control_fd = -1;
#endif
#else // !defined(THREADED_RTS)
    iomgr->blocked_queue_hd = END_TSO_QUEUE;
    iomgr->blocked_queue_tl = END_TSO_QUEUE;
    iomgr->sleeping_queue   = END_TSO_QUEUE;
#endif

}

void markCapabilityIOManager(evac_fn       evac  USED_IF_NOT_THREADS,
                             void         *user  USED_IF_NOT_THREADS,
                             CapIOManager *iomgr USED_IF_NOT_THREADS)
{

#if !defined(THREADED_RTS)
    evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_hd);
    evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_tl);
    evac(user, (StgClosure **)(void *)&iomgr->sleeping_queue);
#endif

}

/* Declared in rts/IOManager.h. Used only by the MIO threaded I/O manager on
 * Unix platforms.
 */
#if !defined(mingw32_HOST_OS)
void
setIOManagerControlFd(uint32_t cap_no USED_IF_THREADS, int fd USED_IF_THREADS) {
#if defined(THREADED_RTS)
    if (cap_no < n_capabilities) {
        RELAXED_STORE(&capabilities[cap_no]->iomgr.control_fd, fd);
    } else {
        errorBelch("warning: setIOManagerControlFd called with illegal capability number.");
    }
#endif
}
#endif

#if !defined(THREADED_RTS)
void appendToIOBlockedQueue(Capability *cap, StgTSO *tso)
{
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (cap->iomgr.blocked_queue_hd == END_TSO_QUEUE) {
        cap->iomgr.blocked_queue_hd = tso;
    } else {
        setTSOLink(cap, cap->iomgr.blocked_queue_tl, tso);
    }
    cap->iomgr.blocked_queue_tl = tso;
}

void insertIntoSleepingQueue(Capability *cap, StgTSO *tso, LowResTime target)
{
    StgTSO *prev = NULL;
    StgTSO *t = cap->iomgr.sleeping_queue;
    while (t != END_TSO_QUEUE && t->block_info.target < target) {
        prev = t;
        t = t->_link;
    }

    tso->_link = t;
    if (prev == NULL) {
        cap->iomgr.sleeping_queue = tso;
    } else {
        setTSOLink(cap, prev, tso);
    }
}
#endif
