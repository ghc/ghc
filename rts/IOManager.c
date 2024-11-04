/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Hooks for the I/O subsystem(s) that are called from other parts of the RTS.
 *
 * There are several different I/O subsystem implementations (aka I/O managers),
 * for different platforms (notably Windows vs others), and for the threaded vs
 * non-threaded RTS. These implementations all need hooks into other parts of
 * the RTS, such as startup/shutdown, the scheduler and other special features.
 *
 * To keep things comprehensible, all the hooks used by all the different I/O
 * subsystem implementations are centralised here. Not all implementations use
 * all hooks.
 *
 * -------------------------------------------------------------------------*/

#include "Rts.h"
#include "rts/IOInterface.h" // exported
#include "IOManager.h"       // RTS internal
#include "Capability.h"
#include "Schedule.h"
#include "Prelude.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "sm/Evac.h"

#include "IOManagerInternals.h"

#if defined(IOMGR_ENABLED_SELECT)
#include "Threads.h"
#include "posix/Select.h"
#include "posix/Signals.h"
#endif

#if defined(IOMGR_ENABLED_MIO_POSIX)
#include "posix/Signals.h"
#include "Prelude.h"
#endif

#if defined(IOMGR_ENABLED_MIO_WIN32)
#include "win32/ThrIOManager.h"
#include "win32/AsyncMIO.h"
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
#include "Threads.h"
#include "win32/AsyncMIO.h"
#include "win32/AwaitEvent.h"
#include "win32/MIOManager.h"
#endif

#if defined(IOMGR_ENABLED_WINIO)
#include "win32/ThrIOManager.h"
#include "win32/AsyncWinIO.h"
#if !defined(THREADED_RTS)
#include "win32/AwaitEvent.h"
#endif
#endif

#include <string.h>


/* We have lots of functions below with conditional implementations for
 * different I/O managers. Some functions, for some I/O managers, naturally
 * have implementations that do nothing or barf. When only one such I/O
 * manager is enabled then the whole function implementation will have an
 * implementation that does nothing or barfs. This then results in warnings
 * from gcc that parameters are unused, or that the function should be marked
 * with attribute noreturn (since barf does not return). The USED_IF_THREADS
 * trick for fine-grained warning supression is fine for just two cases, but an
 * equivalent here would need USED_IF_THE_ONLY_ENABLED_IOMGR_IS_X_OR_Y which
 * would have combinitorial blowup. So we take a coarse grained approach and
 * simply disable these two warnings for the whole file.
 */
#pragma GCC diagnostic push
#if __GNUC__ >= 5
#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"
#endif
#pragma GCC diagnostic ignored "-Wmissing-noreturn"
#pragma GCC diagnostic ignored "-Wunused-parameter"


/* Global var to tell us which I/O manager impl we are using */
IOManagerType iomgr_type;

#if defined(mingw32_HOST_OS)
/* Global var (only on Windows) that is exported to be shared with the I/O code
 * in the base library to tell us which style of I/O manager we are using: one
 * that uses the Windows native API HANDLEs, or one that uses Posix style fds.
 */
bool rts_IOManagerIsWin32Native = false;
#endif

enum IOManagerAvailability
parseIOManagerFlag(const char *iomgrstr, IO_MANAGER_FLAG *flag)
{
    if (strcmp("select", iomgrstr) == 0) {
#if defined(IOMGR_ENABLED_SELECT)
        *flag = IO_MNGR_FLAG_SELECT;
        return IOManagerAvailable;
#else
        return IOManagerUnavailable;
#endif
    }
    else if (strcmp("mio", iomgrstr) == 0) {
#if defined(IOMGR_ENABLED_MIO_POSIX) || defined(IOMGR_ENABLED_MIO_WIN32)
        *flag = IO_MNGR_FLAG_MIO;
        return IOManagerAvailable;
#else
        return IOManagerUnavailable;
#endif
        *flag = IO_MNGR_FLAG_MIO;
    }
    else if (strcmp("winio", iomgrstr) == 0) {
#if defined(IOMGR_ENABLED_WINIO)
        *flag = IO_MNGR_FLAG_WINIO;
        return IOManagerAvailable;
#else
        return IOManagerUnavailable;
#endif
    }
    else if (strcmp("win32-legacy", iomgrstr) == 0) {
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        *flag = IO_MNGR_FLAG_WIN32_LEGACY;
        return IOManagerAvailable;
#else
        return IOManagerUnavailable;
#endif
    }
    else if (strcmp("auto", iomgrstr) == 0) {
        *flag = IO_MNGR_FLAG_AUTO;
        return IOManagerAvailable;
    }
    /* Two deprecated aliases. These aliases only had any effect on Windows,
     * but were available as RTS flags on all platforms. The "native" flag
     * refers to the newer Windows WinIO IO manager (threaded or non-threaded),
     * while (somewhat confusingly) the "posix" flag refers to the older
     * Windows I/O managers (win32-legacy and mio). On non-Windows, we now make
     * these flags equivalent to IO_MNGR_FLAG_AUTO.
     */
    else if (strcmp("native", iomgrstr) == 0) {
#if defined(mingw32_HOST_OS)
    /* On windows "native" is now an alias for IO_MNGR_FLAG_WINIO */
#if defined(IOMGR_ENABLED_WINIO)
        *flag = IO_MNGR_FLAG_WINIO;
        return IOManagerAvailable;
#else
        return IOManagerUnavailable;
#endif
#else // !defined(mingw32_HOST_OS)
        *flag = IO_MNGR_FLAG_AUTO;
        return IOManagerAvailable;
#endif
    }
    else if (strcmp("posix", iomgrstr) == 0) {
#if defined(mingw32_HOST_OS)
        /* On Windows "posix" is now an alias for either IO_MNGR_FLAG_MIO or
         * IO_MNGR_FLAG_WIN32_LEGACY */
#if defined(IOMGR_ENABLED_MIO_WIN32)
        *flag = IO_MNGR_FLAG_MIO;
        return IOManagerAvailable;
#elif defined(IOMGR_ENABLED_WIN32_LEGACY)
        *flag = IO_MNGR_FLAG_WIN32_LEGACY;
        return IOManagerAvailable;
#else
        return IOManagerUnavailable;
#endif
#else // !defined(mingw32_HOST_OS)
        *flag = IO_MNGR_FLAG_AUTO;
        return IOManagerAvailable;
#endif
    }
    else {
        return IOManagerUnrecognised;
    }
}

/* Based on the I/O manager RTS flag, select an I/O manager to use.
 *
 * This fills in the iomgr_type and rts_IOManagerIsWin32Native globals.
 * Must be called before the I/O manager is started.
 *
 * Called early in the RTS initialisation, after the RTS flags have been
 * processed.
 */
void selectIOManager(void)
{
    switch (RtsFlags.MiscFlags.ioManager) {
        case IO_MNGR_FLAG_AUTO:
#if defined(THREADED_RTS)
#if   defined(IOMGR_DEFAULT_THREADED_MIO)
#if defined(mingw32_HOST_OS)
            iomgr_type = IO_MANAGER_MIO_WIN32;
#else
            iomgr_type = IO_MANAGER_MIO_POSIX;
#endif
#elif defined(IOMGR_DEFAULT_THREADED_WINIO)
            iomgr_type = IO_MANAGER_WINIO;
#else
#error No I/O default manager. See IOMGR_DEFAULT_THREADED_ flags
#endif
#else // !defined(THREADED_RTS)
#if   defined(IOMGR_DEFAULT_NON_THREADED_SELECT)
            iomgr_type = IO_MANAGER_SELECT;
#elif defined(IOMGR_DEFAULT_NON_THREADED_WINIO)
            iomgr_type = IO_MANAGER_WINIO;
#elif defined(IOMGR_DEFAULT_NON_THREADED_WIN32_LEGACY)
            iomgr_type = IO_MANAGER_WIN32_LEGACY;
#else
#error No I/O default manager. See IOMGR_DEFAULT_NON_THREADED_ flags
#endif
#endif
            break;

#if defined(IOMGR_ENABLED_SELECT)
        case IO_MNGR_FLAG_SELECT:
            iomgr_type = IO_MANAGER_SELECT;
            break;
#endif

#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MNGR_FLAG_MIO:
            iomgr_type = IO_MANAGER_MIO_POSIX;
            break;
#endif

#if defined(IOMGR_ENABLED_MIO_WIN32)
        case IO_MNGR_FLAG_MIO:
            iomgr_type = IO_MANAGER_MIO_WIN32;
            break;
#endif

#if defined(IOMGR_ENABLED_WINIO)
        case IO_MNGR_FLAG_WINIO:
            iomgr_type = IO_MANAGER_WINIO;
            rts_IOManagerIsWin32Native = true;
            break;
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MNGR_FLAG_WIN32_LEGACY:
            iomgr_type = IO_MANAGER_WIN32_LEGACY;
            break;
#endif

        default:
          barf("selectIOManager: %d", RtsFlags.MiscFlags.ioManager);
    }
}


char * showIOManager(void)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
            return "select";
#endif
#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            return "mio";
#endif
#if defined(IOMGR_ENABLED_MIO_WIN32)
        case IO_MANAGER_MIO_WIN32:
            return "mio";
#endif
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
            return "winio";
#endif
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            return "win32-legacy";
#endif
        default:
          barf("showIOManager: %d", iomgr_type);
    }
}


/* Allocate and initialise the per-capability CapIOManager that lives in each
 * Capability. Called from initCapability(), which is done in the RTS startup
 * in initCapabilities(), and later at runtime via setNumCapabilities().
 *
 * Note that during RTS startup this is called _before_ the storage manager
 * is initialised, so this is not allowed to allocate on the GC heap.
 */
void initCapabilityIOManager(Capability *cap)
{
    debugTrace(DEBUG_iomanager, "initialising I/O manager %s for cap %d",
               showIOManager(), cap->no);

    CapIOManager *iomgr =
      (CapIOManager *) stgMallocBytes(sizeof(CapIOManager),
                                      "initCapabilityIOManager");

    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
            iomgr->blocked_queue_hd = END_TSO_QUEUE;
            iomgr->blocked_queue_tl = END_TSO_QUEUE;
            iomgr->sleeping_queue   = END_TSO_QUEUE;
            break;
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            iomgr->blocked_queue_hd = END_TSO_QUEUE;
            iomgr->blocked_queue_tl = END_TSO_QUEUE;
            break;
#endif

#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            iomgr->control_fd = -1;
            break;
#endif
        default:
            break;
    }

    cap->iomgr = iomgr;
}


/* Called late in the RTS initialisation
 */
void initIOManager(void)
{
    debugTrace(DEBUG_iomanager, "initialising %s I/O manager", showIOManager());

    switch (iomgr_type) {

#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
            /* Make the exception CAF a GC root. See initBuiltinGcRoots for
             * similar examples. We throw this exception if a thread tries to
             * wait on an invalid FD.
             */
            getStablePtr((StgPtr)blockedOnBadFD_closure);
            break;
#endif
#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            /* Posix implementation in posix/Signals.c
             * TODO: rename ioManagerStart to be less generic, impl specific
             */
            ioManagerStart();
            break;
#endif
#if defined(IOMGR_ENABLED_MIO_WIN32)
        case IO_MANAGER_MIO_WIN32:
            /* Win32 implementation in win32/ThrIOManager.c
             * TODO: rename ioManagerStart to be less generic, impl specific
             */
            ioManagerStart();
            break;
#endif
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
            /* The WinIO I/O manager sadly has two implementations of this
             * startup, depending on the threading mode.
             * TODO: rationalise this into one entry point, that internally
             * can do different things in the two cases.
             */
#if defined(THREADED_RTS)
            /* Win32 implementation in win32/ThrIOManager.c
             */
            ioManagerStart();
#else
            startupAsyncWinIO();
#endif
            break;
#endif
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            startupAsyncIO();
            break;
#endif
        default:
            break;
    }
}

/* Called from forkProcess in the child process on the surviving capability.
 */
void
initIOManagerAfterFork(Capability **pcap)
{

    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            /* Posix implementation in posix/Signals.c
             *
             * TODO: figure out if it is necessary for the threaded MIO case
             * for the starting of the IO manager threads to be synchronous.
             * It would be simpler if it could start them asynchronously and
             * thus not have to have the pcap as an inout param, that could be
             * modified. In practice it cannot be modified anyway since in the
             * contexts where it is called (forkProcess), there is only a
             * single cap available.
             */
            ioManagerStartCap(pcap);
            break;
#endif
        /* The IO_MANAGER_SELECT needs no initialisation */

        /* No impl for any of the Windows I/O managers, since no forking. */
        default:
            break;
    }
}


/* Called from setNumCapabilities.
 */
void notifyIOManagerCapabilitiesChanged(Capability **pcap)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            rts_evalIO(pcap, ioManagerCapabilitiesChanged_closure, NULL);
            break;
#endif
        default:
            break;
    }
}


/* Called in the RTS shutdown before the scheduler exits
 */
void
stopIOManager(void)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            /* Posix implementation in posix/Signals.c
             * TODO: rename ioManagerDie to be less generic, impl specific
             */
            ioManagerDie();
            break;
#endif
#if defined(IOMGR_ENABLED_MIO_WIN32) || defined(IOMGR_ENABLED_WINIO)
#if defined(IOMGR_ENABLED_MIO_WIN32)
        case IO_MANAGER_MIO_WIN32:
#endif
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
#endif
            /* Win32 implementation in win32/ThrIOManager.c
             * This impl is currently shared by both MIO and WinIO I/O managers
             * TODO: rename ioManagerDie to be less generic, impl specific
             */
            ioManagerDie();
            break;
#endif
        default:
            break;
    }
}


/* Called in the RTS shutdown after the scheduler exits
 */
void
exitIOManager(bool wait_threads)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
            shutdownAsyncWinIO(wait_threads);
            break;
#endif
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            shutdownAsyncIO(wait_threads);
            break;
#endif
        default:
            break;
    }
}

/* Wakeup hook: called from the scheduler's wakeUpRts (currently only in
 * threaded mode).
 */
void wakeupIOManager(void)
{
    switch (iomgr_type) {

#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
            /* MIO Posix implementation in posix/Signals.c */
            ioManagerWakeup();
            break;
#endif
#if defined(IOMGR_ENABLED_MIO_WIN32)
        case IO_MANAGER_MIO_WIN32:
            /* MIO Windows implementation in win32/ThrIOManager.c
             * Yes, this is shared with the WinIO (threaded) impl.
             */
            ioManagerWakeup();
            break;
#endif
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
#if defined(THREADED_RTS)
            /* WinIO threaded implementation in win32/ThrIOManager.c
             * Yes, this is shared with the MIO win32 impl.
             */
            ioManagerWakeup();
#endif
            break;
#endif
        default:
            break;
    }
}

void markCapabilityIOManager(evac_fn evac, void *user, Capability *cap)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
        {
            CapIOManager *iomgr = cap->iomgr;
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_hd);
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_tl);
            evac(user, (StgClosure **)(void *)&iomgr->sleeping_queue);
            break;
        }
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
        {
            CapIOManager *iomgr = cap->iomgr;
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_hd);
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_tl);
            break;
        }
#endif
        default:
            break;
    }
}


void scavengeTSOIOManager(StgTSO *tso)
{
    switch (iomgr_type) {

            /* case IO_MANAGER_SELECT:
             * BlockedOn{Read,Write} uses block_info.fd
             * BlockedOnDelay        uses block_info.target
             * both of these are not GC pointers, so there is nothing to do.
             */

            /* case IO_MANAGER_WIN32_LEGACY:
             * BlockedOn{Read,Write,DoProc} uses block_info.async_result
             * The StgAsyncIOResult async_result is allocated on the C heap.
             * It'd probably be better if it used the GC heap. If it did we'd
             * scavenge it here.
             */

        default:
            /* All the other I/O managers do not use I/O-related why_blocked
             * reasons, so there are no cases to handle.
             */
            break;
    }
}


/* Declared in rts/IOInterface.h. Used only by the MIO threaded I/O manager on
 * Unix platforms.
 */
#if !defined(mingw32_HOST_OS)
void
setIOManagerControlFd(uint32_t cap_no, int fd) {
#if defined(THREADED_RTS)
    if (cap_no < getNumCapabilities()) {
        RELAXED_STORE(&getCapability(cap_no)->iomgr->control_fd, fd);
    } else {
        errorBelch("warning: setIOManagerControlFd called with illegal capability number.");
    }
#endif
}
#endif


bool anyPendingTimeoutsOrIO(Capability *cap)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
        {
            CapIOManager *iomgr = cap->iomgr;
            return (iomgr->blocked_queue_hd != END_TSO_QUEUE)
                || (iomgr->sleeping_queue   != END_TSO_QUEUE);
        }
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
        {
            CapIOManager *iomgr = cap->iomgr;
            return (iomgr->blocked_queue_hd != END_TSO_QUEUE);
        }
#endif

    /* For the purpose of the scheduler, the threaded I/O managers never have
       pending I/O or timers. Of course in reality they do, but they're
       managed via other primitives that the scheduler can see into (threads,
       MVars and foreign blocking calls).
     */
#if defined(IOMGR_ENABLED_MIO_POSIX)
        case IO_MANAGER_MIO_POSIX:
          return false;
#endif

#if defined(IOMGR_ENABLED_MIO_WIN32)
        case IO_MANAGER_MIO_WIN32:
          return false;
#endif

#if defined(IOMGR_ENABLED_WINIO)
#if defined(THREADED_RTS)
        /* As above, the threaded variants never have pending I/O or timers */
        case IO_MANAGER_WINIO:
          return false;
#else
        case IO_MANAGER_WINIO:
          return false;
        /* FIXME: But what is this? The WinIO I/O manager *also* returns false
           in the non-threaded case! This is *totally bogus*! In the
           non-threaded RTS the scheduler expects to be able to poll for IO.
           The fact that this gives a wrong and useless answer for WinIO is
           probably the cause of the complication in the scheduler with having
           to call awaitCompletedTimeoutsOrIO() in multiple places (on Windows,
           non-threaded).
         */
#endif
#endif
        default:
            barf("anyPendingTimeoutsOrIO not implemented");
    }
}


void pollCompletedTimeoutsOrIO(Capability *cap)
{
    debugTrace(DEBUG_iomanager, "polling for completed IO or timeouts");
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
          awaitCompletedTimeoutsOrIOSelect(cap, false);
          break;
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY) || \
   (defined(IOMGR_ENABLED_WINIO) && !defined(THREADED_RTS))
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
#endif
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
#endif
          awaitCompletedTimeoutsOrIOWin32(cap, false);
          break;
#endif
        default:
            barf("pollCompletedTimeoutsOrIO not implemented");
    }
}


void awaitCompletedTimeoutsOrIO(Capability *cap)
{
    debugTrace(DEBUG_iomanager, "waiting for completed IO or timeouts");
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
          awaitCompletedTimeoutsOrIOSelect(cap, true);
          break;
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY) || \
   (defined(IOMGR_ENABLED_WINIO) && !defined(THREADED_RTS))
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
#endif
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
#endif
          awaitCompletedTimeoutsOrIOWin32(cap, true);
          break;
#endif
        default:
            barf("pollCompletedTimeoutsOrIO not implemented");
    }
    ASSERT(!emptyRunQueue(cap) || getSchedState() != SCHED_RUNNING);
}


void syncIOWaitReady(Capability   *cap,
                     StgTSO       *tso,
                     IOReadOrWrite rw,
                     HsInt         fd)
{
    debugTrace(DEBUG_iomanager,
               "thread %ld waiting for %s I/O readiness on fd %d",
               (long) tso->id, rw == IORead ? "read" : "write", (int) fd);
    ASSERT(tso->why_blocked == NotBlocked);
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
        {
            StgWord why_blocked = rw == IORead ? BlockedOnRead : BlockedOnWrite;
            tso->block_info.fd = fd;
            RELEASE_STORE(&tso->why_blocked, why_blocked);
            appendToIOBlockedQueue(cap, tso);
            break;
        }
#endif
        default:
            barf("waitRead# / waitWrite# not available for current I/O manager");
    }
}


void syncIOCancel(Capability *cap, StgTSO *tso)
{
    debugTrace(DEBUG_iomanager, "cancelling I/O for thread %ld", (long) tso->id);
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
            removeThreadFromDeQueue(cap, &cap->iomgr->blocked_queue_hd,
                                         &cap->iomgr->blocked_queue_tl, tso);
            break;
#endif
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            removeThreadFromDeQueue(cap, &cap->iomgr->blocked_queue_hd,
                                         &cap->iomgr->blocked_queue_tl, tso);
            abandonWorkRequest(tso->block_info.async_result->reqID);
            break;
#endif
        default:
            barf("syncIOCancel not supported for I/O manager %d", iomgr_type);
    }
}


#if defined(IOMGR_ENABLED_SELECT)
static void insertIntoSleepingQueue(Capability *cap, StgTSO *tso, LowResTime target);
#endif


void syncDelay(Capability *cap, StgTSO *tso, HsInt us_delay)
{
    debugTrace(DEBUG_iomanager, "thread %ld waiting for %lld us", tso->id, us_delay);
    ASSERT(tso->why_blocked == NotBlocked);
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
        {
            LowResTime target = getDelayTarget(us_delay);
            tso->block_info.target = target;
            RELEASE_STORE(&tso->why_blocked, BlockedOnDelay);
            insertIntoSleepingQueue(cap, tso, target);
            break;
        }
#endif
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            /* It would be nice to allocate this on the heap instead as it
             * would make the primops more consistent.
             */
        {
            StgAsyncIOResult *ares = stgMallocBytes(sizeof(StgAsyncIOResult),
                                                    "syncDelay");
            ares->reqID   = addDelayRequest(us_delay);
            ares->len     = 0;
            ares->errCode = 0;
            tso->block_info.async_result = ares;

            /* Having all async-blocked threads reside on the blocked_queue
             * simplifies matters, so set the status to OnDoProc and put the
             * delayed thread on the blocked_queue.
             */
            RELEASE_STORE(&tso->why_blocked, BlockedOnDoProc);
            appendToIOBlockedQueue(cap, tso);
            break;
        }
#endif
        default:
            barf("syncDelay not supported for I/O manager %d", iomgr_type);
    }
}


void syncDelayCancel(Capability *cap, StgTSO *tso)
{
    debugTrace(DEBUG_iomanager, "cancelling delay for thread %ld", (long) tso->id);
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
            removeThreadFromQueue(cap, &cap->iomgr->sleeping_queue, tso);
            break;
#endif
        /* Note: no case for IO_MANAGER_WIN32_LEGACY despite it having a case
         * for syncDelay above. This is because the win32 legacy I/O manager
         * treats delay as an I/O operation, using the BlockedOnDoProc blocking
         * reason, rather than the BlockedOnDelay reason. As a consequence,
         * cancellation goes via syncIOCancel instead. Yes, it's a bit weird.
         */

        default:
            barf("syncDelayCancel not supported for I/O manager %d", iomgr_type);
    }
}


#if defined(IOMGR_ENABLED_SELECT) || defined(IOMGR_ENABLED_WIN32_LEGACY)
void appendToIOBlockedQueue(Capability *cap, StgTSO *tso)
{
    CapIOManager *iomgr = cap->iomgr;
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (iomgr->blocked_queue_hd == END_TSO_QUEUE) {
        iomgr->blocked_queue_hd = tso;
    } else {
        setTSOLink(cap, iomgr->blocked_queue_tl, tso);
    }
    iomgr->blocked_queue_tl = tso;
}
#endif

#if defined(IOMGR_ENABLED_SELECT)
/* Insert a thread into the queue of threads blocked on timers.
 *
 * This is used by the select() I/O manager implementation only.
 *
 * The sleeping queue is defined for other non-threaded I/O managers but not
 * used. This is a wart that should be excised.
 */
// TODO: move to Select.c and rename
static void insertIntoSleepingQueue(Capability *cap, StgTSO *tso, LowResTime target)
{
    CapIOManager *iomgr = cap->iomgr;
    StgTSO *prev = NULL;
    StgTSO *t = iomgr->sleeping_queue;
    while (t != END_TSO_QUEUE && t->block_info.target < target) {
        prev = t;
        t = t->_link;
    }

    tso->_link = t;
    if (prev == NULL) {
        iomgr->sleeping_queue = tso;
    } else {
        setTSOLink(cap, prev, tso);
    }
}
#endif

/* Temporary compat helper function used in the Win32 I/O managers.
 * TODO: replace by consulting the iomgr_type global instead.
 */
bool is_io_mng_native_p (void)
{
    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_WINIO)
        case IO_MANAGER_WINIO:
            return true;
#endif
        default:
            return false;
    }
}


/* See comment above with the #pragma GCC diagnostic push */
#pragma GCC diagnostic pop
