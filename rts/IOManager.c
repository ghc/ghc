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
#include "RtsFlags.h"
#include "RtsUtils.h"

#if !defined(mingw32_HOST_OS) && defined(HAVE_SIGNAL_H)
#include "posix/Signals.h"
#endif

#if defined(mingw32_HOST_OS)
#include "win32/ThrIOManager.h"
#include "win32/AsyncMIO.h"
#include "win32/AsyncWinIO.h"
#endif

#include <string.h>


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
 */
static void selectIOManager(void)
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


/* Allocate and initialise the per-capability CapIOManager that lives in each
 * Capability. Called early in the RTS initialisation.
 */
void initCapabilityIOManager(CapIOManager **piomgr)
{
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

    *piomgr = iomgr;
}


/* Called late in the RTS initialisation
 */
void
initIOManager(void)
{
    selectIOManager();

    switch (iomgr_type) {

        /* The IO_MANAGER_SELECT needs no initialisation */

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
#if defined (THREADED_RTS)
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
initIOManagerAfterFork(Capability **pcap USED_IF_THREADS_AND_NOT_MINGW32)
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
exitIOManager(bool wait_threads USED_IF_NOT_THREADS_AND_MINGW32)
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

void markCapabilityIOManager(evac_fn       evac  USED_IF_NOT_THREADS,
                             void         *user  USED_IF_NOT_THREADS,
                             CapIOManager *iomgr USED_IF_NOT_THREADS)
{

    switch (iomgr_type) {
#if defined(IOMGR_ENABLED_SELECT)
        case IO_MANAGER_SELECT:
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_hd);
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_tl);
            evac(user, (StgClosure **)(void *)&iomgr->sleeping_queue);
            break;
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
        case IO_MANAGER_WIN32_LEGACY:
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_hd);
            evac(user, (StgClosure **)(void *)&iomgr->blocked_queue_tl);
            break;
#endif
        default:
            break;
    }

}


/* Declared in rts/IOInterface.h. Used only by the MIO threaded I/O manager on
 * Unix platforms.
 */
#if !defined(mingw32_HOST_OS)
void
setIOManagerControlFd(uint32_t cap_no USED_IF_THREADS, int fd USED_IF_THREADS) {
#if defined(THREADED_RTS)
    if (cap_no < getNumCapabilities()) {
        RELAXED_STORE(&getCapability(cap_no)->iomgr->control_fd, fd);
    } else {
        errorBelch("warning: setIOManagerControlFd called with illegal capability number.");
    }
#endif
}
#endif

#if !defined(THREADED_RTS)
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

void insertIntoSleepingQueue(Capability *cap, StgTSO *tso, LowResTime target)
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
