/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Prototypes for functions in IOManager.c and elsewhere
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

#pragma once

#include "BeginPrivate.h"

#include "sm/GC.h" // for evac_fn
#include "posix/Select.h" // for LowResTime TODO: switch to normal Time

/* The ./configure gives us a set of CPP flags, one for each named I/O manager:
 * IOMGR_BUILD_<name>                : which ones should be built (some)
 * IOMGR_DEFAULT_NON_THREADED_<name> : which one is default (exactly one)
 * IOMGR_DEFAULT_THREADED_<name>     : which one is default (exactly one)
 *
 * The IOMGR_BUILD_<name> flags just says that an I/O manager should be built
 * for _some_ RTS way (i.e. threaded or non-threaded). What we need however are
 * flags to use for conditional compilation of I/O manager code. These flags
 * must take into account whether the particular I/O manager is enabled for the
 * RTS way we're currently building, in particular taking into account if we're
 * building for a threaded or non-threaded RTS.
 *
 * So here we define a set of derived flags IOMGR_ENABLED_<name> which says if
 * each I/O manager is enabled in the RTS way we're building now. We'll then
 * use these flags everywhere else for conditional compilation.
 */

#if defined(IOMGR_BUILD_SELECT) && !defined(THREADED_RTS)
    #define IOMGR_ENABLED_SELECT
#endif
#if defined(IOMGR_BUILD_MIO) && defined(THREADED_RTS)
/* For MIO, it is really two separate I/O manager implementations: one for
 * Windows and one for non-Windows. This is clear from both the C code on the
 * RTS side and the Haskell code in the base library. By treating them as
 * such leads to simpler I/O manager dispatch code.
 *
 * These two implementations do share a common architecture, and so we still
 * use a single name in public interfaces like ./configure and the RTS flags.
 */
#if defined(mingw32_HOST_OS)
    #define IOMGR_ENABLED_MIO_WIN32
#else
    #define IOMGR_ENABLED_MIO_POSIX
#endif
#endif
#if defined(IOMGR_BUILD_WINIO)
    #define IOMGR_ENABLED_WINIO
#endif
#if defined(IOMGR_BUILD_WIN32_LEGACY) && !defined(THREADED_RTS)
    #define IOMGR_ENABLED_WIN32_LEGACY
#endif

/* To provide a string to use for output of +RTS -? we use the
 * IOMGR_DEFAULT_{NON_}THREADED_<name> flags to derived a CPP variable
 * IOMGR_DEFAULT_STR with the string name of the default I/O manager for the
 * _current_ RTS way. At the same time we can do a sanity check that there is
 * actually a default.
 */
#if defined(THREADED_RTS)
#if   defined(IOMGR_DEFAULT_THREADED_MIO)
    #define IOMGR_DEFAULT_STR "mio"
#elif defined(IOMGR_DEFAULT_THREADED_WINIO)
    #define IOMGR_DEFAULT_STR "winio"
#else
#error No I/O default manager. See IOMGR_DEFAULT_THREADED_ flags
#endif
#else // !defined(THREADED_RTS)
#if   defined(IOMGR_DEFAULT_NON_THREADED_SELECT)
    #define IOMGR_DEFAULT_STR "select"
#elif defined(IOMGR_DEFAULT_NON_THREADED_WINIO)
    #define IOMGR_DEFAULT_STR "winio"
#elif defined(IOMGR_DEFAULT_NON_THREADED_WIN32_LEGACY)
    #define IOMGR_DEFAULT_STR "win32-legacy"
#else
#error No I/O default manager. See IOMGR_DEFAULT_NON_THREADED_ flags
#endif
#endif

/* To help with error messages we provide a macro IOMGRS_ENABLED_STR that is
 * the stringy list of all enabled I/O managers (with leading and separating
 * spaces)
 */
#if defined(IOMGR_ENABLED_SELECT)
    #define IOMGR_ENABLED_STR_SELECT " select"
#else
    #define IOMGR_ENABLED_STR_SELECT ""
#endif
#if defined(IOMGR_ENABLED_MIO_POSIX) || defined(IOMGR_ENABLED_MIO_WIN32)
    #define IOMGR_ENABLED_STR_MIO " mio"
#else
    #define IOMGR_ENABLED_STR_MIO ""
#endif
#if defined(IOMGR_ENABLED_WINIO)
    #define IOMGR_ENABLED_STR_WINIO " winio"
#else
    #define IOMGR_ENABLED_STR_WINIO ""
#endif
#if defined(IOMGR_ENABLED_WIN32_LEGACY)
    #define IOMGR_ENABLED_STR_WIN32_LEGACY " win32-legacy"
#else
    #define IOMGR_ENABLED_STR_WIN32_LEGACY ""
#endif
#define IOMGRS_ENABLED_STR \
          IOMGR_ENABLED_STR_SELECT \
          IOMGR_ENABLED_STR_MIO \
          IOMGR_ENABLED_STR_WINIO \
          IOMGR_ENABLED_STR_WIN32_LEGACY


/* The per-capability data structures belonging to the I/O manager.
 *
 * It can be accessed as cap->iomgr.
 *
 * The content of the structure is defined conditionally so it is different for
 * each I/O manager implementation.
 *
 * TODO: once the content of this struct is genuinely private, and not shared
 * with other parts of the RTS, then it can be made opaque, so the content is
 * known only to the I/O manager and not the rest of the RTS.
 */
typedef struct {

#if defined(THREADED_RTS)
#if !defined(mingw32_HOST_OS)
    /* Control FD for the MIO manager for this capability */
    int control_fd;
#endif
#else // !defined(THREADED_RTS)
    /* Thread queue for threads blocked on I/O completion.
     * Used by the select() and Win32 MIO I/O managers. It is not used by
     * the WinIO I/O manager, though it remains defined in this case.
     */
    StgTSO *blocked_queue_hd;
    StgTSO *blocked_queue_tl;

    /* Thread queue for threads blocked on timeouts.
     * Used by the select() I/O manager only. It is grossly inefficient, like
     * everything else to do with the select() I/O manager.
     *
     * TODO: It is not used by any of the Windows I/O managers, though it
     * remains defined for them. This is an oddity that should be resolved.
     */
    StgTSO *sleeping_queue;
#endif

} CapIOManager;


/* Allocate and initialise the per-capability CapIOManager that lives in each
 * Capability. It is called from initCapability, via initScheduler,
 * via hs_init_ghc.
 */
void initCapabilityIOManager(CapIOManager **iomgr);


/* Init hook: called from hs_init_ghc, very late in the startup after almost
 * everything else is done.
 */
void initIOManager(void);

/* Init hook: called from forkProcess in the child process on the surviving
 * capability.
 *
 * Note that this is synchronous and can run Haskell code, so can change the
 * given cap.
 */
void initIOManagerAfterFork(/* inout */ Capability **pcap);

/* TODO: rationalise initIOManager and initIOManagerAfterFork into a single
         per-capability init function.
 */


/* Shutdown hooks: called from hs_exit_ before and after the scheduler exits.
 *
 * The stopIOManager is also called many times (once per-capability) within the
 * scheduler shutdown (but only in threaded mode). This is despite the fact that
 * stopIOManager shuts down the I/O manager for all capabilities.
 * FIXME: this is accidentally quadratic and confusing.
 */
void stopIOManager(void);
void exitIOManager(bool wait_threads);


/* Wakeup hook: called from the scheduler's wakeUpRts (currently only in
 * threaded mode).
 *
 * The I/O manager can be blocked waiting on I/O or timers. Sometimes there are
 * other external events where we need to wake up the I/O manager and return
 * to the schedulr.
 *
 * At the moment, all the non-threaded I/O managers will do this automagically
 * since a signal will interrupt any waiting system calls, so at the moment
 * the implementation for the non-threaded I/O managers does nothing.
 *
 * For the I/O managers in threaded mode, this arranges to unblock the I/O
 * manager if it waa blocked waiting.
 */
void wakeupIOManager(void);


/* GC hook: mark any per-capability GC roots the I/O manager uses.
 */
void markCapabilityIOManager(evac_fn evac, void *user, CapIOManager *iomgr);


#if !defined(THREADED_RTS)
/* Add a thread to the end of the queue of threads blocked on I/O.
 *
 * This is used by the select() and the Windows MIO non-threaded I/O manager
 * implementation.
 */
void appendToIOBlockedQueue(Capability *cap, StgTSO *tso);

/* Insert a thread into the queue of threads blocked on timers.
 *
 * This is used by the select() I/O manager implementation only.
 *
 * The sleeping queue is defined for other non-threaded I/O managers but not
 * used. This is a wart that should be excised.
 */
void insertIntoSleepingQueue(Capability *cap, StgTSO *tso, LowResTime target);
#endif

/* Check to see if there are any pending timeouts or I/O operations
 * in progress with the I/O manager.
 *
 * This is used by the scheduler as part of deadlock-detection, and the
 * "context switch as often as possible" test.
 */
INLINE_HEADER bool anyPendingTimeoutsOrIO(CapIOManager *iomgr);


#if !defined(THREADED_RTS)
/* Check whether there is any completed I/O or expired timers. If so,
 * process the competions as appropriate, which will typically cause some
 * waiting threads to be woken up.
 *
 * Called from schedule() both *before* and *after* scheduleDetectDeadlock().
 *
 * Defined in posix/Select.c
 *         or win32/AwaitEvent.c
 */
void awaitEvent(Capability *cap, bool wait);
#endif


/* Pedantic warning cleanliness
 */
#if !defined(THREADED_RTS) && defined(mingw32_HOST_OS)
#define USED_IF_NOT_THREADS_AND_MINGW32
#else
#define USED_IF_NOT_THREADS_AND_MINGW32 STG_UNUSED
#endif

#if defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
#define USED_IF_THREADS_AND_NOT_MINGW32
#else
#define USED_IF_THREADS_AND_NOT_MINGW32 STG_UNUSED
#endif

/* -----------------------------------------------------------------------------
 * INLINE functions... private from here on down.
 *
 * Some of these hooks are performance sensitive so parts of them are
 * implemented here so they can be inlined.
 * -----------------------------------------------------------------------------
 */

INLINE_HEADER bool anyPendingTimeoutsOrIO(CapIOManager *iomgr USED_IF_NOT_THREADS)
{
#if defined(THREADED_RTS)
    /* For the purpose of the scheduler, the threaded I/O managers never have
       pending I/O or timers. Of course in reality they do, but they're
       managed via other primitives that the scheduler can see into (threads,
       MVars and foreign blocking calls).
     */
    return false;
#else
#if defined(mingw32_HOST_OS)
    /* The MIO I/O manager uses the blocked_queue, while the WinIO does not.
       Note: the latter fact makes this test useless for the WinIO I/O manager,
       and is the probable cause of the complication in the scheduler with
       having to call awaitEvent in multiple places.

       None of the Windows I/O managers use the sleeping_queue
     */
    return (iomgr->blocked_queue_hd != END_TSO_QUEUE);
#else
    /* The select() I/O manager uses the blocked_queue and the sleeping_queue.
     */
    return (iomgr->blocked_queue_hd != END_TSO_QUEUE)
        || (iomgr->sleeping_queue   != END_TSO_QUEUE);
#endif
#endif
}

#include "EndPrivate.h"
