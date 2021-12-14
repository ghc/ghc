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
void appendToIOBlockedQueue(StgTSO *tso);

/* Insert a thread into the queue of threads blocked on timers.
 *
 * This is used by the select() I/O manager implementation only.
 *
 * The sleeping queue is defined for other non-threaded I/O managers but not
 * used. This is a wart that should be excised.
 */
void insertIntoSleepingQueue(StgTSO *tso, LowResTime target);
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

/* TODO: rename and replace these macros by inline functions
 * these have been moved here from Scheduler.h
 */
#if !defined(THREADED_RTS)
#define EMPTY_BLOCKED_QUEUE(cap)  (emptyQueue(cap->iomgr->blocked_queue_hd))
#define EMPTY_SLEEPING_QUEUE(cap) (emptyQueue(cap->iomgr->sleeping_queue))
#endif

#include "EndPrivate.h"
