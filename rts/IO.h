/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Prototypes for functions in IO.c
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

#pragma once

#include "BeginPrivate.h"

#include "sm/GC.h" // for evac_fn
#include "posix/Select.h" // for LowResTime TODO: switch to normal Time

/* Init hook: called from hs_init_ghc.
 */
RTS_PRIVATE void initIOManager(void);


/* Shutdown hooks: called from hs_exit_ before and after the scheduler exits.
 */
RTS_PRIVATE void stopIOManager(void);
RTS_PRIVATE void exitIOManager(bool wait_threads);

/* The per-capability data structures belonging to the I/O manager.
 *
 * It can be accessed as cap->iomgr.
 *
 * The content of the structure is defined conditionally so it is different for
 * each I/O manager implementation.
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
     * TODO@ It is not used by any of the Windows I/O managers, though it
     * remains defined for them. This is an oddity that should be resolved.
     */
    StgTSO *sleeping_queue;
#endif

} CapIOManager;

/* Per-capability init hook: called from initCapability().
 */
RTS_PRIVATE void initCapabilityIOManager(CapIOManager *iomgr);

/* GC hook: mark any per-capability GC roots the I/O manager uses.
 */
RTS_PRIVATE void markCapabilityIOManager(evac_fn evac, void *user,
                                         CapIOManager *iomgr);

#if !defined(THREADED_RTS)
/* Add a thread to the end of the queue of threads blocked on I/O.
 *
 * This is used by the select() and the Windows MIO non-threaded I/O manager
 * implementation.
 */
RTS_PRIVATE void appendToIOBlockedQueue(StgTSO *tso);

/* Insert a thread into the queue of threads blocked on timers.
 *
 * This is used by the select() I/O manager implementation only.
 *
 * The sleeping queue is defined for other non-threaded I/O managers but not
 * used. This is a wart that should be excised.
 */
RTS_PRIVATE void insertIntoSleepingQueue(StgTSO *tso, LowResTime target);
#endif

/* Test to see if there are any in-flight I/O operations with the I/O manager.
 *
 * This is used by the scheduler as part of deadlock-detection, and the
 * "context switch as often as possible" test.
 */
INLINE_HEADER bool emptyPendingIO(CapIOManager *iomgr);

/* Test to see if there are any pending timeouts with the I/O manager.
 *
 * This is used by the scheduler as part of deadlock-detection, and the
 * "context switch as often as possible" test.
 */
INLINE_HEADER bool emptyPendingTimeouts(CapIOManager *iomgr);


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

#if !defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
#define USED_IF_NOT_THREADS_AND_NOT_MINGW32
#else
#define USED_IF_NOT_THREADS_AND_NOT_MINGW32 STG_UNUSED
#endif


/* -----------------------------------------------------------------------------
 * INLINE functions... private from here on down.
 *
 * Some of these hooks are performance sensitive so parts of them are
 * implemented here so they can be inlined.
 * -----------------------------------------------------------------------------
 */

INLINE_HEADER bool emptyPendingIO(CapIOManager *iomgr USED_IF_NOT_THREADS)
{
#if defined(THREADED_RTS)
    /* For the purpose of the scheduler, the threaded I/O managers never have
       pending I/O. Of course in reality they do, but they're managed via other
       primitives that the scheduler can see into (threads, MVars and foreign
       blocking calls).
     */
    return true;
#else
#if defined(mingw32_HOST_OS)
    /* The MIO I/O manager uses the blocked_queue, while the WinIO does not.
       Note: the latter fact makes this test useless for the WinIO I/O manager,
       and is the probable cause of the complication in the scheduler with
       having to call awaitEvent in multiple places.
     */
    return (iomgr->blocked_queue_hd == END_TSO_QUEUE);
#else
    /* The select() I/O manager uses the blocked_queue.
     */
    return (iomgr->blocked_queue_hd == END_TSO_QUEUE);
#endif
#endif
}

INLINE_HEADER bool emptyPendingTimeouts(CapIOManager *iomgr
                                          USED_IF_NOT_THREADS_AND_NOT_MINGW32)
{
#if defined(THREADED_RTS)
    /* For the purpose of the scheduler, the threaded I/O managers never have
       pending timers. Of course in reality they do, but they're managed via
       other primitives that the scheduler can see into (threads, MVars and
       foreign blocking calls).
     */
    return true;
#else
#if defined(mingw32_HOST_OS)
    /* None of the Windows I/O managers use the sleeping_queue
     */
    return true;
#else
    return (iomgr->sleeping_queue == END_TSO_QUEUE);
#endif
#endif
}

#include "EndPrivate.h"

