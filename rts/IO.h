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

#include "EndPrivate.h"

