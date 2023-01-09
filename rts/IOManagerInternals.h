/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Internal type definitions for use within the I/O manager implementations,
 * but not exposed to the rest of the RTS that calls into the I/O managers.
 *
 * In particular this defines the representation of CapIOManager, which is
 * known only to IOManager.c and each individual I/O manager implementation.
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#include "BeginPrivate.h"

/* The per-capability data structures belonging to the I/O manager.
 *
 * It can be accessed as cap->iomgr.
 *
 * The content of the structure is defined conditionally so it is different for
 * each I/O manager implementation.
 *
 * Here is where we actually define the representation.
 */
struct _CapIOManager {

#if defined(IOMGR_ENABLED_SELECT)
    /* Thread queue for threads blocked on I/O completion. */
    StgTSO *blocked_queue_hd;
    StgTSO *blocked_queue_tl;

    /* Thread queue for threads blocked on timeouts. */
    StgTSO *sleeping_queue;
#endif

#if defined(IOMGR_ENABLED_WIN32_LEGACY)
    /* Thread queue for threads blocked on I/O completion. */
    StgTSO *blocked_queue_hd;
    StgTSO *blocked_queue_tl;
#endif

#if defined(IOMGR_ENABLED_MIO_POSIX)
    /* Control FD for the (posix) MIO manager for this capability,
     */
    int control_fd;
#endif

};

#include "EndPrivate.h"

