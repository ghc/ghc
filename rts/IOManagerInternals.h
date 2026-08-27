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

#if defined(IOMGR_ENABLED_POLL)
#include <poll.h> /* for struct pollfd */
#include "ClosureTable.h"
#include "TimeoutQueue.h"
#endif

#if defined(IOMGR_ENABLED_IO_URING)
#include <liburing.h>
#include "ClosureTable.h"
#endif

#include "BeginPrivate.h"

/* The per-capability data structures belonging to the I/O manager.
 *
 * It can be accessed as cap->iomgr. Or given just the iomgr, you can access
 * the owning cap as iomgr->cap.
 *
 * The content of the structure is defined conditionally so it is different for
 * each I/O manager implementation.
 *
 * Here is where we actually define the representation.
 */
struct _CapIOManager {

   /* Back reference to the containing capability */
    Capability *cap;

#if defined(IOMGR_ENABLED_SELECT)
    /* Thread queue for threads blocked on I/O completion. */
    StgTSO *blocked_queue_hd;
    StgTSO *blocked_queue_tl;

    /* Thread queue for threads blocked on timeouts. */
    StgTSO *sleeping_queue;
#endif

#if defined(IOMGR_ENABLED_SELECT) || defined(IOMGR_ENABLED_POLL)
#if defined(HAVE_PREEMPTION)
    /* FDs for waking up the I/O manager when it is blocked waiting */
    int interrupt_fd_r, interrupt_fd_w;
#endif
#endif

#if defined(IOMGR_ENABLED_POLL) || defined(IOMGR_ENABLED_IO_URING)
    /* AIOP and timeout collections shared by several I/O manager impls */
    ClosureTable     aiop_table;
    StgTimeoutQueue *timeout_queue;
#endif

#if defined(IOMGR_ENABLED_POLL)
    /* Auxiliary table with size and indexes matching the aiop_table. This is
     * aliased to the tail of the full poll table, which has a head entry for
     * the wakeup_fd_r above, so we can also poll that fd.
     */
    struct pollfd *aiop_poll_table, *full_poll_table;
#endif

#if defined(IOMGR_ENABLED_IO_URING)
    /* io_uring library structure */
    struct io_uring *uring;

    /* The number of operations submitted (by Haskell threads to the I/O
       manager) and not yet notified of completion. */
    int n_submitted_b;  /* for blocking operations */
    int n_submitted_nb; /* for non-blocking operations */

    /* The number of operations pending in the submission queue, but not yet
       submitted to the kernel (so not in-flight). */
    int n_prepared_b;  /* for blocking operations */
    int n_prepared_nb; /* for non-blocking operations */

    /* The number of operations submitted to the kernel but where the
       corresponding completion has not yet been processed. */
    int n_inflight_b;  /* for blocking operations */
    int n_inflight_nb; /* for non-blocking operations */

    /* The limit on the number of operations we allow to be in-flight */
    int limit_inflight_b;  /* for blocking operations */
    int limit_inflight_nb; /* for non-blocking operations */

    /* The number of operations pending in the overflow queue (so not in the
       submission queue or in flight) */
    /* no overflow for blocking operations */
    int n_overflow_nb;  /* for non-blocking operations */

    /* Invariants:
         n_submitted_b  = n_prepared_b  + n_inflight_b
         n_submitted_nb = n_prepared_nb + n_inflight_nb + n_overflow_nb
         n_prepared_b + n_prepared_nb <= size of submission queue
     */

    /* A queue of threads blocked on I/O submission and a parallel queue of
     * their corresponding SQEs. This is only used when there are more pending
     * (non-blocking) I/O operations than the inflight limit.
     */
    StgTSO *overflow_tso_q_hd, *overflow_tso_q_tl;
    struct overflow_sqe_q_t {
        struct io_uring_sqe     *sqe;
        struct overflow_sqe_q_t *next;
#if defined(DEBUG)
        StgThreadID              tid;
#endif
    } *overflow_sqe_q_hd, *overflow_sqe_q_tl;
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

