#include "Rts.h"

#include "ClosureTable.h"
#include "IOManager.h"
#include "Prelude.h"
#include "RaiseAsync.h"
#include "Rts.h"
#include "Capability.h"
#include "IOManagerInternals.h"
#include "Schedule.h"
#include "Trace.h"
#include "rts/Constants.h"
#include "rts/Types.h"
#include "FdWakeup.h"
#include "rts/storage/Closures.h"
#include "IOUring.h"

#include <liburing.h>
#include <linux/io_uring.h>
#include <unistd.h>
#include <poll.h>

// Canceling IO Operations is tricky. There is a race between the IO operation
// in flight and the cancelation request. There will be 2 completion queue
// entries: one for the original io operation and one for the cancelation
// request. They will have the the same user_data and could arrive in the CQ in
// any order, but we must be careful to not user-after-free the corresponding
// aiop_entry. We hijack the aiop->result value to track the number of pending
// CQEs. Then we free the aiop when the count hits zero.

// TODO test cancelation of IO

// TODO what about signals?

// TODO have some solution to the submision queue being full. We can e.g.
// * keep a dynamically size overflow queue and occasionally flush it to the
//   ring (e.g. when we receive a CQE or on poll / await functions).
//   * According to claude (Sonnet 5) this is what the nginx webserver does
// * dynamically size the ring, reinitializing with a large ring when neded
//   (e.g. doubling the size each time).
// * Maintain multiple rings, though this could complicate poll/wait functions
//   as there may not be an obvious way to wait on multiple rings at once.

// TODO we convert between a few different int types when handling aiop index
// which ends up in the S/CQE as user_data and also in the aiop index. We even
// have a special USER_DATA_INTERUPT_IOMGR value. We should review if this is
// really valid. At first I thought there might be a max index (as a function of
// QUEUE_DEPTH), but I think that's not true as I suspect the kernel can consume
// an unbounded number of long running SQEs (increasing the index) before
// submitting any CQEs (decreasing the index).

// TODO We use TIMOUT op for timers, but we should consider the max possible
// timeout. Other IO Managers seem to have workarounds where a timeout is broken
// into multiple timeouts. Perhaps that's not an issue for io_uring.

// TODO what's a good queue depth? Configurable via RTS opts?
#define QUEUE_DEPTH 256
#define USER_DATA_INTERUPT_IOMGR (__u64)(-1)  // Interup calls blocked on the CQ

bool insertAiop(CapIOManager *iomgr, StgAsyncIOOp* aiop, int* ix);

void initCapabilityIOManagerIOUring(CapIOManager *iomgr) {
  initClosureTable(&iomgr->aiop_table, ClosureTableNonCompact);

  int res;
  unsigned int flags = 0;
  res = io_uring_queue_init(QUEUE_DEPTH, &iomgr->ring, flags);
  if (res < 0) {
    barf("initCapabilityIOManagerIOUring: io_uring_setup failed");
  }

  // TODO we could use flag IORING_SETUP_SINGLE_ISSUER and call
  // io_uring_register_ring_fd() as a single threaded optimization if the ring
  // is used from a single thread, but I'm not sure that's the case. IO Managers
  // should be pinned to a capability, but not necessarily a task (OS thread).
}

void freeCapabilityIOManagerIOUring(CapIOManager *iomgr) {
  io_uring_queue_exit(&iomgr->ring);
}


/* Used to implement syncIOWaitReady.
 * Result is true on success, or false on allocation failure. */
bool syncIOWaitReadyIOUring(CapIOManager *iomgr, StgTSO *tso, IOReadOrWrite rw,
                            HsInt fd) {
  // Set StgAsyncIOOp
  StgAsyncIOOp *aiop;
  aiop = (StgAsyncIOOp *)allocateMightFail(iomgr->cap, sizeofW(StgAsyncIOOp));
  if (RTS_UNLIKELY(aiop == NULL)) return false;
  SET_HDR(aiop, &stg_ASYNCIOOP_info, iomgr->cap->r.rCCCS);
  aiop->notify.tso     = tso;
  aiop->capno          = iomgr->cap->no;
  aiop->outcome        = IOOpOutcomeInFlight;
  aiop->notify_type    = NotifyTSO;
  aiop->live           = &stg_ASYNCIO_LIVE0_closure;
  tso->block_info.aiop = aiop;

  struct io_uring* ring = &iomgr->ring;
  struct io_uring_sqe* sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) {
    // TODO overflow queue
    barf("io_uring: Submition queue is full");
  }

  io_uring_prep_poll_add(sqe, fd, IORead ? POLLIN : POLLOUT);
  int ix;
  bool ok = insertAiop(iomgr, aiop, &ix);
  if (RTS_UNLIKELY(!ok)) return false;
  aiop->index = (uint32_t)ix;
  sqe->user_data = (__u64)ix;
  RELEASE_STORE(&tso->why_blocked,
                rw == IORead ? BlockedOnRead : BlockedOnWrite);

  // Submit SQE
  io_uring_submit(ring);

  return true;
}

bool syncDelayTimeoutIOUring(CapIOManager *iomgr, StgTSO *tso, HsInt us_delay)
{
  // Set StgAsyncIOOp
  StgAsyncIOOp *aiop;
  aiop = (StgAsyncIOOp *)allocateMightFail(iomgr->cap, sizeofW(StgAsyncIOOp));
  if (RTS_UNLIKELY(aiop == NULL)) return false;
  SET_HDR(aiop, &stg_ASYNCIOOP_info, iomgr->cap->r.rCCCS);
  aiop->notify.tso     = tso;
  aiop->capno          = iomgr->cap->no;
  aiop->outcome        = IOOpOutcomeInFlight;
  aiop->notify_type    = NotifyTSO;
  aiop->live           = &stg_ASYNCIO_LIVE0_closure;
  tso->block_info.aiop = aiop;

  // Make timeout SEQ
  struct io_uring *ring = &iomgr->ring;
  struct __kernel_timespec delay = {
      .tv_sec  = us_delay / 1000000,
      .tv_nsec = us_delay % 1000000,
  };
  struct io_uring_sqe* sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) {
    // TODO overflow queue
    barf("io_uring: Submition queue is full");
  }
  io_uring_prep_timeout(sqe, &delay,
    0 /* Just use timeout (don't wait on CQEs) */,
    0 /* delay is a relative (not absolute time) */);
  int ix;
  bool ok = (uint32_t)insertAiop(iomgr, aiop, &ix);
  if (RTS_UNLIKELY(!ok)) return false;
  aiop->index = (uint32_t)ix;
  sqe->user_data = (__u64)ix;

  // Block the thread
  ASSERT(tso->why_blocked == NotBlocked);
  RELEASE_STORE(&tso->why_blocked, BlockedOnDelay);

  debugTrace(DEBUG_iomanager,
    "timer for delay of %lld usec installed",
    us_delay);

  io_uring_submit(ring);
  return true;
}

// This is called due to an async exception. We just need to cancel the IO
// operation and let the calling functions sort out putting the TSO into the
// correct state.
void syncIOCancelIOUring(CapIOManager *iomgr, StgTSO *tso) {
  StgAsyncIOOp *aiop  = tso->block_info.aiop;
  ASSERT(aiop->notify_type == NotifyTSO);
  ASSERT(indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop);

  struct io_uring* ring = &iomgr->ring;
  struct io_uring_sqe* sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) {
    // TODO overflow queue
    barf("io_uring: Submition queue is full");
  }
  io_uring_prep_cancel64(sqe, aiop->index, 0);
  int res = io_uring_submit(ring);
  if (res != 0) {
    barf("io_uring: io_uring_submit full");
  }

  aiop->outcome = IOOpOutcomeCancelled;
  aiop->result = 2; // 2 pending CQEs

  // We keep aiop in the aiop_table as we need to keep the index valid for the
  // inflight SQE. When the CQE arrives, we can then properly handle it (with
  // valid index i.e. user_data) and free the aiop.

  // We are in the context of throwTo, interrupting a thread blocked on IO via
  // an async exception. We don't put the TSO back on the run queue or change
  // the why_blocked status, as that is done by removeFromQueues (in the
  // throwTo* functions).
}

void pollCompletedTimeoutsOrIOIOUring(CapIOManager *iomgr) {
  struct io_uring *ring = &iomgr->ring;
  struct io_uring_cqe *cqe;
  unsigned head;
  unsigned i = 0;
  io_uring_for_each_cqe(ring, head, cqe) {
    i++;

    // Ignore interupt CQEs. These are just to interupt awaitCompletedTimeoutsOrIOIOUring().
    // They don't have a corresponding aiop_table entry
    if (cqe->user_data == USER_DATA_INTERUPT_IOMGR)
      continue;

    // handle completion
    int ix = (int)cqe->user_data;
    StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, ix);
    StgTSO* tso = aiop->notify.tso;
    ASSERT(tso->block_info.aiop == aiop);
    ASSERT(aiop->notify_type == NotifyTSO);
    ASSERT(aiop->outcome == IOOpOutcomeInFlight || aiop->outcome == IOOpOutcomeCancelled);
    int res = cqe->res;

    // Handle cancelled requests
    if (aiop->outcome == IOOpOutcomeCancelled) {
      // In the cancellation case, `result` is a count of pending CQEs for this IO request
      aiop->result--;
      // Only free the aiop on the final pending CQE for this request
      if (aiop->result > 0) continue;
    }
    // Handle completed requests
    else {
      switch (tso->why_blocked) {
        // The SQE was a IORING_OP_TIMEOUT
        case BlockedOnDelay: {
          ASSERT(res == -ETIME);
          aiop->outcome = IOOpOutcomeSuccess;
          break;
        }
        // The SQE was a IORING_OP_POLL_ADD
        case BlockedOnRead:
        case BlockedOnWrite: {
          // Handle error result
          if (res < 0) {
            int err = -res;
            char* err_msg;
            switch (err) {
            case POLLPRI:
                err_msg = "some exceptional file descriptor condition";
                break;
            case POLLERR:
                err_msg = "poll error";
                break;
              case POLLHUP:
                err_msg = "hang up";
                break;
              case POLLNVAL:
                err_msg = "closed file descriptor";
                break;
              default:
                err_msg = "unknown error";
            }

            aiop->outcome = IOOpOutcomeFailed;
            aiop->error = err;
            debugTrace(DEBUG_iomanager,
              "Raising exception in thread %" FMT_StgThreadID
              " blocked on %s\n", tso->id, err_msg);
          }
          // Handle success result
          else {
            aiop->outcome = IOOpOutcomeSuccess;
            aiop->result = 0;
          }
          break;
        }
        default:
          barf("io_uring: thread unexpectedly blocked on reason: %d", tso->why_blocked);
      }

      if (aiop->outcome == IOOpOutcomeSuccess) {
        pushOnRunQueue(iomgr->cap, tso);
        RELEASE_STORE(&tso->why_blocked, NotBlocked);
      } else {
        raiseAsync(
          iomgr->cap,
          tso,
          (StgClosure*)blockedOnBadFD_closure,
          false,
          NULL);
      }
    }

    // Remove from aiop_table
    removeClosureTable(iomgr->cap, &iomgr->aiop_table, ix);
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushClosure(iomgr->cap, (StgClosure *)aiop);
    }
  }

  io_uring_cq_advance(ring, i);
}

bool awaitCompletedTimeoutsOrIOIOUring(CapIOManager *iomgr) {
  struct io_uring *ring = &iomgr->ring;

  // Wait for at least 1 CQE
  // TODO make this interuptable by (interruptIOManagerIOUring)... perhpas with a repeating poll SEQ on a wakup fd
  struct io_uring_cqe *cqe;
  int res = io_uring_wait_cqe(ring, &cqe);
  if (res != 0) {
    barf("initCapabilityIOManagerIOUring: io_uring_wait_cqe failed");
  }

  // Process all (at least 1) CQE
  pollCompletedTimeoutsOrIOIOUring(iomgr);

  return false; // TODO return false if interupted by interruptIOManagerIOUring()
}

void interruptIOManagerIOUring(CapIOManager *iomgr) {
  struct io_uring *ring = &iomgr->ring;
  struct io_uring_sqe* sqe = _io_uring_get_sqe(ring);
  if (sqe == NULL) {
    // TODO overflow queue
    barf("io_uring: Submition queue is full");
  }
  io_uring_prep_nop(sqe);
  sqe->user_data = USER_DATA_INTERUPT_IOMGR;
  io_uring_submit(ring);
}

bool insertAiop(CapIOManager *iomgr, StgAsyncIOOp* aiop, int* ix) {
  if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
    int newCapacity = iomgr->aiop_table.capacity == 0 ? 1 : iomgr->aiop_table.capacity * 2;
    bool ok = enlargeClosureTable(iomgr->cap, &iomgr->aiop_table, newCapacity);
    if (RTS_UNLIKELY(!ok)) return false;
  }
  *ix = insertClosureTable(iomgr->cap, &iomgr->aiop_table, aiop);
  return true;
}
