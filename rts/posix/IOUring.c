#include "ClosureTable.h"
#include "Rts.h"

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
#include "IOUring.h"

#include <liburing.h>
#include <linux/io_uring.h>
#include <unistd.h>
#include <poll.h>


// TODO do we want to set the StgTSOBlockInfo timeout and aiop fields or this
// that not necessary? Perhaps it's information that other code wants to use?

// TODO what about signals?

// TODO what's a good queue depth? Configurable via RTS opts?
#define QUEUE_DEPTH 256
#define USER_DATA_INTERUPT_IOMGR -1

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
  struct io_uring* ring = &iomgr->ring;
  struct io_uring_sqe* sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) {
    // TODO overflow queue
    barf("io_uring: Submition queue is full");
  }

  io_uring_prep_poll_add(sqe, fd, IORead ? POLLIN : POLLOUT);
  int ix = insertClosureTable(&iomgr->cap, iomgr->tso_table, tso);
  __u64 user_data = (__u64)ix;
  sqe->user_data = user_data;

  // Submit SQE
  io_uring_submit(ring);

  RELEASE_STORE(&tso->why_blocked,
                rw == IORead ? BlockedOnRead : BlockedOnWrite);

  return true;
}

// This is called due to an async exception. We just need to cancel the IO
// operation and let the calling functions sort out putting the TSO into the
// correct state.
void syncIOCancelIOUring(CapIOManager *iomgr, StgTSO *tso) {
  __u64 user_data = (__u64)tso->block_info;
  struct io_uring* ring = &iomgr->ring;
  struct io_uring_sqe* sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) {
    // TODO overflow queue
    barf("io_uring: Submition queue is full");
  }
  io_uring_prep_cancel64(sqe, user_data, 0);
  int res = io_uring_submit(ring);
  if (res != 0) {
    barf("io_uring: io_uring_submit full");
  }
}

void pollCompletedTimeoutsOrIOIOUring(CapIOManager *iomgr) {
  struct io_uring *ring = &iomgr->ring;
  struct io_uring_cqe *cqe;
  unsigned head;
  unsigned i = 0;
  io_uring_for_each_cqe(ring, head, cqe) {
    i++;

    // Ignore wakup requests
    if (cqe->user_data == USER_DATA_INTERUPT_IOMGR)
      continue;

    // handle completion
    StgTSO* tso = (StgTSO*)cqe->user_data;
    int res = cqe->res;

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

      debugTrace(DEBUG_iomanager,
        "Raising exception in thread %" FMT_StgThreadID
        " blocked on %s", tso->id, err_msg);
      raiseAsync(iomgr->cap, tso,
        (StgClosure*)blockedOnBadFD_closure, false, NULL);
      break;
    } else {
      pushOnRunQueue(iomgr->cap, tso);
      RELEASE_STORE(&tso->why_blocked, NotBlocked);
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

bool syncDelayTimeoutIOUring(CapIOManager *iomgr, StgTSO *tso, HsInt us_delay)
{
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
  io_uring_submit(ring);

  // Block the thread
  ASSERT(tso->why_blocked == NotBlocked);
  RELEASE_STORE(&tso->why_blocked, BlockedOnDelay);

  debugTrace(DEBUG_iomanager,
              "timer for delay of %lld usec installed",
              us_delay);
  return true;
}
