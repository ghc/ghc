/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2021-2023
 *
 * An I/O manager based on the Linux io_uring API.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "IOManager.h" // defines IOMGR_ENABLED_IO_URING

#if defined(IOMGR_ENABLED_IO_URING)

#include "Capability.h"
#include "Prelude.h"
#include "RaiseAsync.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Threads.h"
#include "rts/Time.h"

#include "IOURing.h"

#include <errno.h>
#include <fcntl.h>
#include <liburing.h>
#include <limits.h>
#include <poll.h> // for poll() flags POLLIN POLLOUT

#include "IOManagerInternals.h"
#include "Timeout.h"
#include "TimeoutQueue.h"

/******************************************************************************

This I/O manager is based on the Linux io_uring API. We rely on the liburing
library, rather than using the system calls directly.

Introduction
============

The io_uring API is an _almost_ generic mechanism for performing Linux syscalls
asynchronously. It supports a range of I/O related operations, including
ordinary file read and write, and waiting for I/O readiness. It works using a
queue to submit I/O operations, and another queue to receive I/O completions.
The io_uring documentation calls these the "submission queue", abbreviated SQ,
and "completion queue", abbreviated CQ. The corresponding queue entries are
abbreviated as SQEs and CQEs.

There is a single system call to both submit operations and/or wait for I/O
completion (or a timeout). It is also possible to poll for new entries in the
completion queue without using a system call at all. This fits the RTS
scheduler design quite well. Every time round the scheduler loop we have to
do a non-blocking check for I/O completion, and it is only when there are no
runnable threads that we want to block and wait for I/O (or timers). So in busy
applications there are a lot more non-blocking than blocking checks for I/O
completion. So being able to do the non-blocking poll without needing a system
call should save significantly on system calls, compared to other APIs.

We use the liburing C library, rather than the system calls directly. This
provides a degree of convenience and portability across kernel versions.

Synchronous I/O
---------------

Classically, asynchronous I/O APIs are slower than ordinary synchronous I/O
APIs for the case of buffered I/O reads where the requested data is already in
the OS page cache (if they support buffered I/O at all). This requires
applications that use asynchronous I/O (and thus care about performance) to use
it only in some cases, and use synchronous I/O when that is expected to be
faster. This is complex, and thus relatively few applications use asynchronous
I/O. Furthermore, knowing whether data is in the page cache is something that
only the kernel knows reliably. User space can only make educated guesses.

A major selling point of io_uring is that it (mostly) solves this problem. It
is designed work for buffered I/O (as well as direct I/O). Operations are
submitted by putting entries (SQEs) into the submission queue (SQ) and then
entering the kernel (io_uring_enter). The kernel starts work on all the
operations. If any of them can complete synchronously then the kernel can place
the completion entries (CQEs) into the completion queue (CQ). Thus the
synchronous operations complete synchronously. Furthermore, the decision about
whether it can complete synchronously is made dynamically by the kernel (e.g.
based on whether the data is available in the page cache). The result is that a
simple file read where the data is available in the page cache can be as quick
as a normal synchronous file read system call.

Overall, this allows the same API to be used for all I/O, without having to
guess about synchronous vs asynchronous completions. Furthermore, io_uring
supports both disk I/O and sockets I/O, whereas older APIs only supported one
or the other (see e.g. epoll for sockets and Linux/Posix AIO for disk files).
Overall this allows a less complex solution, by using the same relatively
uniform API for everything (even through the API itself is somewhat more
complex than other APIs).

General strategy
----------------

Our I/O strategy with io_uring is to prepare but not submit I/O operations in
the I/O primops, and then submit the operations in the scheduler. That is, in
the I/O primops we put the I/O operations (SQEs) into the submission queue (SQ),
but we don't yet call into the kernel to inform it of the new operations.
Instead we do that in the scheduler. At this point in the scheduler, we can
both submit any pending I/O operations and handle any completions. This lets us
submit and collect I/O with a single system call. Handling I/O completions
typically results in waking up threads, which the scheduler can then deal with.

Consider the important example of simple disk reads that complete synchronously.
In this case the completion is available immediately after io_uring_enter
returns and the scheduler can reschedule the thread that submitted the I/O. So
the end result _should_ be as fast as a normal synchronous blocking read()
system call (but benchmarks are needed to verify this).

Important fast-path cases
-------------------------

There are a few important special fast-path cases to keep in mind:

1. Common case: as above, a Haskell thread has pending I/O, so we submit
   it. This requires a system call. Afterwards we also process any
   completions. This means any I/O operations that complete synchronously
   get handled immediately and with a single system call. This is crucial
   for performance of ordinary buffered I/O in the common case that the data
   is available in the page cache.

2. Common case: no pending I/O to submit, but there is outstanding I/O
   that we need to see if it has completed. This does not require a
   system call. We just need to look at the completion ring.

3. Rare case: we have pending I/O to submit but the kernel refuses to
   accept more I/O because the completion queue is full. In this case
   we process the completion queue first, and then retry. This ends up
   using two system calls.

I/O submission overload
-----------------------

Asynchronous I/O APIs enable I/O to be submitted without waiting and thus they
must deal with the problem of having too much I/O in flight at once for the
resources available.

For example, epoll_ctl will return ENOMEM or ENOSPC if it cannot allocate the
necessary memory or hits a resource limit. The epoll backend for MIO turns
these failures into Haskell exceptions. This works ok in practice for epoll
because epoll only supports one kind of async I/O operation: waiting for I/O
readiness. This has relatively low resource use and so systems generally hit
other resource limits first, e.g. file descriptors.

On the other hand io_uring supports a variety of I/O operations with different
behaviour and resource use. They can roughly be divided into two categories:
1. cheap blocking operations; and
2. expensive non-blocking operations.

The cheap blocking operations include waiting for I/O readiness on pipes and
sockets, as with epoll, but it also includes blocking reads/write/send/recv on
pipes and sockets. These operations tend to have very low resource use and
correspondingly the kernel can support very many outstanding operations. This
is needed for some large scale networking use cases where there can be very
many sockets in use at once.

The comparatively expensive non-blocking operations include read and write on
disk files and various file and file system operations. These operations tend
to have higher resource use and so the kernel cannot support too many of them
in progress at once. Furthermore, for non-blocking operations there isn't much
need to have huge numbers in progress at once: the performance benefit is
limited by the hardware concurrency (CPUs, SSD queue depth etc).

Unfortunately, the limits on the number of concurrent operations of each kind
is not known, or at least not reported by the kernel in advance. When trying
to submit new operations, the kernel can report EAGAIN if it is out of
resources. This reporting mechanism is awkward because of the two classes of
operations. The way we would like to respond to hitting a limit is also
different for blocking vs non-blocking operations.

For non-blocking operations, we can simply wait for some operations to complete
and then submit more. This works because non-blocking operations will
eventually complete, and indeed typically complete fairly promptly.

For blocking operations on the other hand, there's not a lot we can do if we
hit a resource limit. Waiting may not help. Blocking operations can block
indefinitely. It is also possible to deadlock if not all I/O readiness
notifications are active simultaneously. So if we hit a resource limit for
these operations then we can't do much better than throw exceptions to the
Haskell threads submitting the operations. This is of course what the MIO epoll
backend does.

We will make the assumption that the kernel does _not_ have separate limits for
these two classes of operations, but assume that it is a common limit based on
the common resource of kernel memory. As noted above, there is no great benefit
to having excessive concurrency of non-blocking operations. We can limit the
concurrency (using a queue) with no change in semantics of the application. On
the other hand an application that wants to wait on zillions of blocking
operations cannot have that concurrency reduced without it having a semantic
effect on the application. We either have to support it or (hopefully
gracefully) fail as resources run out.

So we take the approach of trying to limit the concurrency and thus resource
use of the non-blocking operations so that the remaining kernel memory can
maximise the number of blocking operations we can support. We do that by
imposing a "reasonable" limit on the number of concurrent non-blocking
operations. We also impose a separate larger limit on the number of concurrent
blocking operations. The high level idea is that we treat the non-blocking
limit like a semaphore (with that concurrency limit) on the threads submitting
non-blocking I/O operations. This means we can handle overload transparently.
On the other hand the limit on blocking operations is a hard limit and if we
hit that then we have to throw exceptions (to the threads submitting the I/O).

In the typical application use cases this strategy should avoid encountering
EAGAIN in the first place, however it can still happen and we must handle it
somehow.

Before considering EAGAIN, consider a more normal scenario with lots of
concurrent I/O -- both blocking and non-blocking operations -- where we do hit
the limit on non-blocking operations. We track the number of blocking and
non-blocking operations that are currently "in flight" using the counters
iomgr->uring_inflight_{non}blocking_aiops. These are incremented by the number
of prepared operations when the submission queue is successfully flushed, and
decremented for each completion processed. Hitting the limit means that when a
thread submits a new non-blocking operation we find the count of inflight
non-blocking operations is already at the limit, and thus a new operation would
be over the limit.

Normally when a thread uses a primop to submit an I/O operation we would grab
an existing SQE from the submission queue (SQ) and fill in the SQE. If however
we would go over the inflight limit, then instead we allocate a fresh SQE (on
the C heap using malloc) and then fill in the SQE as normal. Then we put the
SQE and the TSO for the thread that submitted it onto (the end of) a pair of
overflow queues: one for the SQEs and one for the corresponding threads. We use
a pair of queues rather than a queue of pairs because the TSOs are allocated on
the GC heap but the SQEs are on the C heap and there is existing infrastructure
for handling TSO queues, including tracing them for GC. In the scheduler, when
we process the completion of a blocking operation, if there are entries on the
overflow queues then we can dequeue an SQE and corresponding TSO and copy the
SQE into the SQ and reschedule the TSO. This means the SQE is ready to go and
the TSO is unblocked. If we are in this overflow situation and the completions
for several non-blocking operations are processed in one go then will will add
several SQEs to be added to the SQ in one batch. This should naturally lead to
batching in the overflow situation and amortise the syscall overheads.

While all of this is going on, other threads submitting _blocking_ operations
can proceed as normal, preparing their operations into the submission queue.

Now if we do encounter EAGAIN, we assume that it's the blocking operations
that are at fault.

Approaches to batching
----------------------

We currently do no batching, but this section discusses plausible approaches.

With this design we have the opportunity to try to accumulate multiple pending
I/O operations and then submit them in one batch, which could improve
performance in I/O intensive applications by reducing the number of system
calls. Ordinary synchronous I/O primops like threadWaitRead# block the Haskell
thread and return to the scheduler, which means we only accumulate a single
pending I/O operation. There are a couple ways we could achieve batching.

One approach is to allow individual Haskell threads to prepare multiple I/O
operations by providing asynchronous I/O primops. This could be quite effective
at generating a lot of I/O, but it would likely see relatively little use
because it requires changing application designs.

Another approach is by having the scheduler not always submit pending I/O, but
instead let it run other threads in the hope that they will produce more
pending I/O. It would wait until either a time limit or a pending count limit
before submitting the I/O. Such an approach would increase I/O latency:
consider the scheduler running a thread that creates a pending I/O operation,
followed by running a CPU-bound thread for a whole 20 millisecond timeslice,
after which the scheduler submits the I/O. Thus this approach could only be
used for low priority I/O, which itself would require introducing a notion of
I/O priority.

One can imagine variations on this design such as a more sophisticated
scheduler predicting if the next thread to run is likely to be I/O or CPU bound
and using that to decide whether to flush pending I/O or to speculate on
accumulating more. Or the scheduler could set a timer to interrupt CPU bound
threads sooner if it's speculating on gathering more I/O. This could reduce
the cost on average, and bound latency, but there would still be a latency vs
throughput tradeoff, which would almost certainly require some notion of I/O
priority.


io_uring features we use
------------------------

IORING_FEAT_NODROP

io_uring features we could use but don't (yet)
----------------------------------------------

io_uring features we cannot use
-------------------------------

NOTES: cannot use registered ring fd, due to multiple worker threads, even
though it's protected by the lock. Same for registered/direct fds.


Tracking counters
-----------------

We track the number of operations submitted (by Haskell threads to the I/O
manager) and not yet notified of completion:

> int n_submitted_b;
> int n_submitted_nb;

These are incremented when the primop submits I/O to the I/O manager, and are
decremented when the I/O completion is processed. We track blocking and
non-blocking operations separately.

We track the number of operations that are prepared in the submission queue,
but not yet submitted to the kernel.

> int n_prepared_b;
> int n_prepared_nb;

These are incremented when an operation is prepared in the submission queue
and decremented when operations are submitted to the kernel.

We track the number of operations submitted to the kernel and where the
completion has not yet been processed. We call these "in-flight" operations.

> int n_inflight_b;
> int n_inflight_nb;

These are incremented when operations are submitted to the kernel, and
decremented when I/O completions are processed.

We track the limit on the number of operations the I/O manager will allow to be
in-flight with the kernel.

> int limit_inflight_b;
> int limit_inflight_nb;

These are typically set on RTS startup and then rarely changed. The
limit_inflight_b can be reduced dynamically if the I/O manager encounters
EAGAIN when submitting operations.

We track the number of non-blocking operations that are in the overflow queue.
These are operations that have been submitted, and could not be put into the
submission queue (because it would exceed the limit_inflight_nb), and so go
into the overflow queue instead.

> int n_overflow_nb;

This is incremented (instead of n_prepared_nb) if the number of n_inflight_nb
plus n_prepared_nb is at or above the limit_inflight_nb, in which case the
operation (and submitting thread) is put into the overflow queue. It is
decremented when there is space available within the limit_inflight_nb and the
operation can be put into the submission queue (and thus also incrementing the
n_prepared_nb).

We maintain two invariants:

1. n_submitted_b  = n_prepared_b                  + n_inflight_b
2. n_submitted_nb = n_prepared_nb + n_overflow_nb + n_inflight_nb

The implication is that set of prepared, overflow and inflight operations are
distinct from each other and their union is equal to the submitted set. Another
way to look at it is that there distinct states for submitted operations:
prepared, overflow (for non-blocking) and inflight.

******************************************************************************/

/* Forward declarations */
static bool enlargeTables(CapIOManager *iomgr);
static void notifyIOCompletion(CapIOManager *iomgr, StgAsyncIOOp *aiop);
static void enqueueOverflowQueue(CapIOManager *iomgr,
                                 StgTSO *tso, struct io_uring_sqe *sqe);
static void dequeueOverflowQueue(CapIOManager *iomgr, StgTSO **ptso,
                                 struct io_uring_sqe **psqe);

/* Constants */

/* A couple tags we add to the sqe->user_data to tell us about this operation
 * when we process the completion. Currently we distinguish:
 * 1. non-blocking vs blocking operations, just so we can update our counters
 *    which count these separately.
 * 2. cancellation operations
 */
const uint64_t AIOP_TAG_CANCEL = 0x80lu << 56; /* bit 63 */
const uint64_t AIOP_TAG_OP_NB = 0x40lu << 56;  /* bit 62 */
const uint64_t AIOP_TAG_MASK = 0xc0lu << 56;   /* bit 62 | 63 */

void initCapabilityIOManagerIOURing(CapIOManager *iomgr) {
  initClosureTable(&iomgr->aiop_table, ClosureTableCompact);
  iomgr->timeout_queue = emptyTimeoutQueue();

  int sq_entries = RtsFlags.MiscFlags.io_uring_sq_entries;

  iomgr->n_submitted_b = 0;
  iomgr->n_submitted_nb = 0;
  iomgr->n_prepared_b = 0;
  iomgr->n_prepared_nb = 0;
  iomgr->n_inflight_b = 0;
  iomgr->n_inflight_nb = 0;
  iomgr->limit_inflight_b = INT_MAX;
  iomgr->limit_inflight_nb = 4 * sq_entries;
  iomgr->n_overflow_nb = 0;
  iomgr->overflow_tso_q_hd = END_TSO_QUEUE;
  iomgr->overflow_tso_q_tl = END_TSO_QUEUE;
  iomgr->overflow_sqe_q_hd = NULL;
  iomgr->overflow_sqe_q_tl = NULL;

  /* Set the uring params: we want to use independent sizes of submission
   * and completion queues. We typically want a bigger completion queue than
   * a submission queue.
   */
  struct io_uring_params params = {
      .flags =
          IORING_SETUP_CQSIZE | IORING_SETUP_CLAMP | IORING_SETUP_SUBMIT_ALL,
      .cq_entries = RtsFlags.MiscFlags.io_uring_cq_entries};
  // TODO: what happens if we use flags that are not recognised by the kernel
  //  version, e.g. IORING_SETUP_SUBMIT_ALL prior to 5.18?

  /* TODO: see if we want to support IORING_SETUP_SQPOLL. With
    IORING_FEAT_NATIVE_WORKERS, it doesn't need any priviledges. */

  /* Share the same kernel work-queue between the urings for each capability.
   * Do this by using the IORING_SETUP_ATTACH_WQ flag for capabilities > 0,
   * and pass the uring fd for cap 0 (the main capability).
   *
   * TODO why?
   */
  if (iomgr->cap->no > 0) {
    params.flags |= IORING_SETUP_ATTACH_WQ;
    params.wq_fd = MainCapability.iomgr->uring->ring_fd;
  }

  /* Try to initialise the uring */
  struct io_uring *uring =
      stgMallocBytes(sizeof(struct io_uring), "initCapabilityIOManagerUring");
  int res = io_uring_queue_init_params(sq_entries, uring, &params);
  if (res < 0)
    goto fail;

  /* Check for features we require. */
  unsigned required =
      /* Needed for simple handling of ring sizes and limits.
       * TODO: we might be able to support kernels without this
       * by setting the limits to be the same as the ring sizes. */
      IORING_FEAT_NODROP

      /* TODO
       *
       * Early kernel versions required that the SQE struct data pointers
       * remains valid until completion of the IO request. With
       * IORING_FEAT_SUBMIT_STABLE such structs need only be valid untill
       * submission into the SQ. Man pages for io_uring_prep_* functions should
       * have a note if they are affected. The __kernel_timespec struct passed
       * to io_uring_prep_timeout() is affected. io_uring_prep_accept() is also
       * affected. Neither is used by this IO Manager. TODO perhaps we want this
       * feature because it is widely supported (assuming io_uring is supported)
       * and we may add affected IO operations in the future?
       * See https://www.man7.org/linux/man-pages/man3/io_uring_submit.3.html
       */
      | IORING_FEAT_SUBMIT_STABLE

      /* Needed for read/write that updates the file pos. */
      | IORING_FEAT_RW_CUR_POS

      /* Needed for corner cases like reading from /proc/self,
       * or signalfd */
      | IORING_FEAT_NATIVE_WORKERS;

  if ((uring->features & required) != required)
    goto fail;

  /* Arrange for the uring (fd and mmap'ed queues) not to be inherited. */
  res = fcntl(uring->ring_fd, F_SETFD, FD_CLOEXEC);
  if (res < 0)
    goto fail;
  res = io_uring_ring_dontfork(uring);
  if (res < 0)
    goto fail;

  /* Success. Save what we need. */
  iomgr->uring = uring;
  return;

  /* Failure. Clean up. */
fail:
  stgFree(uring);
  barf("uring iomgr: initialisation failed");
  // TODO: we should add support to fail and use a fallback I/O manager
}

void freeCapabilityIOManagerIOURing(CapIOManager *iomgr) {
  io_uring_queue_exit(iomgr->uring);
}

/******************************************************************************
 * Common prologues and epilogues for primops for I/O operations.
 *
 * There are different common prologues/epilogues depending on:
 *  - synchronous or asynchronous primops
 *  - blocking or non-blocking I/O operations
 */

/* The common prologue for for all synchronous primops for both blocking and
 * non-blocking I/O operations.
 */
static int prologueSyncIOOp(CapIOManager *iomgr, StgTSO *tso, int why_blocked,
                            StgAsyncIOOp **paiop) {
  StgAsyncIOOp *aiop;
  aiop = (StgAsyncIOOp *)allocateMightFail(iomgr->cap, sizeofW(StgAsyncIOOp));
  if (RTS_UNLIKELY(aiop == NULL)) {
    return (sizeof(StgAsyncIOOp));
  }
  SET_HDR(aiop, &stg_ASYNCIOOP_info, CCS_SYSTEM); // TODO: get CCCS
  aiop->notify_type = NotifyTSO;
  aiop->notify.tso = tso;
  tso->why_blocked = why_blocked;
  tso->block_info.aiop = aiop;
  *paiop = aiop;
  return 0;
}

/* The common prologue for all non-blocking I/O operations.
 *
 * Allocate a table index
 * Fill in some of the aiop fields
 * Allocate an SQE, either on the ring or on the heap if we're in overflow.
 */
static int prologueNonBlockingIOOp(CapIOManager *iomgr, StgTSO *tso,
                                   StgAsyncIOOp *aiop,
                                   struct io_uring_sqe **psqe,
                                   bool *tso_block) {
  Capability* cap = iomgr->cap;
  if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
    int fail = enlargeTables(iomgr);
    if (RTS_UNLIKELY(fail))
      return fail;
  }

  int index = insertClosureTable(cap, &iomgr->aiop_table, aiop);

  aiop->capno = cap->no;
  aiop->index = index;

  struct io_uring_sqe *sqe;

  if (iomgr->n_inflight_nb + iomgr->n_prepared_nb < iomgr->limit_inflight_nb) {
    /* The typical case. Allocate an SQE from the ring */
    sqe = io_uring_get_sqe(iomgr->uring);
    ASSERT(sqe); /* Otherwise we counted wrong */
    iomgr->n_submitted_nb++;
    iomgr->n_prepared_nb++;
    *tso_block = false;
  } else {
    /* We're going to have to block the submitting thread.
     * Allocate an SQE on the heap and suspend the calling TSO.
     */
    sqe = stgMallocBytes(sizeof(struct io_uring_sqe), "uring iomgr");
    enqueueOverflowQueue(iomgr, tso, sqe);
    iomgr->n_submitted_nb++;
    iomgr->n_overflow_nb++;
    *tso_block = true;
  }
  io_uring_sqe_set_data64(sqe, index);
  *psqe = sqe;
  return 0;
}

/* The common prologue for all blocking I/O operations.
 *
 * Allocate a table index
 * Fill in some of the aiop fields
 * Allocate an SQE, or return failure if we're at the limit.
 */
static bool prologueBlockingIOOp(CapIOManager *iomgr, StgTSO *tso,
                                StgAsyncIOOp *aiop,
                                struct io_uring_sqe **psqe) {
  Capability *cap = iomgr->cap;

  if (iomgr->n_inflight_b + iomgr->n_prepared_b >= iomgr->limit_inflight_b) {
    /* If we reach the limit we fail and throw an exception */
    raiseAsync(cap, tso,
               (StgClosure *)blockedOnBadFD_closure
               /*TODO: use ioopResourcesExhausted_closure */,
               false, NULL);
    return false;
    // TODO: review this
    // TODO: current return value is for memory alloc failure, not for
    //  other failures. Need error result separate from alloc.
  }

  if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
    bool ok = enlargeTables(iomgr);
    if (RTS_UNLIKELY(!ok))
      return false;
  }

  int index = insertClosureTable(cap, &iomgr->aiop_table, aiop);

  aiop->capno = cap->no;
  aiop->index = index;

  /* Allocate an SQE on the ring */
  struct io_uring_sqe *sqe = io_uring_get_sqe(iomgr->uring);
  ASSERT(sqe); /* Otherwise we counted wrong */
  iomgr->n_submitted_b++;
  iomgr->n_prepared_b++;
  io_uring_sqe_set_data64(sqe, index);
  *psqe = sqe;
  return 0;
}

/* The common epilogue for all async non-blocking I/O operations.
 */
static int epilogueAsyncNonBlockingIOOp(CapIOManager *iomgr, StgTSO *tso,
                                        StgAsyncIOOp *aiop, bool tso_block) {
  if (tso_block) {
    tso->why_blocked = BlockedOnIOSubmission;
    tso->block_info.aiop = aiop;
    return -1;
  } else if (io_uring_sq_space_left(iomgr->uring) == 0) {
    return -1;
  } else {
    return 0;
  }
}

/* Common epilogue for all async blocking I/O operations.
 */
static int epilogueAsyncBlockingIOOp(CapIOManager *iomgr) {
  return io_uring_sq_space_left(iomgr->uring) == 0 ? -1 : 0;
}

/******************************************************************************
 * Non-blocking I/O operations. This includes read/write on files (not sockets).
 *
 * The code is organised as common I/O preparation functions and then individual
 * primops (sync and async). They also rely on the common prologue and epilogue
 * functions above.
 */

static int prepareIOReadWrite(CapIOManager *iomgr, StgTSO *tso, bool *tso_block,
                              StgAsyncIOOp *aiop, IOReadOrWrite rw, int fd,
                              StgClosure *live, void *buf, size_t len,
                              off_t off) {
  struct io_uring_sqe *sqe;
  int fail = prologueNonBlockingIOOp(iomgr, tso, aiop, &sqe, tso_block);
  if (RTS_UNLIKELY(fail))
    return fail;

  aiop->live = live;
  if (rw == IORead) {
    io_uring_prep_read(sqe, fd, buf, len, off);
  } else {
    io_uring_prep_write(sqe, fd, buf, len, off);
  }
  return 0;
}

int asyncIOReadWriteIOURing(CapIOManager *iomgr, StgTSO *tso, StgAsyncIOOp *aiop,
                            IOReadOrWrite rw, int fd, StgClosure *live,
                            void *buf, size_t len, off_t off) {
  bool tso_block;
  int fail = prepareIOReadWrite(iomgr, tso, &tso_block, aiop, rw, fd, live, buf,
                                len, off);
  if (RTS_UNLIKELY(fail))
    return fail;

  return epilogueAsyncNonBlockingIOOp(iomgr, tso, aiop, tso_block);
}

int syncIOReadWriteIOURing(CapIOManager *iomgr, StgTSO *tso, IOReadOrWrite rw,
                           int fd, StgClosure *live, void *buf, size_t len,
                           off_t off) {
  StgAsyncIOOp *aiop;
  int why_blocked = rw == IORead ? BlockedOnRead : BlockedOnWrite;
  int fail = prologueSyncIOOp(iomgr, tso, why_blocked, &aiop);
  if (RTS_UNLIKELY(fail))
    return fail;

  bool unused;
  fail =
      prepareIOReadWrite(iomgr, tso, &unused, aiop, rw, fd, live, buf, len, off);
  if (RTS_UNLIKELY(fail))
    return fail;
  return 0;
}

/******************************************************************************
 * Blocking I/O operations. This includes waiting for I/O readiness on sockets,
 * pipes etc.
 *
 * The code is organised as common I/O preparation functions and then individual
 * primops (sync and async). They also rely on the common prologue and epilogue
 * functions above.
 */

static void prepareIOWaitReady(StgAsyncIOOp *aiop, struct io_uring_sqe *sqe,
                               IOReadOrWrite rw, int fd) {
  aiop->live = &stg_ASYNCIO_LIVE0_closure;
  io_uring_prep_poll_add(sqe, fd, rw == IORead ? POLLIN : POLLOUT);
}

bool asyncIOWaitReadyIOURing(CapIOManager *iomgr, StgTSO *tso, StgAsyncIOOp *aiop,
                            IOReadOrWrite rw, int fd) {
  struct io_uring_sqe *sqe;
  bool ok = prologueBlockingIOOp(iomgr, tso, aiop, &sqe);
  if (RTS_UNLIKELY(!ok))
    return false;

  prepareIOWaitReady(aiop, sqe, rw, fd);

  return epilogueAsyncBlockingIOOp(iomgr);
}

bool syncIOWaitReadyIOURing(CapIOManager *iomgr, StgTSO *tso, IOReadOrWrite rw,
                           HsInt fd) {
  StgAsyncIOOp *aiop;
  int why_blocked = rw == IORead ? BlockedOnRead : BlockedOnWrite;
  bool ok = prologueSyncIOOp(iomgr, tso, why_blocked, &aiop);
  if (RTS_UNLIKELY(!ok))
    return false;

  struct io_uring_sqe *sqe;
  ok = prologueBlockingIOOp(iomgr, tso, aiop, &sqe);
  if (RTS_UNLIKELY(!ok))
    return false;

  prepareIOWaitReady(aiop, sqe, rw, fd);

  return 0;
}

/******************************************************************************
 * Actions to cancel outstanding I/O operations. Also support cancelling any
 * outstanding I/O on an fd prior to it being closed.
 *
 * This covers both synchronous and asynchronous operations.
 */

static void ioCancel(CapIOManager *iomgr, StgAsyncIOOp *aiop);

void syncIOCancelIOURing(CapIOManager *iomgr, StgTSO *tso) {
  StgAsyncIOOp *aiop = tso->block_info.aiop;
  ASSERT(aiop->notify_type == NotifyTSO);
  ASSERT(indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop);
  ioCancel(iomgr, aiop);
  /* We cannot use the normal notifyIOCompletion here. We are in the context
   * of throwTo, interrupting a thread blocked on IO via an async exception.
   * We don't put the TSO back on the run queue or change the why_blocked
   * status, as that is done by removeFromQueues (in the throwTo* functions).
   */
  tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;

  // TODO: Synchronous cancellation from throwTo seems to be pretty keen for
  //  the thread to be unblocked immediately so it can start unwinding the
  //  stack. Perhaps it is ok to continue the cancellation in the background.
  //  But if so then we will need to adjust the notify type to be none /
  //  cancelled!
  /* Cancelling thread-synchronous I/O happens from throwTo, which is very
   * keen for the thread to be unblocked immediately so it can unwind the
   * stack and schedule the thread to run an exception handler. This demand
   * to be synchronous is a bit tricky to arrange because cancelling the I/O
   * operation the thread is blocked on is potentially asynchronous.
   */
  aiop->notify_type = NotifyNone;
  aiop->notify.tso = END_TSO_QUEUE;
}

void asyncIOCancelIOURing(CapIOManager *iomgr, StgAsyncIOOp *aiop) {
  /* We can reliably determine if the aiop is still in progress by checking
   * if the aiop_table still points to this aiop object. This is reliable
   * because each aiop is GC heap allocated, so cannot be recycled until it
   * is no longer retained by the application.
   */
  ASSERT(aiop->notify_type != NotifyTSO);
  if (indexClosureTable(&iomgr->aiop_table, aiop->index) == aiop) {
    ioCancel(iomgr, aiop);
    notifyIOCompletion(iomgr, aiop);
  }
}

static void ioCancel(CapIOManager *iomgr, StgAsyncIOOp *aiop) {
  barf("URing.c:ioCancel:TODO");

  int ix = aiop->index;
  struct io_uring_sqe *sqe = io_uring_get_sqe(iomgr->uring);
  // TODO: return status to indicate if we need to return to the scheduler
  //  to flush a full submission queue

  /* io_uring lets us include one word into submission queue entries (SQEs),
   * which come back in the corresponding completion queue entry (CQE). We
   * use this feature to identify the aiop so that we will be able to
   * process the completion properly, e.g. waking up the right thread.
   * We can't use a direct pointer to the aiop because the aiops are heap
   * allocated and GC pointers are not stable. We use the index in the
   * ClosureTable, because this is stable. Indeed the raison d'être of the
   * ClosureTable is to provide stable pointers for thus purpose.
   //TODO: the above is a helpful comment but it belongs elsewhere, e.g.
   //in the intro.
   */
  uint64_t sqe_data = ix;

  /* Although IORING_OP_POLL_ADD has a special separate cancellation using
   * IORING_OP_POLL_REMOVE, apparently it can also be cancelled using the
   * generic IORING_OP_ASYNC_CANCEL, which is good so we don't need to
   * distinguish.
   */
  io_uring_prep_cancel64(sqe, sqe_data, 0 /*flags*/);
  // TODO: if we use other tags in the sqe_data we'll need to reconstruct
  //  them here so we can find the right item. e.g. if we use AIOP_TAG_OP_NB
  //  we'd need to distinguish in the aiop->flags for example.

  /* Cancelling is itself a new uring I/O operation which will have a
   * corresponding completion. Set a high bit to mark this as a cancel
   * operation, but still knowing the index of the original operation.
   */
  io_uring_sqe_set_data64(sqe, sqe_data | AIOP_TAG_CANCEL);

  // TODO: verify for IORING_OP_POLL_ADD, if we do a successful
  //  IORING_OP_POLL_REMOVE then which completions do we get?
  //  Do we get a completion for the IORING_OP_POLL_ADD, and if so with
  //  what result? Or does a successful IORING_OP_POLL_REMOVE mean we
  //  only get a completion for the remove and not the add?

  // TODO: verify similar for normal I/O cancel, e.g. a read on a pipe.
  //  Which completions do we get if we cancel?

  // TODO: is it ok cancel I/O asynchronously here? Do we need to submit the
  //  cancel op? The cancel op will complete synchronously but the cancellation
  //  may only complete later. This might confuse resource cleanup, e.g. because
  //  a file will not get closed until the cancel finishes.
}

/* TODO: need to add support for closing properly.
 * Unfortunately, uring's behaviour for closing a fd when there are outstanding
 * poll (or other async I/O) operations on that fd is unhelpful. The poll
 * operation itself keeps a reference to the file open. Thus the close will
 * not in fact interrupt and cancel the poll.
 * So the I/O manager needs to be notified of fd close, so that we can do
 * something. Fortunately we can use io_uring_prep_cancel_fd to cancel all
 * operations on an fd.
 *
 * I think cancelled ops _do_ generate CQEs. So we should be able to do the
 * appropriate notifications by waiting for the original CQEs. We should
 * probably issue the cancellation
 *
 */

/******************************************************************************
 * The functions called from the scheduler to poll or wait for pending I/O,
 * and process any I/O completions.
 */

bool anyPendingTimeoutsOrIOIOURing(CapIOManager *iomgr) {
  return !isEmptyTimeoutQueue(iomgr->timeout_queue) ||
         !isEmptyClosureTable(&iomgr->aiop_table);
}

static void notifyIOCompletion(CapIOManager *iomgr, StgAsyncIOOp *aiop) {
  switch (aiop->notify_type) {
  case NotifyTSO: {
    if (aiop->outcome == IOOpOutcomeFailed && aiop->error == EBADF) {
      /* The fd is invalid: raise an IOError exception in the blocked
       * thread. (See bug #4934 for what happens without this.)
       */
      StgTSO *tso = aiop->notify.tso;
      debugTrace(DEBUG_iomanager,
                 "Raising exception in thread %" FMT_StgThreadID
                 " blocked on an invalid fd",
                 tso->id);
      raiseAsync(iomgr->cap, tso, (StgClosure *)blockedOnBadFD_closure, false, NULL);
      break;
    } else {
      /* We should be guaranteed that the tso is still on the same
       * cap because the tso was not on the run queue of any cap and
       * so is not subject to thread migration.
       */
      StgTSO *tso = aiop->notify.tso;
      tso->why_blocked = NotBlocked;
      tso->_link = END_TSO_QUEUE;
      pushOnRunQueue(iomgr->cap, tso);
    }
    break;
  }
  case NotifyMVar:
    performTryPutMVar(iomgr->cap, aiop->notify.mvar, Unit_closure);
    break;

  case NotifyTVar:
    barf("uring iomgr: TVar notification not yet supported");
    break;
  }
}

/* Process all the I/O completions that are currently available without
 * blocking.
 *
 * This will correctly deal with completion queue overflow: if the completions
 * queue is empty but has the overflow bit set then io_uring_peek_batch_cqe
 * will do another non-blocking uring enter to refill the completion queue.
 */
static void processIOCompletions(CapIOManager *iomgr) {
  struct io_uring *uring = iomgr->uring;
  while (1) {
    unsigned head, count = 0;
    struct io_uring_cqe *cqe;
    io_uring_for_each_cqe(uring, head, cqe) {
      uint64_t cqe_data = io_uring_cqe_get_data64(cqe);
      if (RTS_UNLIKELY(cqe_data & AIOP_TAG_CANCEL)) {
        // This is a CQE for a cancellation. After posting a cancellation SQE
        // then we expect to get _both_ a cancellation CQE and a CQE for the
        // original operation that was the target of cancellation. This means
        // that (provided it's not an error) we can ignore the cancellation SQE
        // and just process the normal SQE for the target operation.
        //
        // The target operation will either be cancelled successfully
        // immediately (in which case the cancellation cqe->res == 0) or if the
        // target operation is in progress and cannot be cancelled, then we'll
        // get cqe->res == -EALREADY for the cancellation the result of the
        // operation (which may still be an interrupted outcome. If the
        // cancellation fails, we
        if (cqe->res != 0 && cqe->res != -EALREADY)
            sysErrorBelch("uring:processIOCompletions: unknown cancel CQE result");
      } else {
        int ix = cqe_data;
        StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, ix);
        removeClosureTable(iomgr->cap, &iomgr->aiop_table, ix);
        aiop->result = cqe->res;
        // TODO: if we use these tags in the sqe_data we'll need to reconstruct
        //  them for cancel so we can find the right item. e.g. if we use
        //  AIOP_TAG_OP_NB we'd need to distinguish in the aiop->flags for
        //  example.
        if (cqe_data & AIOP_TAG_OP_NB) {
          iomgr->n_inflight_nb--;
          iomgr->n_submitted_nb--;
        } else {
          iomgr->n_inflight_b--;
          iomgr->n_submitted_b--;
        }
        notifyIOCompletion(iomgr, aiop);
      }
      count++;
    }
    io_uring_cq_advance(uring, count);
    if (RTS_UNLIKELY(io_uring_cq_has_overflow(uring))) {
      if (io_uring_get_events(uring) < 0) {
        sysErrorBelch("io_uring_enter");
        stg_exit(EXIT_FAILURE);
      }
      continue;
    } else {
      break;
    }
  }
}

/* Check invariants that must hold on entry to and exit from the scheduler.
 * Used before/after {poll,await}CompletedTimeoutsOrIOURing which are called
 * from the scheduler.
 */
static void assertURingSchedulerInvariants(CapIOManager *iomgr) {
  struct io_uring *uring = iomgr->uring;

  // That our tracking counters are consistent.
  ASSERT(iomgr->n_submitted_b == iomgr->n_prepared_b + iomgr->n_inflight_b);
  ASSERT(iomgr->n_submitted_nb ==
         iomgr->n_prepared_nb + iomgr->n_overflow_nb + iomgr->n_inflight_nb);

  // That we are within limits
  ASSERT(iomgr->n_inflight_b <= iomgr->limit_inflight_b);
  ASSERT(iomgr->n_inflight_nb <= iomgr->limit_inflight_nb);

  // That we correctly track the submission queue size.
  ASSERT((int)io_uring_sq_ready(uring) ==
         iomgr->n_prepared_b + iomgr->n_prepared_nb);

  // That our overflow queue is consistent with the overflow counter.
  ASSERT(iomgr->n_overflow_nb > 0
             ? iomgr->overflow_sqe_q_hd == NULL &&
                   iomgr->overflow_sqe_q_tl == NULL &&
                   iomgr->overflow_tso_q_hd == END_TSO_QUEUE &&
                   iomgr->overflow_tso_q_tl == END_TSO_QUEUE
             : iomgr->n_overflow_nb == 0 && iomgr->overflow_sqe_q_hd != NULL &&
                   iomgr->overflow_sqe_q_tl != NULL &&
                   iomgr->overflow_tso_q_hd != END_TSO_QUEUE &&
                   iomgr->overflow_tso_q_tl != END_TSO_QUEUE);
}

/* If there are any completed I/O operations or expired timers, process the
 * completions as appropriate. If there are none, return without waiting.
 *
 * This is the non-blocking variant. See awaitCompletedTimeoutsOrIOURing
 * for the potentially-blocking variant.
 */
void pollCompletedTimeoutsOrIOIOURing(CapIOManager *iomgr) {
  struct io_uring *uring = iomgr->uring;

  assertURingSchedulerInvariants(iomgr);

  /* Process timeouts, if any, but don't immediately return to the scheduler,
   * since we should submit I/O and reap any completions too.
   */
  if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
    Time now = getProcessElapsedTime();
    processTimeoutCompletions(iomgr, now);
  }

  /* Submit I/O if needed */
  if (io_uring_sq_ready(uring)) {
    int res = io_uring_submit_and_get_events(uring);

    if (RTS_UNLIKELY(res < 0)) {
      if (res == -EBUSY) {
        /* This is an odd one. According to the doc:
         * If the IORING_FEAT_NODROP feature flag is set, then EBUSY
         * will be returned if there were overflow entries,
         * IORING_ENTER_GETEVENTS flag is set and not all of the
         * overflow entries were able to be flushed to the CQ ring.
         *
         * So it's not really an error at all. It just means we will
         * have to do multiple iterations in processIOCompletions()
         * to collect all the completions.
         *
         * Thus EBUSY should imply that there are entries in the CQ.
         */
        ASSERT(io_uring_cq_ready(uring) > 0);
      }
    } else {
      ASSERT(res == iomgr->n_prepared_b + iomgr->n_prepared_nb);
      /* We're using IORING_SETUP_SUBMIT_ALL so we should expect to have
       * all of them submitted, or an error.
       * https://github.com/axboe/liburing/issues/186
       * Alternatively, we could loop and submit the remainder.
       */
      iomgr->n_inflight_b += iomgr->n_prepared_b;
      iomgr->n_inflight_nb += iomgr->n_prepared_nb;
      iomgr->n_prepared_b = 0;
      iomgr->n_prepared_nb = 0;
    }
  }

  if (io_uring_cq_ready(uring)) {
    processIOCompletions(iomgr);
  }
  // TODO: now we need to check if we have any items in our overflow queue
  // and if so, we need to copy some of those into the SQ and submit them.
  // copy in up to either the SQ limit or in-flight limit.

  assertURingSchedulerInvariants(iomgr);
}

/* If there are any completed I/O operations or expired timers, process the
 * completions as appropriate. If there are none, wait until I/O or a timer
 * does complete (or we get a signal with a handler) and process the
 * completions as appropriate.
 *
 * This is the potentially-blocking variant. See pollCompletedTimeoutsOrIOURing
 * for the non-blocking variant.
 */
bool awaitCompletedTimeoutsOrIOIOURing(CapIOManager *iomgr) {
  struct io_uring *uring = iomgr->uring;

  assertURingSchedulerInvariants(iomgr);

  do {

    /* We're being asked (by the scheduler) to block if there's no
     * immediate timer or I/O completions. So there had better be
     * some pending I/O or pending timers, or we'd deadlock.
     */
    ASSERT(!isEmptyTimeoutQueue(iomgr->timeout_queue) ||
           !isEmptyClosureTable(&iomgr->aiop_table));

    Time now = getProcessElapsedTime();
    processTimeoutCompletions(iomgr, now);

    /* If we didn't wake any threads due to expiring timeouts, then we need
     * to wait on I/O. Or to put it another way, even if we did wake some
     * threads, we'll still poll (but not wait) for I/O. This is to ensure
     * we avoid starving threads blocked on I/O.
     */
    bool wait = emptyRunQueue(iomgr->cap);

    /* There are four possible cases:
     * 1. non-blocking check for I/O completion with I/O submission
     * 2. non-blocking check for I/O completion with no I/O submission
     * 3. blocking wait for I/O completion with a timeout
     * 4. blocking wait for I/O completion without a timeout
     *
     * If we woke any threads due to timeouts we're in the first or second
     * case.
     *
     * Otherwise we're in one of the blocking cases. We will use a timeout
     * if the timeout queue is non-empty.
     */

    int res;
    if (!wait && io_uring_sq_ready(uring)) {
      /* Case 1 (as above) */
      res = io_uring_submit_and_get_events(uring);
    } else if (!wait) {
      /* Case 2 (as above) */
      res = io_uring_get_events(uring);
    } else {
      struct timespec tv;
      if (timeoutInNanoseconds(iomgr, true, now, &tv)) {
        /* Case 3 (as above) */
        /* struct timespec and struct __kernel_timespec are compatible
         * but not exactly the same. Sigh. */
        struct __kernel_timespec ts = {.tv_sec = tv.tv_sec,
                                       .tv_nsec = tv.tv_nsec};
        struct io_uring_cqe *cqe_unused;
        res =
            io_uring_submit_and_wait_timeout(uring, &cqe_unused, 1, &ts, NULL);
      } else {
        /* Case 4 (as above) */
        res = io_uring_submit_and_wait(uring, 1);
      }
    }

    if (res >= 0) {
      processIOCompletions(iomgr);
    } else if (errno == EINTR) {

    } else if (errno == EBUSY || errno == EAGAIN) {

    } else {
      sysErrorBelch("io_uring_enter");
      stg_exit(EXIT_FAILURE);
    }
  } while (emptyRunQueue(iomgr->cap) &&
           (!isEmptyClosureTable(&iomgr->aiop_table) ||
            !isEmptyTimeoutQueue(iomgr->timeout_queue)) &&
           getSchedState() == SCHED_RUNNING);

  assertURingSchedulerInvariants(iomgr);

  return true; // TODO return false if interrupted
}

void interruptIOManagerIOURing(CapIOManager *iomgr) {
  barf("interruptIOManagerIOURing:TODO");

  // Here are 2 possible ways to implement interrupt:
  //
  // * On initialization, create a new FD with newFdWakeup() and submit a
  //   recurring POLL SQE. Then on interrupt, call sendFdWakeup() to foce a CQE
  //   to be generated.
  // * Submit a NOP (see io_uring_prep_nop()) SQE. This should produce a trivial
  //   NOP CQE. The issue with this is that the SQE may already be full, which
  //   requires some solution.
}

/******************************************************************************
 * Local helper utilities
 */

static bool enlargeTables(CapIOManager *iomgr) {
  int oldcapacity = capacityClosureTable(&iomgr->aiop_table);
  int newcapacity = (oldcapacity == 0) ? 1 : (oldcapacity * 2);
  return enlargeClosureTable(iomgr->cap, &iomgr->aiop_table, newcapacity);
}

/*
 */
static void enqueueOverflowQueue(CapIOManager *iomgr,
                                 StgTSO *tso, struct io_uring_sqe *sqe) {
  /* Append the TSO to the tail of the overflow queue of TSOs. */
  ASSERT(tso->_link == END_TSO_QUEUE);
  if (iomgr->overflow_tso_q_hd == END_TSO_QUEUE) {
    iomgr->overflow_tso_q_hd = tso;
  } else {
    setTSOLink(iomgr->cap, iomgr->overflow_tso_q_tl, tso);
  }
  iomgr->overflow_tso_q_tl = tso;

  /* And append the SQE to the tail of the overflow queue of SQEs. */
  struct overflow_sqe_q_t *entry;
  entry = stgMallocBytes(sizeof(struct overflow_sqe_q_t), "uring iomgr");
  *entry = (struct overflow_sqe_q_t){.sqe = sqe,
                                     .next = NULL,
#if defined(DEBUG)
                                     .tid = tso->id
#endif
  };
  if (iomgr->overflow_sqe_q_hd == NULL) {
    iomgr->overflow_sqe_q_hd = entry;
  } else {
    iomgr->overflow_sqe_q_tl->next = entry;
  }
  iomgr->overflow_sqe_q_tl = entry;
}

static void dequeueOverflowQueue(CapIOManager *iomgr, StgTSO **ptso,
                                 struct io_uring_sqe **psqe) {
  /* Remove the TSO and SQE from the head of their respective queues */
  StgTSO *tso = iomgr->overflow_tso_q_hd;
  struct overflow_sqe_q_t *entry = iomgr->overflow_sqe_q_hd;

  if (tso == END_TSO_QUEUE) {
    // TODO: decide if we need this or if we should assume the queue is
    //  non-empty
    ASSERT(entry == NULL);
    *ptso = END_TSO_QUEUE;
    *psqe = NULL;
  } else {
    iomgr->overflow_tso_q_hd = tso->_link;
    RELAXED_STORE(&tso->_link, END_TSO_QUEUE);
    if (iomgr->overflow_tso_q_hd == END_TSO_QUEUE) {
      iomgr->overflow_tso_q_tl = END_TSO_QUEUE;
    }

    iomgr->overflow_sqe_q_hd = entry->next;
    if (iomgr->overflow_sqe_q_hd == NULL) {
      iomgr->overflow_sqe_q_tl = NULL;
    }

    *ptso = tso;
    *psqe = entry->sqe;
  }
}

#endif /* IOMGR_ENABLED_IO_URING */
