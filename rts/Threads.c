/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Updates.h"
#include "Threads.h"
#include "STM.h"
#include "Schedule.h"
#include "Trace.h"
#include "ThreadLabels.h"
#include "Updates.h"
#include "Messages.h"
#include "RaiseAsync.h"
#include "Prelude.h"
#include "Printer.h"
#include "sm/Sanity.h"
#include "sm/Storage.h"

#include <string.h>

/* Next thread ID to allocate.
 * LOCK: sched_mutex
 */
static StgThreadID next_thread_id = 1;

/* The smallest stack size that makes any sense is:
 *    RESERVED_STACK_WORDS    (so we can get back from the stack overflow)
 *  + sizeofW(StgStopFrame)   (the stg_stop_thread_info frame)
 *  + 1                       (the closure to enter)
 *  + 1			      (stg_ap_v_ret)
 *  + 1			      (spare slot req'd by stg_ap_v_ret)
 *
 * A thread with this stack will bomb immediately with a stack
 * overflow, which will increase its stack size.
 */
#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 3)

/* ---------------------------------------------------------------------------
   Create a new thread.

   The new thread starts with the given stack size.  Before the
   scheduler can run, however, this thread needs to have a closure
   (and possibly some arguments) pushed on its stack.  See
   pushClosure() in Schedule.h.

   createGenThread() and createIOThread() (in SchedAPI.h) are
   convenient packaged versions of this function.

   currently pri (priority) is only used in a GRAN setup -- HWL
   ------------------------------------------------------------------------ */
StgTSO *
createThread(Capability *cap, W_ size)
{
  StgTSO *tso;
  StgStack *stack;
  nat stack_size;

  /* sched_mutex is *not* required */

  /* catch ridiculously small stack sizes */
  if (size < MIN_STACK_WORDS + sizeofW(StgStack) + sizeofW(StgTSO)) {
    size = MIN_STACK_WORDS + sizeofW(StgStack) + sizeofW(StgTSO);
  }

  /* The size argument we are given includes all the per-thread
   * overheads:
   *
   *    - The TSO structure
   *    - The STACK header
   *
   * This is so that we can use a nice round power of 2 for the
   * default stack size (e.g. 1k), and if we're allocating lots of
   * threads back-to-back they'll fit nicely in a block.  It's a bit
   * of a benchmark hack, but it doesn't do any harm.
   */
  stack_size = round_to_mblocks(size - sizeofW(StgTSO));
  stack = (StgStack *)allocate(cap, stack_size);
  TICK_ALLOC_STACK(stack_size);
  SET_HDR(stack, &stg_STACK_info, CCS_SYSTEM);
  stack->stack_size   = stack_size - sizeofW(StgStack);
  stack->sp           = stack->stack + stack->stack_size;
  stack->dirty        = 1;

  tso = (StgTSO *)allocate(cap, sizeofW(StgTSO));
  TICK_ALLOC_TSO();
  SET_HDR(tso, &stg_TSO_info, CCS_SYSTEM);

  // Always start with the compiled code evaluator
  tso->what_next = ThreadRunGHC;
  tso->why_blocked  = NotBlocked;
  tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;

  tso->schedule_scont_action  = (StgClosure*)defaultUpcall_closure;
  tso->yield_control_action = (StgClosure*)defaultUpcall_closure;
  tso->finalizer      = (StgClosure*)defaultUpcall_closure;
  tso->scont_status   = stmNewTVar (cap, (StgClosure*)initSContStatus_closure);
  tso->tls = (StgClosure*)END_TSO_QUEUE;

  tso->blocked_exceptions = END_BLOCKED_EXCEPTIONS_QUEUE;
  tso->bq = (StgBlockingQueue *)END_TSO_QUEUE;
  tso->flags = 0;
  tso->dirty = 1;
  tso->is_upcall_thread = 0;
  tso->is_sleeping = 0;
  tso->_link = END_TSO_QUEUE;

  tso->saved_errno = 0;
  tso->bound = NULL;
  tso->cap = cap;

  tso->stackobj       = stack;
  tso->tot_stack_size = stack->stack_size;

  tso->trec = NO_TREC;

#ifdef PROFILING
  tso->prof.cccs = CCS_MAIN;
#endif

  // put a stop frame on the stack
  stack->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure*)stack->sp,
          (StgInfoTable *)&stg_stop_thread_info,CCS_SYSTEM);

  /* Link the new thread on the global thread list.
  */
  ACQUIRE_LOCK(&sched_mutex);
  tso->id = next_thread_id++;  // while we have the mutex
  tso->global_link = g0->threads;
  g0->threads = tso;
  RELEASE_LOCK(&sched_mutex);

  // ToDo: report the stack size in the event?
  traceEventCreateThread(cap, tso);

  return tso;
}

/* ---------------------------------------------------------------------------
 * Comparing Thread ids.
 *
 * This is used from STG land in the implementation of the
 * instances of Eq/Ord for ThreadIds.
 * ------------------------------------------------------------------------ */

  int
cmp_thread(StgPtr tso1, StgPtr tso2)
{
  StgThreadID id1 = ((StgTSO *)tso1)->id;
  StgThreadID id2 = ((StgTSO *)tso2)->id;

  if (id1 < id2) return (-1);
  if (id1 > id2) return 1;
  return 0;
}

/* ---------------------------------------------------------------------------
 * Fetching the ThreadID from an StgTSO.
 *
 * This is used in the implementation of Show for ThreadIds.
 * ------------------------------------------------------------------------ */
  int
rts_getThreadId(StgPtr tso)
{
  return ((StgTSO *)tso)->id;
}

/* -----------------------------------------------------------------------------
   Remove a thread from a queue.
   Fails fatally if the TSO is not on the queue.
   -------------------------------------------------------------------------- */

  rtsBool // returns True if we modified queue
removeThreadFromQueue (Capability *cap, StgTSO **queue, StgTSO *tso)
{
  StgTSO *t, *prev;

  prev = NULL;
  for (t = *queue; t != END_TSO_QUEUE; prev = t, t = t->_link) {
    if (t == tso) {
      if (prev) {
        setTSOLink(cap,prev,t->_link);
        t->_link = END_TSO_QUEUE;
        return rtsFalse;
      } else {
        *queue = t->_link;
        t->_link = END_TSO_QUEUE;
        return rtsTrue;
      }
    }
  }
  barf("removeThreadFromQueue: not found");
}

  rtsBool // returns True if we modified head or tail
removeThreadFromDeQueue (Capability *cap,
                         StgTSO **head, StgTSO **tail, StgTSO *tso)
{
  StgTSO *t, *prev;
  rtsBool flag = rtsFalse;

  prev = NULL;
  for (t = *head; t != END_TSO_QUEUE; prev = t, t = t->_link) {
    if (t == tso) {
      if (prev) {
        setTSOLink(cap,prev,t->_link);
        flag = rtsFalse;
      } else {
        *head = t->_link;
        flag = rtsTrue;
      }
      t->_link = END_TSO_QUEUE;
      if (*tail == tso) {
        if (prev) {
          *tail = prev;
        } else {
          *tail = END_TSO_QUEUE;
        }
        return rtsTrue;
      } else {
        return flag;
      }
    }
  }
  barf("removeThreadFromMVarQueue: not found");
}

/* ----------------------------------------------------------------------------
   setOwningCapability ()

   Change a threads owning capability. Thread must belong to this capability and
   must not be running.

   ------------------------------------------------------------------------- */

void
setOwningCapability (Capability *cap USED_IF_DEBUG,
                     StgTSO *tso,
                     nat target) {
  ASSERT (cap == tso->cap);
  debugTrace (DEBUG_sched, "cap %d: Setting the capability of thread %d to %d",
              cap->no, tso->id, target);
  tso->cap = &capabilities[target];
}

/* ----------------------------------------------------------------------------
   tryWakeupThread()

   Attempt to wake up a thread.  tryWakeupThread is idempotent: it is
   always safe to call it too many times, but it is not safe in
   general to omit a call.

   ------------------------------------------------------------------------- */

  void
tryWakeupThread (Capability *cap, StgTSO *tso)
{
  traceEventThreadWakeup (cap, tso, tso->cap->no);

#ifdef THREADED_RTS
  if (tso->cap != cap)
  {
    MessageWakeup *msg;
    msg = (MessageWakeup *)allocate(cap,sizeofW(MessageWakeup));
    SET_HDR(msg, &stg_MSG_TRY_WAKEUP_info, CCS_SYSTEM);
    msg->tso = tso;
    sendMessage(cap, tso->cap, (Message*)msg);
    debugTraceCap(DEBUG_sched, cap, "message: try wakeup thread %ld on cap %d",
                  (W_)tso->id, tso->cap->no);
    return;
  }
#endif

  switch (tso->why_blocked)
  {
    case BlockedOnMVar:
      {
        if (tso->_link == END_TSO_QUEUE) {
          tso->block_info.closure = (StgClosure*)END_TSO_QUEUE;
          if (hasHaskellScheduler (tso))
            goto unblock1;
          else
            goto unblock2;
        } else {
          return;
        }
      }

    case BlockedOnMsgThrowTo:
      {
        const StgInfoTable *i;

        i = lockClosure(tso->block_info.closure);
        unlockClosure(tso->block_info.closure, i);
        if (i != &stg_MSG_NULL_info) {
          debugTraceCap(DEBUG_sched, cap, "thread %ld still blocked on throwto (%p)",
                        (W_)tso->id, tso->block_info.throwto->header.info);
          return;
        }

        // remove the block frame from the stack
        ASSERT(tso->stackobj->sp[0] == (StgWord)&stg_block_throwto_info);
        tso->stackobj->sp += 3;
        goto unblock2;
      }

    case BlockedOnBlackHole:
      if (hasHaskellScheduler (tso)) //Note: Upcall threads do not have a user-level scheduler
        goto unblock1;
      else
        goto unblock2;

    case BlockedOnSTM:
      if (tso->is_sleeping != 0) {
        tso->is_sleeping = 0;
        goto unblock2;
      }
      else
        goto unblock1;

    case ThreadMigrating:
      goto unblock2;

    case BlockedInHaskell:
    case Yielded:
    default:
      // otherwise, do nothing
      return;
  }

unblock1:
  tso->why_blocked = Yielded;
  pushUpcallReturning (cap, getResumeThreadUpcall (cap, tso));
  return;

unblock2:
  // just run the thread now, if the BH is not really available,
  // we'll block again.
  tso->why_blocked = NotBlocked;
  appendToRunQueue(cap,tso);
  return;
}


/* ----------------------------------------------------------------------------
   pushCallToClosure
   ------------------------------------------------------------------------- */

void
pushCallToClosure (Capability *cap, StgTSO *tso, StgClosure *closure) {
  StgStack *stack;
  StgPtr sp;

  // ASSUMES: the thread is not already complete or dead
  // Upper layers should deal with that.
  ASSERT(tso->what_next != ThreadComplete &&
         tso->what_next != ThreadKilled);

  // only if we own this TSO (except that deleteThread() calls this
  ASSERT(tso->cap == cap);

  stack = tso->stackobj;

  // mark it dirty; we're about to change its stack.
  dirty_TSO(cap, tso);
  dirty_STACK(cap, stack);

  sp = stack->sp;

  /* Check if the stack has enough space. Otherwise, grow the stack. */
  if (sp - 3*sizeof(W_) < tso_SpLim (tso)) {
    threadStackOverflow (cap, tso);
    stack = tso->stackobj;
    sp = stack->sp;
  }

  pushClosure(tso, (W_)&stg_ap_v_info);
  pushClosure(tso, (W_)closure);
  pushClosure(tso, (W_)&stg_enter_info);
}



/* ----------------------------------------------------------------------------
   migrateThread
   ------------------------------------------------------------------------- */

  void
migrateThread (Capability *from, StgTSO *tso, Capability *to)
{
  traceEventMigrateThread (from, tso, to->no);
  // ThreadMigrating tells the target cap that it needs to be added to
  // the run queue when it receives the MSG_TRY_WAKEUP.
  tso->why_blocked = ThreadMigrating;
  tso->cap = to;
  tryWakeupThread(from, tso);
}

/* ----------------------------------------------------------------------------
   awakenBlockedQueue

   wakes up all the threads on the specified queue.
   ------------------------------------------------------------------------- */

  void
wakeBlockingQueue(Capability *cap, StgBlockingQueue *bq)
{
  MessageBlackHole *msg;
  const StgInfoTable *i;

  ASSERT(bq->header.info == &stg_BLOCKING_QUEUE_DIRTY_info  ||
         bq->header.info == &stg_BLOCKING_QUEUE_CLEAN_info  );

  for (msg = bq->queue; msg != (MessageBlackHole*)END_TSO_QUEUE;
       msg = msg->link) {
    i = msg->header.info;
    if (i != &stg_IND_info) {
      ASSERT(i == &stg_MSG_BLACKHOLE_info);
      tryWakeupThread (cap, msg->tso);
    }
  }

  // overwrite the BQ with an indirection so it will be
  // collected at the next GC.
#if defined(DEBUG) && !defined(THREADED_RTS)
  // XXX FILL_SLOP, but not if THREADED_RTS because in that case
  // another thread might be looking at this BLOCKING_QUEUE and
  // checking the owner field at the same time.
  bq->bh = 0; bq->queue = 0; bq->owner = 0;
#endif
  OVERWRITE_INFO(bq, &stg_IND_info);
}

// If we update a closure that we know we BLACKHOLE'd, and the closure
// no longer points to the current TSO as its owner, then there may be
// an orphaned BLOCKING_QUEUE closure with blocked threads attached to
// it.  We therefore traverse the BLOCKING_QUEUEs attached to the
// current TSO to see if any can now be woken up.
void
checkBlockingQueues (Capability *cap, StgTSO *tso)
{
  StgBlockingQueue *bq, *next;
  StgClosure *p;

  debugTraceCap(DEBUG_sched, cap,
                "collision occurred; checking blocking queues for thread %ld",
                (W_)tso->id);

  for (bq = tso->bq; bq != (StgBlockingQueue*)END_TSO_QUEUE; bq = next) {
    next = bq->link;

    if (bq->header.info == &stg_IND_info) {
      // ToDo: could short it out right here, to avoid
      // traversing this IND multiple times.
      continue;
    }

    p = bq->bh;

    if (p->header.info != &stg_BLACKHOLE_info ||
        ((StgInd *)p)->indirectee != (StgClosure*)bq)
    {
      wakeBlockingQueue(cap,bq);
    }
  }
}

/* ----------------------------------------------------------------------------
   updateThunk

   Update a thunk with a value.  In order to do this, we need to know
   which TSO owns (or is evaluating) the thunk, in case we need to
   awaken any threads that are blocked on it.
   ------------------------------------------------------------------------- */

  void
updateThunk (Capability *cap, StgTSO *tso, StgClosure *thunk, StgClosure *val)
{
  StgClosure *v;
  StgTSO *owner;
  const StgInfoTable *i;

  i = thunk->header.info;
  if (i != &stg_BLACKHOLE_info &&
      i != &stg_CAF_BLACKHOLE_info &&
      i != &__stg_EAGER_BLACKHOLE_info &&
      i != &stg_WHITEHOLE_info) {
    updateWithIndirection(cap, thunk, val);
    return;
  }

  v = ((StgInd*)thunk)->indirectee;

  updateWithIndirection(cap, thunk, val);

  // sometimes the TSO is locked when we reach here, so its header
  // might be WHITEHOLE.  Hence check for the correct owner using
  // pointer equality first.
  if ((StgTSO*)v == tso) {
    return;
  }

  i = v->header.info;
  if (i == &stg_TSO_info) {
    checkBlockingQueues(cap, tso);
    return;
  }

  if (i != &stg_BLOCKING_QUEUE_CLEAN_info &&
      i != &stg_BLOCKING_QUEUE_DIRTY_info) {
    checkBlockingQueues(cap, tso);
    return;
  }

  owner = ((StgBlockingQueue*)v)->owner;

  if (owner != tso) {
    checkBlockingQueues(cap, tso);
  } else {
    wakeBlockingQueue(cap, (StgBlockingQueue*)v);
  }
}

/* ---------------------------------------------------------------------------
 * rtsSupportsBoundThreads(): is the RTS built to support bound threads?
 * used by Control.Concurrent for error checking.
 * ------------------------------------------------------------------------- */

  HsBool
rtsSupportsBoundThreads(void)
{
#if defined(THREADED_RTS)
  return HS_BOOL_TRUE;
#else
  return HS_BOOL_FALSE;
#endif
}

/* ---------------------------------------------------------------------------
 * isThreadBound(tso): check whether tso is bound to an OS thread.
 * ------------------------------------------------------------------------- */

  StgBool
isThreadBound(StgTSO* tso USED_IF_THREADS)
{
#if defined(THREADED_RTS)
  return (tso->bound != NULL);
#endif
  return rtsFalse;
}

/* ---------------------------------------------------------------------------
 * check whether the given thread is attached to a user-level scheduler
 * ------------------------------------------------------------------------- */

rtsBool
hasHaskellScheduler (StgTSO* tso) {
  return (//Not upcall thread
          tso->is_upcall_thread == rtsFalse &&
          //has switch to next action
          tso->yield_control_action != (StgClosure*)defaultUpcall_closure &&
          //has unblock thread actions
          tso->schedule_scont_action != (StgClosure*)defaultUpcall_closure);
}

/* -----------------------------------------------------------------------------
 * Check if the capability is going to sleep inside a PTM action
 * -------------------------------------------------------------------------- */

rtsBool
isThreadSleeping (StgTSO* tso) {
  return tso->is_sleeping;
}

/* -----------------------------------------------------------------------------
   Stack overflow

   If the thread has reached its maximum stack size, then raise the
   StackOverflow exception in the offending thread.  Otherwise
   relocate the TSO into a larger chunk of memory and adjust its stack
   size appropriately.
   -------------------------------------------------------------------------- */

  void
threadStackOverflow (Capability *cap, StgTSO *tso)
{
    StgStack *new_stack, *old_stack;
    StgUnderflowFrame *frame;
    W_ chunk_size;

    IF_DEBUG(sanity,checkTSO(tso));

    if (tso->tot_stack_size >= RtsFlags.GcFlags.maxStkSize
        && !(tso->flags & TSO_BLOCKEX)) {
        // NB. never raise a StackOverflow exception if the thread is
        // inside Control.Exceptino.block.  It is impractical to protect
        // against stack overflow exceptions, since virtually anything
        // can raise one (even 'catch'), so this is the only sensible
        // thing to do here.  See bug #767.
        //

        if (tso->flags & TSO_SQUEEZED) {
            return;
        }
        // #3677: In a stack overflow situation, stack squeezing may
        // reduce the stack size, but we don't know whether it has been
        // reduced enough for the stack check to succeed if we try
        // again.  Fortunately stack squeezing is idempotent, so all we
        // need to do is record whether *any* squeezing happened.  If we
        // are at the stack's absolute -K limit, and stack squeezing
        // happened, then we try running the thread again.  The
        // TSO_SQUEEZED flag is set by threadPaused() to tell us whether
        // squeezing happened or not.

        debugTrace(DEBUG_gc,
                   "threadStackOverflow of TSO %ld (%p): stack too large (now %ld; max is %ld)",
                   (long)tso->id, tso, (long)tso->stackobj->stack_size,
                   RtsFlags.GcFlags.maxStkSize);
        IF_DEBUG(gc,
                 /* If we're debugging, just print out the top of the stack */
                 printStackChunk(tso->stackobj->sp,
                                 stg_min(tso->stackobj->stack + tso->stackobj->stack_size,
                                         tso->stackobj->sp+64)));

        // Send this thread the StackOverflow exception
        throwToSingleThreaded(cap, tso, (StgClosure *)stackOverflow_closure);
    }
    // #3677: In a stack overflow situation, stack squeezing may
    // reduce the stack size, but we don't know whether it has been
    // reduced enough for the stack check to succeed if we try
    // again.  Fortunately stack squeezing is idempotent, so all we
    // need to do is record whether *any* squeezing happened.  If we
    // are at the stack's absolute -K limit, and stack squeezing
    // happened, then we try running the thread again.  The
    // TSO_SQUEEZED flag is set by threadPaused() to tell us whether
    // squeezing happened or not.

    debugTrace(DEBUG_gc,
               "threadStackOverflow of TSO %ld (%p): stack too large (now %ld; max is %ld)",
               (long)tso->id, tso, (long)tso->stackobj->stack_size,
               RtsFlags.GcFlags.maxStkSize);
    IF_DEBUG(gc,
             /* If we're debugging, just print out the top of the stack */
             printStackChunk(tso->stackobj->sp,
                             stg_min(tso->stackobj->stack + tso->stackobj->stack_size,
                                     tso->stackobj->sp+64)));

    // Send this thread the StackOverflow exception
    throwToSingleThreaded(cap, tso, (StgClosure *)stackOverflow_closure);
  }


  // We also want to avoid enlarging the stack if squeezing has
  // already released some of it.  However, we don't want to get into
  // a pathalogical situation where a thread has a nearly full stack
  // (near its current limit, but not near the absolute -K limit),
  // keeps allocating a little bit, squeezing removes a little bit,
  // and then it runs again.  So to avoid this, if we squeezed *and*
  // there is still less than BLOCK_SIZE_W words free, then we enlarge
  // the stack anyway.
  if ((tso->flags & TSO_SQUEEZED) &&
      ((W_)(tso->stackobj->sp - tso->stackobj->stack) >= BLOCK_SIZE_W)) {
    return;
  }

  old_stack = tso->stackobj;

  // If we used less than half of the previous stack chunk, then we
  // must have failed a stack check for a large amount of stack.  In
  // this case we allocate a double-sized chunk to try to
  // accommodate the large stack request.  If that also fails, the
  // next chunk will be 4x normal size, and so on.
  //
  // It would be better to have the mutator tell us how much stack
  // was needed, as we do with heap allocations, but this works for
  // now.
  //
  if (old_stack->sp > old_stack->stack + old_stack->stack_size / 2)
  {
    chunk_size = stg_max(2 * (old_stack->stack_size + sizeofW(StgStack)),
                         RtsFlags.GcFlags.stkChunkSize);
  }
  else
  {
    chunk_size = RtsFlags.GcFlags.stkChunkSize;
  }

  debugTraceCap(DEBUG_sched, cap,
                "allocating new stack chunk of size %d bytes",
                chunk_size * sizeof(W_));

  new_stack = (StgStack*) allocate(cap, chunk_size);
  SET_HDR(new_stack, &stg_STACK_info, CCS_SYSTEM);
  TICK_ALLOC_STACK(chunk_size);

  new_stack->dirty = 0; // begin clean, we'll mark it dirty below
  new_stack->stack_size = chunk_size - sizeofW(StgStack);
  new_stack->sp = new_stack->stack + new_stack->stack_size;

  tso->tot_stack_size += new_stack->stack_size;

  {
    StgWord *sp;
    nat chunk_words, size;

    // find the boundary of the chunk of old stack we're going to
    // copy to the new stack.  We skip over stack frames until we
    // reach the smaller of
    //
    //   * the chunk buffer size (+RTS -kb)
    //   * the end of the old stack
    //
    for (sp = old_stack->sp;
         sp < stg_min(old_stack->sp + RtsFlags.GcFlags.stkChunkBufferSize,
                      old_stack->stack + old_stack->stack_size); )
    {
        StgWord *sp;
        W_ chunk_words, size;

        // find the boundary of the chunk of old stack we're going to
        // copy to the new stack.  We skip over stack frames until we
        // reach the smaller of
        //
        //   * the chunk buffer size (+RTS -kb)
        //   * the end of the old stack
        //
        for (sp = old_stack->sp;
             sp < stg_min(old_stack->sp + RtsFlags.GcFlags.stkChunkBufferSize,
                          old_stack->stack + old_stack->stack_size); )
        {
            size = stack_frame_sizeW((StgClosure*)sp);

            // if including this frame would exceed the size of the
            // new stack (taking into account the underflow frame),
            // then stop at the previous frame.
            if (sp + size > old_stack->stack + (new_stack->stack_size -
                                                sizeofW(StgUnderflowFrame))) {
                break;
            }
            sp += size;
        }

    if (sp == old_stack->stack + old_stack->stack_size) {
      //
      // the old stack chunk is now empty, so we do *not* insert
      // an underflow frame pointing back to it.  There are two
      // cases: either the old stack chunk was the last one, in
      // which case it ends with a STOP_FRAME, or it is not the
      // last one, and it already ends with an UNDERFLOW_FRAME
      // pointing to the previous chunk.  In the latter case, we
      // will copy the UNDERFLOW_FRAME into the new stack chunk.
      // In both cases, the old chunk will be subsequently GC'd.
      //
      // With the default settings, -ki1k -kb1k, this means the
      // first stack chunk will be discarded after the first
      // overflow, being replaced by a non-moving 32k chunk.
      //
    } else {
      new_stack->sp -= sizeofW(StgUnderflowFrame);
      frame = (StgUnderflowFrame*)new_stack->sp;
      frame->info = &stg_stack_underflow_frame_info;
      frame->next_chunk  = old_stack;
    }

    // copy the stack chunk between tso->sp and sp to
    //   new_tso->sp + (tso->sp - sp)
    chunk_words = sp - old_stack->sp;

    memcpy(/* dest   */ new_stack->sp - chunk_words,
           /* source */ old_stack->sp,
           /* size   */ chunk_words * sizeof(W_));

    old_stack->sp += chunk_words;
    new_stack->sp -= chunk_words;
  }

  tso->stackobj = new_stack;

  // we're about to run it, better mark it dirty
  dirty_STACK(cap, new_stack);

  IF_DEBUG(sanity,checkTSO(tso));
  // IF_DEBUG(scheduler,printTSO(new_tso));
}


/* ---------------------------------------------------------------------------
   Stack underflow - called from the stg_stack_underflow_info frame
   ------------------------------------------------------------------------ */

W_ // returns offset to the return address
threadStackUnderflow (Capability *cap, StgTSO *tso)
{
  StgStack *new_stack, *old_stack;
  StgUnderflowFrame *frame;
  nat retvals;

  debugTraceCap(DEBUG_sched, cap, "stack underflow");

  old_stack = tso->stackobj;

  frame = (StgUnderflowFrame*)(old_stack->stack + old_stack->stack_size
                               - sizeofW(StgUnderflowFrame));
  ASSERT(frame->info == &stg_stack_underflow_frame_info);

  new_stack = (StgStack*)frame->next_chunk;
  tso->stackobj = new_stack;

  retvals = (P_)frame - old_stack->sp;
  if (retvals != 0)
  {
    // we have some return values to copy to the old stack
    if ((W_)(new_stack->sp - new_stack->stack) < retvals)
    {
      barf("threadStackUnderflow: not enough space for return values");
    }

    new_stack->sp -= retvals;

    memcpy(/* dest */ new_stack->sp,
           /* src  */ old_stack->sp,
           /* size */ retvals * sizeof(W_));
  }

  // empty the old stack.  The GC may still visit this object
  // because it is on the mutable list.
  old_stack->sp = old_stack->stack + old_stack->stack_size;

  // restore the stack parameters, and update tot_stack_size
  tso->tot_stack_size -= old_stack->stack_size;

  // we're about to run it, better mark it dirty
  dirty_STACK(cap, new_stack);

  return retvals;
}

/* ----------------------------------------------------------------------------
 * Debugging: why is a thread blocked
 * ------------------------------------------------------------------------- */

#if DEBUG
  void
printThreadBlockage(StgTSO *tso)
{
  switch (tso->why_blocked) {
#if defined(mingw32_HOST_OS)
    case BlockedOnDoProc:
      debugBelch("is blocked on proc (request: %u)", tso->block_info.async_result->reqID);
      break;
#endif
#if !defined(THREADED_RTS)
    case BlockedOnRead:
      debugBelch("is blocked on read from fd %d", (int)(tso->block_info.fd));
      break;
    case BlockedOnWrite:
      debugBelch("is blocked on write to fd %d", (int)(tso->block_info.fd));
      break;
    case BlockedOnDelay:
      debugBelch("is blocked until %ld", (long)(tso->block_info.target));
      break;
#endif
    case BlockedOnMVar:
      debugBelch("is blocked on an MVar @ %p", tso->block_info.closure);
      break;
    case BlockedOnBlackHole:
      debugBelch("is blocked on a black hole %p",
                 ((StgBlockingQueue*)tso->block_info.bh->bh));
      break;
    case BlockedOnMsgThrowTo:
      debugBelch("is blocked on a throwto message");
      break;
    case NotBlocked:
      debugBelch("is not blocked");
      break;
    case ThreadMigrating:
      debugBelch("is runnable, but not on the run queue");
      break;
    case BlockedOnCCall:
      debugBelch("is blocked on an external call");
      break;
    case BlockedOnCCall_Interruptible:
      debugBelch("is blocked on an external call (but may be interrupted)");
      break;
    case BlockedOnSTM:
      debugBelch("is blocked on an STM operation");
      break;
    case BlockedInHaskell:
      debugBelch("is blocked on a user-level concurrent data structure");
      break;
    case Yielded:
      debugBelch("is blocked on a user-level scheduler");
      break;
    default:
      barf("printThreadBlockage: strange tso->why_blocked: %d for TSO %d (%d)",
           tso->why_blocked, tso->id, tso);
  }
}


void
printThreadStatus(StgTSO *t)
{
  debugBelch("\tthread %4lu @ %p ",
             (unsigned long)t->id, (void *)t);
  {
    void *label = lookupThreadLabel(t->id);
    if (label) debugBelch("[\"%s\"] ",(char *)label);
  }
  switch (t->what_next) {
    case ThreadKilled:
      debugBelch("has been killed");
      break;
    case ThreadComplete:
      debugBelch("has completed");
      break;
    default:
      printThreadBlockage(t);
  }
  if (t->dirty) {
    debugBelch(" (TSO_DIRTY)");
  }
  debugBelch("\n");
}

  void
printAllThreads(void)
{
  StgTSO *t, *next;
  nat i, g;
  Capability *cap;

  debugBelch("all threads:\n");

  for (i = 0; i < n_capabilities; i++) {
    cap = &capabilities[i];
    debugBelch("threads on capability %d:\n", cap->no);
    for (t = cap->run_queue_hd; t != END_TSO_QUEUE; t = t->_link) {
      printThreadStatus(t);
    }
  }

  debugBelch("other threads:\n");
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (t = generations[g].threads; t != END_TSO_QUEUE; t = next) {
      printThreadStatus(t);
      next = t->global_link;
    }
  }
}

// useful from gdb
  void
printThreadQueue(StgTSO *t)
{
  nat i = 0;
  for (; t != END_TSO_QUEUE; t = t->_link) {
    printThreadStatus(t);
    i++;
  }
  debugBelch("%d threads on queue\n", i);
}

void
printStackFrames (StgTSO* tso) {
  StgRetInfoTable *info;
  StgPtr sp, frame;
  StgStack *stack;
  rtsBool done;

  done = rtsFalse;
  stack = tso->stackobj;
  sp = stack->sp;

  if (sp[0] == (W_)&stg_enter_info) {
    sp++;
  } else {
    sp--;
    sp[0] = (W_)&stg_dummy_ret_closure;
  }

  frame = sp + 1;

  while (1) {
    info = get_ret_itbl((StgClosure *)frame);

    switch (info->i.type) {

      case UPDATE_FRAME: {
                           fprintf (stderr, "UPDATE\t\tframe %p\n", frame);
                           break;
                         }

      case UNDERFLOW_FRAME: {
                              fprintf (stderr, "UNDERFLOW\tframe %p\n", frame);
                              break;
                            }

      case STOP_FRAME: {
                         fprintf (stderr, "STOP\t\tframe %p\n", frame);
                         done = rtsTrue;
                         break;
                       }

      case CATCH_FRAME: {
                          fprintf (stderr, "CATCH\t\tframe %p\n", frame);
                          break;
                        }

      case ATOMICALLY_FRAME: {
                               fprintf (stderr, "ATOMICALLY\tframe %p\n", frame);
                               break;
                             }

      case CATCH_STM_FRAME: {
                              fprintf (stderr, "CATCH_STM\tframe %p\n", frame);
                              break;
                            }

      case CATCH_RETRY_FRAME: {
                                fprintf (stderr, "CATCH_RETRY\tframe %p\n", frame);
                                break;
                              }

      default:
                              break;
    }

    if (done)
      break;
    // move on to the next stack frame
    frame += stack_frame_sizeW((StgClosure *)frame);
  }
}

#endif /* DEBUG */
