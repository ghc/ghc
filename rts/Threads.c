/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"

#include "Capability.h"
#include "CheckVectorSupport.h"
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
#include "AllocArray.h"

#include <string.h>

/* Next thread ID to allocate.
 * LOCK: sched_mutex
 */
static StgThreadID next_thread_id = 1;

/* The smallest stack size that makes any sense is:
 *    RESERVED_STACK_WORDS    (so we can get back from the stack overflow)
 *  + sizeofW(StgStopFrame)   (the stg_stop_thread_info frame)
 *  + 1                       (the closure to enter)
 *  + 1                       (stg_ap_v_ret)
 *  + 1                       (spare slot req'd by stg_ap_v_ret)
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
   ------------------------------------------------------------------------ */
StgTSO *
createThread(Capability *cap, W_ size)
{
    StgTSO *tso;
    StgStack *stack;
    uint32_t stack_size;

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
    SET_HDR(stack, &stg_STACK_info, cap->r.rCCCS);
    stack->stack_size   = stack_size - sizeofW(StgStack);
    stack->sp           = stack->stack + stack->stack_size;
    stack->dirty        = STACK_DIRTY;
    stack->marking      = 0;

    tso = (StgTSO *)allocate(cap, sizeofW(StgTSO));
    TICK_ALLOC_TSO(sizeofW(StgTSO));
    SET_HDR(tso, &stg_TSO_info, CCS_SYSTEM);

    // Always start with the compiled code evaluator
    tso->what_next = ThreadRunGHC;
    tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
    tso->why_blocked  = NotBlocked;
    tso->blocked_exceptions = END_BLOCKED_EXCEPTIONS_QUEUE;
    tso->bq = (StgBlockingQueue *)END_TSO_QUEUE;
    tso->flags = 0;
    tso->dirty = 1;
    tso->_link = END_TSO_QUEUE;

    tso->saved_errno = 0;
    tso->bound = NULL;
    tso->cap = cap;

    tso->stackobj       = stack;
    tso->tot_stack_size = stack->stack_size;

    ASSIGN_Int64((W_*)&(tso->alloc_limit), 0);

    tso->trec = NO_TREC;
    tso->label = NULL;

#if defined(PROFILING)
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
    /* Mutations above need no memory barrier since this lock will provide
     * a release barrier */
    g0->threads = tso;
    RELEASE_LOCK(&sched_mutex);

    // ToDo: report the stack size in the event?
    traceEventCreateThread(cap, tso);

    return tso;
}

/* ---------------------------------------------------------------------------
 * Equality on Thread ids.
 *
 * This is used from STG land in the implementation of the Eq instance
 * for ThreadIds.
 * ------------------------------------------------------------------------ */

bool
eq_thread(StgPtr tso1, StgPtr tso2)
{
  return tso1 == tso2;
}

/* ---------------------------------------------------------------------------
 * Comparing Thread ids.
 *
 * This is used from STG land in the implementation of the Ord instance
 * for ThreadIds.
 * ------------------------------------------------------------------------ */

int
cmp_thread(StgPtr tso1, StgPtr tso2)
{
  if (tso1 == tso2) return 0;

  StgThreadID id1 = ((StgTSO *)tso1)->id;
  StgThreadID id2 = ((StgTSO *)tso2)->id;

  ASSERT(id1 != id2);

  return id1 < id2 ? -1 : 1;
}

/* ---------------------------------------------------------------------------
 * Fetching the ThreadID from an StgTSO.
 *
 * This is used in the implementation of Show for ThreadIds.
 * ------------------------------------------------------------------------ */
StgThreadID
rts_getThreadId(StgPtr tso)
{
  return ((StgTSO *)tso)->id;
}

/* ---------------------------------------------------------------------------
 * Enabling and disabling the thread allocation limit
 * ------------------------------------------------------------------------ */

void rts_enableThreadAllocationLimit(StgPtr tso)
{
    ((StgTSO *)tso)->flags |= TSO_ALLOC_LIMIT;
}

void rts_disableThreadAllocationLimit(StgPtr tso)
{
    ((StgTSO *)tso)->flags &= ~TSO_ALLOC_LIMIT;
}

/* -----------------------------------------------------------------------------
   Remove a thread from a queue.
   Fails fatally if the TSO is not on the queue.
   -------------------------------------------------------------------------- */

bool // returns true if we modified queue
removeThreadFromQueue (Capability *cap, StgTSO **queue, StgTSO *tso)
{
    StgTSO *t, *prev;

    prev = NULL;
    for (t = *queue; t != END_TSO_QUEUE; prev = t, t = t->_link) {
        if (t == tso) {
            if (prev) {
                setTSOLink(cap,prev,t->_link);
                t->_link = END_TSO_QUEUE;
                return false;
            } else {
                *queue = t->_link;
                t->_link = END_TSO_QUEUE;
                return true;
            }
        }
    }
    barf("removeThreadFromQueue: not found");
}

bool // returns true if we modified head or tail
removeThreadFromDeQueue (Capability *cap,
                         StgTSO **head, StgTSO **tail, StgTSO *tso)
{
    StgTSO *t, *prev;
    bool flag = false;

    prev = NULL;
    for (t = *head; t != END_TSO_QUEUE; prev = t, t = t->_link) {
        if (t == tso) {
            if (prev) {
                setTSOLink(cap,prev,t->_link);
                flag = false;
            } else {
                *head = t->_link;
                flag = true;
            }
            t->_link = END_TSO_QUEUE;
            if (*tail == tso) {
                if (prev) {
                    *tail = prev;
                } else {
                    *tail = END_TSO_QUEUE;
                }
                return true;
            } else {
                return flag;
            }
        }
    }
    barf("removeThreadFromDeQueue: not found");
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

#if defined(THREADED_RTS)
    Capability *tso_owner = RELAXED_LOAD(&tso->cap);
    if (tso_owner != cap)
    {
        MessageWakeup *msg;
        msg = (MessageWakeup *)allocate(cap,sizeofW(MessageWakeup));
        msg->tso = tso;
        SET_HDR(msg, &stg_MSG_TRY_WAKEUP_info, CCS_SYSTEM);
        sendMessage(cap, tso_owner, (Message*)msg);
        debugTraceCap(DEBUG_sched, cap, "message: try wakeup thread %"
                      FMT_StgThreadID " on cap %d", tso->id, tso_owner->no);
        return;
    }
#endif

    switch (ACQUIRE_LOAD(&tso->why_blocked))
    {
    case BlockedOnMVar:
    case BlockedOnMVarRead:
    {
        if (tso->_link == END_TSO_QUEUE) {
            tso->block_info.closure = (StgClosure*)END_TSO_QUEUE;
            goto unblock;
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
            debugTraceCap(DEBUG_sched, cap, "thread %" FMT_StgThreadID " still "
                          "blocked on throwto (%p)", tso->id,
                          tso->block_info.throwto->header.info);
            return;
        }

        // remove the block frame from the stack
        ASSERT(tso->stackobj->sp[0] == (StgWord)&stg_block_throwto_info);
        tso->stackobj->sp += 3;
        goto unblock;
    }

    case BlockedOnSTM:
        tso->block_info.closure = &stg_STM_AWOKEN_closure;
        goto unblock;

    case BlockedOnBlackHole:
    case ThreadMigrating:
        goto unblock;

    default:
        // otherwise, do nothing
        return;
    }

unblock:
    // just run the thread now, if the BH is not really available,
    // we'll block again.
    tso->why_blocked = NotBlocked;
    appendToRunQueue(cap,tso);

    // We used to set the context switch flag here, which would
    // trigger a context switch a short time in the future (at the end
    // of the current nursery block).  The idea is that we have just
    // woken up a thread, so we may need to load-balance and migrate
    // threads to other CPUs.  On the other hand, setting the context
    // switch flag here unfairly penalises the current thread by
    // yielding its time slice too early.
    //
    // The synthetic benchmark nofib/smp/chan can be used to show the
    // difference quite clearly.

    // cap->context_switch = 1;
}

/* ----------------------------------------------------------------------------
   migrateThread
   ------------------------------------------------------------------------- */

// Precondition: The caller must own the `from` capability.
void
migrateThread (Capability *from, StgTSO *tso, Capability *to)
{
    // Sadly we can't assert this since migrateThread is called from
    // scheduleDoGC, where we implicitly own all capabilities.
    //ASSERT_FULL_CAPABILITY_INVARIANTS(from, getTask());

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

static void
wakeBlockingQueue(Capability *cap, StgBlockingQueue *bq)
{
    MessageBlackHole *msg;
    const StgInfoTable *i;

    ASSERT(bq->header.info == &stg_BLOCKING_QUEUE_DIRTY_info  ||
           bq->header.info == &stg_BLOCKING_QUEUE_CLEAN_info  );

    for (msg = bq->queue; msg != (MessageBlackHole*)END_TSO_QUEUE;
         msg = msg->link) {
        i = ACQUIRE_LOAD(&msg->header.info);
        if (i != &stg_IND_info) {
            ASSERT(i == &stg_MSG_BLACKHOLE_info);
            tryWakeupThread(cap,msg->tso);
        }
    }

    // overwrite the BQ with an indirection so it will be
    // collected at the next GC.
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

    debugTraceCap(DEBUG_sched, cap, "collision occurred; checking blocking "
                  "queues for thread %" FMT_StgThreadID, tso->id);

    for (bq = tso->bq; bq != (StgBlockingQueue*)END_TSO_QUEUE; bq = next) {
        next = bq->link;

        const StgInfoTable *bqinfo = ACQUIRE_LOAD(&bq->header.info);
        if (bqinfo == &stg_IND_info) {
            // ToDo: could short it out right here, to avoid
            // traversing this IND multiple times.
            continue;
        }

        // We need to always ensure we untag bh.  While it might seem a
        // sensible assumption that bh will never be tagged, the GC could
        // shortcut the indirection and put a tagged pointer into the
        // indirection.
        //
        // This blew up on aarch64-darwin with misaligned access.  bh pointing
        // to an address that always ended in 0xa.  Thus on architectures that
        // are a little less strict about alignment, this would have read a
        // garbage pinfo, which very, very unlikely would have been equal to
        // stg_BLACKHOLE_info.  Thus while the code would have done the wrong
        // thing the result would be the same in almost all cases. See #20093.
        p = UNTAG_CLOSURE(bq->bh);
        const StgInfoTable *pinfo = ACQUIRE_LOAD(&p->header.info);
        if (pinfo != &stg_BLACKHOLE_info ||
            (RELAXED_LOAD(&((StgInd *)p)->indirectee) != (StgClosure*)bq))
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

    i = ACQUIRE_LOAD(&thunk->header.info);
    if (i != &stg_BLACKHOLE_info &&
        i != &stg_CAF_BLACKHOLE_info &&
        i != &__stg_EAGER_BLACKHOLE_info &&
        i != &stg_WHITEHOLE_info) {
        updateWithIndirection(cap, thunk, val);
        return;
    }

    v = UNTAG_CLOSURE(ACQUIRE_LOAD(&((StgInd*)thunk)->indirectee));

    updateWithIndirection(cap, thunk, val);

    // sometimes the TSO is locked when we reach here, so its header
    // might be WHITEHOLE.  Hence check for the correct owner using
    // pointer equality first.
    if ((StgTSO*)v == tso) {
        return;
    }

    i = ACQUIRE_LOAD(&v->header.info);
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
  return false;
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

    if (RtsFlags.GcFlags.maxStkSize > 0
        && tso->tot_stack_size >= RtsFlags.GcFlags.maxStkSize) {
        // #3677: In a stack overflow situation, stack squeezing may
        // reduce the stack size, but we don't know whether it has been
        // reduced enough for the stack check to succeed if we try
        // again.  Fortunately stack squeezing is idempotent, so all we
        // need to do is record whether *any* squeezing happened.  If we
        // are at the stack's absolute -K limit, and stack squeezing
        // happened, then we try running the thread again.  The
        // TSO_SQUEEZED flag is set by threadPaused() to tell us whether
        // squeezing happened or not.
        if (tso->flags & TSO_SQUEEZED) {
            return;
        }

        debugTrace(DEBUG_gc,
                   "threadStackOverflow of TSO %" FMT_StgThreadID " (%p): stack"
                   " too large (now %ld; max is %ld)", tso->id, tso,
                   (long)tso->stackobj->stack_size, RtsFlags.GcFlags.maxStkSize);
        IF_DEBUG(gc,
                 /* If we're debugging, just print out the top of the stack */
                 printStackChunk(tso->stackobj->sp,
                                 stg_min(tso->stackobj->stack + tso->stackobj->stack_size,
                                         tso->stackobj->sp+64)));

        // See Note [Throw to self when masked], also #767 and #8303.
        throwToSelf(cap, tso, (StgClosure *)stackOverflow_closure);
        return;
    }


    // We also want to avoid enlarging the stack if squeezing has
    // already released some of it.  However, we don't want to get into
    // a pathological situation where a thread has a nearly full stack
    // (near its current limit, but not near the absolute -K limit),
    // keeps allocating a little bit, squeezing removes a little bit,
    // and then it runs again.  So to avoid this, if we squeezed *and*
    // there is still less than BLOCK_SIZE_W words free, then we enlarge
    // the stack anyway.
    //
    // NB: This reasoning only applies if the stack has been squeezed;
    // if no squeezing has occurred, then BLOCK_SIZE_W free space does
    // not mean there is enough stack to run; the thread may have
    // requested a large amount of stack (see below).  If the amount
    // we squeezed is not enough to run the thread, we'll come back
    // here (no squeezing will have occurred and thus we'll enlarge the
    // stack.)
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

    // Charge the current thread for allocating stack.  Stack usage is
    // non-deterministic, because the chunk boundaries might vary from
    // run to run, but accounting for this is better than not
    // accounting for it, since a deep recursion will otherwise not be
    // subject to allocation limits.
    cap->r.rCurrentTSO = tso;
    new_stack = (StgStack*) allocate(cap, chunk_size);
    cap->r.rCurrentTSO = NULL;

    SET_HDR(new_stack, &stg_STACK_info, old_stack->header.prof.ccs);
    TICK_ALLOC_STACK(chunk_size);

    new_stack->dirty = 0; // begin clean, we'll mark it dirty below
    new_stack->marking = 0;
    new_stack->stack_size = chunk_size - sizeofW(StgStack);
    new_stack->sp = new_stack->stack + new_stack->stack_size;

    tso->tot_stack_size += new_stack->stack_size;

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
            if (sp + size > old_stack->sp + (new_stack->stack_size -
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

            // See Note [realArgRegsCover] in GHC.Cmm.CallConv.
            switch (vectorSupportGlobalVar) {
              case 3:
                frame->info = &stg_stack_underflow_frame_v64_info;
                break;
              case 2:
                frame->info = &stg_stack_underflow_frame_v32_info;
                break;
              case 1:
                frame->info = &stg_stack_underflow_frame_v16_info;
                break;
              default:
                frame->info = &stg_stack_underflow_frame_d_info;
                break;
            }
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

    // No write barriers needed; all of the writes above are to structured
    // owned by our capability.
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
    uint32_t retvals;

    debugTraceCap(DEBUG_sched, cap, "stack underflow");

    old_stack = tso->stackobj;

    frame = (StgUnderflowFrame*)(old_stack->stack + old_stack->stack_size
                                 - sizeofW(StgUnderflowFrame));
    ASSERT( frame->info == &stg_stack_underflow_frame_d_info
         || frame->info == &stg_stack_underflow_frame_v16_info
         || frame->info == &stg_stack_underflow_frame_v32_info
         || frame->info == &stg_stack_underflow_frame_v64_info );

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

        memcpy(/* dest */ new_stack->sp - retvals,
               /* src  */ old_stack->sp,
               /* size */ retvals * sizeof(W_));
    }

    // empty the old stack.  The GC may still visit this object
    // because it is on the mutable list.
    old_stack->sp = old_stack->stack + old_stack->stack_size;

    // restore the stack parameters, and update tot_stack_size
    tso->tot_stack_size -= old_stack->stack_size;

    // we're about to run it, better mark it dirty.
    //
    // N.B. the nonmoving collector may mark the stack, meaning that sp must
    // point at a valid stack frame.
    dirty_STACK(cap, new_stack);
    new_stack->sp -= retvals;

    return retvals;
}

/* ----------------------------------------------------------------------------
   Implementation of tryPutMVar#

   NOTE: this should be kept in sync with stg_tryPutMVarzh in PrimOps.cmm
   ------------------------------------------------------------------------- */

bool performTryPutMVar(Capability *cap, StgMVar *mvar, StgClosure *value)
{
    const StgInfoTable *info;
    const StgInfoTable *qinfo;
    StgMVarTSOQueue *q;
    StgTSO *tso;

    info = lockClosure((StgClosure*)mvar);

    if (mvar->value != &stg_END_TSO_QUEUE_closure) {
#if defined(THREADED_RTS)
        unlockClosure((StgClosure*)mvar, info);
#endif
        return false;
    }

    q = mvar->head;
loop:
    if (q == (StgMVarTSOQueue*)&stg_END_TSO_QUEUE_closure) {
        /* No further takes, the MVar is now full. */
        if (info == &stg_MVAR_CLEAN_info) {
            dirty_MVAR(&cap->r, (StgClosure*)mvar, mvar->value);
        }

        mvar->value = value;
        unlockClosure((StgClosure*)mvar, &stg_MVAR_DIRTY_info);
        return true;
    }

    qinfo = ACQUIRE_LOAD(&q->header.info);
    if (qinfo == &stg_IND_info ||
        qinfo == &stg_MSG_NULL_info) {
        q = (StgMVarTSOQueue*) ACQUIRE_LOAD(&((StgInd*)q)->indirectee);
        goto loop;
    }

    // There are takeMVar(s) waiting: wake up the first one
    tso = q->tso;
    mvar->head = q = q->link;
    if (q == (StgMVarTSOQueue*)&stg_END_TSO_QUEUE_closure) {
        mvar->tail = (StgMVarTSOQueue*)&stg_END_TSO_QUEUE_closure;
    } else {
        if (info == &stg_MVAR_CLEAN_info) {
            // Resolve #18919.
            dirty_MVAR(&cap->r, (StgClosure*)mvar, mvar->value);
            info = &stg_MVAR_DIRTY_info;
        }
    }

    // save why_blocked here, because waking up the thread destroys
    // this information
    StgWord why_blocked = ACQUIRE_LOAD(&tso->why_blocked);
    ASSERT(why_blocked == BlockedOnMVarRead || why_blocked == BlockedOnMVar);
    ASSERT(tso->block_info.closure == (StgClosure*)mvar);

    // actually perform the takeMVar
    StgStack* stack = tso->stackobj;
    RELAXED_STORE(&stack->sp[1], (W_)value);
    RELAXED_STORE(&stack->sp[0], (W_)&stg_ret_p_info);

    // indicate that the MVar operation has now completed.
    RELEASE_STORE(&tso->_link, (StgTSO*)&stg_END_TSO_QUEUE_closure);

    if ((stack->dirty & STACK_DIRTY) == 0) {
        dirty_STACK(cap, stack);
    }

    tryWakeupThread(cap, tso);

    // If it was a readMVar, then we can still do work,
    // so loop back. (XXX: This could take a while)
    if (why_blocked == BlockedOnMVarRead)
        goto loop;

    ASSERT(why_blocked == BlockedOnMVar);

    unlockClosure((StgClosure*)mvar, info);

    return true;
}

/* Return NULL on allocation failure */
StgMutArrPtrs *listThreads(Capability *cap)
{
    ACQUIRE_LOCK(&sched_mutex);

    // First count how many threads we have...
    StgWord n_threads = 0;
    for (unsigned g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (StgTSO *t = generations[g].threads; t != END_TSO_QUEUE; t = t->global_link) {
            n_threads++;
        }
    }

    // Allocate a suitably-sized array...
    StgMutArrPtrs *arr = allocateMutArrPtrs(cap, n_threads, cap->r.rCCCS);
    if (RTS_UNLIKELY(arr == NULL)) goto end;

    // Populate it...
    StgWord i = 0;
    for (unsigned g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (StgTSO *t = generations[g].threads; t != END_TSO_QUEUE; t = t->global_link) {
            // It's possible that new threads have been created since we counted.
            // Ignore them.
            if (i == n_threads)
                break;
            arr->payload[i] = (StgClosure *) t;
            i++;
        }
    }
    CHECKM(i == n_threads, "listThreads: Found too few threads");
end:
    RELEASE_LOCK(&sched_mutex);
    return arr;
}

/* ----------------------------------------------------------------------------
 * Debugging: why is a thread blocked
 * ------------------------------------------------------------------------- */

#if defined(DEBUG)
void
printThreadBlockage(StgTSO *tso)
{
  switch (ACQUIRE_LOAD(&tso->why_blocked)) {
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
    break;
  case BlockedOnMVar:
    debugBelch("is blocked on an MVar @ %p", tso->block_info.closure);
    break;
  case BlockedOnMVarRead:
    debugBelch("is blocked on atomic MVar read @ %p", tso->block_info.closure);
    break;
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
  default:
    barf("printThreadBlockage: strange tso->why_blocked: %d for TSO %"
         FMT_StgThreadID " (%p)", tso->why_blocked, tso->id, tso);
  }
}


void
printThreadStatus(StgTSO *t)
{
    debugBelch("\tthread %4lu @ %p ", (unsigned long)t->id, (void *)t);
    if (t->label) {
        debugBelch("[\"%.*s\"] ", (int)t->label->bytes, (char *)t->label->payload);
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
  uint32_t i, g;
  Capability *cap;

  debugBelch("all threads:\n");

  for (i = 0; i < getNumCapabilities(); i++) {
      cap = getCapability(i);
      debugBelch("threads on capability %d:\n", cap->no);
      for (t = cap->run_queue_hd; t != END_TSO_QUEUE; t = t->_link) {
          printThreadStatus(t);
      }
  }

  debugBelch("other threads:\n");
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (t = generations[g].threads; t != END_TSO_QUEUE; t = next) {
      if (t->why_blocked != NotBlocked) {
          printThreadStatus(t);
      }
      next = t->global_link;
    }
  }
}

void
printGlobalThreads(void)
{
  for (uint32_t g = 0; g < RtsFlags.GcFlags.generations; g++) {
    debugBelch("\ngen %d\n", g);
    for (StgTSO *t = generations[g].threads; t != END_TSO_QUEUE; t = t->global_link) {
      debugBelch("thread %p (id=%lu)\n", t, (unsigned long)t->id);
    }
    for (StgTSO *t = generations[g].old_threads; t != END_TSO_QUEUE; t = t->global_link) {
      debugBelch("thread %p (id=%lu) (old)\n", t, (unsigned long)t->id);
    }
  }
}

// useful from gdb
void
printThreadQueue(StgTSO *t)
{
    uint32_t i = 0;
    for (; t != END_TSO_QUEUE; t = t->_link) {
        printThreadStatus(t);
        i++;
    }
    debugBelch("%d threads on queue\n", i);
}

#endif /* DEBUG */
