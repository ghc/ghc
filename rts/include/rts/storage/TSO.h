/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * The definitions for Thread State Objects.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "rts/storage/Closures.h"

/*
 * PROFILING info in a TSO
 */
typedef struct {
  CostCentreStack *cccs;       /* thread's current CCS */
} StgTSOProfInfo;

/*
 * There is no TICKY info in a TSO at this time.
 */

/*
 * Thread IDs are 64 bits.
 */
typedef StgWord64 StgThreadID;

#define FMT_StgThreadID FMT_Word64

#define tsoLocked(tso) ((tso)->flags & TSO_LOCKED)

/*
 * Type returned after running a thread.  Values of this type
 * include HeapOverflow, StackOverflow etc.  See Constants.h for the
 * full list.
 */
typedef unsigned int StgThreadReturnCode;

#if defined(mingw32_HOST_OS)
/* results from an async I/O request + its request ID. */
typedef struct {
  unsigned int reqID;
  int          len;
  int          errCode;
} StgAsyncIOResult;
#endif

/* Reason for thread being blocked. See comment above struct StgTso_. */
typedef union {
  StgClosure *closure;
  StgTSO *prev; // a back-link when the TSO is on the run queue (NotBlocked)
  struct MessageBlackHole_ *bh;
  struct MessageThrowTo_ *throwto;
  struct MessageWakeup_  *wakeup;
  StgInt fd;    /* StgInt instead of int, so that it's the same size as the ptrs */
#if defined(mingw32_HOST_OS)
  StgAsyncIOResult *async_result;
#endif
#if !defined(THREADED_RTS)
  StgWord target;
    // Only for the non-threaded RTS: the target time for a thread
    // blocked in threadDelay, in units of 1ms.  This is a
    // compromise: we don't want to take up much space in the TSO.  If
    // you want better resolution for threadDelay, use -threaded.
#endif
} StgTSOBlockInfo;


/*
 * TSOs live on the heap, and therefore look just like heap objects.
 * Large TSOs will live in their own "block group" allocated by the
 * storage manager, and won't be copied during garbage collection.
 */

/*
 * Threads may be blocked for several reasons.  A blocked thread will
 * have the reason in the why_blocked field of the TSO, and some
 * further info (such as the closure the thread is blocked on, or the
 * file descriptor if the thread is waiting on I/O) in the block_info
 * field.
 */

typedef struct StgTSO_ {
    StgHeader               header;

    /* The link field, for linking threads together in lists (e.g. the
       run queue on a Capability.
    */
    struct StgTSO_*         _link;
    /*
      Currently used for linking TSOs on:
      * cap->run_queue_{hd,tl}
      * (non-THREADED_RTS); the blocked_queue
      * and pointing to the next chunk for a ThreadOldStack

       NOTE!!!  do not modify _link directly, it is subject to
       a write barrier for generational GC.  Instead use the
       setTSOLink() function.  Exceptions to this rule are:

       * setting the link field to END_TSO_QUEUE
       * setting the link field of the currently running TSO, as it
         will already be dirty.
    */

    struct StgTSO_*         global_link;    // Links threads on the
                                            // generation->threads lists

    /*
     * The thread's stack
     */
    struct StgStack_       *stackobj;

    /*
     * The tso->dirty flag indicates that this TSO's stack should be
     * scanned during garbage collection.  It also indicates that this
     * TSO is on the mutable list.
     *
     * NB. The dirty flag gets a word to itself, so that it can be set
     * safely by multiple threads simultaneously (the flags field is
     * not safe for this purpose; see #3429).  It is harmless for the
     * TSO to be on the mutable list multiple times.
     *
     * tso->dirty is set by dirty_TSO(), and unset by the garbage
     * collector (only).
     */

    StgWord16               what_next;      // Values defined in Constants.h
    StgWord32               flags;          // Values defined in Constants.h

    /*
     * N.B. why_blocked only has a handful of values but must be atomically
     * updated; the smallest width which AArch64 supports for is 32-bits.
     */
    StgWord32               why_blocked;    // Values defined in Constants.h
    StgTSOBlockInfo         block_info;     // Barrier provided by why_blocked
    StgThreadID             id;
    StgWord32               saved_errno;
    StgWord32               dirty;          /* non-zero => dirty */
    struct InCall_*         bound;
    struct Capability_*     cap;

    struct StgTRecHeader_ * trec;           /* STM transaction record */
    StgArrBytes*            label;          /* Thread label */

    /*
     * A list of threads blocked on this TSO waiting to throw exceptions.
    */
    struct MessageThrowTo_ * blocked_exceptions;

    /*
     * A list of StgBlockingQueue objects, representing threads
     * blocked on thunks that are under evaluation by this thread.
    */
    struct StgBlockingQueue_ *bq;

    /*
     * The allocation limit for this thread, which is updated as the
     * thread allocates.  If the value drops below zero, and
     * TSO_ALLOC_LIMIT is set in flags, then a handler is triggerd.
     * Either we raise an exception in the thread, and give the thread
     * a little more space to handle the exception before we raise the
     * exception again; or we run a user defined handler.
     *
     * This is an integer, because we might update it in a place where
     * it isn't convenient to raise the exception, so we want it to
     * stay negative until we get around to checking it.
     *
     * Use only PK_Int64/ASSIGN_Int64 macros to get/set the value of alloc_limit
     * in C code otherwise you will cause alignment issues
     */
    StgInt64  alloc_limit;     /* in bytes */

    /*
     * sum of the sizes of all stack chunks (in words), used to decide
     * whether to throw the StackOverflow exception when the stack
     * overflows, or whether to just chain on another stack chunk.
     *
     * Note that this overestimates the real stack size, because each
     * chunk will have a gap at the end, of +RTS -kb<size> words.
     * This means stack overflows are not entirely accurate, because
     * the more gaps there are, the sooner the stack will run into the
     * hard +RTS -K<size> limit.
     */
    StgWord32  tot_stack_size;

#if defined(TICKY_TICKY)
    /* TICKY-specific stuff would go here. */
#endif
#if defined(PROFILING)
    StgTSOProfInfo prof;
#endif
#if defined(mingw32_HOST_OS)
    StgWord32 saved_winerror;
#endif

} *StgTSOPtr; // StgTSO defined in rts/Types.h

/* Note [StgStack dirtiness flags and concurrent marking]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Without concurrent collection by the nonmoving collector the stack dirtiness
 * story is quite simple: The stack is either STACK_DIRTY (meaning it has been
 * added to mut_list) or not.
 *
 * However, things are considerably more complicated with concurrent collection
 * (namely, when nonmoving_write_barrier_enabled is set): In addition to adding
 * the stack to mut_list and flagging it as STACK_DIRTY, we also must ensure
 * that stacks are marked in accordance with the nonmoving collector's snapshot
 * invariant. This is: every stack alive at the time the snapshot is taken must
 * be marked at some point after the moment the snapshot is taken and before it
 * is mutated or the commencement of the sweep phase.
 *
 * This marking may be done by the concurrent mark phase (in the case of a
 * thread that never runs during the concurrent mark) or by the mutator when
 * dirtying the stack. However, it is unsafe for the concurrent collector to
 * traverse the stack while it is under mutation. Consequently, the following
 * handshake is obeyed by the mutator's write barrier and the concurrent mark to
 * ensure this doesn't happen:
 *
 * 1. The entity seeking to mark first checks that the stack lives in the nonmoving
 *    generation; if not then the stack was not alive at the time the snapshot
 *    was taken and therefore we need not mark it.
 *
 * 2. The entity seeking to mark checks the stack's mark bit. If it is set then
 *    no mark is necessary.
 *
 * 3. The entity seeking to mark tries to lock the stack for marking by
 *    atomically setting its `marking` field to the current non-moving mark
 *    epoch:
 *
 *    a. If the mutator finds the concurrent collector has already locked the
 *       stack then it waits until it is finished (indicated by the mark bit
 *       being set) before proceeding with execution.
 *
 *    b. If the concurrent collector finds that the mutator has locked the stack
 *       then it moves on, leaving the mutator to mark it. There is no need to wait;
 *       the mark is guaranteed to finish before sweep due to the post-mark
 *       synchronization with mutators.
 *
 *    c. Whoever succeeds in locking the stack is responsible for marking it and
 *       setting the stack's mark bit (either the BF_MARKED bit for large objects
 *       or otherwise its bit in its segment's mark bitmap).
 *
 * To ensure that mutation does not proceed until the stack is fully marked the
 * mark phase must not set the mark bit until it has finished tracing.
 *
 */

#define STACK_DIRTY 1
// used by sanity checker to verify that all dirty stacks are on the mutable list
#define STACK_SANE 64

typedef struct StgStack_ {
    StgHeader  header;

    /* Size of the `stack` field in *words*. This is not affected by how much of
     * the stack space is used, nor if more stack space is linked to by an
     * UNDERFLOW_FRAME.
     */
    StgWord32  stack_size;

    StgWord8   dirty;          // non-zero => dirty
    StgWord8   marking;        // non-zero => someone is currently marking the stack

    /* Pointer to the "top" of the stack i.e. the most recently written address.
     * The stack is filled downwards, so the "top" of the stack starts with `sp
     * = stack + stack_size` and is decremented as the stack fills with data.
     * See comment on "Invariants" below.
     */
    StgPtr     sp;

    StgWord    stack[];
} StgStack;

INLINE_HEADER StgPtr stack_SpLim(StgStack *stack)
{
    return stack->stack + RESERVED_STACK_WORDS;
}

// Calculate SpLim from a TSO (reads tso->stackobj, but no fields from
// the stackobj itself).
INLINE_HEADER StgPtr tso_SpLim (StgTSO* tso)
{
    return stack_SpLim(tso->stackobj);
}

/* -----------------------------------------------------------------------------
   functions
   -------------------------------------------------------------------------- */

void dirty_TSO  (Capability *cap, StgTSO *tso);
void setTSOLink (Capability *cap, StgTSO *tso, StgTSO *target);
void setTSOPrev (Capability *cap, StgTSO *tso, StgTSO *target);

void dirty_STACK (Capability *cap, StgStack *stack);

/* -----------------------------------------------------------------------------
   Invariants:

   An active thread has the following properties:

      tso->stack < tso->sp < tso->stack+tso->stack_size
      tso->stack_size <= tso->max_stack_size

      RESERVED_STACK_WORDS is large enough for any heap-check or
      stack-check failure.

      The size of the TSO struct plus the stack is either
        (a) smaller than a block, or
        (b) a multiple of BLOCK_SIZE

        tso->why_blocked       tso->block_info      location
        ----------------------------------------------------------------------
        NotBlocked             END_TSO_QUEUE        runnable_queue, or running

        BlockedOnBlackHole     MessageBlackHole *   TSO->bq

        BlockedOnMVar          the MVAR             the MVAR's queue

        BlockedOnSTM           END_TSO_QUEUE        STM wait queue(s)
        BlockedOnSTM           STM_AWOKEN           run queue

        BlockedOnMsgThrowTo    MessageThrowTo *     TSO->blocked_exception

        BlockedOnRead          NULL                 blocked_queue
        BlockedOnWrite         NULL                 blocked_queue
        BlockedOnDelay         NULL                 blocked_queue

      tso->link == END_TSO_QUEUE, if the thread is currently running.

   A zombie thread has the following properties:

      tso->what_next == ThreadComplete or ThreadKilled
      tso->link     ==  (could be on some queue somewhere)
      tso->sp       ==  tso->stack + tso->stack_size - 1 (i.e. top stack word)
      tso->sp[0]    ==  return value of thread, if what_next == ThreadComplete,
                        exception             , if what_next == ThreadKilled

      (tso->sp is left pointing at the top word on the stack so that
      the return value or exception will be retained by a GC).

 ---------------------------------------------------------------------------- */

/* this is the NIL ptr for a TSO queue (e.g. runnable queue) */
#define END_TSO_QUEUE  ((StgTSO *)(void*)&stg_END_TSO_QUEUE_closure)
