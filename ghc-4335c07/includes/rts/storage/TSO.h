/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * The definitions for Thread State Objects.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

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
 * Thread IDs are 32 bits.
 */
typedef StgWord32 StgThreadID;

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
    StgWord16               why_blocked;    // Values defined in Constants.h
    StgWord32               flags;          // Values defined in Constants.h
    StgTSOBlockInfo         block_info;
    StgThreadID             id;
    StgWord32               saved_errno;
    StgWord32               dirty;          /* non-zero => dirty */
    struct InCall_*         bound;
    struct Capability_*     cap;

    struct StgTRecHeader_ * trec;       /* STM transaction record */

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
     * TSO_ALLOC_LIMIT is set in flags, we raise an exception in the
     * thread, and give the thread a little more space to handle the
     * exception before we raise the exception again.
     *
     * This is an integer, because we might update it in a place where
     * it isn't convenient to raise the exception, so we want it to
     * stay negative until we get around to checking it.
     *
     * Use only PK_Int64/ASSIGN_Int64 macros to get/set the value of alloc_limit
     * in C code otherwise you will cause alignment issues on SPARC
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

typedef struct StgStack_ {
    StgHeader  header;
    StgWord32  stack_size;     // stack size in *words*
    StgWord32  dirty;          // non-zero => dirty
    StgPtr     sp;             // current stack pointer
    StgWord    stack[];
} StgStack;

// Calculate SpLim from a TSO (reads tso->stackobj, but no fields from
// the stackobj itself).
INLINE_HEADER StgPtr tso_SpLim (StgTSO* tso)
{
    return tso->stackobj->stack + RESERVED_STACK_WORDS;
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
