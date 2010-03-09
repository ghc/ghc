/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * The definitions for Thread State Objects.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STORAGE_TSO_H
#define RTS_STORAGE_TSO_H

/*
 * PROFILING info in a TSO
 */
typedef struct {
  CostCentreStack *CCCS;	/* thread's current CCS */
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
  struct StgTSO_ *tso;
  StgInt fd;	/* StgInt instead of int, so that it's the same size as the ptrs */
#if defined(mingw32_HOST_OS)
  StgAsyncIOResult *async_result;
#endif
  StgWord target;
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
       NOTE!!!  do not modify _link directly, it is subject to
       a write barrier for generational GC.  Instead use the
       setTSOLink() function.  Exceptions to this rule are:

       * setting the link field to END_TSO_QUEUE
       * putting a TSO on the blackhole_queue
       * setting the link field of the currently running TSO, as it
         will already be dirty.
    */

    struct StgTSO_*         global_link;    /* Links all threads together */
    
    StgWord                 dirty;          /* non-zero => dirty */
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
     *
     * The link field has a separate dirty bit of its own, namely the
     * bit TSO_LINK_DIRTY in the tso->flags field, set by
     * setTSOLink().
     */

    StgWord16               what_next;      /* Values defined in Constants.h */
    StgWord16               why_blocked;    /* Values defined in Constants.h */
    StgWord32               flags;
    StgTSOBlockInfo         block_info;
    StgThreadID             id;
    int                     saved_errno;
    struct InCall_*         bound;
    struct Capability_*     cap;
    struct StgTRecHeader_ * trec;       /* STM transaction record */

    /*
       A list of threads blocked on this TSO waiting to throw
       exceptions.  In order to access this field, the TSO must be
       locked using lockClosure/unlockClosure (see SMP.h).
    */
    struct StgTSO_ *        blocked_exceptions;

#ifdef TICKY_TICKY
    /* TICKY-specific stuff would go here. */
#endif
#ifdef PROFILING
    StgTSOProfInfo prof;
#endif
#ifdef mingw32_HOST_OS
    StgWord32 saved_winerror;
#endif

    /* The thread stack... */
    StgWord32	       stack_size;     /* stack size in *words* */
    StgWord32          max_stack_size; /* maximum stack size in *words* */
    StgPtr             sp;
    
    StgWord            stack[FLEXIBLE_ARRAY];
} *StgTSOPtr;

/* -----------------------------------------------------------------------------
   functions
   -------------------------------------------------------------------------- */

void dirty_TSO  (Capability *cap, StgTSO *tso);
void setTSOLink (Capability *cap, StgTSO *tso, StgTSO *target);

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
	NotBlocked             NULL                 runnable_queue, or running
	
        BlockedOnBlackHole     the BLACKHOLE        blackhole_queue
	
        BlockedOnMVar          the MVAR             the MVAR's queue

	BlockedOnSTM           END_TSO_QUEUE        STM wait queue(s)
	
        BlockedOnException     the TSO              TSO->blocked_exception

        BlockedOnRead          NULL                 blocked_queue
        BlockedOnWrite         NULL		    blocked_queue
        BlockedOnDelay         NULL                 blocked_queue
	BlockedOnGA            closure TSO blocks on   BQ of that closure
	BlockedOnGA_NoSend     closure TSO blocks on   BQ of that closure

      tso->link == END_TSO_QUEUE, if the thread is currently running.

   A zombie thread has the following properties:
      
      tso->what_next == ThreadComplete or ThreadKilled
      tso->link     ==  (could be on some queue somewhere)
      tso->su       ==  tso->stack + tso->stack_size
      tso->sp       ==  tso->stack + tso->stack_size - 1 (i.e. top stack word)
      tso->sp[0]    ==  return value of thread, if what_next == ThreadComplete,
                        exception             , if what_next == ThreadKilled

      (tso->sp is left pointing at the top word on the stack so that
      the return value or exception will be retained by a GC).

   The 2 cases BlockedOnGA and BlockedOnGA_NoSend are needed in a GUM
   setup only. They mark a TSO that has entered a FETCH_ME or
   FETCH_ME_BQ closure, respectively; only the first TSO hitting the
   closure will send a Fetch message.
   Currently we have no separate code for blocking on an RBH; we use the
   BlockedOnBlackHole case for that.   -- HWL

 ---------------------------------------------------------------------------- */

/* Workaround for a bug/quirk in gcc on certain architectures.
 * symptom is that (&tso->stack - &tso->header) /=  sizeof(StgTSO)
 * in other words, gcc pads the structure at the end.
 */

extern StgTSO dummy_tso;

#define TSO_STRUCT_SIZE \
   ((char *)&dummy_tso.stack - (char *)&dummy_tso.header)

#define TSO_STRUCT_SIZEW (TSO_STRUCT_SIZE / sizeof(W_))

/* this is the NIL ptr for a TSO queue (e.g. runnable queue) */
#define END_TSO_QUEUE  ((StgTSO *)(void*)&stg_END_TSO_QUEUE_closure)

#endif /* RTS_STORAGE_TSO_H */
