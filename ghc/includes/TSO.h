/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * The definitions for Thread State Objects.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TSO_H
#define TSO_H

#if DEBUG
#define TSO_MAGIC 4321
#endif

typedef struct {
  StgInt   pri;
  StgInt   magic;
  StgInt   sparkname;
  rtsTime  startedat;
  rtsBool  exported;
  StgInt   basicblocks;
  StgInt   allocs;
  rtsTime  exectime;
  rtsTime  fetchtime;
  rtsTime  fetchcount;
  rtsTime  blocktime;
  StgInt   blockcount;
  rtsTime  blockedat;
  StgInt   globalsparks;
  StgInt   localsparks;
  rtsTime  clock;
} StgTSOStatBuf;

/*
 * GRAN: We distinguish between the various classes of threads in 
 * the system.
 */
typedef enum {
  AdvisoryPriority,
  MandatoryPriority,
  RevalPriority
} StgThreadPriority;

/*
 * PROFILING info in a TSO
 */
typedef struct {
  CostCentreStack *CCCS;	/* thread's current CCS */
} StgTSOProfInfo;

/*
 * PAR info in a TSO
 */
typedef StgTSOStatBuf StgTSOParInfo;

/*
 * DIST info in a TSO
 */
typedef struct {
  StgThreadPriority  priority;   
  StgInt             revalTid;   /* ToDo: merge both into 1 word */
  StgInt             revalSlot;
} StgTSODistInfo;

/*
 * GRAN info in a TSO
 */
typedef StgTSOStatBuf StgTSOGranInfo;

/*
 * There is no TICKY info in a TSO at this time.
 */

/*
 * Thread IDs are 32 bits.
 */
typedef StgWord32 StgThreadID;

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

/* 
 * ToDo: make this structure sensible on a non-32-bit arch.
 */

typedef struct StgTSO_ {
  StgHeader          header;

  struct StgTSO_*    link;	     /* Links threads onto blocking queues */
  struct StgTSO_*    global_link;    /* Links all threads together */
  
  StgWord16           what_next;  /* Values defined in Constants.h */
  StgWord16           why_blocked;  /* Values defined in Constants.h */
  StgTSOBlockInfo    block_info;
  struct StgTSO_*    blocked_exceptions;
  StgThreadID        id;
  int                saved_errno;
  struct Task_*      bound;          // non-NULL for a bound thread
  struct StgTRecHeader_ *trec;       /* STM transaction record */
  
#ifdef TICKY_TICKY
  /* TICKY-specific stuff would go here. */
#endif
#ifdef PROFILING
   StgTSOProfInfo prof;
#endif
#ifdef PAR
   StgTSOParInfo par;
#endif
#ifdef GRAN
   StgTSOGranInfo gran;
#endif
#ifdef DIST
   StgTSODistInfo dist;
#endif

  /* The thread stack... */
  StgWord    	     stack_size;     /* stack size in *words* */
  StgWord            max_stack_size; /* maximum stack size in *words* */
  StgPtr             sp;
  
  StgWord            stack[FLEXIBLE_ARRAY];
} StgTSO;

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
	
        BlockedOnBlackHole     the BLACKHOLE_BQ     the BLACKHOLE_BQ's queue
	
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

   tso->blocked_exceptions is either:

      NULL             if async exceptions are unblocked.

      END_TSO_QUEUE    if async exceptions are blocked, but no threads
                       are currently waiting to deliver.

      (StgTSO *)tso    if threads are currently awaiting delivery of
                       exceptions to this thread.

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
#if IN_STG_CODE
#define END_TSO_QUEUE  (stg_END_TSO_QUEUE_closure)
#else
#define END_TSO_QUEUE  ((StgTSO *)(void*)&stg_END_TSO_QUEUE_closure)
#endif

#if defined(PAR) || defined(GRAN)
/* this is the NIL ptr for a blocking queue */
# define END_BQ_QUEUE  ((StgBlockingQueueElement *)(void*)&stg_END_TSO_QUEUE_closure)
/* this is the NIL ptr for a blocked fetch queue (as in PendingFetches in GUM) */
# define END_BF_QUEUE  ((StgBlockedFetch *)(void*)&stg_END_TSO_QUEUE_closure)
#endif
/* ToDo?: different name for end of sleeping queue ? -- HWL */

#endif /* TSO_H */
