/* -----------------------------------------------------------------------------
 * $Id: TSO.h,v 1.9 1999/12/01 14:34:49 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * The definitions for Thread State Objects.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TSO_H
#define TSO_H

#if defined(PROFILING)
typedef struct {
  CostCentreStack *CCCS;	/* thread's current CCS */
} StgTSOProfInfo;
#else /* !PROFILING */
typedef struct {
} StgTSOProfInfo;
#endif /* PROFILING */

#if defined(PAR)
typedef struct {
} StgTSOParInfo;
#else /* !PAR */
typedef struct {
} StgTSOParInfo;
#endif /* PAR */

#if defined(TICKY_TICKY)
typedef struct {
} StgTSOTickyInfo;
#else /* !TICKY_TICKY */
typedef struct {
} StgTSOTickyInfo;
#endif /* TICKY_TICKY */

typedef enum {
    tso_state_runnable,
    tso_state_stopped
} StgTSOState;

typedef enum {
  ThreadEnterGHC,
  ThreadRunGHC,
  ThreadEnterHugs,
  ThreadKilled,
  ThreadComplete
} StgTSOWhatNext;

/*
 * We are completely paranoid and make thread IDs 64 bits to avoid
 * having to worry about overflow.  A little calculation shows that
 * even doing 10^6 forks per second would take 35 million years to
 * overflow a 64 bit thread ID :-)
 *
 */
typedef StgWord32 StgThreadID;

/*
 * This type is returned to the scheduler by a thread that has
 * stopped for one reason or another.
 */

typedef enum {
  HeapOverflow,			/* might also be StackOverflow */
  StackOverflow,
  ThreadYielding,
  ThreadBlocked,
  ThreadFinished
} StgThreadReturnCode;

/* 
 * Threads may be blocked for several reasons.  A blocked thread will
 * have the reason in the why_blocked field of the TSO, and some
 * further info (such as the closure the thread is blocked on, or the
 * file descriptor if the thread is waiting on I/O) in the block_info
 * field.
 */

typedef enum {
  NotBlocked,
  BlockedOnMVar,
  BlockedOnBlackHole,
  BlockedOnException,
  BlockedOnRead,
  BlockedOnWrite,
  BlockedOnDelay
} StgTSOBlockReason;

typedef union {
  StgClosure *closure;
  struct StgTSO_ *tso;
  int fd;
  unsigned int delay;
} StgTSOBlockInfo;

/*
 * TSOs live on the heap, and therefore look just like heap objects.
 * Large TSOs will live in their own "block group" allocated by the
 * storage manager, and won't be copied during garbage collection.
 */

typedef struct StgTSO_ {
  StgHeader          header;
  struct StgTSO_*    link;
  StgMutClosure *    mut_link;	/* TSO's are mutable of course! */
  StgTSOWhatNext     whatNext;
  StgTSOBlockReason  why_blocked;
  StgTSOBlockInfo    block_info;
  struct StgTSO_*    blocked_exceptions;
  StgThreadID        id;
  StgTSOTickyInfo    ticky; 
  StgTSOProfInfo     prof;
  StgTSOParInfo      par;
  /* GranSim Info? */

  /* The thread stack... */
  StgWord    	     stack_size;     /* stack size in *words* */
  StgWord            max_stack_size; /* maximum stack size in *words* */
  StgPtr             sp;
  StgUpdateFrame*    su;
  StgPtr             splim;
  
  StgWord            stack[0];
} StgTSO;

/* -----------------------------------------------------------------------------
   Invariants:

   An active thread has the following properties:

      tso->stack < tso->sp < tso->stack+tso->stack_size
      tso->stack_size <= tso->max_stack_size
      tso->splim == tso->stack + RESERVED_STACK_WORDS;
      
      RESERVED_STACK_WORDS is large enough for any heap-check or
      stack-check failure.

      The size of the TSO struct plus the stack is either
        (a) smaller than a block, or
	(b) a multiple of BLOCK_SIZE

	tso->block_reason      tso->block_info      location
        ----------------------------------------------------------------------
	NotBlocked             NULL                 runnable_queue, or running
	
        BlockedOnBlackHole     the BLACKHOLE_BQ     the BLACKHOLE_BQ's queue
	
        BlockedOnMVar          the MVAR             the MVAR's queue
	
        BlockedOnException     the TSO              TSO->blocked_exception

        BlockedOnRead          NULL                 blocked_queue
        BlockedOnWrite         NULL		    blocked_queue
        BlockedOnDelay         NULL                 blocked_queue

      tso->link == END_TSO_QUEUE, if the thread is currently running.

   A zombie thread has the following properties:
      
      tso->whatNext == ThreadComplete or ThreadKilled
      tso->link     ==  (could be on some queue somewhere)
      tso->su       ==  tso->stack + tso->stack_size
      tso->sp       ==  tso->stack + tso->stack_size - 1 (i.e. top stack word)
      tso->sp[0]    ==  return value of thread, if whatNext == ThreadComplete,
                        exception             , if whatNext == ThreadKilled

      (tso->sp is left pointing at the top word on the stack so that
      the return value or exception will be retained by a GC).

   tso->blocked_exceptions is either:

      NULL             if async exceptions are unblocked.

      END_TSO_QUEUE    if async exceptions are blocked, but no threads
                       are currently waiting to deliver.

      (StgTSO *)tso    if threads are currently awaiting delivery of
                       exceptions to this thread.

 ---------------------------------------------------------------------------- */

/* Workaround for a bug/quirk in gcc on certain architectures.
 * symptom is that (&tso->stack - &tso->header) /=  sizeof(StgTSO)
 * in other words, gcc pads the structure at the end.
 */

extern StgTSO dummy_tso;

#define TSO_STRUCT_SIZE \
   ((int)&(dummy_tso).stack - (int)&(dummy_tso).header)

#define TSO_STRUCT_SIZEW (TSO_STRUCT_SIZE / sizeof(W_))

#endif /* TSO_H */
