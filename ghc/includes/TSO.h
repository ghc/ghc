/* -----------------------------------------------------------------------------
 * $Id: TSO.h,v 1.3 1999/01/13 17:25:55 simonm Exp $
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

#if defined(TICKY)
typedef struct {
} StgTSOTickyInfo;
#else /* !TICKY */
typedef struct {
} StgTSOTickyInfo;
#endif /* TICKY */

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
 */
typedef StgNat32 StgThreadID;

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
 * TSOs live on the heap, and therefore look just like heap objects.
 * Large TSOs will live in their own "block group" allocated by the
 * storage manager, and won't be copied during garbage collection.
 */

typedef struct StgTSO_ {
  StgHeader          header;
  struct StgTSO_*    link;
  StgMutClosure *    mut_link;	/* TSO's are mutable of course! */
  StgTSOWhatNext     whatNext;
  StgTSOState        state;	/* necessary? */
  StgThreadID        id;
  /* Exception Handlers */
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

extern StgTSO      *CurrentTSO;


/* Workaround for a bug/quirk in gcc on certain architectures.
 * symptom is that (&tso->stack - &tso->header) /=  sizeof(StgTSO)
 * in other words, gcc pads the structure at the end.
 */

extern StgTSO dummy_tso;

#define TSO_STRUCT_SIZE \
   ((int)&(dummy_tso).stack - (int)&(dummy_tso).header)

#define TSO_STRUCT_SIZEW (TSO_STRUCT_SIZE / sizeof(W_))

#endif /* TSO_H */
