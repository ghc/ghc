/* -----------------------------------------------------------------------------
 * $Id: Schedule.h,v 1.7 1999/09/10 11:11:52 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Prototypes for functions in Schedule.c 
 * (RTS internal scheduler interface)
 *
 * ---------------------------------------------------------------------------*/

/* 
 * Initialisation
 */

void    initScheduler(void);

/* 
 * Miscellany
 */

void    awakenBlockedQueue(StgTSO *tso);
StgTSO *unblockOne(StgTSO *tso);
void    initThread(StgTSO *tso, nat stack_size);
void    interruptStgRts(void);
void    raiseAsync(StgTSO *tso, StgClosure *exception);

extern  nat context_switch;

void    awaitEvent(rtsBool wait);  /* In Select.c */
extern  nat ticks_since_select;	   /* ditto */

extern  StgTSO *run_queue_hd, *run_queue_tl;
extern  StgTSO *blocked_queue_hd, *blocked_queue_tl;

#ifdef DEBUG
extern void printThreadBlockage(StgTSO *tso);
#endif

#ifdef COMPILING_RTS_MAIN
extern DLLIMPORT StgTSO *MainTSO; /* temporary hack */
#else
extern StgTSO *MainTSO; /* temporary hack */
#endif
#define END_TSO_QUEUE  ((StgTSO *)(void*)&END_TSO_QUEUE_closure)

/* Add a thread to the end of the run queue.
 * NOTE: tso->link should be END_TSO_QUEUE before calling this macro.
 */
#define PUSH_ON_RUN_QUEUE(tso)			\
    if (run_queue_hd == END_TSO_QUEUE) {        \
      run_queue_hd = tso;			\
    } else {					\
      run_queue_tl->link = tso;			\
    }						\
    run_queue_tl = tso;

#define PUSH_ON_BLOCKED_QUEUE(tso)		\
    if (blocked_queue_hd == END_TSO_QUEUE) {    \
      blocked_queue_hd = tso;			\
    } else {					\
      blocked_queue_tl->link = tso;		\
    }						\
    blocked_queue_tl = tso;

#define END_CAF_LIST  stgCast(StgCAF*,(void*)&END_TSO_QUEUE_closure)
