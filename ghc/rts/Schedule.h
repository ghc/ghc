/* -----------------------------------------------------------------------------
 * $Id: Schedule.h,v 1.2 1998/12/02 13:28:46 simonm Exp $
 *
 * (c) The GHC Team 1998
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

void    awaken_blocked_queue(StgTSO *tso);

void    initThread(StgTSO *tso, nat stack_size);

void    interruptStgRts(void);

extern  nat context_switch;

extern  StgTSO *run_queue_hd, *run_queue_tl;
extern  StgTSO *blocked_queue_hd, *blocked_queue_tl;

extern StgTSO *MainTSO; /* temporary hack */

#define END_TSO_QUEUE  ((StgTSO *)(void*)&END_TSO_QUEUE_closure)

#define PUSH_ON_RUN_QUEUE(tso)			\
    if (run_queue_hd == END_TSO_QUEUE) {        \
      run_queue_hd = tso;			\
    } else {					\
      run_queue_tl->link = tso;			\
    }						\
    run_queue_tl = tso;

#define END_CAF_LIST  stgCast(StgCAF*,(void*)&END_TSO_QUEUE_closure)
