/* -----------------------------------------------------------------------------
 * $Id: Schedule.h,v 1.5 1999/03/16 13:20:17 simonm Exp $
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

void    awaken_blocked_queue(StgTSO *tso);
void    initThread(StgTSO *tso, nat stack_size);
void    interruptStgRts(void);
void    raiseAsync(StgTSO *tso, StgClosure *exception);

extern  nat context_switch;

extern  StgTSO *run_queue_hd, *run_queue_tl;
extern  StgTSO *blocked_queue_hd, *blocked_queue_tl;

#ifdef COMPILING_RTS_MAIN
extern DLLIMPORT StgTSO *MainTSO; /* temporary hack */
#else
extern StgTSO *MainTSO; /* temporary hack */
#endif
#define END_TSO_QUEUE  ((StgTSO *)(void*)&END_TSO_QUEUE_closure)

#define PUSH_ON_RUN_QUEUE(tso)			\
    if (run_queue_hd == END_TSO_QUEUE) {        \
      run_queue_hd = tso;			\
    } else {					\
      run_queue_tl->link = tso;			\
    }						\
    run_queue_tl = tso;

#define END_CAF_LIST  stgCast(StgCAF*,(void*)&END_TSO_QUEUE_closure)
