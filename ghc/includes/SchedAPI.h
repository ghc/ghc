/* -----------------------------------------------------------------------------
 * $Id: SchedAPI.h,v 1.2 1998/12/02 13:21:33 simonm Exp $
 *
 * (c) The GHC Team 1998
 *
 * External API for the scheduler.  For most uses, the functions in
 * RtsAPI.h should be enough.
 *
 * ---------------------------------------------------------------------------*/

#ifndef SCHEDAPI_H
#define SCHEDAPI_H

/*
 * Running the scheduler
 */

typedef enum {
    Success,      
    Killed,	 /* another thread killed us                           */
    Interrupted, /* stopped in response to a call to interruptStgRts   */
    Deadlock,   
    AllBlocked,  /* subtly different from Deadlock                     */
} SchedulerStatus;
      
SchedulerStatus schedule(StgTSO *main_thread, /*out*/StgClosure **ret);

/* 
 * Creating thraeds
 */

StgTSO *createThread   (nat stack_size);

static inline void pushClosure   (StgTSO *tso, StgClosure *c) {
  tso->sp--;
  tso->sp[0] = (W_) c;
}

static inline void pushRealWorld (StgTSO *tso) {
  tso->sp--;
  tso->sp[0] = (W_) REALWORLD_TAG;
}
static inline StgTSO *
createGenThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
  t = createThread(stack_size);
  pushClosure(t,closure);
  return t;
}

static inline StgTSO *
createIOThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
  t = createThread(stack_size);
  pushRealWorld(t);
  pushClosure(t,closure);
  return t;
}

/* 
 * Killing threads
 */

void    deleteThread(StgTSO *tso);

/*
 * Reverting CAFs
 */

void RevertCAFs(void);

#endif
