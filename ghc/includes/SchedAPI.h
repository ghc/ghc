/* -----------------------------------------------------------------------------
 * $Id: SchedAPI.h,v 1.10 2000/04/14 15:18:05 sewardj Exp $
 *
 * (c) The GHC Team 1998
 *
 * External API for the scheduler.  For most uses, the functions in
 * RtsAPI.h should be enough.
 *
 * ---------------------------------------------------------------------------*/

#ifndef SCHEDAPI_H
#define SCHEDAPI_H

#if defined(GRAN)
// Dummy def for NO_PRI if not in GranSim
#define NO_PRI  0
#endif

/* 
 * schedule() plus the thread creation functions are not part
 * part of the external RTS API, so leave them out if we're
 * not compiling rts/ bits.   -- sof 7/99
 * 
 */
SchedulerStatus waitThread(StgTSO *main_thread, /*out*/StgClosure **ret);

/* 
 * Creating threads
 */
#if defined(GRAN)
StgTSO *createThread(nat stack_size, StgInt pri);
#else
StgTSO *createThread(nat stack_size);
#endif
void scheduleThread(StgTSO *tso);

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
#if defined(GRAN)
  t = createThread(stack_size, NO_PRI);
#else
  t = createThread(stack_size);
#endif
  pushClosure(t,closure);
  return t;
}

static inline StgTSO *
createIOThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
#if defined(GRAN)
  t = createThread(stack_size, NO_PRI);
#else
  t = createThread(stack_size);
#endif
  pushRealWorld(t);
  pushClosure(t,closure);
  return t;
}

/*
 * Same as above, but also evaluate the result of the IO action
 * to whnf while we're at it.
 */

static inline StgTSO *
createStrictIOThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
#if defined(GRAN)
  t = createThread(stack_size, NO_PRI);
#else
  t = createThread(stack_size);
#endif
  pushClosure(t,closure);
  pushClosure(t,(StgClosure*)&forceIO_closure);
  return t;
}


/* 
 * Killing threads
 */
extern void deleteThread(StgTSO *tso);
extern void deleteAllThreads ( void );
extern int  howManyThreadsAvail ( void );
/*
 * Run until there are no more threads.
 */
extern void finishAllThreads ( void );

/*
 * Reverting CAFs
 */
extern void RevertCAFs ( void );

#endif
