/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2002
 *
 * External API for the scheduler.  For most uses, the functions in
 * RtsAPI.h should be enough.
 *
 * ---------------------------------------------------------------------------*/

#ifndef SCHEDAPI_H
#define SCHEDAPI_H

#if defined(GRAN)
/* Dummy def for NO_PRI if not in GranSim */
#define NO_PRI  0
#endif

extern SchedulerStatus waitThread(StgTSO *main_thread, /*out*/StgClosure **ret,
                                  Capability *initialCapability);

/* 
 * Creating threads
 */
#if defined(GRAN)
extern StgTSO *createThread(nat stack_size, StgInt pri);
#else
extern StgTSO *createThread(nat stack_size);
#endif
#if defined(PAR) || defined(SMP)
extern void taskStart(void);
#endif
extern void scheduleThread(StgTSO *tso);
extern SchedulerStatus scheduleWaitThread(StgTSO *tso, /*out*/HaskellObj* ret,
                                          Capability *initialCapability);

INLINE_HEADER void pushClosure   (StgTSO *tso, StgWord c) {
  tso->sp--;
  tso->sp[0] = (W_) c;
}

INLINE_HEADER StgTSO *
createGenThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
#if defined(GRAN)
  t = createThread(stack_size, NO_PRI);
#else
  t = createThread(stack_size);
#endif
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

INLINE_HEADER StgTSO *
createIOThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
#if defined(GRAN)
  t = createThread(stack_size, NO_PRI);
#else
  t = createThread(stack_size);
#endif
  pushClosure(t, (W_)&stg_noforceIO_info);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

/*
 * Same as above, but also evaluate the result of the IO action
 * to whnf while we're at it.
 */

INLINE_HEADER StgTSO *
createStrictIOThread(nat stack_size,  StgClosure *closure) {
  StgTSO *t;
#if defined(GRAN)
  t = createThread(stack_size, NO_PRI);
#else
  t = createThread(stack_size);
#endif
  pushClosure(t, (W_)&stg_forceIO_info);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
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

#endif
