/* -----------------------------------------------------------------------------
 * $Id: Schedule.c,v 1.19 1999/03/20 17:33:07 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Scheduler
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "SchedAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "StgRun.h"
#include "StgStartup.h"
#include "GC.h"
#include "Hooks.h"
#include "Schedule.h"
#include "StgMiscClosures.h"
#include "Storage.h"
#include "Evaluator.h"
#include "Printer.h"
#include "Main.h"
#include "Signals.h"
#include "Profiling.h"
#include "Sanity.h"

StgTSO *run_queue_hd, *run_queue_tl;
StgTSO *blocked_queue_hd, *blocked_queue_tl;
StgTSO *ccalling_threads;

#define MAX_SCHEDULE_NESTING 256
nat next_main_thread;
StgTSO *main_threads[MAX_SCHEDULE_NESTING];

static void GetRoots(void);
static StgTSO *threadStackOverflow(StgTSO *tso);

/* flag set by signal handler to precipitate a context switch */
nat context_switch;
/* if this flag is set as well, give up execution */
static nat interrupted;

/* Next thread ID to allocate */
StgThreadID next_thread_id = 1;

/*
 * Pointers to the state of the current thread.
 * Rule of thumb: if CurrentTSO != NULL, then we're running a Haskell
 * thread.  If CurrentTSO == NULL, then we're at the scheduler level.
 */
StgTSO      *CurrentTSO;
StgRegTable  MainRegTable;

/*
 * The thread state for the main thread.
 */
StgTSO   *MainTSO;

/* The smallest stack size that makes any sense is:
 *    RESERVED_STACK_WORDS    (so we can get back from the stack overflow)
 *  + sizeofW(StgStopFrame)   (the stg_stop_thread_info frame)
 *  + 1                       (the realworld token for an IO thread)
 *  + 1                       (the closure to enter)
 *
 * A thread with this stack will bomb immediately with a stack
 * overflow, which will increase its stack size.  
 */

#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 2)

/* -----------------------------------------------------------------------------
 * Static functions
 * -------------------------------------------------------------------------- */
static void unblockThread(StgTSO *tso);

/* -----------------------------------------------------------------------------
   Create a new thread.

   The new thread starts with the given stack size.  Before the
   scheduler can run, however, this thread needs to have a closure
   (and possibly some arguments) pushed on its stack.  See
   pushClosure() in Schedule.h.

   createGenThread() and createIOThread() (in SchedAPI.h) are
   convenient packaged versions of this function.
   -------------------------------------------------------------------------- */

StgTSO *
createThread(nat stack_size)
{
  StgTSO *tso;

  /* catch ridiculously small stack sizes */
  if (stack_size < MIN_STACK_WORDS + TSO_STRUCT_SIZEW) {
    stack_size = MIN_STACK_WORDS + TSO_STRUCT_SIZEW;
  }

  tso = (StgTSO *)allocate(stack_size);
  TICK_ALLOC_TSO(stack_size-sizeofW(StgTSO),0);
  
  initThread(tso, stack_size - TSO_STRUCT_SIZEW);
  return tso;
}

void
initThread(StgTSO *tso, nat stack_size)
{
  SET_INFO(tso,&TSO_info);
  tso->whatNext     = ThreadEnterGHC;
  tso->id           = next_thread_id++;
  tso->blocked_on   = NULL;

  tso->splim        = (P_)&(tso->stack) + RESERVED_STACK_WORDS;
  tso->stack_size   = stack_size;
  tso->max_stack_size = round_to_mblocks(RtsFlags.GcFlags.maxStkSize) 
                              - TSO_STRUCT_SIZEW;
  tso->sp           = (P_)&(tso->stack) + stack_size;

#ifdef PROFILING
  tso->prof.CCCS = CCS_MAIN;
#endif

  /* put a stop frame on the stack */
  tso->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure*)tso->sp,(StgInfoTable *)&stg_stop_thread_info,CCS_MAIN);
  tso->su = (StgUpdateFrame*)tso->sp;

  IF_DEBUG(scheduler,belch("Initialised thread %ld, stack size = %lx words\n", 
			   tso->id, tso->stack_size));

  /* Put the new thread on the head of the runnable queue.
   * The caller of createThread better push an appropriate closure
   * on this thread's stack before the scheduler is invoked.
   */
  tso->link = run_queue_hd;
  run_queue_hd = tso;
  if (run_queue_tl == END_TSO_QUEUE) {
    run_queue_tl = tso;
  }

  IF_DEBUG(scheduler,printTSO(tso));
}

/* -----------------------------------------------------------------------------
 * initScheduler()
 *
 * Initialise the scheduler.  This resets all the queues - if the
 * queues contained any threads, they'll be garbage collected at the
 * next pass.
 * -------------------------------------------------------------------------- */

void initScheduler(void)
{
  run_queue_hd      = END_TSO_QUEUE;
  run_queue_tl      = END_TSO_QUEUE;
  blocked_queue_hd  = END_TSO_QUEUE;
  blocked_queue_tl  = END_TSO_QUEUE;
  ccalling_threads  = END_TSO_QUEUE;
  next_main_thread  = 0;

  context_switch = 0;
  interrupted    = 0;

  enteredCAFs = END_CAF_LIST;
}

/* -----------------------------------------------------------------------------
   Main scheduling loop.

   We use round-robin scheduling, each thread returning to the
   scheduler loop when one of these conditions is detected:

      * stack overflow
      * out of heap space
      * timer expires (thread yields)
      * thread blocks
      * thread ends
   -------------------------------------------------------------------------- */

SchedulerStatus schedule(StgTSO *main, StgClosure **ret_val)
{
  StgTSO *t;
  StgThreadReturnCode ret;
  StgTSO **MainTSO;
  rtsBool in_ccall_gc;

  /* Return value is NULL by default, it is only filled in if the
   * main thread completes successfully.
   */
  if (ret_val) { *ret_val = NULL; }

  /* Save away a pointer to the main thread so that we can keep track
   * of it should a garbage collection happen.  We keep a stack of
   * main threads in order to support scheduler re-entry.  We can't
   * use the normal TSO linkage for this stack, because the main TSO
   * may need to be linked onto other queues.
   */
  main_threads[next_main_thread] = main;
  MainTSO = &main_threads[next_main_thread];
  next_main_thread++;
  IF_DEBUG(scheduler,
	   fprintf(stderr, "Scheduler entered: nesting = %d\n", 
		   next_main_thread););

  /* Are we being re-entered? 
   */
  if (CurrentTSO != NULL) {
    /* This happens when a _ccall_gc from Haskell ends up re-entering
     * the scheduler.
     *
     * Block the current thread (put it on the ccalling_queue) and
     * continue executing.  The calling thread better have stashed
     * away its state properly and left its stack with a proper stack
     * frame on the top.
     */
    threadPaused(CurrentTSO);
    CurrentTSO->link = ccalling_threads;
    ccalling_threads = CurrentTSO;
    in_ccall_gc = rtsTrue;
    IF_DEBUG(scheduler,
	     fprintf(stderr, "Re-entry, thread %d did a _ccall_gc\n", 
		     CurrentTSO->id););
  } else {
    in_ccall_gc = rtsFalse;
  }

  /* Take a thread from the run queue.
   */
  t = run_queue_hd;
  if (t != END_TSO_QUEUE) {
    run_queue_hd = t->link;
    t->link = END_TSO_QUEUE;
    if (run_queue_hd == END_TSO_QUEUE) {
      run_queue_tl = END_TSO_QUEUE;
    }
  }

  while (t != END_TSO_QUEUE) {
    CurrentTSO = t;

    /* If we have more threads on the run queue, set up a context
     * switch at some point in the future.
     */
    if (run_queue_hd != END_TSO_QUEUE) {
      context_switch = 1;
    } else {
      context_switch = 0;
    }
    IF_DEBUG(scheduler, belch("Running thread %ld...\n", t->id));

    /* Be friendly to the storage manager: we're about to *run* this
     * thread, so we better make sure the TSO is mutable.
     */
    if (t->mut_link == NULL) {
      recordMutable((StgMutClosure *)t);
    }

    /* Run the current thread */
    switch (t->whatNext) {
    case ThreadKilled:
    case ThreadComplete:
      /* thread already killed.  Drop it and carry on. */
      goto next_thread;
    case ThreadEnterGHC:
      ret = StgRun((StgFunPtr) stg_enterStackTop);
      break;
    case ThreadRunGHC:
      ret = StgRun((StgFunPtr) stg_returnToStackTop);
      break;
    case ThreadEnterHugs:
#ifdef INTERPRETER
      {  
	  IF_DEBUG(scheduler,belch("entering Hugs"));	  
	  LoadThreadState();
	  /* CHECK_SENSIBLE_REGS(); */
	  {
	      StgClosure* c = (StgClosure *)Sp[0];
	      Sp += 1;
	      ret = enter(c);
	  }	
	  SaveThreadState();
	  break;
      }
#else
      barf("Panic: entered a BCO but no bytecode interpreter in this build");
#endif
    default:
      barf("schedule: invalid whatNext field");
    }

    /* We may have garbage collected while running the thread
     * (eg. something nefarious like _ccall_GC_ performGC), and hence
     * CurrentTSO may have moved.  Update t to reflect this.
     */
    t = CurrentTSO;
    CurrentTSO = NULL;

    /* Costs for the scheduler are assigned to CCS_SYSTEM */
#ifdef PROFILING
    CCCS = CCS_SYSTEM;
#endif

    switch (ret) {

    case HeapOverflow:
      IF_DEBUG(scheduler,belch("Thread %ld stopped: HeapOverflow\n", t->id));
      threadPaused(t);
      PUSH_ON_RUN_QUEUE(t);
      GarbageCollect(GetRoots);
      break;

    case StackOverflow:
      IF_DEBUG(scheduler,belch("Thread %ld stopped, StackOverflow\n", t->id));
      { 
	nat i;
	/* enlarge the stack */
	StgTSO *new_t = threadStackOverflow(t);
	
	/* This TSO has moved, so update any pointers to it from the
	 * main thread stack.  It better not be on any other queues...
	 * (it shouldn't be)
	 */
	for (i = 0; i < next_main_thread; i++) {
	  if (main_threads[i] == t) {
	    main_threads[i] = new_t;
	  }
	}
	t = new_t;
      }
      PUSH_ON_RUN_QUEUE(t);
      break;

    case ThreadYielding:
      IF_DEBUG(scheduler,
               if (t->whatNext == ThreadEnterHugs) {
		   /* ToDo: or maybe a timer expired when we were in Hugs?
		    * or maybe someone hit ctrl-C
                    */
                   belch("Thread %ld stopped to switch to Hugs\n", t->id);
               } else {
                   belch("Thread %ld stopped, timer expired\n", t->id);
               }
               );
      threadPaused(t);
      if (interrupted) {
          IF_DEBUG(scheduler,belch("Scheduler interrupted - returning"));
	  deleteThread(t);
	  while (run_queue_hd != END_TSO_QUEUE) {
	      run_queue_hd = t->link;
	      deleteThread(t);
	  }
	  run_queue_tl = END_TSO_QUEUE;
	  /* ToDo: should I do the same with blocked queues? */
          return Interrupted;
      }

      /* Put the thread back on the run queue, at the end.
       * t->link is already set to END_TSO_QUEUE.
       */
      ASSERT(t->link == END_TSO_QUEUE);
      if (run_queue_tl == END_TSO_QUEUE) {
        run_queue_hd = run_queue_tl = t;
      } else {
        ASSERT(get_itbl(run_queue_tl)->type == TSO);
	if (run_queue_hd == run_queue_tl) {
	  run_queue_hd->link = t;
	  run_queue_tl = t;
	} else {
	  run_queue_tl->link = t;
	  run_queue_tl = t;
	}
      }
      break;

    case ThreadBlocked:
      IF_DEBUG(scheduler,belch("Thread %ld stopped, blocking\n", t->id));
      threadPaused(t);
      /* assume the thread has put itself on some blocked queue
       * somewhere.
       */
      break;

    case ThreadFinished:
      IF_DEBUG(scheduler,belch("Thread %ld finished\n", t->id));
      t->whatNext = ThreadComplete;
      break;

    default:
      barf("schedule: invalid thread return code");
    }

    /* check for signals each time around the scheduler */
#ifndef __MINGW32__
    if (signals_pending()) {
      start_signal_handlers();
    }
#endif
    /* If our main thread has finished or been killed, return.
     * If we were re-entered as a result of a _ccall_gc, then
     * pop the blocked thread off the ccalling_threads stack back
     * into CurrentTSO.
     */
    if ((*MainTSO)->whatNext == ThreadComplete
	|| (*MainTSO)->whatNext == ThreadKilled) {
      next_main_thread--;
      if (in_ccall_gc) {
	CurrentTSO = ccalling_threads;
	ccalling_threads = ccalling_threads->link;
	/* remember to stub the link field of CurrentTSO */
	CurrentTSO->link = END_TSO_QUEUE;
      }
      if ((*MainTSO)->whatNext == ThreadComplete) {
	/* we finished successfully, fill in the return value */
	if (ret_val) { *ret_val = (StgClosure *)(*MainTSO)->sp[0]; };
	return Success;
      } else {
	return Killed;
      }
    }

  next_thread:
    t = run_queue_hd;
    if (t != END_TSO_QUEUE) {
      run_queue_hd = t->link;
      t->link = END_TSO_QUEUE;
      if (run_queue_hd == END_TSO_QUEUE) {
	run_queue_tl = END_TSO_QUEUE;
      }
    }
  }

  if (blocked_queue_hd != END_TSO_QUEUE) {
    return AllBlocked;
  } else {
    return Deadlock;
  }
}

/* -----------------------------------------------------------------------------
   Where are the roots that we know about?

        - all the threads on the runnable queue
        - all the threads on the blocked queue
	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   -------------------------------------------------------------------------- */

static void GetRoots(void)
{
  nat i;

  run_queue_hd      = (StgTSO *)MarkRoot((StgClosure *)run_queue_hd);
  run_queue_tl      = (StgTSO *)MarkRoot((StgClosure *)run_queue_tl);

  blocked_queue_hd  = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_hd);
  blocked_queue_tl  = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_tl);

  ccalling_threads  = (StgTSO *)MarkRoot((StgClosure *)ccalling_threads);

  for (i = 0; i < next_main_thread; i++) {
    main_threads[i] = (StgTSO *)MarkRoot((StgClosure *)main_threads[i]);
  }
}

/* -----------------------------------------------------------------------------
   performGC

   This is the interface to the garbage collector from Haskell land.
   We provide this so that external C code can allocate and garbage
   collect when called from Haskell via _ccall_GC.

   It might be useful to provide an interface whereby the programmer
   can specify more roots (ToDo).
   -------------------------------------------------------------------------- */

void (*extra_roots)(void);

void
performGC(void)
{
  GarbageCollect(GetRoots);
}

static void
AllRoots(void)
{
  GetRoots();			/* the scheduler's roots */
  extra_roots();		/* the user's roots */
}

void
performGCWithRoots(void (*get_roots)(void))
{
  extra_roots = get_roots;

  GarbageCollect(AllRoots);
}

/* -----------------------------------------------------------------------------
   Stack overflow

   If the thread has reached its maximum stack size,
   then bomb out.  Otherwise relocate the TSO into a larger chunk of
   memory and adjust its stack size appropriately.
   -------------------------------------------------------------------------- */

static StgTSO *
threadStackOverflow(StgTSO *tso)
{
  nat new_stack_size, new_tso_size, diff, stack_words;
  StgPtr new_sp;
  StgTSO *dest;

  if (tso->stack_size >= tso->max_stack_size) {
#if 0
    /* If we're debugging, just print out the top of the stack */
    printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
				     tso->sp+64));
#endif
    /* Send this thread the StackOverflow exception */
    raiseAsync(tso, (StgClosure *)&stackOverflow_closure);
    return tso;
  }

  /* Try to double the current stack size.  If that takes us over the
   * maximum stack size for this thread, then use the maximum instead.
   * Finally round up so the TSO ends up as a whole number of blocks.
   */
  new_stack_size = stg_min(tso->stack_size * 2, tso->max_stack_size);
  new_tso_size   = (nat)BLOCK_ROUND_UP(new_stack_size * sizeof(W_) + 
				       TSO_STRUCT_SIZE)/sizeof(W_);
  new_tso_size = round_to_mblocks(new_tso_size);  /* Be MBLOCK-friendly */
  new_stack_size = new_tso_size - TSO_STRUCT_SIZEW;

  IF_DEBUG(scheduler, fprintf(stderr,"increasing stack size from %d words to %d.\n", tso->stack_size, new_stack_size));

  dest = (StgTSO *)allocate(new_tso_size);
  TICK_ALLOC_TSO(new_tso_size-sizeofW(StgTSO),0);

  /* copy the TSO block and the old stack into the new area */
  memcpy(dest,tso,TSO_STRUCT_SIZE);
  stack_words = tso->stack + tso->stack_size - tso->sp;
  new_sp = (P_)dest + new_tso_size - stack_words;
  memcpy(new_sp, tso->sp, stack_words * sizeof(W_));

  /* relocate the stack pointers... */
  diff = (P_)new_sp - (P_)tso->sp; /* In *words* */
  dest->su    = (StgUpdateFrame *) ((P_)dest->su + diff);
  dest->sp    = new_sp;
  dest->splim = (P_)dest->splim + (nat)((P_)dest - (P_)tso);
  dest->stack_size = new_stack_size;
	
  /* and relocate the update frame list */
  relocate_TSO(tso, dest);

  /* Mark the old one as dead so we don't try to scavenge it during
   * garbage collection (the TSO will likely be on a mutables list in
   * some generation, but it'll get collected soon enough).  It's
   * important to set the sp and su values to just beyond the end of
   * the stack, so we don't attempt to scavenge any part of the dead
   * TSO's stack.
   */
  tso->whatNext = ThreadKilled;
  tso->sp = (P_)&(tso->stack[tso->stack_size]);
  tso->su = (StgUpdateFrame *)tso->sp;
  tso->blocked_on = NULL;
  dest->mut_link = NULL;

  IF_DEBUG(sanity,checkTSO(tso));
#if 0
  IF_DEBUG(scheduler,printTSO(dest));
#endif
  if (tso == MainTSO) { /* hack */
      MainTSO = dest;
  }
  return dest;
}

/* -----------------------------------------------------------------------------
   Wake up a queue that was blocked on some resource (usually a
   computation in progress).
   -------------------------------------------------------------------------- */

void awaken_blocked_queue(StgTSO *q)
{
  StgTSO *tso;

  while (q != END_TSO_QUEUE) {
    ASSERT(get_itbl(q)->type == TSO);
    tso = q;
    q = tso->link;
    PUSH_ON_RUN_QUEUE(tso);
    tso->blocked_on = NULL;
    IF_DEBUG(scheduler,belch("Waking up thread %ld", tso->id));
  }
}

/* -----------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   -------------------------------------------------------------------------- */

void
interruptStgRts(void)
{
    interrupted    = 1;
    context_switch = 1;
}

/* -----------------------------------------------------------------------------
   Unblock a thread

   This is for use when we raise an exception in another thread, which
   may be blocked.
   -------------------------------------------------------------------------- */

static void
unblockThread(StgTSO *tso)
{
  StgTSO *t, **last;

  if (tso->blocked_on == NULL) {
    return;  /* not blocked */
  }

  switch (get_itbl(tso->blocked_on)->type) {

  case MVAR:
    {
      StgTSO *last_tso = END_TSO_QUEUE;
      StgMVar *mvar = (StgMVar *)(tso->blocked_on);

      last = &mvar->head;
      for (t = mvar->head; t != END_TSO_QUEUE; 
	   last = &t->link, last_tso = t, t = t->link) {
	if (t == tso) {
	  *last = tso->link;
	  if (mvar->tail == tso) {
	    mvar->tail = last_tso;
	  }
	  goto done;
	}
      }
      barf("unblockThread (MVAR): TSO not found");
    }

  case BLACKHOLE_BQ:
    {
      StgBlockingQueue *bq = (StgBlockingQueue *)(tso->blocked_on);

      last = &bq->blocking_queue;
      for (t = bq->blocking_queue; t != END_TSO_QUEUE; 
	   last = &t->link, t = t->link) {
	if (t == tso) {
	  *last = tso->link;
	  goto done;
	}
      }
      barf("unblockThread (BLACKHOLE): TSO not found");
    }

  default:
    barf("unblockThread");
  }

 done:
  tso->link = END_TSO_QUEUE;
  tso->blocked_on = NULL;
  PUSH_ON_RUN_QUEUE(tso);
}

/* -----------------------------------------------------------------------------
 * raiseAsync()
 *
 * The following function implements the magic for raising an
 * asynchronous exception in an existing thread.
 *
 * We first remove the thread from any queue on which it might be
 * blocked.  The possible blockages are MVARs and BLACKHOLE_BQs.
 *
 * We strip the stack down to the innermost CATCH_FRAME, building
 * thunks in the heap for all the active computations, so they can 
 * be restarted if necessary.  When we reach a CATCH_FRAME, we build
 * an application of the handler to the exception, and push it on
 * the top of the stack.
 * 
 * How exactly do we save all the active computations?  We create an
 * AP_UPD for every UpdateFrame on the stack.  Entering one of these
 * AP_UPDs pushes everything from the corresponding update frame
 * upwards onto the stack.  (Actually, it pushes everything up to the
 * next update frame plus a pointer to the next AP_UPD object.
 * Entering the next AP_UPD object pushes more onto the stack until we
 * reach the last AP_UPD object - at which point the stack should look
 * exactly as it did when we killed the TSO and we can continue
 * execution by entering the closure on top of the stack.
 *
 * We can also kill a thread entirely - this happens if either (a) the 
 * exception passed to raiseAsync is NULL, or (b) there's no
 * CATCH_FRAME on the stack.  In either case, we strip the entire
 * stack and replace the thread with a zombie.
 *
 * -------------------------------------------------------------------------- */
 
void 
deleteThread(StgTSO *tso)
{
  raiseAsync(tso,NULL);
}

void
raiseAsync(StgTSO *tso, StgClosure *exception)
{
  StgUpdateFrame* su = tso->su;
  StgPtr          sp = tso->sp;
  
  /* Thread already dead? */
  if (tso->whatNext == ThreadComplete || tso->whatNext == ThreadKilled) {
    return;
  }

  IF_DEBUG(scheduler, belch("Raising exception in thread %ld.", tso->id));

  /* Remove it from any blocking queues */
  unblockThread(tso);

  /* The stack freezing code assumes there's a closure pointer on
   * the top of the stack.  This isn't always the case with compiled
   * code, so we have to push a dummy closure on the top which just
   * returns to the next return address on the stack.
   */
  if ( LOOKS_LIKE_GHC_INFO((void*)*sp) ) {
    *(--sp) = (W_)&dummy_ret_closure;
  }

  while (1) {
    int words = ((P_)su - (P_)sp) - 1;
    nat i;
    StgAP_UPD * ap;

    /* If we find a CATCH_FRAME, and we've got an exception to raise,
     * then build PAP(handler,exception), and leave it on top of
     * the stack ready to enter.
     */
    if (get_itbl(su)->type == CATCH_FRAME && exception != NULL) {
      StgCatchFrame *cf = (StgCatchFrame *)su;
      /* we've got an exception to raise, so let's pass it to the
       * handler in this frame.
       */
      ap = (StgAP_UPD *)allocate(sizeofW(StgPAP) + 1);
      TICK_ALLOC_THK(2,0);
      SET_HDR(ap,&PAP_info,cf->header.prof.ccs);
	      
      ap->n_args = 1;
      ap->fun = cf->handler;
      ap->payload[0] = (P_)exception;

      /* sp currently points to the word above the CATCH_FRAME on the
       * stack.  Replace the CATCH_FRAME with a pointer to the new handler
       * application.
       */
      sp += sizeofW(StgCatchFrame);
      sp[0] = (W_)ap;
      tso->su = cf->link;
      tso->sp = sp;
      tso->whatNext = ThreadEnterGHC;
      return;
    }

    /* First build an AP_UPD consisting of the stack chunk above the
     * current update frame, with the top word on the stack as the
     * fun field.
     */
    ap = (StgAP_UPD *)allocate(AP_sizeW(words));
    TICK_ALLOC_THK(words+1,0);
    
    ASSERT(words >= 0);
    
    ap->n_args = words;
    ap->fun    = (StgClosure *)sp[0];
    sp++;
    for(i=0; i < (nat)words; ++i) {
      ap->payload[i] = (P_)*sp++;
    }
    
    switch (get_itbl(su)->type) {
      
    case UPDATE_FRAME:
      {
	SET_HDR(ap,&AP_UPD_info,su->header.prof.ccs /* ToDo */); 
	
	IF_DEBUG(scheduler,
		 fprintf(stderr,  "Updating ");
		 printPtr((P_)su->updatee); 
		 fprintf(stderr,  " with ");
		 printObj((StgClosure *)ap);
		 );
	
	/* Replace the updatee with an indirection - happily
	 * this will also wake up any threads currently
	 * waiting on the result.
	 */
	UPD_IND(su->updatee,ap);  /* revert the black hole */
	su = su->link;
	sp += sizeofW(StgUpdateFrame) -1;
	sp[0] = (W_)ap; /* push onto stack */
	break;
      }
      
    case CATCH_FRAME:
      {
	StgCatchFrame *cf = (StgCatchFrame *)su;
	StgClosure* o;
	
	/* We want a PAP, not an AP_UPD.  Fortunately, the
	 * layout's the same.
	 */
	SET_HDR(ap,&PAP_info,su->header.prof.ccs /* ToDo */);
	
	/* now build o = FUN(catch,ap,handler) */
	o = (StgClosure *)allocate(sizeofW(StgClosure)+2);
	TICK_ALLOC_THK(2,0);
	SET_HDR(o,&catch_info,su->header.prof.ccs /* ToDo */);
	o->payload[0] = (StgClosure *)ap;
	o->payload[1] = cf->handler;
	
	IF_DEBUG(scheduler,
		 fprintf(stderr,  "Built ");
		 printObj((StgClosure *)o);
		 );
	
	/* pop the old handler and put o on the stack */
	su = cf->link;
	sp += sizeofW(StgCatchFrame) - 1;
	sp[0] = (W_)o;
	break;
      }
      
    case SEQ_FRAME:
      {
	StgSeqFrame *sf = (StgSeqFrame *)su;
	StgClosure* o;
	
	SET_HDR(ap,&PAP_info,su->header.prof.ccs /* ToDo */);
	
	/* now build o = FUN(seq,ap) */
	o = (StgClosure *)allocate(sizeofW(StgClosure)+1);
	TICK_ALLOC_THK(1,0);
	SET_HDR(o,&seq_info,su->header.prof.ccs /* ToDo */);
	payloadCPtr(o,0) = (StgClosure *)ap;
	
	IF_DEBUG(scheduler,
		 fprintf(stderr,  "Built ");
		 printObj((StgClosure *)o);
		 );
	
	/* pop the old handler and put o on the stack */
	su = sf->link;
	sp += sizeofW(StgSeqFrame) - 1;
	sp[0] = (W_)o;
	break;
      }
      
    case STOP_FRAME:
      /* We've stripped the entire stack, the thread is now dead. */
      sp += sizeofW(StgStopFrame) - 1;
      sp[0] = (W_)exception;	/* save the exception */
      tso->whatNext = ThreadKilled;
      tso->su = (StgUpdateFrame *)(sp+1);
      tso->sp = sp;
      return;
      
    default:
      barf("raiseAsync");
    }
  }
  barf("raiseAsync");
}
