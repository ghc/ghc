/* -----------------------------------------------------------------------------
 * $Id: Schedule.c,v 1.11 1999/02/26 16:44:13 simonm Exp $
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
   Create a new thread.

   The new thread starts with the given stack size.  Before the
   scheduler can run, however, this thread needs to have a closure
   (and possibly some arguments) pushed on its stack.  See
   pushClosure() in Schedule.h.

   createGenThread() and createIOThread() (in Schedule.h) are
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
  tso->state        = tso_state_runnable;
  tso->id           = next_thread_id++;

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
   Delete a thread - reverting all blackholes to (something
   equivalent to) their former state.

   We create an AP_UPD for every UpdateFrame on the stack.
   Entering one of these AP_UPDs pushes everything from the corresponding
   update frame upwards onto the stack.  (Actually, it pushes everything
   up to the next update frame plus a pointer to the next AP_UPD
   object.  Entering the next AP_UPD object pushes more onto the
   stack until we reach the last AP_UPD object - at which point
   the stack should look exactly as it did when we killed the TSO
   and we can continue execution by entering the closure on top of
   the stack.   
   -------------------------------------------------------------------------- */

void deleteThread(StgTSO *tso)
{
    StgUpdateFrame* su = tso->su;
    StgPtr          sp = tso->sp;

    /* Thread already dead? */
    if (tso->whatNext == ThreadComplete || tso->whatNext == ThreadKilled) {
      return;
    }

    IF_DEBUG(scheduler, belch("Killing thread %ld.", tso->id));

    tso->whatNext = ThreadKilled; /* changed to ThreadComplete in schedule() */
    tso->link = END_TSO_QUEUE; /* Just to be on the safe side... */

    /* Threads that finish normally leave Su pointing to the word
     * beyond the top of the stack, and Sp pointing to the last word
     * on the stack, which is the return value of the thread.
     */
    if ((P_)tso->su >= tso->stack + tso->stack_size
	|| get_itbl(tso->su)->type == STOP_FRAME) {
      return;
    }
      
    IF_DEBUG(scheduler,
             fprintf(stderr, "Freezing TSO stack\n");
             printTSO(tso);
             );

    /* The stack freezing code assumes there's a closure pointer on
     * the top of the stack.  This isn't always the case with compiled
     * code, so we have to push a dummy closure on the top which just
     * returns to the next return address on the stack.
     */
    if (LOOKS_LIKE_GHC_INFO(*sp)) {
      *(--sp) = (W_)&dummy_ret_closure;
    }

    while (1) {
      int words = (stgCast(StgPtr,su) - stgCast(StgPtr,sp)) - 1;
      nat i;
      StgAP_UPD* ap = stgCast(StgAP_UPD*,allocate(AP_sizeW(words)));
      TICK_ALLOC_THK(words+1,0);

      /* First build an AP_UPD consisting of the stack chunk above the
       * current update frame, with the top word on the stack as the
       * fun field.
       */
      ASSERT(words >= 0);

      /*      if (words == 0) {  -- optimisation
	ap = stgCast(StgAP_UPD*,*stgCast(StgPtr*,sp)++);
      } else */ {
	ap->n_args = words;
	ap->fun    = stgCast(StgClosure*,*stgCast(StgPtr*,sp)++);
	for(i=0; i < (nat)words; ++i) {
	  payloadWord(ap,i) = *sp++;
	}
      }

      switch (get_itbl(su)->type) {
	
      case UPDATE_FRAME:
	{
	  SET_HDR(ap,&AP_UPD_info,su->header.prof.ccs /* ToDo */); 
	  
	  IF_DEBUG(scheduler,
		   fprintf(stderr,  "Updating ");
		   printPtr(stgCast(StgPtr,su->updatee)); 
		   fprintf(stderr,  " with ");
		   printObj(stgCast(StgClosure*,ap));
		   );

	  /* Replace the updatee with an indirection - happily
	   * this will also wake up any threads currently
	   * waiting on the result.
	   */
	  UPD_IND(su->updatee,ap);  /* revert the black hole */
	  su = su->link;
	  sp += sizeofW(StgUpdateFrame) -1;
	  sp[0] = stgCast(StgWord,ap); /* push onto stack */
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
	  o = stgCast(StgClosure*, allocate(sizeofW(StgClosure)+2));
	  TICK_ALLOC_THK(2,0);
	  SET_HDR(o,&catch_info,su->header.prof.ccs /* ToDo */);
	  payloadCPtr(o,0) = stgCast(StgClosure*,ap);
	  payloadCPtr(o,1) = cf->handler;
	  
	  IF_DEBUG(scheduler,
		   fprintf(stderr,  "Built ");
		   printObj(stgCast(StgClosure*,o));
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
          o = stgCast(StgClosure*, allocate(sizeofW(StgClosure)+1));
	  TICK_ALLOC_THK(1,0);
	  SET_HDR(o,&seq_info,su->header.prof.ccs /* ToDo */);
	  payloadCPtr(o,0) = stgCast(StgClosure*,ap);
	  
	  IF_DEBUG(scheduler,
		   fprintf(stderr,  "Built ");
		   printObj(stgCast(StgClosure*,o));
		   );
	    
	  /* pop the old handler and put o on the stack */
	  su = sf->link;
	  sp += sizeofW(StgSeqFrame) - 1;
	  sp[0] = (W_)o;
	  break;
	}
      
      case STOP_FRAME:
	return;
	
      default:
	barf("freezeTSO");
      }
    }
}

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

void 
run_all_threads ( void )
{
  while (run_queue_hd != END_TSO_QUEUE) {
    schedule(run_queue_hd, NULL);
  }
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
	      StgClosure* c = stgCast(StgClosure*,*Sp);
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
      deleteThread(t);
      t->whatNext = ThreadComplete;
      break;

    default:
      barf("schedule: invalid thread return code");
    }

    /* check for signals each time around the scheduler */
    if (signals_pending()) {
      start_signal_handlers();
    }

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
    /* ToDo: just kill this thread? */
#ifdef DEBUG
    /* If we're debugging, just print out the top of the stack */
    printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
				     tso->sp+64));
#endif
    stackOverflow(tso->max_stack_size);
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
   * some generation, but it'll get collected soon enough).
   */
  tso->whatNext = ThreadKilled;
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
    IF_DEBUG(scheduler,belch("Waking up thread %ld", tso->id));
  }
}

/* -----------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   -------------------------------------------------------------------------- */

void interruptStgRts(void)
{
    interrupted    = 1;
    context_switch = 1;
}

