/* ---------------------------------------------------------------------------
 * $Id: Schedule.c,v 1.57 2000/03/20 09:42:50 andy Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Scheduler
 *
 * The main scheduling code in GranSim is quite different from that in std
 * (concurrent) Haskell: while concurrent Haskell just iterates over the
 * threads in the runnable queue, GranSim is event driven, i.e. it iterates
 * over the events in the global event queue.  -- HWL
 * --------------------------------------------------------------------------*/

//@node Main scheduling code, , ,
//@section Main scheduling code

/* Version with scheduler monitor support for SMPs.

   This design provides a high-level API to create and schedule threads etc.
   as documented in the SMP design document.

   It uses a monitor design controlled by a single mutex to exercise control
   over accesses to shared data structures, and builds on the Posix threads
   library.

   The majority of state is shared.  In order to keep essential per-task state,
   there is a Capability structure, which contains all the information
   needed to run a thread: its STG registers, a pointer to its TSO, a
   nursery etc.  During STG execution, a pointer to the capability is
   kept in a register (BaseReg).

   In a non-SMP build, there is one global capability, namely MainRegTable.

   SDM & KH, 10/99
*/

//@menu
//* Includes::			
//* Variables and Data structures::  
//* Prototypes::		
//* Main scheduling loop::	
//* Suspend and Resume::	
//* Run queue code::		
//* Garbage Collextion Routines::  
//* Blocking Queue Routines::	
//* Exception Handling Routines::  
//* Debugging Routines::	
//* Index::			
//@end menu

//@node Includes, Variables and Data structures, Main scheduling code, Main scheduling code
//@subsection Includes

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
#include "Exception.h"
#include "Printer.h"
#include "Main.h"
#include "Signals.h"
#include "Profiling.h"
#include "Sanity.h"
#include "Stats.h"
#include "Sparks.h"
#include "Itimer.h"
#include "Prelude.h"
#if defined(GRAN) || defined(PAR)
# include "GranSimRts.h"
# include "GranSim.h"
# include "ParallelRts.h"
# include "Parallel.h"
# include "ParallelDebug.h"
# include "FetchMe.h"
# include "HLC.h"
#endif

#include <stdarg.h>

//@node Variables and Data structures, Prototypes, Includes, Main scheduling code
//@subsection Variables and Data structures

/* Main threads:
 *
 * These are the threads which clients have requested that we run.  
 *
 * In an SMP build, we might have several concurrent clients all
 * waiting for results, and each one will wait on a condition variable
 * until the result is available.
 *
 * In non-SMP, clients are strictly nested: the first client calls
 * into the RTS, which might call out again to C with a _ccall_GC, and
 * eventually re-enter the RTS.
 *
 * Main threads information is kept in a linked list:
 */
//@cindex StgMainThread
typedef struct StgMainThread_ {
  StgTSO *         tso;
  SchedulerStatus  stat;
  StgClosure **    ret;
#ifdef SMP
  pthread_cond_t wakeup;
#endif
  struct StgMainThread_ *link;
} StgMainThread;

/* Main thread queue.
 * Locks required: sched_mutex.
 */
static StgMainThread *main_threads;

/* Thread queues.
 * Locks required: sched_mutex.
 */
#if defined(GRAN)

StgTSO* ActiveTSO = NULL; /* for assigning system costs; GranSim-Light only */
/* rtsTime TimeOfNextEvent, EndOfTimeSlice;            now in GranSim.c */

/* 
   In GranSim we have a runable and a blocked queue for each processor.
   In order to minimise code changes new arrays run_queue_hds/tls
   are created. run_queue_hd is then a short cut (macro) for
   run_queue_hds[CurrentProc] (see GranSim.h).
   -- HWL
*/
StgTSO *run_queue_hds[MAX_PROC], *run_queue_tls[MAX_PROC];
StgTSO *blocked_queue_hds[MAX_PROC], *blocked_queue_tls[MAX_PROC];
StgTSO *ccalling_threadss[MAX_PROC];
StgTSO *all_threadss[MAX_PROC];

#else /* !GRAN */

StgTSO *run_queue_hd, *run_queue_tl;
StgTSO *blocked_queue_hd, *blocked_queue_tl;

/* Linked list of all threads.
 * Used for detecting garbage collected threads.
 */
StgTSO *all_threads;

/* Threads suspended in _ccall_GC.
 */
static StgTSO *suspended_ccalling_threads;

static void GetRoots(void);
static StgTSO *threadStackOverflow(StgTSO *tso);
#endif

/* KH: The following two flags are shared memory locations.  There is no need
       to lock them, since they are only unset at the end of a scheduler
       operation.
*/

/* flag set by signal handler to precipitate a context switch */
//@cindex context_switch
nat context_switch;

/* if this flag is set as well, give up execution */
//@cindex interrupted
rtsBool interrupted;

/* Next thread ID to allocate.
 * Locks required: sched_mutex
 */
//@cindex next_thread_id
StgThreadID next_thread_id = 1;

/*
 * Pointers to the state of the current thread.
 * Rule of thumb: if CurrentTSO != NULL, then we're running a Haskell
 * thread.  If CurrentTSO == NULL, then we're at the scheduler level.
 */
 
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

/* Free capability list.
 * Locks required: sched_mutex.
 */
#ifdef SMP
//@cindex free_capabilities
//@cindex n_free_capabilities
Capability *free_capabilities; /* Available capabilities for running threads */
nat n_free_capabilities;       /* total number of available capabilities */
#else
//@cindex MainRegTable
Capability MainRegTable;       /* for non-SMP, we have one global capability */
#endif

#if defined(GRAN)
StgTSO      *CurrentTSOs[MAX_PROC];
#else
StgTSO      *CurrentTSO;
#endif

rtsBool ready_to_gc;

/* All our current task ids, saved in case we need to kill them later.
 */
#ifdef SMP
//@cindex task_ids
task_info *task_ids;
#endif

void            addToBlockedQueue ( StgTSO *tso );

static void     schedule          ( void );
       void     interruptStgRts   ( void );
static StgTSO * createThread_     ( nat size, rtsBool have_lock );

#ifdef DEBUG
static void sched_belch(char *s, ...);
#endif

#ifdef SMP
//@cindex sched_mutex
//@cindex term_mutex
//@cindex thread_ready_cond
//@cindex gc_pending_cond
pthread_mutex_t sched_mutex       = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t term_mutex        = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  thread_ready_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t  gc_pending_cond   = PTHREAD_COND_INITIALIZER;

nat await_death;
#endif

#if defined(PAR)
StgTSO *LastTSO;
rtsTime TimeOfLastYield;
#endif

/*
 * The thread state for the main thread.
// ToDo: check whether not needed any more
StgTSO   *MainTSO;
 */


//@node Prototypes, Main scheduling loop, Variables and Data structures, Main scheduling code
//@subsection Prototypes

//@node Main scheduling loop, Suspend and Resume, Prototypes, Main scheduling code
//@subsection Main scheduling loop

/* ---------------------------------------------------------------------------
   Main scheduling loop.

   We use round-robin scheduling, each thread returning to the
   scheduler loop when one of these conditions is detected:

      * out of heap space
      * timer expires (thread yields)
      * thread blocks
      * thread ends
      * stack overflow

   Locking notes:  we acquire the scheduler lock once at the beginning
   of the scheduler loop, and release it when
    
      * running a thread, or
      * waiting for work, or
      * waiting for a GC to complete.

   ------------------------------------------------------------------------ */
//@cindex schedule
static void
schedule( void )
{
  StgTSO *t;
  Capability *cap;
  StgThreadReturnCode ret;
#if defined(GRAN)
  rtsEvent *event;
#elif defined(PAR)
  rtsSpark spark;
  StgTSO *tso;
  GlobalTaskId pe;
#endif
  rtsBool was_interrupted = rtsFalse;
  
  ACQUIRE_LOCK(&sched_mutex);

#if defined(GRAN)
# error ToDo: implement GranSim scheduler
#elif defined(PAR)
  while (!GlobalStopPending) {          /* GlobalStopPending set in par_exit */

    if (PendingFetches != END_BF_QUEUE) {
        processFetches();
    }
#else
  while (1) {
#endif

    IF_DEBUG(scheduler, printAllThreads());

    /* If we're interrupted (the user pressed ^C, or some other
     * termination condition occurred), kill all the currently running
     * threads.
     */
    if (interrupted) {
      IF_DEBUG(scheduler, sched_belch("interrupted"));
      for (t = run_queue_hd; t != END_TSO_QUEUE; t = t->link) {
	deleteThread(t);
      }
      for (t = blocked_queue_hd; t != END_TSO_QUEUE; t = t->link) {
	deleteThread(t);
      }
      run_queue_hd = run_queue_tl = END_TSO_QUEUE;
      blocked_queue_hd = blocked_queue_tl = END_TSO_QUEUE;
      interrupted = rtsFalse;
      was_interrupted = rtsTrue;
    }

    /* Go through the list of main threads and wake up any
     * clients whose computations have finished.  ToDo: this
     * should be done more efficiently without a linear scan
     * of the main threads list, somehow...
     */
#ifdef SMP
    { 
      StgMainThread *m, **prev;
      prev = &main_threads;
      for (m = main_threads; m != NULL; m = m->link) {
	switch (m->tso->what_next) {
	case ThreadComplete:
	  if (m->ret) {
	    *(m->ret) = (StgClosure *)m->tso->sp[0];
	  }
	  *prev = m->link;
	  m->stat = Success;
	  pthread_cond_broadcast(&m->wakeup);
	  break;
	case ThreadKilled:
	  *prev = m->link;
	  if (was_interrupted) {
	    m->stat = Interrupted;
	  } else {
	    m->stat = Killed;
	  }
	  pthread_cond_broadcast(&m->wakeup);
	  break;
	default:
	  break;
	}
      }
    }
#else
    /* If our main thread has finished or been killed, return.
     */
    {
      StgMainThread *m = main_threads;
      if (m->tso->what_next == ThreadComplete
	  || m->tso->what_next == ThreadKilled) {
	main_threads = main_threads->link;
	if (m->tso->what_next == ThreadComplete) {
	  /* we finished successfully, fill in the return value */
	  if (m->ret) { *(m->ret) = (StgClosure *)m->tso->sp[0]; };
	  m->stat = Success;
	  return;
	} else {
	  if (was_interrupted) {
	    m->stat = Interrupted;
	  } else {
	    m->stat = Killed;
	  }
	  return;
	}
      }
    }
#endif

    /* Top up the run queue from our spark pool.  We try to make the
     * number of threads in the run queue equal to the number of
     * free capabilities.
     */
#if defined(SMP)
    {
      nat n = n_free_capabilities;
      StgTSO *tso = run_queue_hd;

      /* Count the run queue */
      while (n > 0 && tso != END_TSO_QUEUE) {
	tso = tso->link;
	n--;
      }

      for (; n > 0; n--) {
	StgClosure *spark;
	spark = findSpark();
	if (spark == NULL) {
	  break; /* no more sparks in the pool */
	} else {
	  /* I'd prefer this to be done in activateSpark -- HWL */
	  /* tricky - it needs to hold the scheduler lock and
	   * not try to re-acquire it -- SDM */
	  StgTSO *tso;
	  tso = createThread_(RtsFlags.GcFlags.initialStkSize, rtsTrue);
	  pushClosure(tso,spark);
	  PUSH_ON_RUN_QUEUE(tso);
#ifdef PAR
	  advisory_thread_count++;
#endif
	  
	  IF_DEBUG(scheduler,
		   sched_belch("turning spark of closure %p into a thread",
			       (StgClosure *)spark));
	}
      }
      /* We need to wake up the other tasks if we just created some
       * work for them.
       */
      if (n_free_capabilities - n > 1) {
	  pthread_cond_signal(&thread_ready_cond);
      }
    }
#endif /* SMP */

    /* Check whether any waiting threads need to be woken up.  If the
     * run queue is empty, and there are no other tasks running, we
     * can wait indefinitely for something to happen.
     * ToDo: what if another client comes along & requests another
     * main thread?
     */
    if (blocked_queue_hd != END_TSO_QUEUE) {
      awaitEvent(
	   (run_queue_hd == END_TSO_QUEUE)
#ifdef SMP
	&& (n_free_capabilities == RtsFlags.ParFlags.nNodes)
#endif
	);
    }
    
    /* check for signals each time around the scheduler */
#ifndef __MINGW32__
    if (signals_pending()) {
      start_signal_handlers();
    }
#endif

    /* Detect deadlock: when we have no threads to run, there are
     * no threads waiting on I/O or sleeping, and all the other
     * tasks are waiting for work, we must have a deadlock.  Inform
     * all the main threads.
     */
#ifdef SMP
    if (blocked_queue_hd == END_TSO_QUEUE
	&& run_queue_hd == END_TSO_QUEUE
	&& (n_free_capabilities == RtsFlags.ParFlags.nNodes)
	) {
      StgMainThread *m;
      for (m = main_threads; m != NULL; m = m->link) {
	  m->ret = NULL;
	  m->stat = Deadlock;
	  pthread_cond_broadcast(&m->wakeup);
      }
      main_threads = NULL;
    }
#else /* ! SMP */
    if (blocked_queue_hd == END_TSO_QUEUE
	&& run_queue_hd == END_TSO_QUEUE) {
      StgMainThread *m = main_threads;
      m->ret = NULL;
      m->stat = Deadlock;
      main_threads = m->link;
      return;
    }
#endif

#ifdef SMP
    /* If there's a GC pending, don't do anything until it has
     * completed.
     */
    if (ready_to_gc) {
      IF_DEBUG(scheduler,sched_belch("waiting for GC"));
      pthread_cond_wait(&gc_pending_cond, &sched_mutex);
    }
    
    /* block until we've got a thread on the run queue and a free
     * capability.
     */
    while (run_queue_hd == END_TSO_QUEUE || free_capabilities == NULL) {
      IF_DEBUG(scheduler, sched_belch("waiting for work"));
      pthread_cond_wait(&thread_ready_cond, &sched_mutex);
      IF_DEBUG(scheduler, sched_belch("work now available"));
    }
#endif

#if defined(GRAN)
# error ToDo: implement GranSim scheduler
#elif defined(PAR)
    /* ToDo: phps merge with spark activation above */
    /* check whether we have local work and send requests if we have none */
    if (run_queue_hd == END_TSO_QUEUE) {  /* no runnable threads */
      /* :-[  no local threads => look out for local sparks */
      if (advisory_thread_count < RtsFlags.ParFlags.maxThreads &&
	  (pending_sparks_hd[REQUIRED_POOL] < pending_sparks_tl[REQUIRED_POOL] ||
	   pending_sparks_hd[ADVISORY_POOL] < pending_sparks_tl[ADVISORY_POOL])) {
	/* 
	 * ToDo: add GC code check that we really have enough heap afterwards!!
	 * Old comment:
	 * If we're here (no runnable threads) and we have pending
	 * sparks, we must have a space problem.  Get enough space
	 * to turn one of those pending sparks into a
	 * thread... 
	 */
	
	spark = findSpark();                /* get a spark */
	if (spark != (rtsSpark) NULL) {
	  tso = activateSpark(spark);       /* turn the spark into a thread */
	  IF_PAR_DEBUG(verbose,
		       belch("== [%x] schedule: Created TSO %p (%d); %d threads active",
			     mytid, tso, tso->id, advisory_thread_count));

	  if (tso==END_TSO_QUEUE) { /* failed to activate spark->back to loop */
	    belch("^^ failed to activate spark");
	    goto next_thread;
	  }               /* otherwise fall through & pick-up new tso */
	} else {
	  IF_PAR_DEBUG(verbose,
		       belch("^^ no local sparks (spark pool contains only NFs: %d)", 
			     spark_queue_len(ADVISORY_POOL)));
	  goto next_thread;
	}
      } else  
      /* =8-[  no local sparks => look for work on other PEs */
      {
	/*
	 * We really have absolutely no work.  Send out a fish
	 * (there may be some out there already), and wait for
	 * something to arrive.  We clearly can't run any threads
	 * until a SCHEDULE or RESUME arrives, and so that's what
	 * we're hoping to see.  (Of course, we still have to
	 * respond to other types of messages.)
	 */
	if (//!fishing &&  
	    outstandingFishes < RtsFlags.ParFlags.maxFishes ) { // &&
	  // (last_fish_arrived_at+FISH_DELAY < CURRENT_TIME)) {
	  /* fishing set in sendFish, processFish;
	     avoid flooding system with fishes via delay */
	  pe = choosePE();
	  sendFish(pe, mytid, NEW_FISH_AGE, NEW_FISH_HISTORY, 
		   NEW_FISH_HUNGER);
	}
	
	processMessages();
	goto next_thread;
	// ReSchedule(0);
      }
    } else if (PacketsWaiting()) {  /* Look for incoming messages */
      processMessages();
    }

    /* Now we are sure that we have some work available */
    ASSERT(run_queue_hd != END_TSO_QUEUE);
    /* Take a thread from the run queue, if we have work */
    t = take_off_run_queue(END_TSO_QUEUE);

    /* ToDo: write something to the log-file
    if (RTSflags.ParFlags.granSimStats && !sameThread)
        DumpGranEvent(GR_SCHEDULE, RunnableThreadsHd);
    */

    CurrentTSO = t;

    IF_DEBUG(scheduler, belch("--^^ %d sparks on [%#x] (hd=%x; tl=%x; lim=%x)", 
			      spark_queue_len(ADVISORY_POOL), CURRENT_PROC,
			      pending_sparks_hd[ADVISORY_POOL], 
			      pending_sparks_tl[ADVISORY_POOL], 
			      pending_sparks_lim[ADVISORY_POOL]));

    IF_DEBUG(scheduler, belch("--== %d threads on [%#x] (hd=%x; tl=%x)", 
			      run_queue_len(), CURRENT_PROC,
			      run_queue_hd, run_queue_tl));

    if (t != LastTSO) {
      /* 
	 we are running a different TSO, so write a schedule event to log file
	 NB: If we use fair scheduling we also have to write  a deschedule 
	     event for LastTSO; with unfair scheduling we know that the
	     previous tso has blocked whenever we switch to another tso, so
	     we don't need it in GUM for now
      */
      DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC,
		       GR_SCHEDULE, t, (StgClosure *)NULL, 0, 0);
      
    }

#else /* !GRAN && !PAR */
  
    /* grab a thread from the run queue
     */
    t = POP_RUN_QUEUE();
    IF_DEBUG(sanity,checkTSO(t));

#endif
    
    /* grab a capability
     */
#ifdef SMP
    cap = free_capabilities;
    free_capabilities = cap->link;
    n_free_capabilities--;
#else
    cap = &MainRegTable;
#endif
    
    cap->rCurrentTSO = t;
    
    /* set the context_switch flag
     */
    if (run_queue_hd == END_TSO_QUEUE)
      context_switch = 0;
    else
      context_switch = 1;

    RELEASE_LOCK(&sched_mutex);
    
    IF_DEBUG(scheduler,sched_belch("running thread %d", t->id));

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    /* Run the current thread 
     */
    switch (cap->rCurrentTSO->what_next) {
    case ThreadKilled:
    case ThreadComplete:
      /* Thread already finished, return to scheduler. */
      ret = ThreadFinished;
      break;
    case ThreadEnterGHC:
      ret = StgRun((StgFunPtr) stg_enterStackTop, cap);
      break;
    case ThreadRunGHC:
      ret = StgRun((StgFunPtr) stg_returnToStackTop, cap);
      break;
    case ThreadEnterHugs:
#ifdef INTERPRETER
      {
         StgClosure* c;
	 IF_DEBUG(scheduler,sched_belch("entering Hugs"));
	 c = (StgClosure *)(cap->rCurrentTSO->sp[0]);
	 cap->rCurrentTSO->sp += 1;
	 ret = enter(cap,c);
         break;
      }
#else
      barf("Panic: entered a BCO but no bytecode interpreter in this build");
#endif
    default:
      barf("schedule: invalid what_next field");
    }
    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    
    /* Costs for the scheduler are assigned to CCS_SYSTEM */
#ifdef PROFILING
    CCCS = CCS_SYSTEM;
#endif
    
    ACQUIRE_LOCK(&sched_mutex);

#ifdef SMP
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler (task %ld): ", pthread_self()););
#else
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler: "););
#endif
    t = cap->rCurrentTSO;
    
    switch (ret) {
    case HeapOverflow:
      /* make all the running tasks block on a condition variable,
       * maybe set context_switch and wait till they all pile in,
       * then have them wait on a GC condition variable.
       */
      IF_DEBUG(scheduler,belch("thread %ld stopped: HeapOverflow", t->id));
      threadPaused(t);
      
      ready_to_gc = rtsTrue;
      context_switch = 1;		/* stop other threads ASAP */
      PUSH_ON_RUN_QUEUE(t);
      break;
      
    case StackOverflow:
      /* just adjust the stack for this thread, then pop it back
       * on the run queue.
       */
      IF_DEBUG(scheduler,belch("thread %ld stopped, StackOverflow", t->id));
      threadPaused(t);
      { 
	StgMainThread *m;
	/* enlarge the stack */
	StgTSO *new_t = threadStackOverflow(t);
	
	/* This TSO has moved, so update any pointers to it from the
	 * main thread stack.  It better not be on any other queues...
	 * (it shouldn't be).
	 */
	for (m = main_threads; m != NULL; m = m->link) {
	  if (m->tso == t) {
	    m->tso = new_t;
	  }
	}
	threadPaused(new_t);
	PUSH_ON_RUN_QUEUE(new_t);
      }
      break;

    case ThreadYielding:
#if defined(GRAN)
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_yields++;
#elif defined(PAR)
      IF_DEBUG(par, 
	       DumpGranEvent(GR_DESCHEDULE, t));
#endif
      /* put the thread back on the run queue.  Then, if we're ready to
       * GC, check whether this is the last task to stop.  If so, wake
       * up the GC thread.  getThread will block during a GC until the
       * GC is finished.
       */
      IF_DEBUG(scheduler,
	       if (t->what_next == ThreadEnterHugs) {
		 /* ToDo: or maybe a timer expired when we were in Hugs?
		  * or maybe someone hit ctrl-C
		  */
		 belch("thread %ld stopped to switch to Hugs", t->id);
	       } else {
		 belch("thread %ld stopped, yielding", t->id);
	       }
	       );
      threadPaused(t);
      APPEND_TO_RUN_QUEUE(t);
      break;
      
    case ThreadBlocked:
#if defined(GRAN)
# error ToDo: implement GranSim scheduler
#elif defined(PAR)
      IF_DEBUG(par, 
	       DumpGranEvent(GR_DESCHEDULE, t)); 
#else
#endif
      /* don't need to do anything.  Either the thread is blocked on
       * I/O, in which case we'll have called addToBlockedQueue
       * previously, or it's blocked on an MVar or Blackhole, in which
       * case it'll be on the relevant queue already.
       */
      IF_DEBUG(scheduler,
	       fprintf(stderr, "thread %d stopped, ", t->id);
	       printThreadBlockage(t);
	       fprintf(stderr, "\n"));
      threadPaused(t);
      break;
      
    case ThreadFinished:
      /* Need to check whether this was a main thread, and if so, signal
       * the task that started it with the return value.  If we have no
       * more main threads, we probably need to stop all the tasks until
       * we get a new one.
       */
      IF_DEBUG(scheduler,belch("thread %ld finished", t->id));
      t->what_next = ThreadComplete;
#if defined(GRAN)
      // ToDo: endThread(t, CurrentProc); // clean-up the thread
#elif defined(PAR)
      advisory_thread_count--;
      if (RtsFlags.ParFlags.ParStats.Full) 
	DumpEndEvent(CURRENT_PROC, t, rtsFalse /* not mandatory */);
#endif
      break;
      
    default:
      barf("doneThread: invalid thread return code");
    }
    
#ifdef SMP
    cap->link = free_capabilities;
    free_capabilities = cap;
    n_free_capabilities++;
#endif

#ifdef SMP
    if (ready_to_gc && n_free_capabilities == RtsFlags.ParFlags.nNodes) 
#else
    if (ready_to_gc) 
#endif
      {
      /* everybody back, start the GC.
       * Could do it in this thread, or signal a condition var
       * to do it in another thread.  Either way, we need to
       * broadcast on gc_pending_cond afterward.
       */
#ifdef SMP
      IF_DEBUG(scheduler,sched_belch("doing GC"));
#endif
      GarbageCollect(GetRoots);
      ready_to_gc = rtsFalse;
#ifdef SMP
      pthread_cond_broadcast(&gc_pending_cond);
#endif
    }
#if defined(GRAN)
  next_thread:
    IF_GRAN_DEBUG(unused,
		  print_eventq(EventHd));

    event = get_next_event();

#elif defined(PAR)
  next_thread:
    /* ToDo: wait for next message to arrive rather than busy wait */

#else /* GRAN */
  /* not any more
  next_thread:
    t = take_off_run_queue(END_TSO_QUEUE);
  */
#endif /* GRAN */
  } /* end of while(1) */
}

/* A hack for Hugs concurrency support.  Needs sanitisation (?) */
void deleteAllThreads ( void )
{
  StgTSO* t;
  IF_DEBUG(scheduler,sched_belch("deleteAllThreads()"));
  for (t = run_queue_hd; t != END_TSO_QUEUE; t = t->link) {
    deleteThread(t);
  }
  for (t = blocked_queue_hd; t != END_TSO_QUEUE; t = t->link) {
    deleteThread(t);
  }
  run_queue_hd = run_queue_tl = END_TSO_QUEUE;
  blocked_queue_hd = blocked_queue_tl = END_TSO_QUEUE;
}

/* startThread and  insertThread are now in GranSim.c -- HWL */

//@node Suspend and Resume, Run queue code, Main scheduling loop, Main scheduling code
//@subsection Suspend and Resume

/* ---------------------------------------------------------------------------
 * Suspending & resuming Haskell threads.
 * 
 * When making a "safe" call to C (aka _ccall_GC), the task gives back
 * its capability before calling the C function.  This allows another
 * task to pick up the capability and carry on running Haskell
 * threads.  It also means that if the C call blocks, it won't lock
 * the whole system.
 *
 * The Haskell thread making the C call is put to sleep for the
 * duration of the call, on the susepended_ccalling_threads queue.  We
 * give out a token to the task, which it can use to resume the thread
 * on return from the C function.
 * ------------------------------------------------------------------------- */
   
StgInt
suspendThread( Capability *cap )
{
  nat tok;

  ACQUIRE_LOCK(&sched_mutex);

  IF_DEBUG(scheduler,
	   sched_belch("thread %d did a _ccall_gc\n", cap->rCurrentTSO->id));

  threadPaused(cap->rCurrentTSO);
  cap->rCurrentTSO->link = suspended_ccalling_threads;
  suspended_ccalling_threads = cap->rCurrentTSO;

  /* Use the thread ID as the token; it should be unique */
  tok = cap->rCurrentTSO->id;

#ifdef SMP
  cap->link = free_capabilities;
  free_capabilities = cap;
  n_free_capabilities++;
#endif

  RELEASE_LOCK(&sched_mutex);
  return tok; 
}

Capability *
resumeThread( StgInt tok )
{
  StgTSO *tso, **prev;
  Capability *cap;

  ACQUIRE_LOCK(&sched_mutex);

  prev = &suspended_ccalling_threads;
  for (tso = suspended_ccalling_threads; 
       tso != END_TSO_QUEUE; 
       prev = &tso->link, tso = tso->link) {
    if (tso->id == (StgThreadID)tok) {
      *prev = tso->link;
      break;
    }
  }
  if (tso == END_TSO_QUEUE) {
    barf("resumeThread: thread not found");
  }

#ifdef SMP
  while (free_capabilities == NULL) {
    IF_DEBUG(scheduler, sched_belch("waiting to resume"));
    pthread_cond_wait(&thread_ready_cond, &sched_mutex);
    IF_DEBUG(scheduler, sched_belch("resuming thread %d", tso->id));
  }
  cap = free_capabilities;
  free_capabilities = cap->link;
  n_free_capabilities--;
#else  
  cap = &MainRegTable;
#endif

  cap->rCurrentTSO = tso;

  RELEASE_LOCK(&sched_mutex);
  return cap;
}


/* ---------------------------------------------------------------------------
 * Static functions
 * ------------------------------------------------------------------------ */
static void unblockThread(StgTSO *tso);

/* ---------------------------------------------------------------------------
 * Comparing Thread ids.
 *
 * This is used from STG land in the implementation of the
 * instances of Eq/Ord for ThreadIds.
 * ------------------------------------------------------------------------ */

int cmp_thread(const StgTSO *tso1, const StgTSO *tso2) 
{ 
  StgThreadID id1 = tso1->id; 
  StgThreadID id2 = tso2->id;
 
  if (id1 < id2) return (-1);
  if (id1 > id2) return 1;
  return 0;
}

/* ---------------------------------------------------------------------------
   Create a new thread.

   The new thread starts with the given stack size.  Before the
   scheduler can run, however, this thread needs to have a closure
   (and possibly some arguments) pushed on its stack.  See
   pushClosure() in Schedule.h.

   createGenThread() and createIOThread() (in SchedAPI.h) are
   convenient packaged versions of this function.
   ------------------------------------------------------------------------ */
//@cindex createThread
#if defined(GRAN)
/* currently pri (priority) is only used in a GRAN setup -- HWL */
StgTSO *
createThread(nat stack_size, StgInt pri)
{
  return createThread_(stack_size, rtsFalse, pri);
}

static StgTSO *
createThread_(nat size, rtsBool have_lock, StgInt pri)
{
#else
StgTSO *
createThread(nat stack_size)
{
  return createThread_(stack_size, rtsFalse);
}

static StgTSO *
createThread_(nat size, rtsBool have_lock)
{
#endif
    StgTSO *tso;
    nat stack_size;

    /* First check whether we should create a thread at all */
#if defined(PAR)
  /* check that no more than RtsFlags.ParFlags.maxThreads threads are created */
  if (advisory_thread_count >= RtsFlags.ParFlags.maxThreads) {
    threadsIgnored++;
    belch("{createThread}Daq ghuH: refusing to create another thread; no more than %d threads allowed (currently %d)",
	  RtsFlags.ParFlags.maxThreads, advisory_thread_count);
    return END_TSO_QUEUE;
  }
  threadsCreated++;
#endif

#if defined(GRAN)
  ASSERT(!RtsFlags.GranFlags.Light || CurrentProc==0);
#endif

  // ToDo: check whether size = stack_size - TSO_STRUCT_SIZEW

  /* catch ridiculously small stack sizes */
  if (size < MIN_STACK_WORDS + TSO_STRUCT_SIZEW) {
    size = MIN_STACK_WORDS + TSO_STRUCT_SIZEW;
  }

  stack_size = size - TSO_STRUCT_SIZEW;

  tso = (StgTSO *)allocate(size);
  TICK_ALLOC_TSO(size-TSO_STRUCT_SIZEW, 0);

  SET_HDR(tso, &TSO_info, CCS_MAIN);
#if defined(GRAN)
  SET_GRAN_HDR(tso, ThisPE);
#endif
  tso->what_next     = ThreadEnterGHC;

  /* tso->id needs to be unique.  For now we use a heavyweight mutex to
   * protect the increment operation on next_thread_id.
   * In future, we could use an atomic increment instead.
   */
  if (!have_lock) { ACQUIRE_LOCK(&sched_mutex); }
  tso->id = next_thread_id++; 
  if (!have_lock) { RELEASE_LOCK(&sched_mutex); }

  tso->why_blocked  = NotBlocked;
  tso->blocked_exceptions = NULL;

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

  IF_DEBUG(scheduler,belch("---- Initialised TSO %ld (%p), stack size = %lx words", 
			   tso->id, tso, tso->stack_size));

  // ToDo: check this
#if defined(GRAN)
  tso->link = END_TSO_QUEUE;
  /* uses more flexible routine in GranSim */
  insertThread(tso, CurrentProc);
#else
  /* In a non-GranSim setup the pushing of a TSO onto the runq is separated
   * from its creation
   */
#endif

  /* Link the new thread on the global thread list.
   */
#if defined(GRAN)
#error ToDo
#else
  tso->global_link = all_threads;
  all_threads = tso;
#endif

#if defined(GRAN)
  tso->gran.pri = pri;
  tso->gran.magic = TSO_MAGIC; // debugging only
  tso->gran.sparkname   = 0;
  tso->gran.startedat   = CURRENT_TIME; 
  tso->gran.exported    = 0;
  tso->gran.basicblocks = 0;
  tso->gran.allocs      = 0;
  tso->gran.exectime    = 0;
  tso->gran.fetchtime   = 0;
  tso->gran.fetchcount  = 0;
  tso->gran.blocktime   = 0;
  tso->gran.blockcount  = 0;
  tso->gran.blockedat   = 0;
  tso->gran.globalsparks = 0;
  tso->gran.localsparks  = 0;
  if (RtsFlags.GranFlags.Light)
    tso->gran.clock  = Now; /* local clock */
  else
    tso->gran.clock  = 0;

  IF_DEBUG(gran,printTSO(tso));
#elif defined(PAR)
  tso->par.sparkname   = 0;
  tso->par.startedat   = CURRENT_TIME; 
  tso->par.exported    = 0;
  tso->par.basicblocks = 0;
  tso->par.allocs      = 0;
  tso->par.exectime    = 0;
  tso->par.fetchtime   = 0;
  tso->par.fetchcount  = 0;
  tso->par.blocktime   = 0;
  tso->par.blockcount  = 0;
  tso->par.blockedat   = 0;
  tso->par.globalsparks = 0;
  tso->par.localsparks  = 0;
#endif

#if defined(GRAN)
  globalGranStats.tot_threads_created++;
  globalGranStats.threads_created_on_PE[CurrentProc]++;
  globalGranStats.tot_sq_len += spark_queue_len(CurrentProc);
  globalGranStats.tot_sq_probes++;
#endif 

  IF_DEBUG(scheduler,sched_belch("created thread %ld, stack size = %lx words", 
				 tso->id, tso->stack_size));
  return tso;
}

/* ---------------------------------------------------------------------------
 * scheduleThread()
 *
 * scheduleThread puts a thread on the head of the runnable queue.
 * This will usually be done immediately after a thread is created.
 * The caller of scheduleThread must create the thread using e.g.
 * createThread and push an appropriate closure
 * on this thread's stack before the scheduler is invoked.
 * ------------------------------------------------------------------------ */

void
scheduleThread(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);

  /* Put the new thread on the head of the runnable queue.  The caller
   * better push an appropriate closure on this thread's stack
   * beforehand.  In the SMP case, the thread may start running as
   * soon as we release the scheduler lock below.
   */
  PUSH_ON_RUN_QUEUE(tso);
  THREAD_RUNNABLE();

  IF_DEBUG(scheduler,printTSO(tso));
  RELEASE_LOCK(&sched_mutex);
}

/* ---------------------------------------------------------------------------
 * startTasks()
 *
 * Start up Posix threads to run each of the scheduler tasks.
 * I believe the task ids are not needed in the system as defined.
 *  KH @ 25/10/99
 * ------------------------------------------------------------------------ */

#ifdef SMP
static void *
taskStart( void *arg STG_UNUSED )
{
  schedule();
  return NULL;
}
#endif

/* ---------------------------------------------------------------------------
 * initScheduler()
 *
 * Initialise the scheduler.  This resets all the queues - if the
 * queues contained any threads, they'll be garbage collected at the
 * next pass.
 *
 * This now calls startTasks(), so should only be called once!  KH @ 25/10/99
 * ------------------------------------------------------------------------ */

#ifdef SMP
static void
term_handler(int sig STG_UNUSED)
{
  stat_workerStop();
  ACQUIRE_LOCK(&term_mutex);
  await_death--;
  RELEASE_LOCK(&term_mutex);
  pthread_exit(NULL);
}
#endif

//@cindex initScheduler
void 
initScheduler(void)
{
#if defined(GRAN)
  nat i;

  for (i=0; i<=MAX_PROC; i++) {
    run_queue_hds[i]      = END_TSO_QUEUE;
    run_queue_tls[i]      = END_TSO_QUEUE;
    blocked_queue_hds[i]  = END_TSO_QUEUE;
    blocked_queue_tls[i]  = END_TSO_QUEUE;
    ccalling_threadss[i]  = END_TSO_QUEUE;
  }
#else
  run_queue_hd      = END_TSO_QUEUE;
  run_queue_tl      = END_TSO_QUEUE;
  blocked_queue_hd  = END_TSO_QUEUE;
  blocked_queue_tl  = END_TSO_QUEUE;
#endif 

  suspended_ccalling_threads  = END_TSO_QUEUE;

  main_threads = NULL;
  all_threads  = END_TSO_QUEUE;

  context_switch = 0;
  interrupted    = 0;

  enteredCAFs = END_CAF_LIST;

  /* Install the SIGHUP handler */
#ifdef SMP
  {
    struct sigaction action,oact;

    action.sa_handler = term_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, &oact) != 0) {
      barf("can't install TERM handler");
    }
  }
#endif

#ifdef SMP
  /* Allocate N Capabilities */
  {
    nat i;
    Capability *cap, *prev;
    cap  = NULL;
    prev = NULL;
    for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
      cap = stgMallocBytes(sizeof(Capability), "initScheduler:capabilities");
      cap->link = prev;
      prev = cap;
    }
    free_capabilities = cap;
    n_free_capabilities = RtsFlags.ParFlags.nNodes;
  }
  IF_DEBUG(scheduler,fprintf(stderr,"scheduler: Allocated %d capabilities\n",
			     n_free_capabilities););
#endif

#if defined(SMP) || defined(PAR)
  initSparkPools();
#endif
}

#ifdef SMP
void
startTasks( void )
{
  nat i;
  int r;
  pthread_t tid;
  
  /* make some space for saving all the thread ids */
  task_ids = stgMallocBytes(RtsFlags.ParFlags.nNodes * sizeof(task_info),
			    "initScheduler:task_ids");
  
  /* and create all the threads */
  for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
    r = pthread_create(&tid,NULL,taskStart,NULL);
    if (r != 0) {
      barf("startTasks: Can't create new Posix thread");
    }
    task_ids[i].id = tid;
    task_ids[i].mut_time = 0.0;
    task_ids[i].mut_etime = 0.0;
    task_ids[i].gc_time = 0.0;
    task_ids[i].gc_etime = 0.0;
    task_ids[i].elapsedtimestart = elapsedtime();
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler: Started task: %ld\n",tid););
  }
}
#endif

void
exitScheduler( void )
{
#ifdef SMP
  nat i;

  /* Don't want to use pthread_cancel, since we'd have to install
   * these silly exception handlers (pthread_cleanup_{push,pop}) around
   * all our locks.
   */
#if 0
  /* Cancel all our tasks */
  for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
    pthread_cancel(task_ids[i].id);
  }
  
  /* Wait for all the tasks to terminate */
  for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler: waiting for task %ld\n", 
			       task_ids[i].id));
    pthread_join(task_ids[i].id, NULL);
  }
#endif

  /* Send 'em all a SIGHUP.  That should shut 'em up.
   */
  await_death = RtsFlags.ParFlags.nNodes;
  for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
    pthread_kill(task_ids[i].id,SIGTERM);
  }
  while (await_death > 0) {
    sched_yield();
  }
#endif
}

/* -----------------------------------------------------------------------------
   Managing the per-task allocation areas.
   
   Each capability comes with an allocation area.  These are
   fixed-length block lists into which allocation can be done.

   ToDo: no support for two-space collection at the moment???
   -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * waitThread is the external interface for running a new computataion
 * and waiting for the result.
 *
 * In the non-SMP case, we create a new main thread, push it on the 
 * main-thread stack, and invoke the scheduler to run it.  The
 * scheduler will return when the top main thread on the stack has
 * completed or died, and fill in the necessary fields of the
 * main_thread structure.
 *
 * In the SMP case, we create a main thread as before, but we then
 * create a new condition variable and sleep on it.  When our new
 * main thread has completed, we'll be woken up and the status/result
 * will be in the main_thread struct.
 * -------------------------------------------------------------------------- */

SchedulerStatus
waitThread(StgTSO *tso, /*out*/StgClosure **ret)
{
  StgMainThread *m;
  SchedulerStatus stat;

  ACQUIRE_LOCK(&sched_mutex);
  
  m = stgMallocBytes(sizeof(StgMainThread), "waitThread");

  m->tso = tso;
  m->ret = ret;
  m->stat = NoStatus;
#ifdef SMP
  pthread_cond_init(&m->wakeup, NULL);
#endif

  m->link = main_threads;
  main_threads = m;

  IF_DEBUG(scheduler, fprintf(stderr, "scheduler: new main thread (%d)\n", 
			      m->tso->id));

#ifdef SMP
  do {
    pthread_cond_wait(&m->wakeup, &sched_mutex);
  } while (m->stat == NoStatus);
#else
  schedule();
  ASSERT(m->stat != NoStatus);
#endif

  stat = m->stat;

#ifdef SMP
  pthread_cond_destroy(&m->wakeup);
#endif

  IF_DEBUG(scheduler, fprintf(stderr, "scheduler: main thread (%d) finished\n", 
			      m->tso->id));
  free(m);

  RELEASE_LOCK(&sched_mutex);

  return stat;
}

//@node Run queue code, Garbage Collextion Routines, Suspend and Resume, Main scheduling code
//@subsection Run queue code 

#if 0
/* 
   NB: In GranSim we have many run queues; run_queue_hd is actually a macro
       unfolding to run_queue_hds[CurrentProc], thus CurrentProc is an
       implicit global variable that has to be correct when calling these
       fcts -- HWL 
*/

/* Put the new thread on the head of the runnable queue.
 * The caller of createThread better push an appropriate closure
 * on this thread's stack before the scheduler is invoked.
 */
static /* inline */ void
add_to_run_queue(tso)
StgTSO* tso; 
{
  ASSERT(tso!=run_queue_hd && tso!=run_queue_tl);
  tso->link = run_queue_hd;
  run_queue_hd = tso;
  if (run_queue_tl == END_TSO_QUEUE) {
    run_queue_tl = tso;
  }
}

/* Put the new thread at the end of the runnable queue. */
static /* inline */ void
push_on_run_queue(tso)
StgTSO* tso; 
{
  ASSERT(get_itbl((StgClosure *)tso)->type == TSO);
  ASSERT(run_queue_hd!=NULL && run_queue_tl!=NULL);
  ASSERT(tso!=run_queue_hd && tso!=run_queue_tl);
  if (run_queue_hd == END_TSO_QUEUE) {
    run_queue_hd = tso;
  } else {
    run_queue_tl->link = tso;
  }
  run_queue_tl = tso;
}

/* 
   Should be inlined because it's used very often in schedule.  The tso
   argument is actually only needed in GranSim, where we want to have the
   possibility to schedule *any* TSO on the run queue, irrespective of the
   actual ordering. Therefore, if tso is not the nil TSO then we traverse
   the run queue and dequeue the tso, adjusting the links in the queue. 
*/
//@cindex take_off_run_queue
static /* inline */ StgTSO*
take_off_run_queue(StgTSO *tso) {
  StgTSO *t, *prev;

  /* 
     qetlaHbogh Qu' ngaSbogh ghomDaQ {tso} yIteq!

     if tso is specified, unlink that tso from the run_queue (doesn't have
     to be at the beginning of the queue); GranSim only 
  */
  if (tso!=END_TSO_QUEUE) {
    /* find tso in queue */
    for (t=run_queue_hd, prev=END_TSO_QUEUE; 
	 t!=END_TSO_QUEUE && t!=tso;
	 prev=t, t=t->link) 
      /* nothing */ ;
    ASSERT(t==tso);
    /* now actually dequeue the tso */
    if (prev!=END_TSO_QUEUE) {
      ASSERT(run_queue_hd!=t);
      prev->link = t->link;
    } else {
      /* t is at beginning of thread queue */
      ASSERT(run_queue_hd==t);
      run_queue_hd = t->link;
    }
    /* t is at end of thread queue */
    if (t->link==END_TSO_QUEUE) {
      ASSERT(t==run_queue_tl);
      run_queue_tl = prev;
    } else {
      ASSERT(run_queue_tl!=t);
    }
    t->link = END_TSO_QUEUE;
  } else {
    /* take tso from the beginning of the queue; std concurrent code */
    t = run_queue_hd;
    if (t != END_TSO_QUEUE) {
      run_queue_hd = t->link;
      t->link = END_TSO_QUEUE;
      if (run_queue_hd == END_TSO_QUEUE) {
	run_queue_tl = END_TSO_QUEUE;
      }
    }
  }
  return t;
}

#endif /* 0 */

//@node Garbage Collextion Routines, Blocking Queue Routines, Run queue code, Main scheduling code
//@subsection Garbage Collextion Routines

/* ---------------------------------------------------------------------------
   Where are the roots that we know about?

        - all the threads on the runnable queue
        - all the threads on the blocked queue
	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   ------------------------------------------------------------------------ */

/* This has to be protected either by the scheduler monitor, or by the
	garbage collection monitor (probably the latter).
	KH @ 25/10/99
*/

static void GetRoots(void)
{
  StgMainThread *m;

#if defined(GRAN)
  {
    nat i;
    for (i=0; i<=RtsFlags.GranFlags.proc; i++) {
      if ((run_queue_hds[i] != END_TSO_QUEUE) && ((run_queue_hds[i] != NULL)))
	run_queue_hds[i]    = (StgTSO *)MarkRoot((StgClosure *)run_queue_hds[i]);
      if ((run_queue_tls[i] != END_TSO_QUEUE) && ((run_queue_tls[i] != NULL)))
	run_queue_tls[i]    = (StgTSO *)MarkRoot((StgClosure *)run_queue_tls[i]);
      
      if ((blocked_queue_hds[i] != END_TSO_QUEUE) && ((blocked_queue_hds[i] != NULL)))
	blocked_queue_hds[i] = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_hds[i]);
      if ((blocked_queue_tls[i] != END_TSO_QUEUE) && ((blocked_queue_tls[i] != NULL)))
	blocked_queue_tls[i] = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_tls[i]);
      if ((ccalling_threadss[i] != END_TSO_QUEUE) && ((ccalling_threadss[i] != NULL)))
	ccalling_threadss[i] = (StgTSO *)MarkRoot((StgClosure *)ccalling_threadss[i]);
    }
  }

  markEventQueue();

#else /* !GRAN */
  run_queue_hd      = (StgTSO *)MarkRoot((StgClosure *)run_queue_hd);
  run_queue_tl      = (StgTSO *)MarkRoot((StgClosure *)run_queue_tl);

  blocked_queue_hd  = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_hd);
  blocked_queue_tl  = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_tl);
#endif 

  for (m = main_threads; m != NULL; m = m->link) {
    m->tso = (StgTSO *)MarkRoot((StgClosure *)m->tso);
  }
  suspended_ccalling_threads = 
    (StgTSO *)MarkRoot((StgClosure *)suspended_ccalling_threads);

#if defined(SMP) || defined(PAR) || defined(GRAN)
  markSparkQueue();
#endif
}

/* -----------------------------------------------------------------------------
   performGC

   This is the interface to the garbage collector from Haskell land.
   We provide this so that external C code can allocate and garbage
   collect when called from Haskell via _ccall_GC.

   It might be useful to provide an interface whereby the programmer
   can specify more roots (ToDo).
   
   This needs to be protected by the GC condition variable above.  KH.
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

   If the thread has reached its maximum stack size, then raise the
   StackOverflow exception in the offending thread.  Otherwise
   relocate the TSO into a larger chunk of memory and adjust its stack
   size appropriately.
   -------------------------------------------------------------------------- */

static StgTSO *
threadStackOverflow(StgTSO *tso)
{
  nat new_stack_size, new_tso_size, diff, stack_words;
  StgPtr new_sp;
  StgTSO *dest;

  IF_DEBUG(sanity,checkTSO(tso));
  if (tso->stack_size >= tso->max_stack_size) {
#if 0
    /* If we're debugging, just print out the top of the stack */
    printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
				     tso->sp+64));
#endif
#ifdef INTERPRETER
    fprintf(stderr, "fatal: stack overflow in Hugs; aborting\n" );
    exit(1);
#else
    /* Send this thread the StackOverflow exception */
    raiseAsync(tso, (StgClosure *)stackOverflow_closure);
#endif
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

  IF_DEBUG(scheduler, fprintf(stderr,"scheduler: increasing stack size from %d words to %d.\n", tso->stack_size, new_stack_size));

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

  /* Mark the old TSO as relocated.  We have to check for relocated
   * TSOs in the garbage collector and any primops that deal with TSOs.
   *
   * It's important to set the sp and su values to just beyond the end
   * of the stack, so we don't attempt to scavenge any part of the
   * dead TSO's stack.
   */
  tso->what_next = ThreadRelocated;
  tso->link = dest;
  tso->sp = (P_)&(tso->stack[tso->stack_size]);
  tso->su = (StgUpdateFrame *)tso->sp;
  tso->why_blocked = NotBlocked;
  dest->mut_link = NULL;

  IF_DEBUG(sanity,checkTSO(tso));
#if 0
  IF_DEBUG(scheduler,printTSO(dest));
#endif

  return dest;
}

//@node Blocking Queue Routines, Exception Handling Routines, Garbage Collextion Routines, Main scheduling code
//@subsection Blocking Queue Routines

/* ---------------------------------------------------------------------------
   Wake up a queue that was blocked on some resource.
   ------------------------------------------------------------------------ */

/* ToDo: check push_on_run_queue vs. PUSH_ON_RUN_QUEUE */

#if defined(GRAN)
static inline void
unblockCount ( StgBlockingQueueElement *bqe, StgClosure *node )
{
}
#elif defined(PAR)
static inline void
unblockCount ( StgBlockingQueueElement *bqe, StgClosure *node )
{
  /* write RESUME events to log file and
     update blocked and fetch time (depending on type of the orig closure) */
  if (RtsFlags.ParFlags.ParStats.Full) {
    DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC, 
		     GR_RESUMEQ, ((StgTSO *)bqe), ((StgTSO *)bqe)->block_info.closure,
		     0, 0 /* spark_queue_len(ADVISORY_POOL) */);

    switch (get_itbl(node)->type) {
	case FETCH_ME_BQ:
	  ((StgTSO *)bqe)->par.fetchtime += CURRENT_TIME-((StgTSO *)bqe)->par.blockedat;
	  break;
	case RBH:
	case FETCH_ME:
	case BLACKHOLE_BQ:
	  ((StgTSO *)bqe)->par.blocktime += CURRENT_TIME-((StgTSO *)bqe)->par.blockedat;
	  break;
	default:
	  barf("{unblockOneLocked}Daq Qagh: unexpected closure in blocking queue");
	}
      }
}
#endif

#if defined(GRAN)
static StgBlockingQueueElement *
unblockOneLocked(StgBlockingQueueElement *bqe, StgClosure *node)
{
    StgBlockingQueueElement *next;
    PEs node_loc, tso_loc;

    node_loc = where_is(node); // should be lifted out of loop
    tso = (StgTSO *)bqe;  // wastes an assignment to get the type right
    tso_loc = where_is(tso);
    if (IS_LOCAL_TO(PROCS(node),tso_loc)) { // TSO is local
      /* !fake_fetch => TSO is on CurrentProc is same as IS_LOCAL_TO */
      ASSERT(CurrentProc!=node_loc || tso_loc==CurrentProc);
      bq_processing_time += RtsFlags.GranFlags.Costs.lunblocktime;
      // insertThread(tso, node_loc);
      new_event(tso_loc, tso_loc,
		CurrentTime[CurrentProc]+bq_processing_time,
		ResumeThread,
		tso, node, (rtsSpark*)NULL);
      tso->link = END_TSO_QUEUE; // overwrite link just to be sure 
      // len_local++;
      // len++;
    } else { // TSO is remote (actually should be FMBQ)
      bq_processing_time += RtsFlags.GranFlags.Costs.mpacktime;
      bq_processing_time += RtsFlags.GranFlags.Costs.gunblocktime;
      new_event(tso_loc, CurrentProc, 
		CurrentTime[CurrentProc]+bq_processing_time+
		RtsFlags.GranFlags.Costs.latency,
		UnblockThread,
		tso, node, (rtsSpark*)NULL);
      tso->link = END_TSO_QUEUE; // overwrite link just to be sure 
      bq_processing_time += RtsFlags.GranFlags.Costs.mtidytime;
      // len++;
    }      
    /* the thread-queue-overhead is accounted for in either Resume or UnblockThread */
    IF_GRAN_DEBUG(bq,
		  fprintf(stderr," %s TSO %d (%p) [PE %d] (blocked_on=%p) (next=%p) ,",
			  (node_loc==tso_loc ? "Local" : "Global"), 
			  tso->id, tso, CurrentProc, tso->blocked_on, tso->link))
    tso->blocked_on = NULL;
    IF_DEBUG(scheduler,belch("-- Waking up thread %ld (%p)", 
			     tso->id, tso));
  }

  /* if this is the BQ of an RBH, we have to put back the info ripped out of
     the closure to make room for the anchor of the BQ */
  if (next!=END_BQ_QUEUE) {
    ASSERT(get_itbl(node)->type == RBH && get_itbl(next)->type == CONSTR);
    /*
    ASSERT((info_ptr==&RBH_Save_0_info) ||
	   (info_ptr==&RBH_Save_1_info) ||
	   (info_ptr==&RBH_Save_2_info));
    */
    /* cf. convertToRBH in RBH.c for writing the RBHSave closure */
    ((StgRBH *)node)->blocking_queue = ((StgRBHSave *)next)->payload[0];
    ((StgRBH *)node)->mut_link       = ((StgRBHSave *)next)->payload[1];

    IF_GRAN_DEBUG(bq,
		  belch("## Filled in RBH_Save for %p (%s) at end of AwBQ",
			node, info_type(node)));
  }
}
#elif defined(PAR)
static StgBlockingQueueElement *
unblockOneLocked(StgBlockingQueueElement *bqe, StgClosure *node)
{
    StgBlockingQueueElement *next;

    switch (get_itbl(bqe)->type) {
    case TSO:
      ASSERT(((StgTSO *)bqe)->why_blocked != NotBlocked);
      /* if it's a TSO just push it onto the run_queue */
      next = bqe->link;
      // ((StgTSO *)bqe)->link = END_TSO_QUEUE; // debugging?
      PUSH_ON_RUN_QUEUE((StgTSO *)bqe); 
      THREAD_RUNNABLE();
      unblockCount(bqe, node);
      /* reset blocking status after dumping event */
      ((StgTSO *)bqe)->why_blocked = NotBlocked;
      break;

    case BLOCKED_FETCH:
      /* if it's a BLOCKED_FETCH put it on the PendingFetches list */
      next = bqe->link;
      bqe->link = PendingFetches;
      PendingFetches = bqe;
      break;

# if defined(DEBUG)
      /* can ignore this case in a non-debugging setup; 
	 see comments on RBHSave closures above */
    case CONSTR:
      /* check that the closure is an RBHSave closure */
      ASSERT(get_itbl((StgClosure *)bqe) == &RBH_Save_0_info ||
	     get_itbl((StgClosure *)bqe) == &RBH_Save_1_info ||
	     get_itbl((StgClosure *)bqe) == &RBH_Save_2_info);
      break;

    default:
      barf("{unblockOneLocked}Daq Qagh: Unexpected IP (%#lx; %s) in blocking queue at %#lx\n",
	   get_itbl((StgClosure *)bqe), info_type((StgClosure *)bqe), 
	   (StgClosure *)bqe);
# endif
    }
  // IF_DEBUG(scheduler,sched_belch("waking up thread %ld", tso->id));
  return next;
}

#else /* !GRAN && !PAR */
static StgTSO *
unblockOneLocked(StgTSO *tso)
{
  StgTSO *next;

  ASSERT(get_itbl(tso)->type == TSO);
  ASSERT(tso->why_blocked != NotBlocked);
  tso->why_blocked = NotBlocked;
  next = tso->link;
  PUSH_ON_RUN_QUEUE(tso);
  THREAD_RUNNABLE();
  IF_DEBUG(scheduler,sched_belch("waking up thread %ld", tso->id));
  return next;
}
#endif

#if defined(PAR) || defined(GRAN)
inline StgTSO *
unblockOne(StgTSO *tso, StgClosure *node)
{
  ACQUIRE_LOCK(&sched_mutex);
  tso = unblockOneLocked(tso, node);
  RELEASE_LOCK(&sched_mutex);
  return tso;
}
#else
inline StgTSO *
unblockOne(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  tso = unblockOneLocked(tso);
  RELEASE_LOCK(&sched_mutex);
  return tso;
}
#endif

#if defined(GRAN)
void 
awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node)
{
  StgBlockingQueueElement *bqe, *next;
  StgTSO *tso;
  PEs node_loc, tso_loc;
  rtsTime bq_processing_time = 0;
  nat len = 0, len_local = 0;

  IF_GRAN_DEBUG(bq, 
		belch("## AwBQ for node %p on PE %d @ %ld by TSO %d (%p): ", \
		      node, CurrentProc, CurrentTime[CurrentProc], 
		      CurrentTSO->id, CurrentTSO));

  node_loc = where_is(node);

  ASSERT(get_itbl(q)->type == TSO ||   // q is either a TSO or an RBHSave
	 get_itbl(q)->type == CONSTR); // closure (type constructor)
  ASSERT(is_unique(node));

  /* FAKE FETCH: magically copy the node to the tso's proc;
     no Fetch necessary because in reality the node should not have been 
     moved to the other PE in the first place
  */
  if (CurrentProc!=node_loc) {
    IF_GRAN_DEBUG(bq, 
		  belch("## node %p is on PE %d but CurrentProc is %d (TSO %d); assuming fake fetch and adjusting bitmask (old: %#x)",
			node, node_loc, CurrentProc, CurrentTSO->id, 
			// CurrentTSO, where_is(CurrentTSO),
			node->header.gran.procs));
    node->header.gran.procs = (node->header.gran.procs) | PE_NUMBER(CurrentProc);
    IF_GRAN_DEBUG(bq, 
		  belch("## new bitmask of node %p is %#x",
			node, node->header.gran.procs));
    if (RtsFlags.GranFlags.GranSimStats.Global) {
      globalGranStats.tot_fake_fetches++;
    }
  }

  bqe = q;
  // ToDo: check: ASSERT(CurrentProc==node_loc);
  while (get_itbl(bqe)->type==TSO) { // q != END_TSO_QUEUE) {
    //next = bqe->link;
    /* 
       bqe points to the current element in the queue
       next points to the next element in the queue
    */
    //tso = (StgTSO *)bqe;  // wastes an assignment to get the type right
    //tso_loc = where_is(tso);
    bqe = unblockOneLocked(bqe, node);
  }

  /* statistics gathering */
  /* ToDo: fix counters
  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_bq_processing_time += bq_processing_time;
    globalGranStats.tot_bq_len += len;      // total length of all bqs awakened
    globalGranStats.tot_bq_len_local += len_local;  // same for local TSOs only
    globalGranStats.tot_awbq++;             // total no. of bqs awakened
  }
  IF_GRAN_DEBUG(bq,
		fprintf(stderr,"## BQ Stats of %p: [%d entries, %d local] %s\n",
			node, len, len_local, (next!=END_TSO_QUEUE) ? "RBH" : ""));
  */
}
#elif defined(PAR)
void 
awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node)
{
  StgBlockingQueueElement *bqe, *next;

  ACQUIRE_LOCK(&sched_mutex);

  IF_PAR_DEBUG(verbose, 
	       belch("## AwBQ for node %p on [%x]: ",
		     node, mytid));

  ASSERT(get_itbl(q)->type == TSO ||           
  	 get_itbl(q)->type == BLOCKED_FETCH || 
  	 get_itbl(q)->type == CONSTR); 

  bqe = q;
  while (get_itbl(bqe)->type==TSO || 
	 get_itbl(bqe)->type==BLOCKED_FETCH) {
    bqe = unblockOneLocked(bqe, node);
  }
  RELEASE_LOCK(&sched_mutex);
}

#else   /* !GRAN && !PAR */
void
awakenBlockedQueue(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  while (tso != END_TSO_QUEUE) {
    tso = unblockOneLocked(tso);
  }
  RELEASE_LOCK(&sched_mutex);
}
#endif

//@node Exception Handling Routines, Debugging Routines, Blocking Queue Routines, Main scheduling code
//@subsection Exception Handling Routines

/* ---------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   ------------------------------------------------------------------------ */

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
   This has nothing to do with the UnblockThread event in GranSim. -- HWL
   -------------------------------------------------------------------------- */

static void
unblockThread(StgTSO *tso)
{
  StgTSO *t, **last;

  ACQUIRE_LOCK(&sched_mutex);
  switch (tso->why_blocked) {

  case NotBlocked:
    return;  /* not blocked */

  case BlockedOnMVar:
    ASSERT(get_itbl(tso->block_info.closure)->type == MVAR);
    {
      StgTSO *last_tso = END_TSO_QUEUE;
      StgMVar *mvar = (StgMVar *)(tso->block_info.closure);

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

  case BlockedOnBlackHole:
    ASSERT(get_itbl(tso->block_info.closure)->type == BLACKHOLE_BQ);
    {
      StgBlockingQueue *bq = (StgBlockingQueue *)(tso->block_info.closure);

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

  case BlockedOnException:
    {
      StgTSO *target  = tso->block_info.tso;

      ASSERT(get_itbl(target)->type == TSO);
      ASSERT(target->blocked_exceptions != NULL);

      last = &target->blocked_exceptions;
      for (t = target->blocked_exceptions; t != END_TSO_QUEUE; 
	   last = &t->link, t = t->link) {
	ASSERT(get_itbl(t)->type == TSO);
	if (t == tso) {
	  *last = tso->link;
	  goto done;
	}
      }
      barf("unblockThread (Exception): TSO not found");
    }

  case BlockedOnDelay:
  case BlockedOnRead:
  case BlockedOnWrite:
    {
      StgTSO *prev = NULL;
      for (t = blocked_queue_hd; t != END_TSO_QUEUE; 
	   prev = t, t = t->link) {
	if (t == tso) {
	  if (prev == NULL) {
	    blocked_queue_hd = t->link;
	    if (blocked_queue_tl == t) {
	      blocked_queue_tl = END_TSO_QUEUE;
	    }
	  } else {
	    prev->link = t->link;
	    if (blocked_queue_tl == t) {
	      blocked_queue_tl = prev;
	    }
	  }
	  goto done;
	}
      }
      barf("unblockThread (I/O): TSO not found");
    }

  default:
    barf("unblockThread");
  }

 done:
  tso->link = END_TSO_QUEUE;
  tso->why_blocked = NotBlocked;
  tso->block_info.closure = NULL;
  PUSH_ON_RUN_QUEUE(tso);
  RELEASE_LOCK(&sched_mutex);
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
  if (tso->what_next == ThreadComplete || tso->what_next == ThreadKilled) {
    return;
  }

  IF_DEBUG(scheduler, sched_belch("raising exception in thread %ld.", tso->id));

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
     * then build PAP(handler,exception,realworld#), and leave it on
     * top of the stack ready to enter.
     */
    if (get_itbl(su)->type == CATCH_FRAME && exception != NULL) {
      StgCatchFrame *cf = (StgCatchFrame *)su;
      /* we've got an exception to raise, so let's pass it to the
       * handler in this frame.
       */
      ap = (StgAP_UPD *)allocate(sizeofW(StgPAP) + 2);
      TICK_ALLOC_UPD_PAP(3,0);
      SET_HDR(ap,&PAP_info,cf->header.prof.ccs);
	      
      ap->n_args = 2;
      ap->fun = cf->handler;	/* :: Exception -> IO a */
      ap->payload[0] = (P_)exception;
      ap->payload[1] = ARG_TAG(0); /* realworld token */

      /* throw away the stack from Sp up to and including the
       * CATCH_FRAME.
       */
      sp = (P_)su + sizeofW(StgCatchFrame) - 1; 
      tso->su = cf->link;

      /* Restore the blocked/unblocked state for asynchronous exceptions
       * at the CATCH_FRAME.  
       *
       * If exceptions were unblocked at the catch, arrange that they
       * are unblocked again after executing the handler by pushing an
       * unblockAsyncExceptions_ret stack frame.
       */
      if (!cf->exceptions_blocked) {
	*(sp--) = (W_)&unblockAsyncExceptionszh_ret_info;
      }
      
      /* Ensure that async exceptions are blocked when running the handler.
       */
      if (tso->blocked_exceptions == NULL) {
	tso->blocked_exceptions = END_TSO_QUEUE;
      }
      
      /* Put the newly-built PAP on top of the stack, ready to execute
       * when the thread restarts.
       */
      sp[0] = (W_)ap;
      tso->sp = sp;
      tso->what_next = ThreadEnterGHC;
      return;
    }

    /* First build an AP_UPD consisting of the stack chunk above the
     * current update frame, with the top word on the stack as the
     * fun field.
     */
    ap = (StgAP_UPD *)allocate(AP_sizeW(words));
    
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
	TICK_ALLOC_UP_THK(words+1,0);
	
	IF_DEBUG(scheduler,
		 fprintf(stderr,  "scheduler: Updating ");
		 printPtr((P_)su->updatee); 
		 fprintf(stderr,  " with ");
		 printObj((StgClosure *)ap);
		 );
	
	/* Replace the updatee with an indirection - happily
	 * this will also wake up any threads currently
	 * waiting on the result.
	 */
	UPD_IND_NOLOCK(su->updatee,ap);  /* revert the black hole */
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
	TICK_ALLOC_UPD_PAP(words+1,0);
	
	/* now build o = FUN(catch,ap,handler) */
	o = (StgClosure *)allocate(sizeofW(StgClosure)+2);
	TICK_ALLOC_FUN(2,0);
	SET_HDR(o,&catch_info,su->header.prof.ccs /* ToDo */);
	o->payload[0] = (StgClosure *)ap;
	o->payload[1] = cf->handler;
	
	IF_DEBUG(scheduler,
		 fprintf(stderr,  "scheduler: Built ");
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
	TICK_ALLOC_UPD_PAP(words+1,0);
	
	/* now build o = FUN(seq,ap) */
	o = (StgClosure *)allocate(sizeofW(StgClosure)+1);
	TICK_ALLOC_SE_THK(1,0);
	SET_HDR(o,&seq_info,su->header.prof.ccs /* ToDo */);
	o->payload[0] = (StgClosure *)ap;
	
	IF_DEBUG(scheduler,
		 fprintf(stderr,  "scheduler: Built ");
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
      tso->what_next = ThreadKilled;
      tso->su = (StgUpdateFrame *)(sp+1);
      tso->sp = sp;
      return;
      
    default:
      barf("raiseAsync");
    }
  }
  barf("raiseAsync");
}

/* -----------------------------------------------------------------------------
   resurrectThreads is called after garbage collection on the list of
   threads found to be garbage.  Each of these threads will be woken
   up and sent a signal: BlockedOnDeadMVar if the thread was blocked
   on an MVar, or NonTermination if the thread was blocked on a Black
   Hole.
   -------------------------------------------------------------------------- */

void
resurrectThreads( StgTSO *threads )
{
  StgTSO *tso, *next;

  for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
    next = tso->global_link;
    tso->global_link = all_threads;
    all_threads = tso;
    IF_DEBUG(scheduler, sched_belch("resurrecting thread %d", tso->id));

    switch (tso->why_blocked) {
    case BlockedOnMVar:
    case BlockedOnException:
      raiseAsync(tso,(StgClosure *)BlockedOnDeadMVar_closure);
      break;
    case BlockedOnBlackHole:
      raiseAsync(tso,(StgClosure *)NonTermination_closure);
      break;
    case NotBlocked:
      /* This might happen if the thread was blocked on a black hole
       * belonging to a thread that we've just woken up (raiseAsync
       * can wake up threads, remember...).
       */
      continue;
    default:
      barf("resurrectThreads: thread blocked in a strange way");
    }
  }
}

//@node Debugging Routines, Index, Exception Handling Routines, Main scheduling code
//@subsection Debugging Routines

/* -----------------------------------------------------------------------------
   Debugging: why is a thread blocked
   -------------------------------------------------------------------------- */

#ifdef DEBUG

void
printThreadBlockage(StgTSO *tso)
{
  switch (tso->why_blocked) {
  case BlockedOnRead:
    fprintf(stderr,"blocked on read from fd %d", tso->block_info.fd);
    break;
  case BlockedOnWrite:
    fprintf(stderr,"blocked on write to fd %d", tso->block_info.fd);
    break;
  case BlockedOnDelay:
#if defined(HAVE_SETITIMER)
    fprintf(stderr,"blocked on delay of %d ms", tso->block_info.delay);
#else
    fprintf(stderr,"blocked on delay of %d ms", 
	    tso->block_info.target - getourtimeofday());
#endif
    break;
  case BlockedOnMVar:
    fprintf(stderr,"blocked on an MVar");
    break;
  case BlockedOnException:
    fprintf(stderr,"blocked on delivering an exception to thread %d",
	    tso->block_info.tso->id);
    break;
  case BlockedOnBlackHole:
    fprintf(stderr,"blocked on a black hole");
    break;
  case NotBlocked:
    fprintf(stderr,"not blocked");
    break;
#if defined(PAR)
  case BlockedOnGA:
    fprintf(stderr,"blocked on global address");
    break;
#endif
  }
}

void
printThreadStatus(StgTSO *tso)
{
  switch (tso->what_next) {
  case ThreadKilled:
    fprintf(stderr,"has been killed");
    break;
  case ThreadComplete:
    fprintf(stderr,"has completed");
    break;
  default:
    printThreadBlockage(tso);
  }
}

void
printAllThreads(void)
{
  StgTSO *t;

  sched_belch("all threads:");
  for (t = all_threads; t != END_TSO_QUEUE; t = t->global_link) {
    fprintf(stderr, "\tthread %d is ", t->id);
    printThreadStatus(t);
    fprintf(stderr,"\n");
  }
}
    
/* 
   Print a whole blocking queue attached to node (debugging only).
*/
//@cindex print_bq
# if defined(PAR)
void 
print_bq (StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  StgTSO *tso;
  rtsBool end;

  fprintf(stderr,"## BQ of closure %p (%s): ",
	  node, info_type(node));

  /* should cover all closures that may have a blocking queue */
  ASSERT(get_itbl(node)->type == BLACKHOLE_BQ ||
	 get_itbl(node)->type == FETCH_ME_BQ ||
	 get_itbl(node)->type == RBH);
    
  ASSERT(node!=(StgClosure*)NULL);         // sanity check
  /* 
     NB: In a parallel setup a BQ of an RBH must end with an RBH_Save closure;
  */
  for (bqe = ((StgBlockingQueue*)node)->blocking_queue, end = (bqe==END_BQ_QUEUE);
       !end; // iterate until bqe points to a CONSTR
       end = (get_itbl(bqe)->type == CONSTR) || (bqe->link==END_BQ_QUEUE), bqe = end ? END_BQ_QUEUE : bqe->link) {
    ASSERT(bqe != END_BQ_QUEUE);             // sanity check
    ASSERT(bqe != (StgTSO*)NULL);            // sanity check
    /* types of closures that may appear in a blocking queue */
    ASSERT(get_itbl(bqe)->type == TSO ||           
	   get_itbl(bqe)->type == BLOCKED_FETCH || 
	   get_itbl(bqe)->type == CONSTR); 
    /* only BQs of an RBH end with an RBH_Save closure */
    ASSERT(get_itbl(bqe)->type != CONSTR || get_itbl(node)->type == RBH);

    switch (get_itbl(bqe)->type) {
    case TSO:
      fprintf(stderr," TSO %d (%x),",
	      ((StgTSO *)bqe)->id, ((StgTSO *)bqe));
      break;
    case BLOCKED_FETCH:
      fprintf(stderr," BF (node=%p, ga=((%x, %d, %x)),",
	      ((StgBlockedFetch *)bqe)->node, 
	      ((StgBlockedFetch *)bqe)->ga.payload.gc.gtid,
	      ((StgBlockedFetch *)bqe)->ga.payload.gc.slot,
	      ((StgBlockedFetch *)bqe)->ga.weight);
      break;
    case CONSTR:
      fprintf(stderr," %s (IP %p),",
	      (get_itbl(bqe) == &RBH_Save_0_info ? "RBH_Save_0" :
	       get_itbl(bqe) == &RBH_Save_1_info ? "RBH_Save_1" :
	       get_itbl(bqe) == &RBH_Save_2_info ? "RBH_Save_2" :
	       "RBH_Save_?"), get_itbl(bqe));
      break;
    default:
      barf("Unexpected closure type %s in blocking queue of %p (%s)",
	   info_type(bqe), node, info_type(node));
      break;
    }
  } /* for */
  fputc('\n', stderr);
}
# elif defined(GRAN)
void 
print_bq (StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  StgTSO *tso;
  PEs node_loc, tso_loc;
  rtsBool end;

  /* should cover all closures that may have a blocking queue */
  ASSERT(get_itbl(node)->type == BLACKHOLE_BQ ||
	 get_itbl(node)->type == FETCH_ME_BQ ||
	 get_itbl(node)->type == RBH);
    
  ASSERT(node!=(StgClosure*)NULL);         // sanity check
  node_loc = where_is(node);

  fprintf(stderr,"## BQ of closure %p (%s) on [PE %d]: ",
	  node, info_type(node), node_loc);

  /* 
     NB: In a parallel setup a BQ of an RBH must end with an RBH_Save closure;
  */
  for (bqe = ((StgBlockingQueue*)node)->blocking_queue, end = (bqe==END_BQ_QUEUE);
       !end; // iterate until bqe points to a CONSTR
       end = (get_itbl(bqe)->type == CONSTR) || (bqe->link==END_BQ_QUEUE), bqe = end ? END_BQ_QUEUE : bqe->link) {
    ASSERT(bqe != END_BQ_QUEUE);             // sanity check
    ASSERT(bqe != (StgTSO*)NULL);            // sanity check
    /* types of closures that may appear in a blocking queue */
    ASSERT(get_itbl(bqe)->type == TSO ||           
	   get_itbl(bqe)->type == CONSTR); 
    /* only BQs of an RBH end with an RBH_Save closure */
    ASSERT(get_itbl(bqe)->type != CONSTR || get_itbl(node)->type == RBH);

    tso_loc = where_is((StgClosure *)bqe);
    switch (get_itbl(bqe)->type) {
    case TSO:
      fprintf(stderr," TSO %d (%x) on [PE %d],",
	      ((StgTSO *)bqe)->id, ((StgTSO *)bqe), tso_loc);
      break;
    case CONSTR:
      fprintf(stderr," %s (IP %p),",
	      (get_itbl(bqe) == &RBH_Save_0_info ? "RBH_Save_0" :
	       get_itbl(bqe) == &RBH_Save_1_info ? "RBH_Save_1" :
	       get_itbl(bqe) == &RBH_Save_2_info ? "RBH_Save_2" :
	       "RBH_Save_?"), get_itbl(bqe));
      break;
    default:
      barf("Unexpected closure type %s in blocking queue of %p (%s)",
	   info_type(bqe), node, info_type(node));
      break;
    }
  } /* for */
  fputc('\n', stderr);
}
#else
/* 
   Nice and easy: only TSOs on the blocking queue
*/
void 
print_bq (StgClosure *node)
{
  StgTSO *tso;

  ASSERT(node!=(StgClosure*)NULL);         // sanity check
  for (tso = ((StgBlockingQueue*)node)->blocking_queue;
       tso != END_TSO_QUEUE; 
       tso=tso->link) {
    ASSERT(tso!=NULL && tso!=END_TSO_QUEUE);   // sanity check
    ASSERT(get_itbl(tso)->type == TSO);  // guess what, sanity check
    fprintf(stderr," TSO %d (%p),", tso->id, tso);
  }
  fputc('\n', stderr);
}
# endif

#if defined(PAR)
static nat
run_queue_len(void)
{
  nat i;
  StgTSO *tso;

  for (i=0, tso=run_queue_hd; 
       tso != END_TSO_QUEUE;
       i++, tso=tso->link)
    /* nothing */

  return i;
}
#endif

static void
sched_belch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
#ifdef SMP
  fprintf(stderr, "scheduler (task %ld): ", pthread_self());
#else
  fprintf(stderr, "scheduler: ");
#endif
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

#endif /* DEBUG */


//@node Index,  , Debugging Routines, Main scheduling code
//@subsection Index

//@index
//* MainRegTable::  @cindex\s-+MainRegTable
//* StgMainThread::  @cindex\s-+StgMainThread
//* awaken_blocked_queue::  @cindex\s-+awaken_blocked_queue
//* blocked_queue_hd::  @cindex\s-+blocked_queue_hd
//* blocked_queue_tl::  @cindex\s-+blocked_queue_tl
//* context_switch::  @cindex\s-+context_switch
//* createThread::  @cindex\s-+createThread
//* free_capabilities::  @cindex\s-+free_capabilities
//* gc_pending_cond::  @cindex\s-+gc_pending_cond
//* initScheduler::  @cindex\s-+initScheduler
//* interrupted::  @cindex\s-+interrupted
//* n_free_capabilities::  @cindex\s-+n_free_capabilities
//* next_thread_id::  @cindex\s-+next_thread_id
//* print_bq::  @cindex\s-+print_bq
//* run_queue_hd::  @cindex\s-+run_queue_hd
//* run_queue_tl::  @cindex\s-+run_queue_tl
//* sched_mutex::  @cindex\s-+sched_mutex
//* schedule::  @cindex\s-+schedule
//* take_off_run_queue::  @cindex\s-+take_off_run_queue
//* task_ids::  @cindex\s-+task_ids
//* term_mutex::  @cindex\s-+term_mutex
//* thread_ready_cond::  @cindex\s-+thread_ready_cond
//@end index
