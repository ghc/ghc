/* -----------------------------------------------------------------------------
 * $Id: Schedule.c,v 1.40 2000/01/13 10:37:31 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Scheduler
 *
 * ---------------------------------------------------------------------------*/

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

#include <stdarg.h>

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
StgTSO *run_queue_hd, *run_queue_tl;
StgTSO *blocked_queue_hd, *blocked_queue_tl;

/* Threads suspended in _ccall_GC.
 * Locks required: sched_mutex.
 */
static StgTSO *suspended_ccalling_threads;

static void GetRoots(void);
static StgTSO *threadStackOverflow(StgTSO *tso);

/* KH: The following two flags are shared memory locations.  There is no need
       to lock them, since they are only unset at the end of a scheduler
       operation.
*/

/* flag set by signal handler to precipitate a context switch */
nat context_switch;

/* if this flag is set as well, give up execution */
rtsBool interrupted;

/* Next thread ID to allocate.
 * Locks required: sched_mutex
 */
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
Capability *free_capabilities;	/* Available capabilities for running threads */
nat n_free_capabilities;        /* total number of available capabilities */
#else
Capability MainRegTable;	/* for non-SMP, we have one global capability */
#endif

rtsBool ready_to_gc;

/* All our current task ids, saved in case we need to kill them later.
 */
#ifdef SMP
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
pthread_mutex_t sched_mutex       = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t term_mutex        = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  thread_ready_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t  gc_pending_cond   = PTHREAD_COND_INITIALIZER;

nat await_death;
#endif

/* -----------------------------------------------------------------------------
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

   -------------------------------------------------------------------------- */

static void
schedule( void )
{
  StgTSO *t;
  Capability *cap;
  StgThreadReturnCode ret;
  
  ACQUIRE_LOCK(&sched_mutex);

  while (1) {

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
	switch (m->tso->whatNext) {
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
	  m->stat = Killed;
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
      if (m->tso->whatNext == ThreadComplete
	  || m->tso->whatNext == ThreadKilled) {
	main_threads = main_threads->link;
	if (m->tso->whatNext == ThreadComplete) {
	  /* we finished successfully, fill in the return value */
	  if (m->ret) { *(m->ret) = (StgClosure *)m->tso->sp[0]; };
	  m->stat = Success;
	  return;
	} else {
	  m->stat = Killed;
	  return;
	}
      }
    }
#endif

    /* Top up the run queue from our spark pool.  We try to make the
     * number of threads in the run queue equal to the number of
     * free capabilities.
     */
#if defined(SMP) || defined(PAR)
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
	  StgTSO *tso;
	  tso = createThread_(RtsFlags.GcFlags.initialStkSize, rtsTrue);
	  pushClosure(tso,spark);
	  PUSH_ON_RUN_QUEUE(tso);
#ifdef ToDo
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
#endif /* SMP || PAR */

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
  
    /* grab a thread from the run queue
     */
    t = POP_RUN_QUEUE();
    
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

    /* Run the current thread 
     */
    switch (cap->rCurrentTSO->whatNext) {
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
      barf("schedule: invalid whatNext field");
    }
    
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
	 * (it shouldn't be)
	 */
	for (m = main_threads; m != NULL; m = m->link) {
	  if (m->tso == t) {
	    m->tso = new_t;
	  }
	}
	PUSH_ON_RUN_QUEUE(new_t);
      }
      break;

    case ThreadYielding:
      /* put the thread back on the run queue.  Then, if we're ready to
       * GC, check whether this is the last task to stop.  If so, wake
       * up the GC thread.  getThread will block during a GC until the
       * GC is finished.
       */
      IF_DEBUG(scheduler,
	       if (t->whatNext == ThreadEnterHugs) {
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
      t->whatNext = ThreadComplete;
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
    if (ready_to_gc && n_free_capabilities == RtsFlags.ParFlags.nNodes) {
#else
    if (ready_to_gc) {
#endif
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


/* -----------------------------------------------------------------------------
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
 * -------------------------------------------------------------------------- */
   
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

/* -----------------------------------------------------------------------------
 * Static functions
 * -------------------------------------------------------------------------- */
static void unblockThread(StgTSO *tso);

/* -----------------------------------------------------------------------------
 * Comparing Thread ids.
 *
 * This is used from STG land in the implementation of the
 * instances of Eq/Ord for ThreadIds.
 * -------------------------------------------------------------------------- */

int cmp_thread(const StgTSO *tso1, const StgTSO *tso2) 
{ 
  StgThreadID id1 = tso1->id; 
  StgThreadID id2 = tso2->id;
 
  if (id1 < id2) return (-1);
  if (id1 > id2) return 1;
  return 0;
}

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
createThread(nat size)
{
  return createThread_(size, rtsFalse);
}

static StgTSO *
createThread_(nat size, rtsBool have_lock)
{
  StgTSO *tso;
  nat stack_size;

  /* catch ridiculously small stack sizes */
  if (size < MIN_STACK_WORDS + TSO_STRUCT_SIZEW) {
    size = MIN_STACK_WORDS + TSO_STRUCT_SIZEW;
  }

  tso = (StgTSO *)allocate(size);
  TICK_ALLOC_TSO(size-sizeofW(StgTSO),0);
  
  stack_size = size - TSO_STRUCT_SIZEW;

  SET_HDR(tso, &TSO_info, CCS_MAIN);
  tso->whatNext = ThreadEnterGHC;
  
  /* tso->id needs to be unique.  For now we use a heavyweight mutex to
  	 protect the increment operation on next_thread_id.
  	 In future, we could use an atomic increment instead.
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

  IF_DEBUG(scheduler,sched_belch("created thread %ld, stack size = %lx words", 
				 tso->id, tso->stack_size));
  return tso;
}


/* -----------------------------------------------------------------------------
 * scheduleThread()
 *
 * scheduleThread puts a thread on the head of the runnable queue.
 * This will usually be done immediately after a thread is created.
 * The caller of scheduleThread must create the thread using e.g.
 * createThread and push an appropriate closure
 * on this thread's stack before the scheduler is invoked.
 * -------------------------------------------------------------------------- */

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


/* -----------------------------------------------------------------------------
 * startTasks()
 *
 * Start up Posix threads to run each of the scheduler tasks.
 * I believe the task ids are not needed in the system as defined.
  * KH @ 25/10/99
 * -------------------------------------------------------------------------- */

#ifdef SMP
static void *
taskStart( void *arg STG_UNUSED )
{
  schedule();
  return NULL;
}
#endif

/* -----------------------------------------------------------------------------
 * initScheduler()
 *
 * Initialise the scheduler.  This resets all the queues - if the
 * queues contained any threads, they'll be garbage collected at the
 * next pass.
 *
 * This now calls startTasks(), so should only be called once!  KH @ 25/10/99
 * -------------------------------------------------------------------------- */

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

void initScheduler(void)
{
  run_queue_hd      = END_TSO_QUEUE;
  run_queue_tl      = END_TSO_QUEUE;
  blocked_queue_hd  = END_TSO_QUEUE;
  blocked_queue_tl  = END_TSO_QUEUE;

  suspended_ccalling_threads  = END_TSO_QUEUE;

  main_threads = NULL;

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
  
/* -----------------------------------------------------------------------------
   Debugging: why is a thread blocked
   -------------------------------------------------------------------------- */

#ifdef DEBUG
void printThreadBlockage(StgTSO *tso)
{
  switch (tso->why_blocked) {
  case BlockedOnRead:
    fprintf(stderr,"blocked on read from fd %d", tso->block_info.fd);
    break;
  case BlockedOnWrite:
    fprintf(stderr,"blocked on write to fd %d", tso->block_info.fd);
    break;
  case BlockedOnDelay:
    fprintf(stderr,"blocked on delay of %d ms", tso->block_info.delay);
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
  }
}
#endif

/* -----------------------------------------------------------------------------
   Where are the roots that we know about?

        - all the threads on the runnable queue
        - all the threads on the blocked queue
	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   -------------------------------------------------------------------------- */

/* This has to be protected either by the scheduler monitor, or by the
	garbage collection monitor (probably the latter).
	KH @ 25/10/99
*/

static void GetRoots(void)
{
  StgMainThread *m;

  run_queue_hd      = (StgTSO *)MarkRoot((StgClosure *)run_queue_hd);
  run_queue_tl      = (StgTSO *)MarkRoot((StgClosure *)run_queue_tl);

  blocked_queue_hd  = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_hd);
  blocked_queue_tl  = (StgTSO *)MarkRoot((StgClosure *)blocked_queue_tl);

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
#ifdef INTERPRETER
    fprintf(stderr, "fatal: stack overflow in Hugs; aborting\n" );
    exit(1);
#else
    /* Send this thread the StackOverflow exception */
    raiseAsync(tso, (StgClosure *)&stackOverflow_closure);
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
  tso->why_blocked = NotBlocked;
  dest->mut_link = NULL;

  IF_DEBUG(sanity,checkTSO(tso));
#if 0
  IF_DEBUG(scheduler,printTSO(dest));
#endif

#if 0
  /* This will no longer work: KH */
  if (tso == MainTSO) { /* hack */
      MainTSO = dest;
  }
#endif
  return dest;
}

/* -----------------------------------------------------------------------------
   Wake up a queue that was blocked on some resource.
   -------------------------------------------------------------------------- */

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

inline StgTSO *
unblockOne(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  tso = unblockOneLocked(tso);
  RELEASE_LOCK(&sched_mutex);
  return tso;
}

void
awakenBlockedQueue(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  while (tso != END_TSO_QUEUE) {
    tso = unblockOneLocked(tso);
  }
  RELEASE_LOCK(&sched_mutex);
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
  if (tso->whatNext == ThreadComplete || tso->whatNext == ThreadKilled) {
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
     * then build PAP(handler,exception), and leave it on top of
     * the stack ready to enter.
     */
    if (get_itbl(su)->type == CATCH_FRAME && exception != NULL) {
      StgCatchFrame *cf = (StgCatchFrame *)su;
      /* we've got an exception to raise, so let's pass it to the
       * handler in this frame.
       */
      ap = (StgAP_UPD *)allocate(sizeofW(StgPAP) + 1);
      TICK_ALLOC_UPD_PAP(2,0);
      SET_HDR(ap,&PAP_info,cf->header.prof.ccs);
	      
      ap->n_args = 1;
      ap->fun = cf->handler;
      ap->payload[0] = (P_)exception;

      /* sp currently points to the word above the CATCH_FRAME on the stack.
       */
      sp += sizeofW(StgCatchFrame);
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
      tso->whatNext = ThreadEnterGHC;
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
	payloadCPtr(o,0) = (StgClosure *)ap;
	
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

/* -----------------------------------------------------------------------------
   Debuggery...
   -------------------------------------------------------------------------- */

#ifdef DEBUG
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
#endif
