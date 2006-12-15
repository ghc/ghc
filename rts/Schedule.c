/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * The scheduler and thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "SchedAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "OSThreads.h"
#include "Storage.h"
#include "StgRun.h"
#include "Hooks.h"
#include "Schedule.h"
#include "StgMiscClosures.h"
#include "Interpreter.h"
#include "Printer.h"
#include "RtsSignals.h"
#include "Sanity.h"
#include "Stats.h"
#include "STM.h"
#include "Timer.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#include "LdvProfile.h"
#include "Updates.h"
#ifdef PROFILING
#include "Proftimer.h"
#include "ProfHeap.h"
#endif
#if defined(GRAN) || defined(PARALLEL_HASKELL)
# include "GranSimRts.h"
# include "GranSim.h"
# include "ParallelRts.h"
# include "Parallel.h"
# include "ParallelDebug.h"
# include "FetchMe.h"
# include "HLC.h"
#endif
#include "Sparks.h"
#include "Capability.h"
#include "Task.h"
#include "AwaitEvent.h"
#if defined(mingw32_HOST_OS)
#include "win32/IOManager.h"
#endif
#include "Trace.h"
#include "RaiseAsync.h"
#include "Threads.h"
#include "ThrIOManager.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

// Turn off inlining when debugging - it obfuscates things
#ifdef DEBUG
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

/* -----------------------------------------------------------------------------
 * Global variables
 * -------------------------------------------------------------------------- */

#if defined(GRAN)

StgTSO* ActiveTSO = NULL; /* for assigning system costs; GranSim-Light only */
/* rtsTime TimeOfNextEvent, EndOfTimeSlice;            now in GranSim.c */

/* 
   In GranSim we have a runnable and a blocked queue for each processor.
   In order to minimise code changes new arrays run_queue_hds/tls
   are created. run_queue_hd is then a short cut (macro) for
   run_queue_hds[CurrentProc] (see GranSim.h).
   -- HWL
*/
StgTSO *run_queue_hds[MAX_PROC], *run_queue_tls[MAX_PROC];
StgTSO *blocked_queue_hds[MAX_PROC], *blocked_queue_tls[MAX_PROC];
StgTSO *ccalling_threadss[MAX_PROC];
/* We use the same global list of threads (all_threads) in GranSim as in
   the std RTS (i.e. we are cheating). However, we don't use this list in
   the GranSim specific code at the moment (so we are only potentially
   cheating).  */

#else /* !GRAN */

#if !defined(THREADED_RTS)
// Blocked/sleeping thrads
StgTSO *blocked_queue_hd = NULL;
StgTSO *blocked_queue_tl = NULL;
StgTSO *sleeping_queue = NULL;    // perhaps replace with a hash table?
#endif

/* Threads blocked on blackholes.
 * LOCK: sched_mutex+capability, or all capabilities
 */
StgTSO *blackhole_queue = NULL;
#endif

/* The blackhole_queue should be checked for threads to wake up.  See
 * Schedule.h for more thorough comment.
 * LOCK: none (doesn't matter if we miss an update)
 */
rtsBool blackholes_need_checking = rtsFalse;

/* Linked list of all threads.
 * Used for detecting garbage collected threads.
 * LOCK: sched_mutex+capability, or all capabilities
 */
StgTSO *all_threads = NULL;

/* flag set by signal handler to precipitate a context switch
 * LOCK: none (just an advisory flag)
 */
int context_switch = 0;

/* flag that tracks whether we have done any execution in this time slice.
 * LOCK: currently none, perhaps we should lock (but needs to be
 * updated in the fast path of the scheduler).
 */
nat recent_activity = ACTIVITY_YES;

/* if this flag is set as well, give up execution
 * LOCK: none (changes once, from false->true)
 */
rtsBool sched_state = SCHED_RUNNING;

#if defined(GRAN)
StgTSO *CurrentTSO;
#endif

/*  This is used in `TSO.h' and gcc 2.96 insists that this variable actually 
 *  exists - earlier gccs apparently didn't.
 *  -= chak
 */
StgTSO dummy_tso;

/*
 * Set to TRUE when entering a shutdown state (via shutdownHaskellAndExit()) --
 * in an MT setting, needed to signal that a worker thread shouldn't hang around
 * in the scheduler when it is out of work.
 */
rtsBool shutting_down_scheduler = rtsFalse;

/*
 * This mutex protects most of the global scheduler data in
 * the THREADED_RTS runtime.
 */
#if defined(THREADED_RTS)
Mutex sched_mutex;
#endif

#if defined(PARALLEL_HASKELL)
StgTSO *LastTSO;
rtsTime TimeOfLastYield;
rtsBool emitSchedule = rtsTrue;
#endif

#if !defined(mingw32_HOST_OS)
#define FORKPROCESS_PRIMOP_SUPPORTED
#endif

/* -----------------------------------------------------------------------------
 * static function prototypes
 * -------------------------------------------------------------------------- */

static Capability *schedule (Capability *initialCapability, Task *task);

//
// These function all encapsulate parts of the scheduler loop, and are
// abstracted only to make the structure and control flow of the
// scheduler clearer.
//
static void schedulePreLoop (void);
#if defined(THREADED_RTS)
static void schedulePushWork(Capability *cap, Task *task);
#endif
static void scheduleStartSignalHandlers (Capability *cap);
static void scheduleCheckBlockedThreads (Capability *cap);
static void scheduleCheckWakeupThreads(Capability *cap USED_IF_NOT_THREADS);
static void scheduleCheckBlackHoles (Capability *cap);
static void scheduleDetectDeadlock (Capability *cap, Task *task);
#if defined(GRAN)
static StgTSO *scheduleProcessEvent(rtsEvent *event);
#endif
#if defined(PARALLEL_HASKELL)
static StgTSO *scheduleSendPendingMessages(void);
static void scheduleActivateSpark(void);
static rtsBool scheduleGetRemoteWork(rtsBool *receivedFinish);
#endif
#if defined(PAR) || defined(GRAN)
static void scheduleGranParReport(void);
#endif
static void schedulePostRunThread(void);
static rtsBool scheduleHandleHeapOverflow( Capability *cap, StgTSO *t );
static void scheduleHandleStackOverflow( Capability *cap, Task *task, 
					 StgTSO *t);
static rtsBool scheduleHandleYield( Capability *cap, StgTSO *t, 
				    nat prev_what_next );
static void scheduleHandleThreadBlocked( StgTSO *t );
static rtsBool scheduleHandleThreadFinished( Capability *cap, Task *task,
					     StgTSO *t );
static rtsBool scheduleDoHeapProfile(rtsBool ready_to_gc);
static Capability *scheduleDoGC(Capability *cap, Task *task,
				rtsBool force_major);

static rtsBool checkBlackHoles(Capability *cap);

static StgTSO *threadStackOverflow(Capability *cap, StgTSO *tso);

static void deleteThread (Capability *cap, StgTSO *tso);
static void deleteAllThreads (Capability *cap);

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void deleteThread_(Capability *cap, StgTSO *tso);
#endif

#if defined(PARALLEL_HASKELL)
StgTSO * createSparkThread(rtsSpark spark);
StgTSO * activateSpark (rtsSpark spark);  
#endif

#ifdef DEBUG
static char *whatNext_strs[] = {
  "(unknown)",
  "ThreadRunGHC",
  "ThreadInterpret",
  "ThreadKilled",
  "ThreadRelocated",
  "ThreadComplete"
};
#endif

/* -----------------------------------------------------------------------------
 * Putting a thread on the run queue: different scheduling policies
 * -------------------------------------------------------------------------- */

STATIC_INLINE void
addToRunQueue( Capability *cap, StgTSO *t )
{
#if defined(PARALLEL_HASKELL)
    if (RtsFlags.ParFlags.doFairScheduling) { 
	// this does round-robin scheduling; good for concurrency
	appendToRunQueue(cap,t);
    } else {
	// this does unfair scheduling; good for parallelism
	pushOnRunQueue(cap,t);
    }
#else
    // this does round-robin scheduling; good for concurrency
    appendToRunQueue(cap,t);
#endif
}

/* ---------------------------------------------------------------------------
   Main scheduling loop.

   We use round-robin scheduling, each thread returning to the
   scheduler loop when one of these conditions is detected:

      * out of heap space
      * timer expires (thread yields)
      * thread blocks
      * thread ends
      * stack overflow

   GRAN version:
     In a GranSim setup this loop iterates over the global event queue.
     This revolves around the global event queue, which determines what 
     to do next. Therefore, it's more complicated than either the 
     concurrent or the parallel (GUM) setup.

   GUM version:
     GUM iterates over incoming messages.
     It starts with nothing to do (thus CurrentTSO == END_TSO_QUEUE),
     and sends out a fish whenever it has nothing to do; in-between
     doing the actual reductions (shared code below) it processes the
     incoming messages and deals with delayed operations 
     (see PendingFetches).
     This is not the ugliest code you could imagine, but it's bloody close.

   ------------------------------------------------------------------------ */

static Capability *
schedule (Capability *initialCapability, Task *task)
{
  StgTSO *t;
  Capability *cap;
  StgThreadReturnCode ret;
#if defined(GRAN)
  rtsEvent *event;
#elif defined(PARALLEL_HASKELL)
  StgTSO *tso;
  GlobalTaskId pe;
  rtsBool receivedFinish = rtsFalse;
# if defined(DEBUG)
  nat tp_size, sp_size; // stats only
# endif
#endif
  nat prev_what_next;
  rtsBool ready_to_gc;
#if defined(THREADED_RTS)
  rtsBool first = rtsTrue;
#endif
  
  cap = initialCapability;

  // Pre-condition: this task owns initialCapability.
  // The sched_mutex is *NOT* held
  // NB. on return, we still hold a capability.

  debugTrace (DEBUG_sched, 
	      "### NEW SCHEDULER LOOP (task: %p, cap: %p)",
	      task, initialCapability);

  schedulePreLoop();

  // -----------------------------------------------------------
  // Scheduler loop starts here:

#if defined(PARALLEL_HASKELL)
#define TERMINATION_CONDITION        (!receivedFinish)
#elif defined(GRAN)
#define TERMINATION_CONDITION        ((event = get_next_event()) != (rtsEvent*)NULL) 
#else
#define TERMINATION_CONDITION        rtsTrue
#endif

  while (TERMINATION_CONDITION) {

#if defined(GRAN)
      /* Choose the processor with the next event */
      CurrentProc = event->proc;
      CurrentTSO = event->tso;
#endif

#if defined(THREADED_RTS)
      if (first) {
	  // don't yield the first time, we want a chance to run this
	  // thread for a bit, even if there are others banging at the
	  // door.
	  first = rtsFalse;
	  ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
      } else {
	  // Yield the capability to higher-priority tasks if necessary.
	  yieldCapability(&cap, task);
      }
#endif
      
#if defined(THREADED_RTS)
      schedulePushWork(cap,task);
#endif

    // Check whether we have re-entered the RTS from Haskell without
    // going via suspendThread()/resumeThread (i.e. a 'safe' foreign
    // call).
    if (cap->in_haskell) {
    	  errorBelch("schedule: re-entered unsafely.\n"
    		     "   Perhaps a 'foreign import unsafe' should be 'safe'?");
    	  stg_exit(EXIT_FAILURE);
    }

    // The interruption / shutdown sequence.
    // 
    // In order to cleanly shut down the runtime, we want to:
    //   * make sure that all main threads return to their callers
    //     with the state 'Interrupted'.
    //   * clean up all OS threads assocated with the runtime
    //   * free all memory etc.
    //
    // So the sequence for ^C goes like this:
    //
    //   * ^C handler sets sched_state := SCHED_INTERRUPTING and
    //     arranges for some Capability to wake up
    //
    //   * all threads in the system are halted, and the zombies are
    //     placed on the run queue for cleaning up.  We acquire all
    //     the capabilities in order to delete the threads, this is
    //     done by scheduleDoGC() for convenience (because GC already
    //     needs to acquire all the capabilities).  We can't kill
    //     threads involved in foreign calls.
    // 
    //   * somebody calls shutdownHaskell(), which calls exitScheduler()
    //
    //   * sched_state := SCHED_SHUTTING_DOWN
    //
    //   * all workers exit when the run queue on their capability
    //     drains.  All main threads will also exit when their TSO
    //     reaches the head of the run queue and they can return.
    //
    //   * eventually all Capabilities will shut down, and the RTS can
    //     exit.
    //
    //   * We might be left with threads blocked in foreign calls, 
    //     we should really attempt to kill these somehow (TODO);
    
    switch (sched_state) {
    case SCHED_RUNNING:
	break;
    case SCHED_INTERRUPTING:
	debugTrace(DEBUG_sched, "SCHED_INTERRUPTING");
#if defined(THREADED_RTS)
	discardSparksCap(cap);
#endif
	/* scheduleDoGC() deletes all the threads */
	cap = scheduleDoGC(cap,task,rtsFalse);
	break;
    case SCHED_SHUTTING_DOWN:
	debugTrace(DEBUG_sched, "SCHED_SHUTTING_DOWN");
	// If we are a worker, just exit.  If we're a bound thread
	// then we will exit below when we've removed our TSO from
	// the run queue.
	if (task->tso == NULL && emptyRunQueue(cap)) {
	    return cap;
	}
	break;
    default:
	barf("sched_state: %d", sched_state);
    }

#if defined(THREADED_RTS)
    // If the run queue is empty, take a spark and turn it into a thread.
    {
	if (emptyRunQueue(cap)) {
	    StgClosure *spark;
	    spark = findSpark(cap);
	    if (spark != NULL) {
		debugTrace(DEBUG_sched,
			   "turning spark of closure %p into a thread",
			   (StgClosure *)spark);
		createSparkThread(cap,spark);	  
	    }
	}
    }
#endif // THREADED_RTS

    scheduleStartSignalHandlers(cap);

    // Only check the black holes here if we've nothing else to do.
    // During normal execution, the black hole list only gets checked
    // at GC time, to avoid repeatedly traversing this possibly long
    // list each time around the scheduler.
    if (emptyRunQueue(cap)) { scheduleCheckBlackHoles(cap); }

    scheduleCheckWakeupThreads(cap);

    scheduleCheckBlockedThreads(cap);

    scheduleDetectDeadlock(cap,task);
#if defined(THREADED_RTS)
    cap = task->cap;    // reload cap, it might have changed
#endif

    // Normally, the only way we can get here with no threads to
    // run is if a keyboard interrupt received during 
    // scheduleCheckBlockedThreads() or scheduleDetectDeadlock().
    // Additionally, it is not fatal for the
    // threaded RTS to reach here with no threads to run.
    //
    // win32: might be here due to awaitEvent() being abandoned
    // as a result of a console event having been delivered.
    if ( emptyRunQueue(cap) ) {
#if !defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
	ASSERT(sched_state >= SCHED_INTERRUPTING);
#endif
	continue; // nothing to do
    }

#if defined(PARALLEL_HASKELL)
    scheduleSendPendingMessages();
    if (emptyRunQueue(cap) && scheduleActivateSpark()) 
	continue;

#if defined(SPARKS)
    ASSERT(next_fish_to_send_at==0);  // i.e. no delayed fishes left!
#endif

    /* If we still have no work we need to send a FISH to get a spark
       from another PE */
    if (emptyRunQueue(cap)) {
	if (!scheduleGetRemoteWork(&receivedFinish)) continue;
	ASSERT(rtsFalse); // should not happen at the moment
    }
    // from here: non-empty run queue.
    //  TODO: merge above case with this, only one call processMessages() !
    if (PacketsWaiting()) {  /* process incoming messages, if
				any pending...  only in else
				because getRemoteWork waits for
				messages as well */
	receivedFinish = processMessages();
    }
#endif

#if defined(GRAN)
    scheduleProcessEvent(event);
#endif

    // 
    // Get a thread to run
    //
    t = popRunQueue(cap);

#if defined(GRAN) || defined(PAR)
    scheduleGranParReport(); // some kind of debuging output
#else
    // Sanity check the thread we're about to run.  This can be
    // expensive if there is lots of thread switching going on...
    IF_DEBUG(sanity,checkTSO(t));
#endif

#if defined(THREADED_RTS)
    // Check whether we can run this thread in the current task.
    // If not, we have to pass our capability to the right task.
    {
	Task *bound = t->bound;
      
	if (bound) {
	    if (bound == task) {
		debugTrace(DEBUG_sched,
			   "### Running thread %lu in bound thread", (unsigned long)t->id);
		// yes, the Haskell thread is bound to the current native thread
	    } else {
		debugTrace(DEBUG_sched,
			   "### thread %lu bound to another OS thread", (unsigned long)t->id);
		// no, bound to a different Haskell thread: pass to that thread
		pushOnRunQueue(cap,t);
		continue;
	    }
	} else {
	    // The thread we want to run is unbound.
	    if (task->tso) { 
		debugTrace(DEBUG_sched,
			   "### this OS thread cannot run thread %lu", (unsigned long)t->id);
		// no, the current native thread is bound to a different
		// Haskell thread, so pass it to any worker thread
		pushOnRunQueue(cap,t);
		continue; 
	    }
	}
    }
#endif

    cap->r.rCurrentTSO = t;
    
    /* context switches are initiated by the timer signal, unless
     * the user specified "context switch as often as possible", with
     * +RTS -C0
     */
    if (RtsFlags.ConcFlags.ctxtSwitchTicks == 0
	&& !emptyThreadQueues(cap)) {
	context_switch = 1;
    }
	 
run_thread:

    debugTrace(DEBUG_sched, "-->> running thread %ld %s ...", 
			      (long)t->id, whatNext_strs[t->what_next]);

#if defined(PROFILING)
    startHeapProfTimer();
#endif

    // Check for exceptions blocked on this thread
    maybePerformBlockedException (cap, t);

    // ----------------------------------------------------------------------
    // Run the current thread 

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);

    prev_what_next = t->what_next;

    errno = t->saved_errno;
    cap->in_haskell = rtsTrue;

    dirtyTSO(t);

    recent_activity = ACTIVITY_YES;

    switch (prev_what_next) {
	
    case ThreadKilled:
    case ThreadComplete:
	/* Thread already finished, return to scheduler. */
	ret = ThreadFinished;
	break;
	
    case ThreadRunGHC:
    {
	StgRegTable *r;
	r = StgRun((StgFunPtr) stg_returnToStackTop, &cap->r);
	cap = regTableToCapability(r);
	ret = r->rRet;
	break;
    }
    
    case ThreadInterpret:
	cap = interpretBCO(cap);
	ret = cap->r.rRet;
	break;
	
    default:
	barf("schedule: invalid what_next field");
    }

    cap->in_haskell = rtsFalse;

    // The TSO might have moved, eg. if it re-entered the RTS and a GC
    // happened.  So find the new location:
    t = cap->r.rCurrentTSO;

    // We have run some Haskell code: there might be blackhole-blocked
    // threads to wake up now.
    // Lock-free test here should be ok, we're just setting a flag.
    if ( blackhole_queue != END_TSO_QUEUE ) {
	blackholes_need_checking = rtsTrue;
    }
    
    // And save the current errno in this thread.
    // XXX: possibly bogus for SMP because this thread might already
    // be running again, see code below.
    t->saved_errno = errno;

#if defined(THREADED_RTS)
    // If ret is ThreadBlocked, and this Task is bound to the TSO that
    // blocked, we are in limbo - the TSO is now owned by whatever it
    // is blocked on, and may in fact already have been woken up,
    // perhaps even on a different Capability.  It may be the case
    // that task->cap != cap.  We better yield this Capability
    // immediately and return to normaility.
    if (ret == ThreadBlocked) {
	debugTrace(DEBUG_sched,
		   "--<< thread %lu (%s) stopped: blocked",
		   (unsigned long)t->id, whatNext_strs[t->what_next]);
	continue;
    }
#endif

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);

    // ----------------------------------------------------------------------
    
    // Costs for the scheduler are assigned to CCS_SYSTEM
#if defined(PROFILING)
    stopHeapProfTimer();
    CCCS = CCS_SYSTEM;
#endif
    
    schedulePostRunThread();

    ready_to_gc = rtsFalse;

    switch (ret) {
    case HeapOverflow:
	ready_to_gc = scheduleHandleHeapOverflow(cap,t);
	break;

    case StackOverflow:
	scheduleHandleStackOverflow(cap,task,t);
	break;

    case ThreadYielding:
	if (scheduleHandleYield(cap, t, prev_what_next)) {
            // shortcut for switching between compiler/interpreter:
	    goto run_thread; 
	}
	break;

    case ThreadBlocked:
	scheduleHandleThreadBlocked(t);
	break;

    case ThreadFinished:
	if (scheduleHandleThreadFinished(cap, task, t)) return cap;
	ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
	break;

    default:
      barf("schedule: invalid thread return code %d", (int)ret);
    }

    if (scheduleDoHeapProfile(ready_to_gc)) { ready_to_gc = rtsFalse; }
    if (ready_to_gc) {
      cap = scheduleDoGC(cap,task,rtsFalse);
    }
  } /* end of while() */

  debugTrace(PAR_DEBUG_verbose,
	     "== Leaving schedule() after having received Finish");
}

/* ----------------------------------------------------------------------------
 * Setting up the scheduler loop
 * ------------------------------------------------------------------------- */

static void
schedulePreLoop(void)
{
#if defined(GRAN) 
    /* set up first event to get things going */
    /* ToDo: assign costs for system setup and init MainTSO ! */
    new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
	      ContinueThread, 
	      CurrentTSO, (StgClosure*)NULL, (rtsSpark*)NULL);
    
    debugTrace (DEBUG_gran,
		"GRAN: Init CurrentTSO (in schedule) = %p", 
		CurrentTSO);
    IF_DEBUG(gran, G_TSO(CurrentTSO, 5));
    
    if (RtsFlags.GranFlags.Light) {
	/* Save current time; GranSim Light only */
	CurrentTSO->gran.clock = CurrentTime[CurrentProc];
    }      
#endif
}

/* -----------------------------------------------------------------------------
 * schedulePushWork()
 *
 * Push work to other Capabilities if we have some.
 * -------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
static void
schedulePushWork(Capability *cap USED_IF_THREADS, 
		 Task *task      USED_IF_THREADS)
{
    Capability *free_caps[n_capabilities], *cap0;
    nat i, n_free_caps;

    // migration can be turned off with +RTS -qg
    if (!RtsFlags.ParFlags.migrate) return;

    // Check whether we have more threads on our run queue, or sparks
    // in our pool, that we could hand to another Capability.
    if ((emptyRunQueue(cap) || cap->run_queue_hd->link == END_TSO_QUEUE)
	&& sparkPoolSizeCap(cap) < 2) {
	return;
    }

    // First grab as many free Capabilities as we can.
    for (i=0, n_free_caps=0; i < n_capabilities; i++) {
	cap0 = &capabilities[i];
	if (cap != cap0 && tryGrabCapability(cap0,task)) {
	    if (!emptyRunQueue(cap0) || cap->returning_tasks_hd != NULL) {
		// it already has some work, we just grabbed it at 
		// the wrong moment.  Or maybe it's deadlocked!
		releaseCapability(cap0);
	    } else {
		free_caps[n_free_caps++] = cap0;
	    }
	}
    }

    // we now have n_free_caps free capabilities stashed in
    // free_caps[].  Share our run queue equally with them.  This is
    // probably the simplest thing we could do; improvements we might
    // want to do include:
    //
    //   - giving high priority to moving relatively new threads, on 
    //     the gournds that they haven't had time to build up a
    //     working set in the cache on this CPU/Capability.
    //
    //   - giving low priority to moving long-lived threads

    if (n_free_caps > 0) {
	StgTSO *prev, *t, *next;
	rtsBool pushed_to_all;

	debugTrace(DEBUG_sched, "excess threads on run queue and %d free capabilities, sharing...", n_free_caps);

	i = 0;
	pushed_to_all = rtsFalse;

	if (cap->run_queue_hd != END_TSO_QUEUE) {
	    prev = cap->run_queue_hd;
	    t = prev->link;
	    prev->link = END_TSO_QUEUE;
	    for (; t != END_TSO_QUEUE; t = next) {
		next = t->link;
		t->link = END_TSO_QUEUE;
		if (t->what_next == ThreadRelocated
		    || t->bound == task // don't move my bound thread
		    || tsoLocked(t)) {  // don't move a locked thread
		    prev->link = t;
		    prev = t;
		} else if (i == n_free_caps) {
		    pushed_to_all = rtsTrue;
		    i = 0;
		    // keep one for us
		    prev->link = t;
		    prev = t;
		} else {
		    debugTrace(DEBUG_sched, "pushing thread %lu to capability %d", (unsigned long)t->id, free_caps[i]->no);
		    appendToRunQueue(free_caps[i],t);
		    if (t->bound) { t->bound->cap = free_caps[i]; }
		    t->cap = free_caps[i];
		    i++;
		}
	    }
	    cap->run_queue_tl = prev;
	}

	// If there are some free capabilities that we didn't push any
	// threads to, then try to push a spark to each one.
	if (!pushed_to_all) {
	    StgClosure *spark;
	    // i is the next free capability to push to
	    for (; i < n_free_caps; i++) {
		if (emptySparkPoolCap(free_caps[i])) {
		    spark = findSpark(cap);
		    if (spark != NULL) {
			debugTrace(DEBUG_sched, "pushing spark %p to capability %d", spark, free_caps[i]->no);
			newSpark(&(free_caps[i]->r), spark);
		    }
		}
	    }
	}

	// release the capabilities
	for (i = 0; i < n_free_caps; i++) {
	    task->cap = free_caps[i];
	    releaseCapability(free_caps[i]);
	}
    }
    task->cap = cap; // reset to point to our Capability.
}
#endif

/* ----------------------------------------------------------------------------
 * Start any pending signal handlers
 * ------------------------------------------------------------------------- */

#if defined(RTS_USER_SIGNALS) && !defined(THREADED_RTS)
static void
scheduleStartSignalHandlers(Capability *cap)
{
    if (signals_pending()) { // safe outside the lock
	startSignalHandlers(cap);
    }
}
#else
static void
scheduleStartSignalHandlers(Capability *cap STG_UNUSED)
{
}
#endif

/* ----------------------------------------------------------------------------
 * Check for blocked threads that can be woken up.
 * ------------------------------------------------------------------------- */

static void
scheduleCheckBlockedThreads(Capability *cap USED_IF_NOT_THREADS)
{
#if !defined(THREADED_RTS)
    //
    // Check whether any waiting threads need to be woken up.  If the
    // run queue is empty, and there are no other tasks running, we
    // can wait indefinitely for something to happen.
    //
    if ( !emptyQueue(blocked_queue_hd) || !emptyQueue(sleeping_queue) )
    {
	awaitEvent( emptyRunQueue(cap) && !blackholes_need_checking );
    }
#endif
}


/* ----------------------------------------------------------------------------
 * Check for threads woken up by other Capabilities
 * ------------------------------------------------------------------------- */

static void
scheduleCheckWakeupThreads(Capability *cap USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    // Any threads that were woken up by other Capabilities get
    // appended to our run queue.
    if (!emptyWakeupQueue(cap)) {
	ACQUIRE_LOCK(&cap->lock);
	if (emptyRunQueue(cap)) {
	    cap->run_queue_hd = cap->wakeup_queue_hd;
	    cap->run_queue_tl = cap->wakeup_queue_tl;
	} else {
	    cap->run_queue_tl->link = cap->wakeup_queue_hd;
	    cap->run_queue_tl = cap->wakeup_queue_tl;
	}
	cap->wakeup_queue_hd = cap->wakeup_queue_tl = END_TSO_QUEUE;
	RELEASE_LOCK(&cap->lock);
    }
#endif
}

/* ----------------------------------------------------------------------------
 * Check for threads blocked on BLACKHOLEs that can be woken up
 * ------------------------------------------------------------------------- */
static void
scheduleCheckBlackHoles (Capability *cap)
{
    if ( blackholes_need_checking ) // check without the lock first
    {
	ACQUIRE_LOCK(&sched_mutex);
	if ( blackholes_need_checking ) {
	    checkBlackHoles(cap);
	    blackholes_need_checking = rtsFalse;
	}
	RELEASE_LOCK(&sched_mutex);
    }
}

/* ----------------------------------------------------------------------------
 * Detect deadlock conditions and attempt to resolve them.
 * ------------------------------------------------------------------------- */

static void
scheduleDetectDeadlock (Capability *cap, Task *task)
{

#if defined(PARALLEL_HASKELL)
    // ToDo: add deadlock detection in GUM (similar to THREADED_RTS) -- HWL
    return;
#endif

    /* 
     * Detect deadlock: when we have no threads to run, there are no
     * threads blocked, waiting for I/O, or sleeping, and all the
     * other tasks are waiting for work, we must have a deadlock of
     * some description.
     */
    if ( emptyThreadQueues(cap) )
    {
#if defined(THREADED_RTS)
	/* 
	 * In the threaded RTS, we only check for deadlock if there
	 * has been no activity in a complete timeslice.  This means
	 * we won't eagerly start a full GC just because we don't have
	 * any threads to run currently.
	 */
	if (recent_activity != ACTIVITY_INACTIVE) return;
#endif

	debugTrace(DEBUG_sched, "deadlocked, forcing major GC...");

	// Garbage collection can release some new threads due to
	// either (a) finalizers or (b) threads resurrected because
	// they are unreachable and will therefore be sent an
	// exception.  Any threads thus released will be immediately
	// runnable.
	cap = scheduleDoGC (cap, task, rtsTrue/*force  major GC*/);

	recent_activity = ACTIVITY_DONE_GC;
	
	if ( !emptyRunQueue(cap) ) return;

#if defined(RTS_USER_SIGNALS) && !defined(THREADED_RTS)
	/* If we have user-installed signal handlers, then wait
	 * for signals to arrive rather then bombing out with a
	 * deadlock.
	 */
	if ( anyUserHandlers() ) {
	    debugTrace(DEBUG_sched,
		       "still deadlocked, waiting for signals...");

	    awaitUserSignals();

	    if (signals_pending()) {
		startSignalHandlers(cap);
	    }

	    // either we have threads to run, or we were interrupted:
	    ASSERT(!emptyRunQueue(cap) || sched_state >= SCHED_INTERRUPTING);
	}
#endif

#if !defined(THREADED_RTS)
	/* Probably a real deadlock.  Send the current main thread the
	 * Deadlock exception.
	 */
	if (task->tso) {
	    switch (task->tso->why_blocked) {
	    case BlockedOnSTM:
	    case BlockedOnBlackHole:
	    case BlockedOnException:
	    case BlockedOnMVar:
		throwToSingleThreaded(cap, task->tso, 
				      (StgClosure *)NonTermination_closure);
		return;
	    default:
		barf("deadlock: main thread blocked in a strange way");
	    }
	}
	return;
#endif
    }
}

/* ----------------------------------------------------------------------------
 * Process an event (GRAN only)
 * ------------------------------------------------------------------------- */

#if defined(GRAN)
static StgTSO *
scheduleProcessEvent(rtsEvent *event)
{
    StgTSO *t;

    if (RtsFlags.GranFlags.Light)
      GranSimLight_enter_system(event, &ActiveTSO); // adjust ActiveTSO etc

    /* adjust time based on time-stamp */
    if (event->time > CurrentTime[CurrentProc] &&
        event->evttype != ContinueThread)
      CurrentTime[CurrentProc] = event->time;
    
    /* Deal with the idle PEs (may issue FindWork or MoveSpark events) */
    if (!RtsFlags.GranFlags.Light)
      handleIdlePEs();

    IF_DEBUG(gran, debugBelch("GRAN: switch by event-type\n"));

    /* main event dispatcher in GranSim */
    switch (event->evttype) {
      /* Should just be continuing execution */
    case ContinueThread:
      IF_DEBUG(gran, debugBelch("GRAN: doing ContinueThread\n"));
      /* ToDo: check assertion
      ASSERT(run_queue_hd != (StgTSO*)NULL &&
	     run_queue_hd != END_TSO_QUEUE);
      */
      /* Ignore ContinueThreads for fetching threads (if synchr comm) */
      if (!RtsFlags.GranFlags.DoAsyncFetch &&
	  procStatus[CurrentProc]==Fetching) {
	debugBelch("ghuH: Spurious ContinueThread while Fetching ignored; TSO %d (%p) [PE %d]\n",
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	goto next_thread;
      }	
      /* Ignore ContinueThreads for completed threads */
      if (CurrentTSO->what_next == ThreadComplete) {
	debugBelch("ghuH: found a ContinueThread event for completed thread %d (%p) [PE %d] (ignoring ContinueThread)\n", 
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	goto next_thread;
      }	
      /* Ignore ContinueThreads for threads that are being migrated */
      if (PROCS(CurrentTSO)==Nowhere) { 
	debugBelch("ghuH: trying to run the migrating TSO %d (%p) [PE %d] (ignoring ContinueThread)\n",
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	goto next_thread;
      }
      /* The thread should be at the beginning of the run queue */
      if (CurrentTSO!=run_queue_hds[CurrentProc]) { 
	debugBelch("ghuH: TSO %d (%p) [PE %d] is not at the start of the run_queue when doing a ContinueThread\n",
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	break; // run the thread anyway
      }
      /*
      new_event(proc, proc, CurrentTime[proc],
		FindWork,
		(StgTSO*)NULL, (StgClosure*)NULL, (rtsSpark*)NULL);
      goto next_thread; 
      */ /* Catches superfluous CONTINUEs -- should be unnecessary */
      break; // now actually run the thread; DaH Qu'vam yImuHbej 

    case FetchNode:
      do_the_fetchnode(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case GlobalBlock:
      do_the_globalblock(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case FetchReply:
      do_the_fetchreply(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case UnblockThread:   /* Move from the blocked queue to the tail of */
      do_the_unblock(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case ResumeThread:  /* Move from the blocked queue to the tail of */
      /* the runnable queue ( i.e. Qu' SImqa'lu') */ 
      event->tso->gran.blocktime += 
	CurrentTime[CurrentProc] - event->tso->gran.blockedat;
      do_the_startthread(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case StartThread:
      do_the_startthread(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case MoveThread:
      do_the_movethread(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case MoveSpark:
      do_the_movespark(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case FindWork:
      do_the_findwork(event);
      goto next_thread;             /* handle next event in event queue  */
      
    default:
      barf("Illegal event type %u\n", event->evttype);
    }  /* switch */
    
    /* This point was scheduler_loop in the old RTS */

    IF_DEBUG(gran, debugBelch("GRAN: after main switch\n"));

    TimeOfLastEvent = CurrentTime[CurrentProc];
    TimeOfNextEvent = get_time_of_next_event();
    IgnoreEvents=(TimeOfNextEvent==0); // HWL HACK
    // CurrentTSO = ThreadQueueHd;

    IF_DEBUG(gran, debugBelch("GRAN: time of next event is: %ld\n", 
			 TimeOfNextEvent));

    if (RtsFlags.GranFlags.Light) 
      GranSimLight_leave_system(event, &ActiveTSO); 

    EndOfTimeSlice = CurrentTime[CurrentProc]+RtsFlags.GranFlags.time_slice;

    IF_DEBUG(gran, 
	     debugBelch("GRAN: end of time-slice is %#lx\n", EndOfTimeSlice));

    /* in a GranSim setup the TSO stays on the run queue */
    t = CurrentTSO;
    /* Take a thread from the run queue. */
    POP_RUN_QUEUE(t); // take_off_run_queue(t);

    IF_DEBUG(gran, 
	     debugBelch("GRAN: About to run current thread, which is\n");
	     G_TSO(t,5));

    context_switch = 0; // turned on via GranYield, checking events and time slice

    IF_DEBUG(gran, 
	     DumpGranEvent(GR_SCHEDULE, t));

    procStatus[CurrentProc] = Busy;
}
#endif // GRAN

/* ----------------------------------------------------------------------------
 * Send pending messages (PARALLEL_HASKELL only)
 * ------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL)
static StgTSO *
scheduleSendPendingMessages(void)
{
    StgSparkPool *pool;
    rtsSpark spark;
    StgTSO *t;

# if defined(PAR) // global Mem.Mgmt., omit for now
    if (PendingFetches != END_BF_QUEUE) {
        processFetches();
    }
# endif
    
    if (RtsFlags.ParFlags.BufferTime) {
	// if we use message buffering, we must send away all message
	// packets which have become too old...
	sendOldBuffers(); 
    }
}
#endif

/* ----------------------------------------------------------------------------
 * Activate spark threads (PARALLEL_HASKELL only)
 * ------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL)
static void
scheduleActivateSpark(void)
{
#if defined(SPARKS)
  ASSERT(emptyRunQueue());
/* We get here if the run queue is empty and want some work.
   We try to turn a spark into a thread, and add it to the run queue,
   from where it will be picked up in the next iteration of the scheduler
   loop.
*/

      /* :-[  no local threads => look out for local sparks */
      /* the spark pool for the current PE */
      pool = &(cap.r.rSparks); // JB: cap = (old) MainCap
      if (advisory_thread_count < RtsFlags.ParFlags.maxThreads &&
	  pool->hd < pool->tl) {
	/* 
	 * ToDo: add GC code check that we really have enough heap afterwards!!
	 * Old comment:
	 * If we're here (no runnable threads) and we have pending
	 * sparks, we must have a space problem.  Get enough space
	 * to turn one of those pending sparks into a
	 * thread... 
	 */

  	spark = findSpark(rtsFalse);            /* get a spark */
  	if (spark != (rtsSpark) NULL) {
  	  tso = createThreadFromSpark(spark);       /* turn the spark into a thread */
  	  IF_PAR_DEBUG(fish, // schedule,
  		       debugBelch("==== schedule: Created TSO %d (%p); %d threads active\n",
  			     tso->id, tso, advisory_thread_count));

  	  if (tso==END_TSO_QUEUE) { /* failed to activate spark->back to loop */
  	    IF_PAR_DEBUG(fish, // schedule,
  			 debugBelch("==^^ failed to create thread from spark @ %lx\n",
                            spark));
  	    return rtsFalse; /* failed to generate a thread */
  	  }                  /* otherwise fall through & pick-up new tso */
  	} else {
  	  IF_PAR_DEBUG(fish, // schedule,
  		       debugBelch("==^^ no local sparks (spark pool contains only NFs: %d)\n", 
  			     spark_queue_len(pool)));
  	  return rtsFalse;  /* failed to generate a thread */
  	}
  	return rtsTrue;  /* success in generating a thread */
  } else { /* no more threads permitted or pool empty */
    return rtsFalse;  /* failed to generateThread */
  }
#else
  tso = NULL; // avoid compiler warning only
  return rtsFalse;  /* dummy in non-PAR setup */
#endif // SPARKS
}
#endif // PARALLEL_HASKELL

/* ----------------------------------------------------------------------------
 * Get work from a remote node (PARALLEL_HASKELL only)
 * ------------------------------------------------------------------------- */
    
#if defined(PARALLEL_HASKELL)
static rtsBool
scheduleGetRemoteWork(rtsBool *receivedFinish)
{
  ASSERT(emptyRunQueue());

  if (RtsFlags.ParFlags.BufferTime) {
	IF_PAR_DEBUG(verbose, 
	        debugBelch("...send all pending data,"));
        {
	  nat i;
	  for (i=1; i<=nPEs; i++)
	    sendImmediately(i); // send all messages away immediately
	}
  }
# ifndef SPARKS
	//++EDEN++ idle() , i.e. send all buffers, wait for work
	// suppress fishing in EDEN... just look for incoming messages
	// (blocking receive)
  IF_PAR_DEBUG(verbose, 
	       debugBelch("...wait for incoming messages...\n"));
  *receivedFinish = processMessages(); // blocking receive...

	// and reenter scheduling loop after having received something
	// (return rtsFalse below)

# else /* activate SPARKS machinery */
/* We get here, if we have no work, tried to activate a local spark, but still
   have no work. We try to get a remote spark, by sending a FISH message.
   Thread migration should be added here, and triggered when a sequence of 
   fishes returns without work. */
	delay = (RtsFlags.ParFlags.fishDelay!=0ll ? RtsFlags.ParFlags.fishDelay : 0ll);

      /* =8-[  no local sparks => look for work on other PEs */
	/*
	 * We really have absolutely no work.  Send out a fish
	 * (there may be some out there already), and wait for
	 * something to arrive.  We clearly can't run any threads
	 * until a SCHEDULE or RESUME arrives, and so that's what
	 * we're hoping to see.  (Of course, we still have to
	 * respond to other types of messages.)
	 */
	rtsTime now = msTime() /*CURRENT_TIME*/;
	IF_PAR_DEBUG(verbose, 
		     debugBelch("--  now=%ld\n", now));
	IF_PAR_DEBUG(fish, // verbose,
  	     if (outstandingFishes < RtsFlags.ParFlags.maxFishes &&
  		 (last_fish_arrived_at!=0 &&
  		  last_fish_arrived_at+delay > now)) {
  	       debugBelch("--$$ <%llu> delaying FISH until %llu (last fish %llu, delay %llu)\n",
  		     now, last_fish_arrived_at+delay, 
		     last_fish_arrived_at,
  		     delay);
  	     });
  
	if (outstandingFishes < RtsFlags.ParFlags.maxFishes &&
	    advisory_thread_count < RtsFlags.ParFlags.maxThreads) { // send a FISH, but when?
	  if (last_fish_arrived_at==0 ||
	      (last_fish_arrived_at+delay <= now)) {           // send FISH now!
	    /* outstandingFishes is set in sendFish, processFish;
	       avoid flooding system with fishes via delay */
    next_fish_to_send_at = 0;  
  } else {
    /* ToDo: this should be done in the main scheduling loop to avoid the
             busy wait here; not so bad if fish delay is very small  */
    int iq = 0; // DEBUGGING -- HWL
    next_fish_to_send_at = last_fish_arrived_at+delay; // remember when to send  
    /* send a fish when ready, but process messages that arrive in the meantime */
    do {
      if (PacketsWaiting()) {
        iq++; // DEBUGGING
        *receivedFinish = processMessages();
      }
      now = msTime();
    } while (!*receivedFinish || now<next_fish_to_send_at);
    // JB: This means the fish could become obsolete, if we receive
    // work. Better check for work again? 
    // last line: while (!receivedFinish || !haveWork || now<...)
    // next line: if (receivedFinish || haveWork )

    if (*receivedFinish) // no need to send a FISH if we are finishing anyway
      return rtsFalse;  // NB: this will leave scheduler loop
			// immediately after return!
  			  
    IF_PAR_DEBUG(fish, // verbose,
  	       debugBelch("--$$ <%llu> sent delayed fish (%d processMessages); active/total threads=%d/%d\n",now,iq,run_queue_len(),advisory_thread_count));

  }

    // JB: IMHO, this should all be hidden inside sendFish(...)
    /* pe = choosePE(); 
       sendFish(pe, thisPE, NEW_FISH_AGE, NEW_FISH_HISTORY, 
                NEW_FISH_HUNGER);

    // Global statistics: count no. of fishes
    if (RtsFlags.ParFlags.ParStats.Global &&
         RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	   globalParStats.tot_fish_mess++;
	   }
    */ 

  /* delayed fishes must have been sent by now! */
  next_fish_to_send_at = 0;  
  }
      
  *receivedFinish = processMessages();
# endif /* SPARKS */

 return rtsFalse;
 /* NB: this function always returns rtsFalse, meaning the scheduler
    loop continues with the next iteration; 
    rationale: 
      return code means success in finding work; we enter this function
      if there is no local work, thus have to send a fish which takes
      time until it arrives with work; in the meantime we should process
      messages in the main loop;
 */
}
#endif // PARALLEL_HASKELL

/* ----------------------------------------------------------------------------
 * PAR/GRAN: Report stats & debugging info(?)
 * ------------------------------------------------------------------------- */

#if defined(PAR) || defined(GRAN)
static void
scheduleGranParReport(void)
{
  ASSERT(run_queue_hd != END_TSO_QUEUE);

  /* Take a thread from the run queue, if we have work */
  POP_RUN_QUEUE(t);  // take_off_run_queue(END_TSO_QUEUE);

    /* If this TSO has got its outport closed in the meantime, 
     *   it mustn't be run. Instead, we have to clean it up as if it was finished.
     * It has to be marked as TH_DEAD for this purpose.
     * If it is TH_TERM instead, it is supposed to have finished in the normal way.

JB: TODO: investigate wether state change field could be nuked
     entirely and replaced by the normal tso state (whatnext
     field). All we want to do is to kill tsos from outside.
     */

    /* ToDo: write something to the log-file
    if (RTSflags.ParFlags.granSimStats && !sameThread)
        DumpGranEvent(GR_SCHEDULE, RunnableThreadsHd);

    CurrentTSO = t;
    */
    /* the spark pool for the current PE */
    pool = &(cap.r.rSparks); //  cap = (old) MainCap

    IF_DEBUG(scheduler, 
	     debugBelch("--=^ %d threads, %d sparks on [%#x]\n", 
		   run_queue_len(), spark_queue_len(pool), CURRENT_PROC));

    IF_PAR_DEBUG(fish,
	     debugBelch("--=^ %d threads, %d sparks on [%#x]\n", 
		   run_queue_len(), spark_queue_len(pool), CURRENT_PROC));

    if (RtsFlags.ParFlags.ParStats.Full && 
	(t->par.sparkname != (StgInt)0) && // only log spark generated threads
	(emitSchedule || // forced emit
         (t && LastTSO && t->id != LastTSO->id))) {
      /* 
	 we are running a different TSO, so write a schedule event to log file
	 NB: If we use fair scheduling we also have to write  a deschedule 
	     event for LastTSO; with unfair scheduling we know that the
	     previous tso has blocked whenever we switch to another tso, so
	     we don't need it in GUM for now
      */
      IF_PAR_DEBUG(fish, // schedule,
		   debugBelch("____ scheduling spark generated thread %d (%lx) (%lx) via a forced emit\n",t->id,t,t->par.sparkname));

      DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC,
		       GR_SCHEDULE, t, (StgClosure *)NULL, 0, 0);
      emitSchedule = rtsFalse;
    }
}     
#endif

/* ----------------------------------------------------------------------------
 * After running a thread...
 * ------------------------------------------------------------------------- */

static void
schedulePostRunThread(void)
{
#if defined(PAR)
    /* HACK 675: if the last thread didn't yield, make sure to print a 
       SCHEDULE event to the log file when StgRunning the next thread, even
       if it is the same one as before */
    LastTSO = t; 
    TimeOfLastYield = CURRENT_TIME;
#endif

  /* some statistics gathering in the parallel case */

#if defined(GRAN) || defined(PAR) || defined(EDEN)
  switch (ret) {
    case HeapOverflow:
# if defined(GRAN)
      IF_DEBUG(gran, DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_heapover++;
# elif defined(PAR)
      globalParStats.tot_heapover++;
# endif
      break;

     case StackOverflow:
# if defined(GRAN)
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_stackover++;
# elif defined(PAR)
      // IF_DEBUG(par, 
      // DumpGranEvent(GR_DESCHEDULE, t);
      globalParStats.tot_stackover++;
# endif
      break;

    case ThreadYielding:
# if defined(GRAN)
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_yields++;
# elif defined(PAR)
      // IF_DEBUG(par, 
      // DumpGranEvent(GR_DESCHEDULE, t);
      globalParStats.tot_yields++;
# endif
      break; 

    case ThreadBlocked:
# if defined(GRAN)
	debugTrace(DEBUG_sched, 
		   "--<< thread %ld (%p; %s) stopped, blocking on node %p [PE %d] with BQ: ", 
		   t->id, t, whatNext_strs[t->what_next], t->block_info.closure, 
		   (t->block_info.closure==(StgClosure*)NULL ? 99 : where_is(t->block_info.closure)));
	       if (t->block_info.closure!=(StgClosure*)NULL)
	         print_bq(t->block_info.closure);
	       debugBelch("\n"));

      // ??? needed; should emit block before
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t)); 
      prune_eventq(t, (StgClosure *)NULL); // prune ContinueThreads for t
      /*
	ngoq Dogh!
      ASSERT(procStatus[CurrentProc]==Busy || 
	      ((procStatus[CurrentProc]==Fetching) && 
	      (t->block_info.closure!=(StgClosure*)NULL)));
      if (run_queue_hds[CurrentProc] == END_TSO_QUEUE &&
	  !(!RtsFlags.GranFlags.DoAsyncFetch &&
	    procStatus[CurrentProc]==Fetching)) 
	procStatus[CurrentProc] = Idle;
      */
# elif defined(PAR)
//++PAR++  blockThread() writes the event (change?)
# endif
    break;

  case ThreadFinished:
    break;

  default:
    barf("parGlobalStats: unknown return code");
    break;
    }
#endif
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadHeepOverflow
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleHandleHeapOverflow( Capability *cap, StgTSO *t )
{
    // did the task ask for a large block?
    if (cap->r.rHpAlloc > BLOCK_SIZE) {
	// if so, get one and push it on the front of the nursery.
	bdescr *bd;
	lnat blocks;
	
	blocks = (lnat)BLOCK_ROUND_UP(cap->r.rHpAlloc) / BLOCK_SIZE;
	
	debugTrace(DEBUG_sched,
		   "--<< thread %ld (%s) stopped: requesting a large block (size %ld)\n", 
		   (long)t->id, whatNext_strs[t->what_next], blocks);
    
	// don't do this if the nursery is (nearly) full, we'll GC first.
	if (cap->r.rCurrentNursery->link != NULL ||
	    cap->r.rNursery->n_blocks == 1) {  // paranoia to prevent infinite loop
	                                       // if the nursery has only one block.
	    
	    ACQUIRE_SM_LOCK
	    bd = allocGroup( blocks );
	    RELEASE_SM_LOCK
	    cap->r.rNursery->n_blocks += blocks;
	    
	    // link the new group into the list
	    bd->link = cap->r.rCurrentNursery;
	    bd->u.back = cap->r.rCurrentNursery->u.back;
	    if (cap->r.rCurrentNursery->u.back != NULL) {
		cap->r.rCurrentNursery->u.back->link = bd;
	    } else {
#if !defined(THREADED_RTS)
		ASSERT(g0s0->blocks == cap->r.rCurrentNursery &&
		       g0s0 == cap->r.rNursery);
#endif
		cap->r.rNursery->blocks = bd;
	    }		  
	    cap->r.rCurrentNursery->u.back = bd;
	    
	    // initialise it as a nursery block.  We initialise the
	    // step, gen_no, and flags field of *every* sub-block in
	    // this large block, because this is easier than making
	    // sure that we always find the block head of a large
	    // block whenever we call Bdescr() (eg. evacuate() and
	    // isAlive() in the GC would both have to do this, at
	    // least).
	    { 
		bdescr *x;
		for (x = bd; x < bd + blocks; x++) {
		    x->step = cap->r.rNursery;
		    x->gen_no = 0;
		    x->flags = 0;
		}
	    }
	    
	    // This assert can be a killer if the app is doing lots
	    // of large block allocations.
	    IF_DEBUG(sanity, checkNurserySanity(cap->r.rNursery));
	    
	    // now update the nursery to point to the new block
	    cap->r.rCurrentNursery = bd;
	    
	    // we might be unlucky and have another thread get on the
	    // run queue before us and steal the large block, but in that
	    // case the thread will just end up requesting another large
	    // block.
	    pushOnRunQueue(cap,t);
	    return rtsFalse;  /* not actually GC'ing */
	}
    }
    
    debugTrace(DEBUG_sched,
	       "--<< thread %ld (%s) stopped: HeapOverflow\n", 
	       (long)t->id, whatNext_strs[t->what_next]);

#if defined(GRAN)
    ASSERT(!is_on_queue(t,CurrentProc));
#elif defined(PARALLEL_HASKELL)
    /* Currently we emit a DESCHEDULE event before GC in GUM.
       ToDo: either add separate event to distinguish SYSTEM time from rest
       or just nuke this DESCHEDULE (and the following SCHEDULE) */
    if (0 && RtsFlags.ParFlags.ParStats.Full) {
	DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC,
			 GR_DESCHEDULE, t, (StgClosure *)NULL, 0, 0);
	emitSchedule = rtsTrue;
    }
#endif
      
    pushOnRunQueue(cap,t);
    return rtsTrue;
    /* actual GC is done at the end of the while loop in schedule() */
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadStackOverflow
 * -------------------------------------------------------------------------- */

static void
scheduleHandleStackOverflow (Capability *cap, Task *task, StgTSO *t)
{
    debugTrace (DEBUG_sched,
		"--<< thread %ld (%s) stopped, StackOverflow", 
		(long)t->id, whatNext_strs[t->what_next]);

    /* just adjust the stack for this thread, then pop it back
     * on the run queue.
     */
    { 
	/* enlarge the stack */
	StgTSO *new_t = threadStackOverflow(cap, t);
	
	/* The TSO attached to this Task may have moved, so update the
	 * pointer to it.
	 */
	if (task->tso == t) {
	    task->tso = new_t;
	}
	pushOnRunQueue(cap,new_t);
    }
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadYielding
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleHandleYield( Capability *cap, StgTSO *t, nat prev_what_next )
{
    // Reset the context switch flag.  We don't do this just before
    // running the thread, because that would mean we would lose ticks
    // during GC, which can lead to unfair scheduling (a thread hogs
    // the CPU because the tick always arrives during GC).  This way
    // penalises threads that do a lot of allocation, but that seems
    // better than the alternative.
    context_switch = 0;
    
    /* put the thread back on the run queue.  Then, if we're ready to
     * GC, check whether this is the last task to stop.  If so, wake
     * up the GC thread.  getThread will block during a GC until the
     * GC is finished.
     */
#ifdef DEBUG
    if (t->what_next != prev_what_next) {
	debugTrace(DEBUG_sched,
		   "--<< thread %ld (%s) stopped to switch evaluators", 
		   (long)t->id, whatNext_strs[t->what_next]);
    } else {
	debugTrace(DEBUG_sched,
		   "--<< thread %ld (%s) stopped, yielding",
		   (long)t->id, whatNext_strs[t->what_next]);
    }
#endif
    
    IF_DEBUG(sanity,
	     //debugBelch("&& Doing sanity check on yielding TSO %ld.", t->id);
	     checkTSO(t));
    ASSERT(t->link == END_TSO_QUEUE);
    
    // Shortcut if we're just switching evaluators: don't bother
    // doing stack squeezing (which can be expensive), just run the
    // thread.
    if (t->what_next != prev_what_next) {
	return rtsTrue;
    }
    
#if defined(GRAN)
    ASSERT(!is_on_queue(t,CurrentProc));
      
    IF_DEBUG(sanity,
	     //debugBelch("&& Doing sanity check on all ThreadQueues (and their TSOs).");
	     checkThreadQsSanity(rtsTrue));

#endif

    addToRunQueue(cap,t);

#if defined(GRAN)
    /* add a ContinueThread event to actually process the thread */
    new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
	      ContinueThread,
	      t, (StgClosure*)NULL, (rtsSpark*)NULL);
    IF_GRAN_DEBUG(bq, 
		  debugBelch("GRAN: eventq and runnableq after adding yielded thread to queue again:\n");
		  G_EVENTQ(0);
		  G_CURR_THREADQ(0));
#endif
    return rtsFalse;
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadBlocked
 * -------------------------------------------------------------------------- */

static void
scheduleHandleThreadBlocked( StgTSO *t
#if !defined(GRAN) && !defined(DEBUG)
    STG_UNUSED
#endif
    )
{
#if defined(GRAN)
    IF_DEBUG(scheduler,
	     debugBelch("--<< thread %ld (%p; %s) stopped, blocking on node %p [PE %d] with BQ: \n", 
			t->id, t, whatNext_strs[t->what_next], t->block_info.closure, (t->block_info.closure==(StgClosure*)NULL ? 99 : where_is(t->block_info.closure)));
	     if (t->block_info.closure!=(StgClosure*)NULL) print_bq(t->block_info.closure));
    
    // ??? needed; should emit block before
    IF_DEBUG(gran, 
	     DumpGranEvent(GR_DESCHEDULE, t)); 
    prune_eventq(t, (StgClosure *)NULL); // prune ContinueThreads for t
    /*
      ngoq Dogh!
      ASSERT(procStatus[CurrentProc]==Busy || 
      ((procStatus[CurrentProc]==Fetching) && 
      (t->block_info.closure!=(StgClosure*)NULL)));
      if (run_queue_hds[CurrentProc] == END_TSO_QUEUE &&
      !(!RtsFlags.GranFlags.DoAsyncFetch &&
      procStatus[CurrentProc]==Fetching)) 
      procStatus[CurrentProc] = Idle;
    */
#elif defined(PAR)
    IF_DEBUG(scheduler,
	     debugBelch("--<< thread %ld (%p; %s) stopped, blocking on node %p with BQ: \n", 
			t->id, t, whatNext_strs[t->what_next], t->block_info.closure));
    IF_PAR_DEBUG(bq,
		 
		 if (t->block_info.closure!=(StgClosure*)NULL) 
		 print_bq(t->block_info.closure));
    
    /* Send a fetch (if BlockedOnGA) and dump event to log file */
    blockThread(t);
    
    /* whatever we schedule next, we must log that schedule */
    emitSchedule = rtsTrue;
    
#else /* !GRAN */

      // We don't need to do anything.  The thread is blocked, and it
      // has tidied up its stack and placed itself on whatever queue
      // it needs to be on.

#if !defined(THREADED_RTS)
    ASSERT(t->why_blocked != NotBlocked);
	     // This might not be true under THREADED_RTS: we don't have
	     // exclusive access to this TSO, so someone might have
	     // woken it up by now.  This actually happens: try
	     // conc023 +RTS -N2.
#endif

#ifdef DEBUG
    if (traceClass(DEBUG_sched)) {
	debugTraceBegin("--<< thread %lu (%s) stopped: ", 
			(unsigned long)t->id, whatNext_strs[t->what_next]);
	printThreadBlockage(t);
	debugTraceEnd();
    }
#endif
    
    /* Only for dumping event to log file 
       ToDo: do I need this in GranSim, too?
       blockThread(t);
    */
#endif
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadFinished
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleHandleThreadFinished (Capability *cap STG_UNUSED, Task *task, StgTSO *t)
{
    /* Need to check whether this was a main thread, and if so,
     * return with the return value.
     *
     * We also end up here if the thread kills itself with an
     * uncaught exception, see Exception.cmm.
     */
    debugTrace(DEBUG_sched, "--++ thread %lu (%s) finished", 
	       (unsigned long)t->id, whatNext_strs[t->what_next]);

    /* Inform the Hpc that a thread has finished */
    hs_hpc_event("Thread Finished",t);

#if defined(GRAN)
      endThread(t, CurrentProc); // clean-up the thread
#elif defined(PARALLEL_HASKELL)
      /* For now all are advisory -- HWL */
      //if(t->priority==AdvisoryPriority) ??
      advisory_thread_count--; // JB: Caution with this counter, buggy!
      
# if defined(DIST)
      if(t->dist.priority==RevalPriority)
	FinishReval(t);
# endif
    
# if defined(EDENOLD)
      // the thread could still have an outport... (BUG)
      if (t->eden.outport != -1) {
      // delete the outport for the tso which has finished...
	IF_PAR_DEBUG(eden_ports,
		   debugBelch("WARNING: Scheduler removes outport %d for TSO %d.\n",
			      t->eden.outport, t->id));
	deleteOPT(t);
      }
      // thread still in the process (HEAVY BUG! since outport has just been closed...)
      if (t->eden.epid != -1) {
	IF_PAR_DEBUG(eden_ports,
		   debugBelch("WARNING: Scheduler removes TSO %d from process %d .\n",
			   t->id, t->eden.epid));
	removeTSOfromProcess(t);
      }
# endif 

# if defined(PAR)
      if (RtsFlags.ParFlags.ParStats.Full &&
	  !RtsFlags.ParFlags.ParStats.Suppressed) 
	DumpEndEvent(CURRENT_PROC, t, rtsFalse /* not mandatory */);

      //  t->par only contains statistics: left out for now...
      IF_PAR_DEBUG(fish,
		   debugBelch("**** end thread: ended sparked thread %d (%lx); sparkname: %lx\n",
			      t->id,t,t->par.sparkname));
# endif
#endif // PARALLEL_HASKELL

      //
      // Check whether the thread that just completed was a bound
      // thread, and if so return with the result.  
      //
      // There is an assumption here that all thread completion goes
      // through this point; we need to make sure that if a thread
      // ends up in the ThreadKilled state, that it stays on the run
      // queue so it can be dealt with here.
      //

      if (t->bound) {

	  if (t->bound != task) {
#if !defined(THREADED_RTS)
	      // Must be a bound thread that is not the topmost one.  Leave
	      // it on the run queue until the stack has unwound to the
	      // point where we can deal with this.  Leaving it on the run
	      // queue also ensures that the garbage collector knows about
	      // this thread and its return value (it gets dropped from the
	      // all_threads list so there's no other way to find it).
	      appendToRunQueue(cap,t);
	      return rtsFalse;
#else
	      // this cannot happen in the threaded RTS, because a
	      // bound thread can only be run by the appropriate Task.
	      barf("finished bound thread that isn't mine");
#endif
	  }

	  ASSERT(task->tso == t);

	  if (t->what_next == ThreadComplete) {
	      if (task->ret) {
		  // NOTE: return val is tso->sp[1] (see StgStartup.hc)
		  *(task->ret) = (StgClosure *)task->tso->sp[1]; 
	      }
	      task->stat = Success;
	  } else {
	      if (task->ret) {
		  *(task->ret) = NULL;
	      }
	      if (sched_state >= SCHED_INTERRUPTING) {
		  task->stat = Interrupted;
	      } else {
		  task->stat = Killed;
	      }
	  }
#ifdef DEBUG
	  removeThreadLabel((StgWord)task->tso->id);
#endif
	  return rtsTrue; // tells schedule() to return
      }

      return rtsFalse;
}

/* -----------------------------------------------------------------------------
 * Perform a heap census, if PROFILING
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleDoHeapProfile( rtsBool ready_to_gc STG_UNUSED )
{
#if defined(PROFILING)
    // When we have +RTS -i0 and we're heap profiling, do a census at
    // every GC.  This lets us get repeatable runs for debugging.
    if (performHeapProfile ||
	(RtsFlags.ProfFlags.profileInterval==0 &&
	 RtsFlags.ProfFlags.doHeapProfile && ready_to_gc)) {

	// checking black holes is necessary before GC, otherwise
	// there may be threads that are unreachable except by the
	// blackhole queue, which the GC will consider to be
	// deadlocked.
	scheduleCheckBlackHoles(&MainCapability);

	debugTrace(DEBUG_sched, "garbage collecting before heap census");
	GarbageCollect(rtsTrue);

	debugTrace(DEBUG_sched, "performing heap census");
	heapCensus();

	performHeapProfile = rtsFalse;
	return rtsTrue;  // true <=> we already GC'd
    }
#endif
    return rtsFalse;
}

/* -----------------------------------------------------------------------------
 * Perform a garbage collection if necessary
 * -------------------------------------------------------------------------- */

static Capability *
scheduleDoGC (Capability *cap, Task *task USED_IF_THREADS, rtsBool force_major)
{
    StgTSO *t;
#ifdef THREADED_RTS
    static volatile StgWord waiting_for_gc;
    rtsBool was_waiting;
    nat i;
#endif

#ifdef THREADED_RTS
    // In order to GC, there must be no threads running Haskell code.
    // Therefore, the GC thread needs to hold *all* the capabilities,
    // and release them after the GC has completed.  
    //
    // This seems to be the simplest way: previous attempts involved
    // making all the threads with capabilities give up their
    // capabilities and sleep except for the *last* one, which
    // actually did the GC.  But it's quite hard to arrange for all
    // the other tasks to sleep and stay asleep.
    //
	
    was_waiting = cas(&waiting_for_gc, 0, 1);
    if (was_waiting) {
	do {
	    debugTrace(DEBUG_sched, "someone else is trying to GC...");
	    if (cap) yieldCapability(&cap,task);
	} while (waiting_for_gc);
	return cap;  // NOTE: task->cap might have changed here
    }

    for (i=0; i < n_capabilities; i++) {
	debugTrace(DEBUG_sched, "ready_to_gc, grabbing all the capabilies (%d/%d)", i, n_capabilities);
	if (cap != &capabilities[i]) {
	    Capability *pcap = &capabilities[i];
	    // we better hope this task doesn't get migrated to
	    // another Capability while we're waiting for this one.
	    // It won't, because load balancing happens while we have
	    // all the Capabilities, but even so it's a slightly
	    // unsavoury invariant.
	    task->cap = pcap;
	    context_switch = 1;
	    waitForReturnCapability(&pcap, task);
	    if (pcap != &capabilities[i]) {
		barf("scheduleDoGC: got the wrong capability");
	    }
	}
    }

    waiting_for_gc = rtsFalse;
#endif

    /* Kick any transactions which are invalid back to their
     * atomically frames.  When next scheduled they will try to
     * commit, this commit will fail and they will retry.
     */
    { 
	StgTSO *next;

	for (t = all_threads; t != END_TSO_QUEUE; t = next) {
	    if (t->what_next == ThreadRelocated) {
		next = t->link;
	    } else {
		next = t->global_link;
		
		// This is a good place to check for blocked
		// exceptions.  It might be the case that a thread is
		// blocked on delivering an exception to a thread that
		// is also blocked - we try to ensure that this
		// doesn't happen in throwTo(), but it's too hard (or
		// impossible) to close all the race holes, so we
		// accept that some might get through and deal with
		// them here.  A GC will always happen at some point,
		// even if the system is otherwise deadlocked.
		maybePerformBlockedException (&capabilities[0], t);

		if (t -> trec != NO_TREC && t -> why_blocked == NotBlocked) {
		    if (!stmValidateNestOfTransactions (t -> trec)) {
			debugTrace(DEBUG_sched | DEBUG_stm,
				   "trec %p found wasting its time", t);
			
			// strip the stack back to the
			// ATOMICALLY_FRAME, aborting the (nested)
			// transaction, and saving the stack of any
			// partially-evaluated thunks on the heap.
			throwToSingleThreaded_(&capabilities[0], t, 
					       NULL, rtsTrue, NULL);
			
#ifdef REG_R1
			ASSERT(get_itbl((StgClosure *)t->sp)->type == ATOMICALLY_FRAME);
#endif
		    }
		}
	    }
	}
    }
    
    // so this happens periodically:
    if (cap) scheduleCheckBlackHoles(cap);
    
    IF_DEBUG(scheduler, printAllThreads());

    /*
     * We now have all the capabilities; if we're in an interrupting
     * state, then we should take the opportunity to delete all the
     * threads in the system.
     */
    if (sched_state >= SCHED_INTERRUPTING) {
	deleteAllThreads(&capabilities[0]);
	sched_state = SCHED_SHUTTING_DOWN;
    }

    /* everybody back, start the GC.
     * Could do it in this thread, or signal a condition var
     * to do it in another thread.  Either way, we need to
     * broadcast on gc_pending_cond afterward.
     */
#if defined(THREADED_RTS)
    debugTrace(DEBUG_sched, "doing GC");
#endif
    GarbageCollect(force_major);
    
#if defined(THREADED_RTS)
    // release our stash of capabilities.
    for (i = 0; i < n_capabilities; i++) {
	if (cap != &capabilities[i]) {
	    task->cap = &capabilities[i];
	    releaseCapability(&capabilities[i]);
	}
    }
    if (cap) {
	task->cap = cap;
    } else {
	task->cap = NULL;
    }
#endif

#if defined(GRAN)
    /* add a ContinueThread event to continue execution of current thread */
    new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
	      ContinueThread,
	      t, (StgClosure*)NULL, (rtsSpark*)NULL);
    IF_GRAN_DEBUG(bq, 
		  debugBelch("GRAN: eventq and runnableq after Garbage collection:\n\n");
		  G_EVENTQ(0);
		  G_CURR_THREADQ(0));
#endif /* GRAN */

    return cap;
}

/* ---------------------------------------------------------------------------
 * Singleton fork(). Do not copy any running threads.
 * ------------------------------------------------------------------------- */

StgInt
forkProcess(HsStablePtr *entry
#ifndef FORKPROCESS_PRIMOP_SUPPORTED
	    STG_UNUSED
#endif
           )
{
#ifdef FORKPROCESS_PRIMOP_SUPPORTED
    Task *task;
    pid_t pid;
    StgTSO* t,*next;
    Capability *cap;
    
#if defined(THREADED_RTS)
    if (RtsFlags.ParFlags.nNodes > 1) {
	errorBelch("forking not supported with +RTS -N<n> greater than 1");
	stg_exit(EXIT_FAILURE);
    }
#endif

    debugTrace(DEBUG_sched, "forking!");
    
    // ToDo: for SMP, we should probably acquire *all* the capabilities
    cap = rts_lock();
    
    pid = fork();
    
    if (pid) { // parent
	
	// just return the pid
	rts_unlock(cap);
	return pid;
	
    } else { // child
	
	// Now, all OS threads except the thread that forked are
	// stopped.  We need to stop all Haskell threads, including
	// those involved in foreign calls.  Also we need to delete
	// all Tasks, because they correspond to OS threads that are
	// now gone.

	for (t = all_threads; t != END_TSO_QUEUE; t = next) {
	    if (t->what_next == ThreadRelocated) {
		next = t->link;
	    } else {
		next = t->global_link;
		// don't allow threads to catch the ThreadKilled
		// exception, but we do want to raiseAsync() because these
		// threads may be evaluating thunks that we need later.
		deleteThread_(cap,t);
	    }
	}
	
	// Empty the run queue.  It seems tempting to let all the
	// killed threads stay on the run queue as zombies to be
	// cleaned up later, but some of them correspond to bound
	// threads for which the corresponding Task does not exist.
	cap->run_queue_hd = END_TSO_QUEUE;
	cap->run_queue_tl = END_TSO_QUEUE;

	// Any suspended C-calling Tasks are no more, their OS threads
	// don't exist now:
	cap->suspended_ccalling_tasks = NULL;

	// Empty the all_threads list.  Otherwise, the garbage
	// collector may attempt to resurrect some of these threads.
	all_threads = END_TSO_QUEUE;

	// Wipe the task list, except the current Task.
	ACQUIRE_LOCK(&sched_mutex);
	for (task = all_tasks; task != NULL; task=task->all_link) {
	    if (task != cap->running_task) {
		discardTask(task);
	    }
	}
	RELEASE_LOCK(&sched_mutex);

#if defined(THREADED_RTS)
	// Wipe our spare workers list, they no longer exist.  New
	// workers will be created if necessary.
	cap->spare_workers = NULL;
	cap->returning_tasks_hd = NULL;
	cap->returning_tasks_tl = NULL;
#endif

	cap = rts_evalStableIO(cap, entry, NULL);  // run the action
	rts_checkSchedStatus("forkProcess",cap);
	
	rts_unlock(cap);
	hs_exit();                      // clean up and exit
	stg_exit(EXIT_SUCCESS);
    }
#else /* !FORKPROCESS_PRIMOP_SUPPORTED */
    barf("forkProcess#: primop not supported on this platform, sorry!\n");
    return -1;
#endif
}

/* ---------------------------------------------------------------------------
 * Delete all the threads in the system
 * ------------------------------------------------------------------------- */
   
static void
deleteAllThreads ( Capability *cap )
{
    // NOTE: only safe to call if we own all capabilities.

    StgTSO* t, *next;
    debugTrace(DEBUG_sched,"deleting all threads");
    for (t = all_threads; t != END_TSO_QUEUE; t = next) {
	if (t->what_next == ThreadRelocated) {
	    next = t->link;
	} else {
	    next = t->global_link;
	    deleteThread(cap,t);
	}
    }      

    // The run queue now contains a bunch of ThreadKilled threads.  We
    // must not throw these away: the main thread(s) will be in there
    // somewhere, and the main scheduler loop has to deal with it.
    // Also, the run queue is the only thing keeping these threads from
    // being GC'd, and we don't want the "main thread has been GC'd" panic.

#if !defined(THREADED_RTS)
    ASSERT(blocked_queue_hd == END_TSO_QUEUE);
    ASSERT(sleeping_queue == END_TSO_QUEUE);
#endif
}

/* -----------------------------------------------------------------------------
   Managing the suspended_ccalling_tasks list.
   Locks required: sched_mutex
   -------------------------------------------------------------------------- */

STATIC_INLINE void
suspendTask (Capability *cap, Task *task)
{
    ASSERT(task->next == NULL && task->prev == NULL);
    task->next = cap->suspended_ccalling_tasks;
    task->prev = NULL;
    if (cap->suspended_ccalling_tasks) {
	cap->suspended_ccalling_tasks->prev = task;
    }
    cap->suspended_ccalling_tasks = task;
}

STATIC_INLINE void
recoverSuspendedTask (Capability *cap, Task *task)
{
    if (task->prev) {
	task->prev->next = task->next;
    } else {
	ASSERT(cap->suspended_ccalling_tasks == task);
	cap->suspended_ccalling_tasks = task->next;
    }
    if (task->next) {
	task->next->prev = task->prev;
    }
    task->next = task->prev = NULL;
}

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
   
void *
suspendThread (StgRegTable *reg)
{
  Capability *cap;
  int saved_errno = errno;
  StgTSO *tso;
  Task *task;

  /* assume that *reg is a pointer to the StgRegTable part of a Capability.
   */
  cap = regTableToCapability(reg);

  task = cap->running_task;
  tso = cap->r.rCurrentTSO;

  debugTrace(DEBUG_sched, 
	     "thread %lu did a safe foreign call", 
	     (unsigned long)cap->r.rCurrentTSO->id);

  // XXX this might not be necessary --SDM
  tso->what_next = ThreadRunGHC;

  threadPaused(cap,tso);

  if ((tso->flags & TSO_BLOCKEX) == 0)  {
      tso->why_blocked = BlockedOnCCall;
      tso->flags |= TSO_BLOCKEX;
      tso->flags &= ~TSO_INTERRUPTIBLE;
  } else {
      tso->why_blocked = BlockedOnCCall_NoUnblockExc;
  }

  // Hand back capability
  task->suspended_tso = tso;

  ACQUIRE_LOCK(&cap->lock);

  suspendTask(cap,task);
  cap->in_haskell = rtsFalse;
  releaseCapability_(cap);
  
  RELEASE_LOCK(&cap->lock);

#if defined(THREADED_RTS)
  /* Preparing to leave the RTS, so ensure there's a native thread/task
     waiting to take over.
  */
  debugTrace(DEBUG_sched, "thread %lu: leaving RTS", (unsigned long)tso->id);
#endif

  errno = saved_errno;
  return task;
}

StgRegTable *
resumeThread (void *task_)
{
    StgTSO *tso;
    Capability *cap;
    int saved_errno = errno;
    Task *task = task_;

    cap = task->cap;
    // Wait for permission to re-enter the RTS with the result.
    waitForReturnCapability(&cap,task);
    // we might be on a different capability now... but if so, our
    // entry on the suspended_ccalling_tasks list will also have been
    // migrated.

    // Remove the thread from the suspended list
    recoverSuspendedTask(cap,task);

    tso = task->suspended_tso;
    task->suspended_tso = NULL;
    tso->link = END_TSO_QUEUE;
    debugTrace(DEBUG_sched, "thread %lu: re-entering RTS", (unsigned long)tso->id);
    
    if (tso->why_blocked == BlockedOnCCall) {
	awakenBlockedExceptionQueue(cap,tso);
	tso->flags &= ~(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
    }
    
    /* Reset blocking status */
    tso->why_blocked  = NotBlocked;
    
    cap->r.rCurrentTSO = tso;
    cap->in_haskell = rtsTrue;
    errno = saved_errno;

    /* We might have GC'd, mark the TSO dirty again */
    dirtyTSO(tso);

    IF_DEBUG(sanity, checkTSO(tso));

    return &cap->r;
}

/* ---------------------------------------------------------------------------
 * scheduleThread()
 *
 * scheduleThread puts a thread on the end  of the runnable queue.
 * This will usually be done immediately after a thread is created.
 * The caller of scheduleThread must create the thread using e.g.
 * createThread and push an appropriate closure
 * on this thread's stack before the scheduler is invoked.
 * ------------------------------------------------------------------------ */

void
scheduleThread(Capability *cap, StgTSO *tso)
{
    // The thread goes at the *end* of the run-queue, to avoid possible
    // starvation of any threads already on the queue.
    appendToRunQueue(cap,tso);
}

void
scheduleThreadOn(Capability *cap, StgWord cpu USED_IF_THREADS, StgTSO *tso)
{
#if defined(THREADED_RTS)
    tso->flags |= TSO_LOCKED; // we requested explicit affinity; don't
			      // move this thread from now on.
    cpu %= RtsFlags.ParFlags.nNodes;
    if (cpu == cap->no) {
	appendToRunQueue(cap,tso);
    } else {
	migrateThreadToCapability_lock(&capabilities[cpu],tso);
    }
#else
    appendToRunQueue(cap,tso);
#endif
}

Capability *
scheduleWaitThread (StgTSO* tso, /*[out]*/HaskellObj* ret, Capability *cap)
{
    Task *task;

    // We already created/initialised the Task
    task = cap->running_task;

    // This TSO is now a bound thread; make the Task and TSO
    // point to each other.
    tso->bound = task;
    tso->cap = cap;

    task->tso = tso;
    task->ret = ret;
    task->stat = NoStatus;

    appendToRunQueue(cap,tso);

    debugTrace(DEBUG_sched, "new bound thread (%lu)", (unsigned long)tso->id);

#if defined(GRAN)
    /* GranSim specific init */
    CurrentTSO = m->tso;                // the TSO to run
    procStatus[MainProc] = Busy;        // status of main PE
    CurrentProc = MainProc;             // PE to run it on
#endif

    cap = schedule(cap,task);

    ASSERT(task->stat != NoStatus);
    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    debugTrace(DEBUG_sched, "bound thread (%lu) finished", (unsigned long)task->tso->id);
    return cap;
}

/* ----------------------------------------------------------------------------
 * Starting Tasks
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
void
workerStart(Task *task)
{
    Capability *cap;

    // See startWorkerTask().
    ACQUIRE_LOCK(&task->lock);
    cap = task->cap;
    RELEASE_LOCK(&task->lock);

    // set the thread-local pointer to the Task:
    taskEnter(task);

    // schedule() runs without a lock.
    cap = schedule(cap,task);

    // On exit from schedule(), we have a Capability.
    releaseCapability(cap);
    workerTaskStop(task);
}
#endif

/* ---------------------------------------------------------------------------
 * initScheduler()
 *
 * Initialise the scheduler.  This resets all the queues - if the
 * queues contained any threads, they'll be garbage collected at the
 * next pass.
 *
 * ------------------------------------------------------------------------ */

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
    blackhole_queue[i]    = END_TSO_QUEUE;
    sleeping_queue        = END_TSO_QUEUE;
  }
#elif !defined(THREADED_RTS)
  blocked_queue_hd  = END_TSO_QUEUE;
  blocked_queue_tl  = END_TSO_QUEUE;
  sleeping_queue    = END_TSO_QUEUE;
#endif

  blackhole_queue   = END_TSO_QUEUE;
  all_threads       = END_TSO_QUEUE;

  context_switch = 0;
  sched_state    = SCHED_RUNNING;

#if defined(THREADED_RTS)
  /* Initialise the mutex and condition variables used by
   * the scheduler. */
  initMutex(&sched_mutex);
#endif
  
  ACQUIRE_LOCK(&sched_mutex);

  /* A capability holds the state a native thread needs in
   * order to execute STG code. At least one capability is
   * floating around (only THREADED_RTS builds have more than one).
   */
  initCapabilities();

  initTaskManager();

#if defined(THREADED_RTS) || defined(PARALLEL_HASKELL)
  initSparkPools();
#endif

#if defined(THREADED_RTS)
  /*
   * Eagerly start one worker to run each Capability, except for
   * Capability 0.  The idea is that we're probably going to start a
   * bound thread on Capability 0 pretty soon, so we don't want a
   * worker task hogging it.
   */
  { 
      nat i;
      Capability *cap;
      for (i = 1; i < n_capabilities; i++) {
	  cap = &capabilities[i];
	  ACQUIRE_LOCK(&cap->lock);
	  startWorkerTask(cap, workerStart);
	  RELEASE_LOCK(&cap->lock);
      }
  }
#endif

  trace(TRACE_sched, "start: %d capabilities", n_capabilities);

  RELEASE_LOCK(&sched_mutex);
}

void
exitScheduler( void )
{
    Task *task = NULL;

#if defined(THREADED_RTS)
    ACQUIRE_LOCK(&sched_mutex);
    task = newBoundTask();
    RELEASE_LOCK(&sched_mutex);
#endif

    // If we haven't killed all the threads yet, do it now.
    if (sched_state < SCHED_SHUTTING_DOWN) {
	sched_state = SCHED_INTERRUPTING;
	scheduleDoGC(NULL,task,rtsFalse);    
    }
    sched_state = SCHED_SHUTTING_DOWN;

#if defined(THREADED_RTS)
    { 
	nat i;
	
	for (i = 0; i < n_capabilities; i++) {
	    shutdownCapability(&capabilities[i], task);
	}
	boundTaskExiting(task);
	stopTaskManager();
    }
#else
    freeCapability(&MainCapability);
#endif
}

void
freeScheduler( void )
{
    freeTaskManager();
    if (n_capabilities != 1) {
        stgFree(capabilities);
    }
#if defined(THREADED_RTS)
    closeMutex(&sched_mutex);
#endif
}

/* ---------------------------------------------------------------------------
   Where are the roots that we know about?

        - all the threads on the runnable queue
        - all the threads on the blocked queue
        - all the threads on the sleeping queue
	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   ------------------------------------------------------------------------ */

/* This has to be protected either by the scheduler monitor, or by the
	garbage collection monitor (probably the latter).
	KH @ 25/10/99
*/

void
GetRoots( evac_fn evac )
{
    nat i;
    Capability *cap;
    Task *task;

#if defined(GRAN)
    for (i=0; i<=RtsFlags.GranFlags.proc; i++) {
	if ((run_queue_hds[i] != END_TSO_QUEUE) && ((run_queue_hds[i] != NULL)))
	    evac((StgClosure **)&run_queue_hds[i]);
	if ((run_queue_tls[i] != END_TSO_QUEUE) && ((run_queue_tls[i] != NULL)))
	    evac((StgClosure **)&run_queue_tls[i]);
	
	if ((blocked_queue_hds[i] != END_TSO_QUEUE) && ((blocked_queue_hds[i] != NULL)))
	    evac((StgClosure **)&blocked_queue_hds[i]);
	if ((blocked_queue_tls[i] != END_TSO_QUEUE) && ((blocked_queue_tls[i] != NULL)))
	    evac((StgClosure **)&blocked_queue_tls[i]);
	if ((ccalling_threadss[i] != END_TSO_QUEUE) && ((ccalling_threadss[i] != NULL)))
	    evac((StgClosure **)&ccalling_threads[i]);
    }

    markEventQueue();

#else /* !GRAN */

    for (i = 0; i < n_capabilities; i++) {
	cap = &capabilities[i];
	evac((StgClosure **)(void *)&cap->run_queue_hd);
	evac((StgClosure **)(void *)&cap->run_queue_tl);
#if defined(THREADED_RTS)
	evac((StgClosure **)(void *)&cap->wakeup_queue_hd);
	evac((StgClosure **)(void *)&cap->wakeup_queue_tl);
#endif
	for (task = cap->suspended_ccalling_tasks; task != NULL; 
	     task=task->next) {
	    debugTrace(DEBUG_sched,
		       "evac'ing suspended TSO %lu", (unsigned long)task->suspended_tso->id);
	    evac((StgClosure **)(void *)&task->suspended_tso);
	}

    }
    

#if !defined(THREADED_RTS)
    evac((StgClosure **)(void *)&blocked_queue_hd);
    evac((StgClosure **)(void *)&blocked_queue_tl);
    evac((StgClosure **)(void *)&sleeping_queue);
#endif 
#endif

    // evac((StgClosure **)&blackhole_queue);

#if defined(THREADED_RTS) || defined(PARALLEL_HASKELL) || defined(GRAN)
    markSparkQueue(evac);
#endif
    
#if defined(RTS_USER_SIGNALS)
    // mark the signal handlers (signals should be already blocked)
    markSignalHandlers(evac);
#endif
}

/* -----------------------------------------------------------------------------
   performGC

   This is the interface to the garbage collector from Haskell land.
   We provide this so that external C code can allocate and garbage
   collect when called from Haskell via _ccall_GC.
   -------------------------------------------------------------------------- */

static void
performGC_(rtsBool force_major)
{
    Task *task;
    // We must grab a new Task here, because the existing Task may be
    // associated with a particular Capability, and chained onto the 
    // suspended_ccalling_tasks queue.
    ACQUIRE_LOCK(&sched_mutex);
    task = newBoundTask();
    RELEASE_LOCK(&sched_mutex);
    scheduleDoGC(NULL,task,force_major);
    boundTaskExiting(task);
}

void
performGC(void)
{
    performGC_(rtsFalse);
}

void
performMajorGC(void)
{
    performGC_(rtsTrue);
}

/* -----------------------------------------------------------------------------
   Stack overflow

   If the thread has reached its maximum stack size, then raise the
   StackOverflow exception in the offending thread.  Otherwise
   relocate the TSO into a larger chunk of memory and adjust its stack
   size appropriately.
   -------------------------------------------------------------------------- */

static StgTSO *
threadStackOverflow(Capability *cap, StgTSO *tso)
{
  nat new_stack_size, stack_words;
  lnat new_tso_size;
  StgPtr new_sp;
  StgTSO *dest;

  IF_DEBUG(sanity,checkTSO(tso));

  // don't allow throwTo() to modify the blocked_exceptions queue
  // while we are moving the TSO:
  lockClosure((StgClosure *)tso);

  if (tso->stack_size >= tso->max_stack_size) {

      debugTrace(DEBUG_gc,
		 "threadStackOverflow of TSO %ld (%p): stack too large (now %ld; max is %ld)",
		 (long)tso->id, tso, (long)tso->stack_size, (long)tso->max_stack_size);
      IF_DEBUG(gc,
	       /* If we're debugging, just print out the top of the stack */
	       printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
						tso->sp+64)));

      // Send this thread the StackOverflow exception
      unlockTSO(tso);
      throwToSingleThreaded(cap, tso, (StgClosure *)stackOverflow_closure);
      return tso;
  }

  /* Try to double the current stack size.  If that takes us over the
   * maximum stack size for this thread, then use the maximum instead.
   * Finally round up so the TSO ends up as a whole number of blocks.
   */
  new_stack_size = stg_min(tso->stack_size * 2, tso->max_stack_size);
  new_tso_size   = (lnat)BLOCK_ROUND_UP(new_stack_size * sizeof(W_) + 
				       TSO_STRUCT_SIZE)/sizeof(W_);
  new_tso_size = round_to_mblocks(new_tso_size);  /* Be MBLOCK-friendly */
  new_stack_size = new_tso_size - TSO_STRUCT_SIZEW;

  debugTrace(DEBUG_sched, 
	     "increasing stack size from %ld words to %d.",
	     (long)tso->stack_size, new_stack_size);

  dest = (StgTSO *)allocate(new_tso_size);
  TICK_ALLOC_TSO(new_stack_size,0);

  /* copy the TSO block and the old stack into the new area */
  memcpy(dest,tso,TSO_STRUCT_SIZE);
  stack_words = tso->stack + tso->stack_size - tso->sp;
  new_sp = (P_)dest + new_tso_size - stack_words;
  memcpy(new_sp, tso->sp, stack_words * sizeof(W_));

  /* relocate the stack pointers... */
  dest->sp         = new_sp;
  dest->stack_size = new_stack_size;
	
  /* Mark the old TSO as relocated.  We have to check for relocated
   * TSOs in the garbage collector and any primops that deal with TSOs.
   *
   * It's important to set the sp value to just beyond the end
   * of the stack, so we don't attempt to scavenge any part of the
   * dead TSO's stack.
   */
  tso->what_next = ThreadRelocated;
  tso->link = dest;
  tso->sp = (P_)&(tso->stack[tso->stack_size]);
  tso->why_blocked = NotBlocked;

  IF_PAR_DEBUG(verbose,
	       debugBelch("@@ threadStackOverflow of TSO %d (now at %p): stack size increased to %ld\n",
		     tso->id, tso, tso->stack_size);
	       /* If we're debugging, just print out the top of the stack */
	       printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
						tso->sp+64)));
  
  unlockTSO(dest);
  unlockTSO(tso);

  IF_DEBUG(sanity,checkTSO(dest));
#if 0
  IF_DEBUG(scheduler,printTSO(dest));
#endif

  return dest;
}

/* ---------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   ------------------------------------------------------------------------ */

void
interruptStgRts(void)
{
    sched_state = SCHED_INTERRUPTING;
    context_switch = 1;
    wakeUpRts();
}

/* -----------------------------------------------------------------------------
   Wake up the RTS
   
   This function causes at least one OS thread to wake up and run the
   scheduler loop.  It is invoked when the RTS might be deadlocked, or
   an external event has arrived that may need servicing (eg. a
   keyboard interrupt).

   In the single-threaded RTS we don't do anything here; we only have
   one thread anyway, and the event that caused us to want to wake up
   will have interrupted any blocking system call in progress anyway.
   -------------------------------------------------------------------------- */

void
wakeUpRts(void)
{
#if defined(THREADED_RTS)
    // This forces the IO Manager thread to wakeup, which will
    // in turn ensure that some OS thread wakes up and runs the
    // scheduler loop, which will cause a GC and deadlock check.
    ioManagerWakeup();
#endif
}

/* -----------------------------------------------------------------------------
 * checkBlackHoles()
 *
 * Check the blackhole_queue for threads that can be woken up.  We do
 * this periodically: before every GC, and whenever the run queue is
 * empty.
 *
 * An elegant solution might be to just wake up all the blocked
 * threads with awakenBlockedQueue occasionally: they'll go back to
 * sleep again if the object is still a BLACKHOLE.  Unfortunately this
 * doesn't give us a way to tell whether we've actually managed to
 * wake up any threads, so we would be busy-waiting.
 *
 * -------------------------------------------------------------------------- */

static rtsBool
checkBlackHoles (Capability *cap)
{
    StgTSO **prev, *t;
    rtsBool any_woke_up = rtsFalse;
    StgHalfWord type;

    // blackhole_queue is global:
    ASSERT_LOCK_HELD(&sched_mutex);

    debugTrace(DEBUG_sched, "checking threads blocked on black holes");

    // ASSUMES: sched_mutex
    prev = &blackhole_queue;
    t = blackhole_queue;
    while (t != END_TSO_QUEUE) {
	ASSERT(t->why_blocked == BlockedOnBlackHole);
	type = get_itbl(t->block_info.closure)->type;
	if (type != BLACKHOLE && type != CAF_BLACKHOLE) {
	    IF_DEBUG(sanity,checkTSO(t));
	    t = unblockOne(cap, t);
	    // urk, the threads migrate to the current capability
	    // here, but we'd like to keep them on the original one.
	    *prev = t;
	    any_woke_up = rtsTrue;
	} else {
	    prev = &t->link;
	    t = t->link;
	}
    }

    return any_woke_up;
}

/* -----------------------------------------------------------------------------
   Deleting threads

   This is used for interruption (^C) and forking, and corresponds to
   raising an exception but without letting the thread catch the
   exception.
   -------------------------------------------------------------------------- */

static void 
deleteThread (Capability *cap, StgTSO *tso)
{
    // NOTE: must only be called on a TSO that we have exclusive
    // access to, because we will call throwToSingleThreaded() below.
    // The TSO must be on the run queue of the Capability we own, or 
    // we must own all Capabilities.

    if (tso->why_blocked != BlockedOnCCall &&
	tso->why_blocked != BlockedOnCCall_NoUnblockExc) {
	throwToSingleThreaded(cap,tso,NULL);
    }
}

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void 
deleteThread_(Capability *cap, StgTSO *tso)
{ // for forkProcess only:
  // like deleteThread(), but we delete threads in foreign calls, too.

    if (tso->why_blocked == BlockedOnCCall ||
	tso->why_blocked == BlockedOnCCall_NoUnblockExc) {
	unblockOne(cap,tso);
	tso->what_next = ThreadKilled;
    } else {
	deleteThread(cap,tso);
    }
}
#endif

/* -----------------------------------------------------------------------------
   raiseExceptionHelper
   
   This function is called by the raise# primitve, just so that we can
   move some of the tricky bits of raising an exception from C-- into
   C.  Who knows, it might be a useful re-useable thing here too.
   -------------------------------------------------------------------------- */

StgWord
raiseExceptionHelper (StgRegTable *reg, StgTSO *tso, StgClosure *exception)
{
    Capability *cap = regTableToCapability(reg);
    StgThunk *raise_closure = NULL;
    StgPtr p, next;
    StgRetInfoTable *info;
    //
    // This closure represents the expression 'raise# E' where E
    // is the exception raise.  It is used to overwrite all the
    // thunks which are currently under evaluataion.
    //

    // OLD COMMENT (we don't have MIN_UPD_SIZE now):
    // LDV profiling: stg_raise_info has THUNK as its closure
    // type. Since a THUNK takes at least MIN_UPD_SIZE words in its
    // payload, MIN_UPD_SIZE is more approprate than 1.  It seems that
    // 1 does not cause any problem unless profiling is performed.
    // However, when LDV profiling goes on, we need to linearly scan
    // small object pool, where raise_closure is stored, so we should
    // use MIN_UPD_SIZE.
    //
    // raise_closure = (StgClosure *)RET_STGCALL1(P_,allocate,
    // 				       sizeofW(StgClosure)+1);
    //

    //
    // Walk up the stack, looking for the catch frame.  On the way,
    // we update any closures pointed to from update frames with the
    // raise closure that we just built.
    //
    p = tso->sp;
    while(1) {
	info = get_ret_itbl((StgClosure *)p);
	next = p + stack_frame_sizeW((StgClosure *)p);
	switch (info->i.type) {
	    
	case UPDATE_FRAME:
	    // Only create raise_closure if we need to.
	    if (raise_closure == NULL) {
		raise_closure = 
		    (StgThunk *)allocateLocal(cap,sizeofW(StgThunk)+1);
		SET_HDR(raise_closure, &stg_raise_info, CCCS);
		raise_closure->payload[0] = exception;
	    }
	    UPD_IND(((StgUpdateFrame *)p)->updatee,(StgClosure *)raise_closure);
	    p = next;
	    continue;

        case ATOMICALLY_FRAME:
	    debugTrace(DEBUG_stm, "found ATOMICALLY_FRAME at %p", p);
            tso->sp = p;
            return ATOMICALLY_FRAME;
	    
	case CATCH_FRAME:
	    tso->sp = p;
	    return CATCH_FRAME;

        case CATCH_STM_FRAME:
	    debugTrace(DEBUG_stm, "found CATCH_STM_FRAME at %p", p);
            tso->sp = p;
            return CATCH_STM_FRAME;
	    
	case STOP_FRAME:
	    tso->sp = p;
	    return STOP_FRAME;

        case CATCH_RETRY_FRAME:
	default:
	    p = next; 
	    continue;
	}
    }
}


/* -----------------------------------------------------------------------------
   findRetryFrameHelper

   This function is called by the retry# primitive.  It traverses the stack
   leaving tso->sp referring to the frame which should handle the retry.  

   This should either be a CATCH_RETRY_FRAME (if the retry# is within an orElse#) 
   or should be a ATOMICALLY_FRAME (if the retry# reaches the top level).  

   We skip CATCH_STM_FRAMEs (aborting and rolling back the nested tx that they
   create) because retries are not considered to be exceptions, despite the
   similar implementation.

   We should not expect to see CATCH_FRAME or STOP_FRAME because those should
   not be created within memory transactions.
   -------------------------------------------------------------------------- */

StgWord
findRetryFrameHelper (StgTSO *tso)
{
  StgPtr           p, next;
  StgRetInfoTable *info;

  p = tso -> sp;
  while (1) {
    info = get_ret_itbl((StgClosure *)p);
    next = p + stack_frame_sizeW((StgClosure *)p);
    switch (info->i.type) {
      
    case ATOMICALLY_FRAME:
	debugTrace(DEBUG_stm,
		   "found ATOMICALLY_FRAME at %p during retry", p);
	tso->sp = p;
	return ATOMICALLY_FRAME;
      
    case CATCH_RETRY_FRAME:
	debugTrace(DEBUG_stm,
		   "found CATCH_RETRY_FRAME at %p during retrry", p);
	tso->sp = p;
	return CATCH_RETRY_FRAME;
      
    case CATCH_STM_FRAME: {
        debugTrace(DEBUG_stm,
		   "found CATCH_STM_FRAME at %p during retry", p);
        StgTRecHeader *trec = tso -> trec;
	StgTRecHeader *outer = stmGetEnclosingTRec(trec);
        debugTrace(DEBUG_stm, "trec=%p outer=%p", trec, outer);
	stmAbortTransaction(tso -> cap, trec);
	stmFreeAbortedTRec(tso -> cap, trec);
	tso -> trec = outer;
        p = next; 
        continue;
    }
      

    default:
      ASSERT(info->i.type != CATCH_FRAME);
      ASSERT(info->i.type != STOP_FRAME);
      p = next; 
      continue;
    }
  }
}

/* -----------------------------------------------------------------------------
   resurrectThreads is called after garbage collection on the list of
   threads found to be garbage.  Each of these threads will be woken
   up and sent a signal: BlockedOnDeadMVar if the thread was blocked
   on an MVar, or NonTermination if the thread was blocked on a Black
   Hole.

   Locks: assumes we hold *all* the capabilities.
   -------------------------------------------------------------------------- */

void
resurrectThreads (StgTSO *threads)
{
    StgTSO *tso, *next;
    Capability *cap;

    for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
	next = tso->global_link;
	tso->global_link = all_threads;
	all_threads = tso;
	debugTrace(DEBUG_sched, "resurrecting thread %lu", (unsigned long)tso->id);
	
	// Wake up the thread on the Capability it was last on
	cap = tso->cap;
	
	switch (tso->why_blocked) {
	case BlockedOnMVar:
	case BlockedOnException:
	    /* Called by GC - sched_mutex lock is currently held. */
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)BlockedOnDeadMVar_closure);
	    break;
	case BlockedOnBlackHole:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)NonTermination_closure);
	    break;
	case BlockedOnSTM:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)BlockedIndefinitely_closure);
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
