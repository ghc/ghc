/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * The scheduler and thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#define KEEP_LOCKCLOSURE
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
#include "Proftimer.h"
#include "ProfHeap.h"

/* PARALLEL_HASKELL includes go here */

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

/* The blackhole_queue should be checked for threads to wake up.  See
 * Schedule.h for more thorough comment.
 * LOCK: none (doesn't matter if we miss an update)
 */
rtsBool blackholes_need_checking = rtsFalse;

/* flag that tracks whether we have done any execution in this time slice.
 * LOCK: currently none, perhaps we should lock (but needs to be
 * updated in the fast path of the scheduler).
 */
nat recent_activity = ACTIVITY_YES;

/* if this flag is set as well, give up execution
 * LOCK: none (changes once, from false->true)
 */
rtsBool sched_state = SCHED_RUNNING;

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
#if defined(PARALLEL_HASKELL)
static rtsBool scheduleGetRemoteWork(Capability *cap);
static void scheduleSendPendingMessages(void);
static void scheduleActivateSpark(Capability *cap);
#endif
static void schedulePostRunThread(Capability *cap, StgTSO *t);
static rtsBool scheduleHandleHeapOverflow( Capability *cap, StgTSO *t );
static void scheduleHandleStackOverflow( Capability *cap, Task *task, 
					 StgTSO *t);
static rtsBool scheduleHandleYield( Capability *cap, StgTSO *t, 
				    nat prev_what_next );
static void scheduleHandleThreadBlocked( StgTSO *t );
static rtsBool scheduleHandleThreadFinished( Capability *cap, Task *task,
					     StgTSO *t );
static rtsBool scheduleNeedHeapProfile(rtsBool ready_to_gc);
static Capability *scheduleDoGC(Capability *cap, Task *task,
				rtsBool force_major);

static rtsBool checkBlackHoles(Capability *cap);

static StgTSO *threadStackOverflow(Capability *cap, StgTSO *tso);
static StgTSO *threadStackUnderflow(Task *task, StgTSO *tso);

static void deleteThread (Capability *cap, StgTSO *tso);
static void deleteAllThreads (Capability *cap);

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void deleteThread_(Capability *cap, StgTSO *tso);
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
  This version has been entirely removed (JB 2008/08).

   GUM version:
     GUM iterates over incoming messages.
     It starts with nothing to do (thus CurrentTSO == END_TSO_QUEUE),
     and sends out a fish whenever it has nothing to do; in-between
     doing the actual reductions (shared code below) it processes the
     incoming messages and deals with delayed operations 
     (see PendingFetches).
     This is not the ugliest code you could imagine, but it's bloody close.

  (JB 2008/08) This version was formerly indicated by a PP-Flag PAR,
  now by PP-flag PARALLEL_HASKELL. The Eden RTS (in GHC-6.x) uses it,
  as well as future GUM versions. This file has been refurbished to
  only contain valid code, which is however incomplete, refers to
  invalid includes etc.

   ------------------------------------------------------------------------ */

static Capability *
schedule (Capability *initialCapability, Task *task)
{
  StgTSO *t;
  Capability *cap;
  StgThreadReturnCode ret;
#if defined(PARALLEL_HASKELL)
  rtsBool receivedFinish = rtsFalse;
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
#else
#define TERMINATION_CONDITION        rtsTrue
#endif

  while (TERMINATION_CONDITION) {

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

#if defined(PARALLEL_HASKELL)
    /* message processing and work distribution goes here */ 

    /* if messages have been buffered... a NOOP in THREADED_RTS */
    scheduleSendPendingMessages();

    /* If the run queue is empty,...*/
    if (emptyRunQueue(cap)) {
      /* ...take one of our own sparks and turn it into a thread */
      scheduleActivateSpark(cap);

	/* if this did not work, try to steal a spark from someone else */
      if (emptyRunQueue(cap)) {
	receivedFinish = scheduleGetRemoteWork(cap);
	continue; //  a new round, (hopefully) with new work
	/* 
	   in GUM, this a) sends out a FISH and returns IF no fish is
	                   out already
			b) (blocking) awaits and receives messages
	   
	   in Eden, this is only the blocking receive, as b) in GUM.
	*/
      }
    } 

    /* since we perform a blocking receive and continue otherwise,
       either we never reach here or we definitely have work! */
    // from here: non-empty run queue
    ASSERT(!emptyRunQueue(cap));

    if (PacketsWaiting()) {  /* now process incoming messages, if any
				pending...  

				CAUTION: scheduleGetRemoteWork called
				above, waits for messages as well! */
      processMessages(cap, &receivedFinish);
    }
#endif // PARALLEL_HASKELL

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

    // 
    // Get a thread to run
    //
    t = popRunQueue(cap);

    // Sanity check the thread we're about to run.  This can be
    // expensive if there is lots of thread switching going on...
    IF_DEBUG(sanity,checkTSO(t));

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

    /* context switches are initiated by the timer signal, unless
     * the user specified "context switch as often as possible", with
     * +RTS -C0
     */
    if (RtsFlags.ConcFlags.ctxtSwitchTicks == 0
	&& !emptyThreadQueues(cap)) {
	cap->context_switch = 1;
    }
	 
run_thread:

    // CurrentTSO is the thread to run.  t might be different if we
    // loop back to run_thread, so make sure to set CurrentTSO after
    // that.
    cap->r.rCurrentTSO = t;

    debugTrace(DEBUG_sched, "-->> running thread %ld %s ...", 
			      (long)t->id, whatNext_strs[t->what_next]);

    startHeapProfTimer();

    // Check for exceptions blocked on this thread
    maybePerformBlockedException (cap, t);

    // ----------------------------------------------------------------------
    // Run the current thread 

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);

    prev_what_next = t->what_next;

    errno = t->saved_errno;
#if mingw32_HOST_OS
    SetLastError(t->saved_winerror);
#endif

    cap->in_haskell = rtsTrue;

    dirty_TSO(cap,t);

#if defined(THREADED_RTS)
    if (recent_activity == ACTIVITY_DONE_GC) {
        // ACTIVITY_DONE_GC means we turned off the timer signal to
        // conserve power (see #1623).  Re-enable it here.
        nat prev;
        prev = xchg((P_)&recent_activity, ACTIVITY_YES);
        if (prev == ACTIVITY_DONE_GC) {
            startTimer();
        }
    } else {
        recent_activity = ACTIVITY_YES;
    }
#endif

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
#if mingw32_HOST_OS
    // Similarly for Windows error code
    t->saved_winerror = GetLastError();
#endif

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
    stopHeapProfTimer();
#if defined(PROFILING)
    CCCS = CCS_SYSTEM;
#endif
    
    schedulePostRunThread(cap,t);

    t = threadStackUnderflow(task,t);

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

    if (ready_to_gc || scheduleNeedHeapProfile(ready_to_gc)) {
      cap = scheduleDoGC(cap,task,rtsFalse);
    }
  } /* end of while() */
}

/* ----------------------------------------------------------------------------
 * Setting up the scheduler loop
 * ------------------------------------------------------------------------- */

static void
schedulePreLoop(void)
{
  // initialisation for scheduler - what cannot go into initScheduler()  
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
    if ((emptyRunQueue(cap) || cap->run_queue_hd->_link == END_TSO_QUEUE)
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
	    t = prev->_link;
	    prev->_link = END_TSO_QUEUE;
	    for (; t != END_TSO_QUEUE; t = next) {
		next = t->_link;
		t->_link = END_TSO_QUEUE;
		if (t->what_next == ThreadRelocated
		    || t->bound == task // don't move my bound thread
		    || tsoLocked(t)) {  // don't move a locked thread
		    setTSOLink(cap, prev, t);
		    prev = t;
		} else if (i == n_free_caps) {
		    pushed_to_all = rtsTrue;
		    i = 0;
		    // keep one for us
		    setTSOLink(cap, prev, t);
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
    if (RtsFlags.MiscFlags.install_signal_handlers && signals_pending()) {
        // safe outside the lock
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
	    setTSOLink(cap, cap->run_queue_tl, cap->wakeup_queue_hd);
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
        // disable timer signals (see #1623)
        stopTimer();
	
	if ( !emptyRunQueue(cap) ) return;

#if defined(RTS_USER_SIGNALS) && !defined(THREADED_RTS)
	/* If we have user-installed signal handlers, then wait
	 * for signals to arrive rather then bombing out with a
	 * deadlock.
	 */
	if ( RtsFlags.MiscFlags.install_signal_handlers && anyUserHandlers() ) {
	    debugTrace(DEBUG_sched,
		       "still deadlocked, waiting for signals...");

	    awaitUserSignals();

	    if (signals_pending()) {
		startSignalHandlers(cap);
	    }

	    // either we have threads to run, or we were interrupted:
	    ASSERT(!emptyRunQueue(cap) || sched_state >= SCHED_INTERRUPTING);

            return;
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
				      (StgClosure *)nonTermination_closure);
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
 * Send pending messages (PARALLEL_HASKELL only)
 * ------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL)
static StgTSO *
scheduleSendPendingMessages(void)
{

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
scheduleActivateSpark(Capability *cap)
{
    StgClosure *spark;

/* We only want to stay here if the run queue is empty and we want some
   work. We try to turn a spark into a thread, and add it to the run
   queue, from where it will be picked up in the next iteration of the
   scheduler loop.  
*/
    if (!emptyRunQueue(cap)) 
      /* In the threaded RTS, another task might have pushed a thread
	 on our run queue in the meantime ? But would need a lock.. */
      return;

    spark = findSpark(cap); // defined in Sparks.c

    if (spark != NULL) {
      debugTrace(DEBUG_sched,
		 "turning spark of closure %p into a thread",
		 (StgClosure *)spark);
      createSparkThread(cap,spark); // defined in Sparks.c
    }
}
#endif // PARALLEL_HASKELL

/* ----------------------------------------------------------------------------
 * Get work from a remote node (PARALLEL_HASKELL only)
 * ------------------------------------------------------------------------- */
    
#if defined(PARALLEL_HASKELL)
static rtsBool
scheduleGetRemoteWork(Capability *cap)
{
#if defined(PARALLEL_HASKELL)
  rtsBool receivedFinish = rtsFalse;

  // idle() , i.e. send all buffers, wait for work
  if (RtsFlags.ParFlags.BufferTime) {
	IF_PAR_DEBUG(verbose, 
	        debugBelch("...send all pending data,"));
        {
	  nat i;
	  for (i=1; i<=nPEs; i++)
	    sendImmediately(i); // send all messages away immediately
	}
  }

  /* this would be the place for fishing in GUM... 

     if (no-earlier-fish-around) 
          sendFish(choosePe());
   */

  // Eden:just look for incoming messages (blocking receive)
  IF_PAR_DEBUG(verbose, 
	       debugBelch("...wait for incoming messages...\n"));
  processMessages(cap, &receivedFinish); // blocking receive...


  return receivedFinish;
  // reenter scheduling look after having received something

#else /* !PARALLEL_HASKELL, i.e. THREADED_RTS */

  return rtsFalse; /* return value unused in THREADED_RTS */

#endif /* PARALLEL_HASKELL */
}
#endif // PARALLEL_HASKELL

/* ----------------------------------------------------------------------------
 * After running a thread...
 * ------------------------------------------------------------------------- */

static void
schedulePostRunThread (Capability *cap, StgTSO *t)
{
    // We have to be able to catch transactions that are in an
    // infinite loop as a result of seeing an inconsistent view of
    // memory, e.g. 
    //
    //   atomically $ do
    //       [a,b] <- mapM readTVar [ta,tb]
    //       when (a == b) loop
    //
    // and a is never equal to b given a consistent view of memory.
    //
    if (t -> trec != NO_TREC && t -> why_blocked == NotBlocked) {
        if (!stmValidateNestOfTransactions (t -> trec)) {
            debugTrace(DEBUG_sched | DEBUG_stm,
                       "trec %p found wasting its time", t);
            
            // strip the stack back to the
            // ATOMICALLY_FRAME, aborting the (nested)
            // transaction, and saving the stack of any
            // partially-evaluated thunks on the heap.
            throwToSingleThreaded_(cap, t, NULL, rtsTrue, NULL);
            
            ASSERT(get_itbl((StgClosure *)t->sp)->type == ATOMICALLY_FRAME);
        }
    }

  /* some statistics gathering in the parallel case */
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
	       "--<< thread %ld (%s) stopped: HeapOverflow",
	       (long)t->id, whatNext_strs[t->what_next]);

    if (cap->context_switch) {
        // Sometimes we miss a context switch, e.g. when calling
        // primitives in a tight loop, MAYBE_GC() doesn't check the
        // context switch flag, and we end up waiting for a GC.
        // See #1984, and concurrent/should_run/1984
        cap->context_switch = 0;
        addToRunQueue(cap,t);
    } else {
        pushOnRunQueue(cap,t);
    }
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
    cap->context_switch = 0;
    
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
    ASSERT(t->_link == END_TSO_QUEUE);
    
    // Shortcut if we're just switching evaluators: don't bother
    // doing stack squeezing (which can be expensive), just run the
    // thread.
    if (t->what_next != prev_what_next) {
	return rtsTrue;
    }

    addToRunQueue(cap,t);

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

      // We don't need to do anything.  The thread is blocked, and it
      // has tidied up its stack and placed itself on whatever queue
      // it needs to be on.

    // ASSERT(t->why_blocked != NotBlocked);
    // Not true: for example,
    //    - in THREADED_RTS, the thread may already have been woken
    //      up by another Capability.  This actually happens: try
    //      conc023 +RTS -N2.
    //    - the thread may have woken itself up already, because
    //      threadPaused() might have raised a blocked throwTo
    //      exception, see maybePerformBlockedException().

#ifdef DEBUG
    if (traceClass(DEBUG_sched)) {
	debugTraceBegin("--<< thread %lu (%s) stopped: ", 
			(unsigned long)t->id, whatNext_strs[t->what_next]);
	printThreadBlockage(t);
	debugTraceEnd();
    }
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
	      // step->threads list so there's no other way to find it).
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
 * Perform a heap census
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleNeedHeapProfile( rtsBool ready_to_gc STG_UNUSED )
{
    // When we have +RTS -i0 and we're heap profiling, do a census at
    // every GC.  This lets us get repeatable runs for debugging.
    if (performHeapProfile ||
	(RtsFlags.ProfFlags.profileInterval==0 &&
	 RtsFlags.ProfFlags.doHeapProfile && ready_to_gc)) {
        return rtsTrue;
    } else {
        return rtsFalse;
    }
}

/* -----------------------------------------------------------------------------
 * Perform a garbage collection if necessary
 * -------------------------------------------------------------------------- */

static Capability *
scheduleDoGC (Capability *cap, Task *task USED_IF_THREADS, rtsBool force_major)
{
    rtsBool heap_census;
#ifdef THREADED_RTS
    /* extern static volatile StgWord waiting_for_gc; 
       lives inside capability.c */
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
	
    /*  Other capabilities are prevented from running yet more Haskell
	threads if waiting_for_gc is set. Tested inside
	yieldCapability() and releaseCapability() in Capability.c */

    was_waiting = cas(&waiting_for_gc, 0, 1);
    if (was_waiting) {
	do {
	    debugTrace(DEBUG_sched, "someone else is trying to GC...");
	    if (cap) yieldCapability(&cap,task);
	} while (waiting_for_gc);
	return cap;  // NOTE: task->cap might have changed here
    }

    setContextSwitches();
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
	    waitForReturnCapability(&pcap, task);
	    if (pcap != &capabilities[i]) {
		barf("scheduleDoGC: got the wrong capability");
	    }
	}
    }

    waiting_for_gc = rtsFalse;
#endif

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
    
    heap_census = scheduleNeedHeapProfile(rtsTrue);

    /* everybody back, start the GC.
     * Could do it in this thread, or signal a condition var
     * to do it in another thread.  Either way, we need to
     * broadcast on gc_pending_cond afterward.
     */
#if defined(THREADED_RTS)
    debugTrace(DEBUG_sched, "doing GC");
#endif
    GarbageCollect(force_major || heap_census);
    
    if (heap_census) {
        debugTrace(DEBUG_sched, "performing heap census");
        heapCensus();
	performHeapProfile = rtsFalse;
    }

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

    return cap;
}

/* ---------------------------------------------------------------------------
 * Singleton fork(). Do not copy any running threads.
 * ------------------------------------------------------------------------- */

pid_t
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
    nat s;
    
#if defined(THREADED_RTS)
    if (RtsFlags.ParFlags.nNodes > 1) {
	errorBelch("forking not supported with +RTS -N<n> greater than 1");
	stg_exit(EXIT_FAILURE);
    }
#endif

    debugTrace(DEBUG_sched, "forking!");
    
    // ToDo: for SMP, we should probably acquire *all* the capabilities
    cap = rts_lock();
    
    // no funny business: hold locks while we fork, otherwise if some
    // other thread is holding a lock when the fork happens, the data
    // structure protected by the lock will forever be in an
    // inconsistent state in the child.  See also #1391.
    ACQUIRE_LOCK(&sched_mutex);
    ACQUIRE_LOCK(&cap->lock);
    ACQUIRE_LOCK(&cap->running_task->lock);

    pid = fork();
    
    if (pid) { // parent
	
        RELEASE_LOCK(&sched_mutex);
        RELEASE_LOCK(&cap->lock);
        RELEASE_LOCK(&cap->running_task->lock);

	// just return the pid
	rts_unlock(cap);
	return pid;
	
    } else { // child
	
#if defined(THREADED_RTS)
        initMutex(&sched_mutex);
        initMutex(&cap->lock);
        initMutex(&cap->running_task->lock);
#endif

	// Now, all OS threads except the thread that forked are
	// stopped.  We need to stop all Haskell threads, including
	// those involved in foreign calls.  Also we need to delete
	// all Tasks, because they correspond to OS threads that are
	// now gone.

        for (s = 0; s < total_steps; s++) {
          for (t = all_steps[s].threads; t != END_TSO_QUEUE; t = next) {
	    if (t->what_next == ThreadRelocated) {
		next = t->_link;
	    } else {
		next = t->global_link;
		// don't allow threads to catch the ThreadKilled
		// exception, but we do want to raiseAsync() because these
		// threads may be evaluating thunks that we need later.
		deleteThread_(cap,t);
	    }
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

	// Empty the threads lists.  Otherwise, the garbage
	// collector may attempt to resurrect some of these threads.
        for (s = 0; s < total_steps; s++) {
            all_steps[s].threads = END_TSO_QUEUE;
        }

	// Wipe the task list, except the current Task.
	ACQUIRE_LOCK(&sched_mutex);
	for (task = all_tasks; task != NULL; task=task->all_link) {
	    if (task != cap->running_task) {
#if defined(THREADED_RTS)
                initMutex(&task->lock); // see #1391
#endif
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

        // On Unix, all timers are reset in the child, so we need to start
        // the timer again.
        initTimer();
        startTimer();

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
    nat s;

    debugTrace(DEBUG_sched,"deleting all threads");
    for (s = 0; s < total_steps; s++) {
      for (t = all_steps[s].threads; t != END_TSO_QUEUE; t = next) {
	if (t->what_next == ThreadRelocated) {
	    next = t->_link;
	} else {
	    next = t->global_link;
	    deleteThread(cap,t);
	}
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
  int saved_errno;
  StgTSO *tso;
  Task *task;
#if mingw32_HOST_OS
  StgWord32 saved_winerror;
#endif

  saved_errno = errno;
#if mingw32_HOST_OS
  saved_winerror = GetLastError();
#endif

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
#if mingw32_HOST_OS
  SetLastError(saved_winerror);
#endif
  return task;
}

StgRegTable *
resumeThread (void *task_)
{
    StgTSO *tso;
    Capability *cap;
    Task *task = task_;
    int saved_errno;
#if mingw32_HOST_OS
    StgWord32 saved_winerror;
#endif

    saved_errno = errno;
#if mingw32_HOST_OS
    saved_winerror = GetLastError();
#endif

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
    tso->_link = END_TSO_QUEUE; // no write barrier reqd
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
#if mingw32_HOST_OS
    SetLastError(saved_winerror);
#endif

    /* We might have GC'd, mark the TSO dirty again */
    dirty_TSO(cap,tso);

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
	wakeupThreadOnCapability(cap, &capabilities[cpu], tso);
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
void OSThreadProcAttr
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
#if !defined(THREADED_RTS)
  blocked_queue_hd  = END_TSO_QUEUE;
  blocked_queue_tl  = END_TSO_QUEUE;
  sleeping_queue    = END_TSO_QUEUE;
#endif

  blackhole_queue   = END_TSO_QUEUE;

  sched_state    = SCHED_RUNNING;
  recent_activity = ACTIVITY_YES;

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
exitScheduler(
    rtsBool wait_foreign
#if !defined(THREADED_RTS)
                         __attribute__((unused))
#endif
)
               /* see Capability.c, shutdownCapability() */
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
	    shutdownCapability(&capabilities[i], task, wait_foreign);
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

  if (tso->stack_size >= tso->max_stack_size && !(tso->flags & TSO_BLOCKEX)) {
      // NB. never raise a StackOverflow exception if the thread is
      // inside Control.Exceptino.block.  It is impractical to protect
      // against stack overflow exceptions, since virtually anything
      // can raise one (even 'catch'), so this is the only sensible
      // thing to do here.  See bug #767.

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
   * maximum stack size for this thread, then use the maximum instead
   * (that is, unless we're already at or over the max size and we
   * can't raise the StackOverflow exception (see above), in which
   * case just double the size). Finally round up so the TSO ends up as
   * a whole number of blocks.
   */
  if (tso->stack_size >= tso->max_stack_size) {
      new_stack_size = tso->stack_size * 2;
  } else { 
      new_stack_size = stg_min(tso->stack_size * 2, tso->max_stack_size);
  }
  new_tso_size   = (lnat)BLOCK_ROUND_UP(new_stack_size * sizeof(W_) + 
				       TSO_STRUCT_SIZE)/sizeof(W_);
  new_tso_size = round_to_mblocks(new_tso_size);  /* Be MBLOCK-friendly */
  new_stack_size = new_tso_size - TSO_STRUCT_SIZEW;

  debugTrace(DEBUG_sched, 
	     "increasing stack size from %ld words to %d.",
	     (long)tso->stack_size, new_stack_size);

  dest = (StgTSO *)allocateLocal(cap,new_tso_size);
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
  setTSOLink(cap,tso,dest);
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

static StgTSO *
threadStackUnderflow (Task *task STG_UNUSED, StgTSO *tso)
{
    bdescr *bd, *new_bd;
    lnat free_w, tso_size_w;
    StgTSO *new_tso;

    tso_size_w = tso_sizeW(tso);

    if (tso_size_w < MBLOCK_SIZE_W || 
        (nat)(tso->stack + tso->stack_size - tso->sp) > tso->stack_size / 4) 
    {
        return tso;
    }

    // don't allow throwTo() to modify the blocked_exceptions queue
    // while we are moving the TSO:
    lockClosure((StgClosure *)tso);

    // this is the number of words we'll free
    free_w = round_to_mblocks(tso_size_w/2);

    bd = Bdescr((StgPtr)tso);
    new_bd = splitLargeBlock(bd, free_w / BLOCK_SIZE_W);
    bd->free = bd->start + TSO_STRUCT_SIZEW;

    new_tso = (StgTSO *)new_bd->start;
    memcpy(new_tso,tso,TSO_STRUCT_SIZE);
    new_tso->stack_size = new_bd->free - new_tso->stack;

    debugTrace(DEBUG_sched, "thread %ld: reducing TSO size from %lu words to %lu",
               (long)tso->id, tso_size_w, tso_sizeW(new_tso));

    tso->what_next = ThreadRelocated;
    tso->_link = new_tso; // no write barrier reqd: same generation

    // The TSO attached to this Task may have moved, so update the
    // pointer to it.
    if (task->tso == tso) {
        task->tso = new_tso;
    }

    unlockTSO(new_tso);
    unlockTSO(tso);

    IF_DEBUG(sanity,checkTSO(new_tso));

    return new_tso;
}

/* ---------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   ------------------------------------------------------------------------ */

void
interruptStgRts(void)
{
    sched_state = SCHED_INTERRUPTING;
    setContextSwitches();
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
	type = get_itbl(UNTAG_CLOSURE(t->block_info.closure))->type;
	if (type != BLACKHOLE && type != CAF_BLACKHOLE) {
	    IF_DEBUG(sanity,checkTSO(t));
	    t = unblockOne(cap, t);
	    *prev = t;
	    any_woke_up = rtsTrue;
	} else {
	    prev = &t->_link;
	    t = t->_link;
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
        StgTRecHeader *trec = tso -> trec;
	StgTRecHeader *outer = stmGetEnclosingTRec(trec);
        debugTrace(DEBUG_stm,
		   "found CATCH_STM_FRAME at %p during retry", p);
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
    step *step;

    for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
	next = tso->global_link;

        step = Bdescr((P_)tso)->step;
	tso->global_link = step->threads;
	step->threads = tso;

	debugTrace(DEBUG_sched, "resurrecting thread %lu", (unsigned long)tso->id);
	
	// Wake up the thread on the Capability it was last on
	cap = tso->cap;
	
	switch (tso->why_blocked) {
	case BlockedOnMVar:
	case BlockedOnException:
	    /* Called by GC - sched_mutex lock is currently held. */
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)blockedOnDeadMVar_closure);
	    break;
	case BlockedOnBlackHole:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)nonTermination_closure);
	    break;
	case BlockedOnSTM:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)blockedIndefinitely_closure);
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

/* -----------------------------------------------------------------------------
   performPendingThrowTos is called after garbage collection, and
   passed a list of threads that were found to have pending throwTos
   (tso->blocked_exceptions was not empty), and were blocked.
   Normally this doesn't happen, because we would deliver the
   exception directly if the target thread is blocked, but there are
   small windows where it might occur on a multiprocessor (see
   throwTo()).

   NB. we must be holding all the capabilities at this point, just
   like resurrectThreads().
   -------------------------------------------------------------------------- */

void
performPendingThrowTos (StgTSO *threads)
{
    StgTSO *tso, *next;
    Capability *cap;
    step *step;

    for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
	next = tso->global_link;

        step = Bdescr((P_)tso)->step;
	tso->global_link = step->threads;
	step->threads = tso;

	debugTrace(DEBUG_sched, "performing blocked throwTo to thread %lu", (unsigned long)tso->id);
	
	cap = tso->cap;
        maybePerformBlockedException(cap, tso);
    }
}
