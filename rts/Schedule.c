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

#include "sm/Storage.h"
#include "RtsUtils.h"
#include "StgRun.h"
#include "Schedule.h"
#include "Interpreter.h"
#include "Printer.h"
#include "RtsSignals.h"
#include "sm/Sanity.h"
#include "Stats.h"
#include "STM.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#include "Updates.h"
#include "Proftimer.h"
#include "ProfHeap.h"
#include "Weak.h"
#include "sm/GC.h" // waitForGcThreads, releaseGCThreads, N
#include "sm/GCThread.h"
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
#include "Timer.h"
#include "ThreadPaused.h"
#include "Messages.h"
#include "Stable.h"

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

#ifdef TRACING
#include "eventlog/EventLog.h"
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

/* Set to true when the latest garbage collection failed to reclaim
 * enough space, and the runtime should proceed to shut itself down in
 * an orderly fashion (emitting profiling info etc.)
 */
rtsBool heap_overflow = rtsFalse;

/* flag that tracks whether we have done any execution in this time slice.
 * LOCK: currently none, perhaps we should lock (but needs to be
 * updated in the fast path of the scheduler).
 *
 * NB. must be StgWord, we do xchg() on it.
 */
volatile StgWord recent_activity = ACTIVITY_YES;

/* if this flag is set as well, give up execution
 * LOCK: none (changes monotonically)
 */
volatile StgWord sched_state = SCHED_RUNNING;

/*  This is used in `TSO.h' and gcc 2.96 insists that this variable actually 
 *  exists - earlier gccs apparently didn't.
 *  -= chak
 */
StgTSO dummy_tso;

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

// Local stats
#ifdef THREADED_RTS
static nat n_failed_trygrab_idles = 0, n_idle_caps = 0;
#endif

/* -----------------------------------------------------------------------------
 * static function prototypes
 * -------------------------------------------------------------------------- */

static Capability *schedule (Capability *initialCapability, Task *task);

//
// These functions all encapsulate parts of the scheduler loop, and are
// abstracted only to make the structure and control flow of the
// scheduler clearer.
//
static void schedulePreLoop (void);
static void scheduleFindWork (Capability **pcap);
#if defined(THREADED_RTS)
static void scheduleYield (Capability **pcap, Task *task);
#endif
#if defined(THREADED_RTS)
static nat requestSync (Capability **pcap, Task *task, nat sync_type);
static void acquireAllCapabilities(Capability *cap, Task *task);
static void releaseAllCapabilities(nat n, Capability *cap, Task *task);
static void startWorkerTasks (nat from USED_IF_THREADS, nat to USED_IF_THREADS);
#endif
static void scheduleStartSignalHandlers (Capability *cap);
static void scheduleCheckBlockedThreads (Capability *cap);
static void scheduleProcessInbox(Capability **cap);
static void scheduleDetectDeadlock (Capability **pcap, Task *task);
static void schedulePushWork(Capability *cap, Task *task);
#if defined(THREADED_RTS)
static void scheduleActivateSpark(Capability *cap);
#endif
static void schedulePostRunThread(Capability *cap, StgTSO *t);
static rtsBool scheduleHandleHeapOverflow( Capability *cap, StgTSO *t );
static rtsBool scheduleHandleYield( Capability *cap, StgTSO *t,
				    nat prev_what_next );
static void scheduleHandleThreadBlocked( StgTSO *t );
static rtsBool scheduleHandleThreadFinished( Capability *cap, Task *task,
					     StgTSO *t );
static rtsBool scheduleNeedHeapProfile(rtsBool ready_to_gc);
static void scheduleDoGC(Capability **pcap, Task *task, rtsBool force_major);

static void deleteThread (Capability *cap, StgTSO *tso);
static void deleteAllThreads (Capability *cap);

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void deleteThread_(Capability *cap, StgTSO *tso);
#endif

/* ---------------------------------------------------------------------------
   Main scheduling loop.

   We use round-robin scheduling, each thread returning to the
   scheduler loop when one of these conditions is detected:

      * out of heap space
      * timer expires (thread yields)
      * thread blocks
      * thread ends
      * stack overflow

   ------------------------------------------------------------------------ */

static Capability *
schedule (Capability *initialCapability, Task *task)
{
  StgTSO *t;
  Capability *cap;
  StgThreadReturnCode ret;
  nat prev_what_next;
  rtsBool ready_to_gc;
#if defined(THREADED_RTS)
  rtsBool first = rtsTrue;
#endif
  
  cap = initialCapability;

  // Pre-condition: this task owns initialCapability.
  // The sched_mutex is *NOT* held
  // NB. on return, we still hold a capability.

  debugTrace (DEBUG_sched, "cap %d: schedule()", initialCapability->no);

  schedulePreLoop();

  // -----------------------------------------------------------
  // Scheduler loop starts here:

  while (1) {

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
        /* scheduleDoGC() deletes all the threads */
        scheduleDoGC(&cap,task,rtsFalse);

        // after scheduleDoGC(), we must be shutting down.  Either some
        // other Capability did the final GC, or we did it above,
        // either way we can fall through to the SCHED_SHUTTING_DOWN
        // case now.
        ASSERT(sched_state == SCHED_SHUTTING_DOWN);
        // fall through

    case SCHED_SHUTTING_DOWN:
	debugTrace(DEBUG_sched, "SCHED_SHUTTING_DOWN");
	// If we are a worker, just exit.  If we're a bound thread
	// then we will exit below when we've removed our TSO from
	// the run queue.
	if (!isBoundTask(task) && emptyRunQueue(cap)) {
	    return cap;
	}
	break;
    default:
	barf("sched_state: %d", sched_state);
    }

    scheduleFindWork(&cap);

    /* work pushing, currently relevant only for THREADED_RTS:
       (pushes threads, wakes up idle capabilities for stealing) */
    schedulePushWork(cap,task);

    scheduleDetectDeadlock(&cap,task);

    // Normally, the only way we can get here with no threads to
    // run is if a keyboard interrupt received during 
    // scheduleCheckBlockedThreads() or scheduleDetectDeadlock().
    // Additionally, it is not fatal for the
    // threaded RTS to reach here with no threads to run.
    //
    // win32: might be here due to awaitEvent() being abandoned
    // as a result of a console event having been delivered.
    
#if defined(THREADED_RTS)
    if (first) 
    {
    // XXX: ToDo
    //     // don't yield the first time, we want a chance to run this
    //     // thread for a bit, even if there are others banging at the
    //     // door.
    //     first = rtsFalse;
    //     ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    }

    scheduleYield(&cap,task);

    if (emptyRunQueue(cap)) continue; // look for work again
#endif

#if !defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
    if ( emptyRunQueue(cap) ) {
	ASSERT(sched_state >= SCHED_INTERRUPTING);
    }
#endif

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
        InCall *bound = t->bound;
      
	if (bound) {
	    if (bound->task == task) {
		// yes, the Haskell thread is bound to the current native thread
	    } else {
		debugTrace(DEBUG_sched,
			   "thread %lu bound to another OS thread",
                           (unsigned long)t->id);
		// no, bound to a different Haskell thread: pass to that thread
		pushOnRunQueue(cap,t);
		continue;
	    }
	} else {
	    // The thread we want to run is unbound.
	    if (task->incall->tso) { 
		debugTrace(DEBUG_sched,
			   "this OS thread cannot run thread %lu",
                           (unsigned long)t->id);
		// no, the current native thread is bound to a different
		// Haskell thread, so pass it to any worker thread
		pushOnRunQueue(cap,t);
		continue; 
	    }
	}
    }
#endif

    // If we're shutting down, and this thread has not yet been
    // killed, kill it now.  This sometimes happens when a finalizer
    // thread is created by the final GC, or a thread previously
    // in a foreign call returns.
    if (sched_state >= SCHED_INTERRUPTING &&
        !(t->what_next == ThreadComplete || t->what_next == ThreadKilled)) {
        deleteThread(cap,t);
    }

    // If this capability is disabled, migrate the thread away rather
    // than running it.  NB. but not if the thread is bound: it is
    // really hard for a bound thread to migrate itself.  Believe me,
    // I tried several ways and couldn't find a way to do it.
    // Instead, when everything is stopped for GC, we migrate all the
    // threads on the run queue then (see scheduleDoGC()).
    //
    // ToDo: what about TSO_LOCKED?  Currently we're migrating those
    // when the number of capabilities drops, but we never migrate
    // them back if it rises again.  Presumably we should, but after
    // the thread has been migrated we no longer know what capability
    // it was originally on.
#ifdef THREADED_RTS
    if (cap->disabled && !t->bound) {
        Capability *dest_cap = &capabilities[cap->no % enabled_capabilities];
        migrateThread(cap, t, dest_cap);
        continue;
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

    startHeapProfTimer();

    // ----------------------------------------------------------------------
    // Run the current thread 

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);
    ASSERT(t->bound ? t->bound->task->cap == cap : 1);

    prev_what_next = t->what_next;

    errno = t->saved_errno;
#if mingw32_HOST_OS
    SetLastError(t->saved_winerror);
#endif

    // reset the interrupt flag before running Haskell code
    cap->interrupt = 0;

    cap->in_haskell = rtsTrue;
    cap->idle = 0;

    dirty_TSO(cap,t);
    dirty_STACK(cap,t->stackobj);

    switch (recent_activity)
    {
    case ACTIVITY_DONE_GC: {
        // ACTIVITY_DONE_GC means we turned off the timer signal to
        // conserve power (see #1623).  Re-enable it here.
        nat prev;
        prev = xchg((P_)&recent_activity, ACTIVITY_YES);
        if (prev == ACTIVITY_DONE_GC) {
#ifndef PROFILING
            startTimer();
#endif
        }
        break;
    }
    case ACTIVITY_INACTIVE:
        // If we reached ACTIVITY_INACTIVE, then don't reset it until
        // we've done the GC.  The thread running here might just be
        // the IO manager thread that handle_tick() woke up via
        // wakeUpRts().
        break;
    default:
        recent_activity = ACTIVITY_YES;
    }

    traceEventRunThread(cap, t);

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

    // And save the current errno in this thread.
    // XXX: possibly bogus for SMP because this thread might already
    // be running again, see code below.
    t->saved_errno = errno;
#if mingw32_HOST_OS
    // Similarly for Windows error code
    t->saved_winerror = GetLastError();
#endif

    if (ret == ThreadBlocked) {
        if (t->why_blocked == BlockedOnBlackHole) {
            StgTSO *owner = blackHoleOwner(t->block_info.bh->bh);
            traceEventStopThread(cap, t, t->why_blocked + 6,
                                 owner != NULL ? owner->id : 0);
        } else {
            traceEventStopThread(cap, t, t->why_blocked + 6, 0);
        }
    } else {
        traceEventStopThread(cap, t, ret, 0);
    }

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);

    // ----------------------------------------------------------------------
    
    // Costs for the scheduler are assigned to CCS_SYSTEM
    stopHeapProfTimer();
#if defined(PROFILING)
    cap->r.rCCCS = CCS_SYSTEM;
#endif
    
    schedulePostRunThread(cap,t);

    ready_to_gc = rtsFalse;

    switch (ret) {
    case HeapOverflow:
	ready_to_gc = scheduleHandleHeapOverflow(cap,t);
	break;

    case StackOverflow:
        // just adjust the stack for this thread, then pop it back
        // on the run queue.
        threadStackOverflow(cap, t);
        pushOnRunQueue(cap,t);
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
      scheduleDoGC(&cap,task,rtsFalse);
    }
  } /* end of while() */
}

/* -----------------------------------------------------------------------------
 * Run queue operations
 * -------------------------------------------------------------------------- */

void
removeFromRunQueue (Capability *cap, StgTSO *tso)
{
    if (tso->block_info.prev == END_TSO_QUEUE) {
        ASSERT(cap->run_queue_hd == tso);
        cap->run_queue_hd = tso->_link;
    } else {
        setTSOLink(cap, tso->block_info.prev, tso->_link);
    }
    if (tso->_link == END_TSO_QUEUE) {
        ASSERT(cap->run_queue_tl == tso);
        cap->run_queue_tl = tso->block_info.prev;
    } else {
        setTSOPrev(cap, tso->_link, tso->block_info.prev);
    }
    tso->_link = tso->block_info.prev = END_TSO_QUEUE;

    IF_DEBUG(sanity, checkRunQueue(cap));
}

void
promoteInRunQueue (Capability *cap, StgTSO *tso)
{
    removeFromRunQueue(cap, tso);
    pushOnRunQueue(cap, tso);
}

/* ----------------------------------------------------------------------------
 * Setting up the scheduler loop
 * ------------------------------------------------------------------------- */

static void
schedulePreLoop(void)
{
  // initialisation for scheduler - what cannot go into initScheduler()  

#if defined(mingw32_HOST_OS) && !defined(USE_MINIINTERPRETER)
    win32AllocStack();
#endif
}

/* -----------------------------------------------------------------------------
 * scheduleFindWork()
 *
 * Search for work to do, and handle messages from elsewhere.
 * -------------------------------------------------------------------------- */

static void
scheduleFindWork (Capability **pcap)
{
    scheduleStartSignalHandlers(*pcap);

    scheduleProcessInbox(pcap);

    scheduleCheckBlockedThreads(*pcap);

#if defined(THREADED_RTS)
    if (emptyRunQueue(*pcap)) { scheduleActivateSpark(*pcap); }
#endif
}

#if defined(THREADED_RTS)
STATIC_INLINE rtsBool
shouldYieldCapability (Capability *cap, Task *task, rtsBool didGcLast)
{
    // we need to yield this capability to someone else if..
    //   - another thread is initiating a GC, and we didn't just do a GC
    //     (see Note [GC livelock])
    //   - another Task is returning from a foreign call
    //   - the thread at the head of the run queue cannot be run
    //     by this Task (it is bound to another Task, or it is unbound
    //     and this task it bound).
    //
    // Note [GC livelock]
    //
    // If we are interrupted to do a GC, then we do not immediately do
    // another one.  This avoids a starvation situation where one
    // Capability keeps forcing a GC and the other Capabilities make no
    // progress at all.

    return ((pending_sync && !didGcLast) ||
            cap->returning_tasks_hd != NULL ||
            (!emptyRunQueue(cap) && (task->incall->tso == NULL
                                     ? peekRunQueue(cap)->bound != NULL
                                     : peekRunQueue(cap)->bound != task->incall)));
}

// This is the single place where a Task goes to sleep.  There are
// two reasons it might need to sleep:
//    - there are no threads to run
//    - we need to yield this Capability to someone else 
//      (see shouldYieldCapability())
//
// Careful: the scheduler loop is quite delicate.  Make sure you run
// the tests in testsuite/concurrent (all ways) after modifying this,
// and also check the benchmarks in nofib/parallel for regressions.

static void
scheduleYield (Capability **pcap, Task *task)
{
    Capability *cap = *pcap;
    int didGcLast = rtsFalse;

    // if we have work, and we don't need to give up the Capability, continue.
    //
    if (!shouldYieldCapability(cap,task,rtsFalse) && 
        (!emptyRunQueue(cap) ||
         !emptyInbox(cap) ||
         sched_state >= SCHED_INTERRUPTING)) {
        return;
    }

    // otherwise yield (sleep), and keep yielding if necessary.
    do {
        didGcLast = yieldCapability(&cap,task, !didGcLast);
    } 
    while (shouldYieldCapability(cap,task,didGcLast));

    // note there may still be no threads on the run queue at this
    // point, the caller has to check.

    *pcap = cap;
    return;
}
#endif
    
/* -----------------------------------------------------------------------------
 * schedulePushWork()
 *
 * Push work to other Capabilities if we have some.
 * -------------------------------------------------------------------------- */

static void
schedulePushWork(Capability *cap USED_IF_THREADS, 
		 Task *task      USED_IF_THREADS)
{
  /* following code not for PARALLEL_HASKELL. I kept the call general,
     future GUM versions might use pushing in a distributed setup */
#if defined(THREADED_RTS)

    Capability *free_caps[n_capabilities], *cap0;
    nat i, n_free_caps;

    // migration can be turned off with +RTS -qm
    if (!RtsFlags.ParFlags.migrate) return;

    // Check whether we have more threads on our run queue, or sparks
    // in our pool, that we could hand to another Capability.
    if (emptyRunQueue(cap)) {
        if (sparkPoolSizeCap(cap) < 2) return;
    } else {
        if (singletonRunQueue(cap) &&
            sparkPoolSizeCap(cap) < 1) return;
    }

    // First grab as many free Capabilities as we can.
    for (i=0, n_free_caps=0; i < n_capabilities; i++) {
	cap0 = &capabilities[i];
        if (cap != cap0 && !cap0->disabled && tryGrabCapability(cap0,task)) {
	    if (!emptyRunQueue(cap0)
                || cap0->returning_tasks_hd != NULL
                || cap0->inbox != (Message*)END_TSO_QUEUE) {
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
#ifdef SPARK_PUSHING
	rtsBool pushed_to_all;
#endif

	debugTrace(DEBUG_sched, 
		   "cap %d: %s and %d free capabilities, sharing...", 
		   cap->no, 
		   (!emptyRunQueue(cap) && !singletonRunQueue(cap))?
		   "excess threads on run queue":"sparks to share (>=2)",
		   n_free_caps);

	i = 0;
#ifdef SPARK_PUSHING
	pushed_to_all = rtsFalse;
#endif

	if (cap->run_queue_hd != END_TSO_QUEUE) {
	    prev = cap->run_queue_hd;
	    t = prev->_link;
	    prev->_link = END_TSO_QUEUE;
	    for (; t != END_TSO_QUEUE; t = next) {
		next = t->_link;
		t->_link = END_TSO_QUEUE;
                if (t->bound == task->incall // don't move my bound thread
		    || tsoLocked(t)) {  // don't move a locked thread
		    setTSOLink(cap, prev, t);
                    setTSOPrev(cap, t, prev);
		    prev = t;
		} else if (i == n_free_caps) {
#ifdef SPARK_PUSHING
		    pushed_to_all = rtsTrue;
#endif
		    i = 0;
		    // keep one for us
		    setTSOLink(cap, prev, t);
                    setTSOPrev(cap, t, prev);
		    prev = t;
		} else {
		    appendToRunQueue(free_caps[i],t);

                    traceEventMigrateThread (cap, t, free_caps[i]->no);

		    if (t->bound) { t->bound->task->cap = free_caps[i]; }
		    t->cap = free_caps[i];
		    i++;
		}
	    }
	    cap->run_queue_tl = prev;

            IF_DEBUG(sanity, checkRunQueue(cap));
	}

#ifdef SPARK_PUSHING
	/* JB I left this code in place, it would work but is not necessary */

	// If there are some free capabilities that we didn't push any
	// threads to, then try to push a spark to each one.
	if (!pushed_to_all) {
	    StgClosure *spark;
	    // i is the next free capability to push to
	    for (; i < n_free_caps; i++) {
		if (emptySparkPoolCap(free_caps[i])) {
		    spark = tryStealSpark(cap->sparks);
		    if (spark != NULL) {
                        /* TODO: if anyone wants to re-enable this code then
                         * they must consider the fizzledSpark(spark) case
                         * and update the per-cap spark statistics.
                         */
			debugTrace(DEBUG_sched, "pushing spark %p to capability %d", spark, free_caps[i]->no);

            traceEventStealSpark(free_caps[i], t, cap->no);

			newSpark(&(free_caps[i]->r), spark);
		    }
		}
	    }
	}
#endif /* SPARK_PUSHING */

	// release the capabilities
	for (i = 0; i < n_free_caps; i++) {
	    task->cap = free_caps[i];
	    releaseAndWakeupCapability(free_caps[i]);
	}
    }
    task->cap = cap; // reset to point to our Capability.

#endif /* THREADED_RTS */

}

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
	awaitEvent (emptyRunQueue(cap));
    }
#endif
}

/* ----------------------------------------------------------------------------
 * Detect deadlock conditions and attempt to resolve them.
 * ------------------------------------------------------------------------- */

static void
scheduleDetectDeadlock (Capability **pcap, Task *task)
{
    Capability *cap = *pcap;
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
        scheduleDoGC (pcap, task, rtsTrue/*force major GC*/);
        cap = *pcap;
        // when force_major == rtsTrue. scheduleDoGC sets
        // recent_activity to ACTIVITY_DONE_GC and turns off the timer
        // signal.

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
	if (task->incall->tso) {
	    switch (task->incall->tso->why_blocked) {
	    case BlockedOnSTM:
	    case BlockedOnBlackHole:
	    case BlockedOnMsgThrowTo:
	    case BlockedOnMVar:
	    case BlockedOnMVarRead:
		throwToSingleThreaded(cap, task->incall->tso, 
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
static void
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
 * Process message in the current Capability's inbox
 * ------------------------------------------------------------------------- */

static void
scheduleProcessInbox (Capability **pcap USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    Message *m, *next;
    int r;
    Capability *cap = *pcap;

    while (!emptyInbox(cap)) {
        if (cap->r.rCurrentNursery->link == NULL ||
            g0->n_new_large_words >= large_alloc_lim) {
            scheduleDoGC(pcap, cap->running_task, rtsFalse);
            cap = *pcap;
        }

        // don't use a blocking acquire; if the lock is held by
        // another thread then just carry on.  This seems to avoid
        // getting stuck in a message ping-pong situation with other
        // processors.  We'll check the inbox again later anyway.
        //
        // We should really use a more efficient queue data structure
        // here.  The trickiness is that we must ensure a Capability
        // never goes idle if the inbox is non-empty, which is why we
        // use cap->lock (cap->lock is released as the last thing
        // before going idle; see Capability.c:releaseCapability()).
        r = TRY_ACQUIRE_LOCK(&cap->lock);
        if (r != 0) return;

        m = cap->inbox;
        cap->inbox = (Message*)END_TSO_QUEUE;

        RELEASE_LOCK(&cap->lock);

        while (m != (Message*)END_TSO_QUEUE) {
            next = m->link;
            executeMessage(cap, m);
            m = next;
        }
    }
#endif
}

/* ----------------------------------------------------------------------------
 * Activate spark threads (PARALLEL_HASKELL and THREADED_RTS)
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
static void
scheduleActivateSpark(Capability *cap)
{
    if (anySparks() && !cap->disabled)
    {
        createSparkThread(cap);
        debugTrace(DEBUG_sched, "creating a spark thread");
    }
}
#endif // PARALLEL_HASKELL || THREADED_RTS

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
        if (!stmValidateNestOfTransactions(cap, t -> trec)) {
            debugTrace(DEBUG_sched | DEBUG_stm,
                       "trec %p found wasting its time", t);
            
            // strip the stack back to the
            // ATOMICALLY_FRAME, aborting the (nested)
            // transaction, and saving the stack of any
            // partially-evaluated thunks on the heap.
            throwToSingleThreaded_(cap, t, NULL, rtsTrue);
            
//            ASSERT(get_itbl((StgClosure *)t->sp)->type == ATOMICALLY_FRAME);
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
	W_ blocks;
	
	blocks = (W_)BLOCK_ROUND_UP(cap->r.rHpAlloc) / BLOCK_SIZE;
	
        if (blocks > BLOCKS_PER_MBLOCK) {
            barf("allocation of %ld bytes too large (GHC should have complained at compile-time)", (long)cap->r.rHpAlloc);
        }

	debugTrace(DEBUG_sched,
		   "--<< thread %ld (%s) stopped: requesting a large block (size %ld)\n", 
		   (long)t->id, what_next_strs[t->what_next], blocks);
    
	// don't do this if the nursery is (nearly) full, we'll GC first.
	if (cap->r.rCurrentNursery->link != NULL ||
	    cap->r.rNursery->n_blocks == 1) {  // paranoia to prevent infinite loop
	                                       // if the nursery has only one block.
	    
            bd = allocGroup_lock(blocks);
            cap->r.rNursery->n_blocks += blocks;
	    
	    // link the new group into the list
	    bd->link = cap->r.rCurrentNursery;
	    bd->u.back = cap->r.rCurrentNursery->u.back;
	    if (cap->r.rCurrentNursery->u.back != NULL) {
		cap->r.rCurrentNursery->u.back->link = bd;
	    } else {
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
                    initBdescr(x,g0,g0);
                    x->free = x->start;
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
    
    if (cap->r.rHpLim == NULL || cap->context_switch) {
        // Sometimes we miss a context switch, e.g. when calling
        // primitives in a tight loop, MAYBE_GC() doesn't check the
        // context switch flag, and we end up waiting for a GC.
        // See #1984, and concurrent/should_run/1984
        cap->context_switch = 0;
        appendToRunQueue(cap,t);
    } else {
        pushOnRunQueue(cap,t);
    }
    return rtsTrue;
    /* actual GC is done at the end of the while loop in schedule() */
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadYielding
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleHandleYield( Capability *cap, StgTSO *t, nat prev_what_next )
{
    /* put the thread back on the run queue.  Then, if we're ready to
     * GC, check whether this is the last task to stop.  If so, wake
     * up the GC thread.  getThread will block during a GC until the
     * GC is finished.
     */

    ASSERT(t->_link == END_TSO_QUEUE);
    
    // Shortcut if we're just switching evaluators: don't bother
    // doing stack squeezing (which can be expensive), just run the
    // thread.
    if (cap->context_switch == 0 && t->what_next != prev_what_next) {
	debugTrace(DEBUG_sched,
		   "--<< thread %ld (%s) stopped to switch evaluators", 
		   (long)t->id, what_next_strs[t->what_next]);
	return rtsTrue;
    }

    // Reset the context switch flag.  We don't do this just before
    // running the thread, because that would mean we would lose ticks
    // during GC, which can lead to unfair scheduling (a thread hogs
    // the CPU because the tick always arrives during GC).  This way
    // penalises threads that do a lot of allocation, but that seems
    // better than the alternative.
    if (cap->context_switch != 0) {
        cap->context_switch = 0;
        appendToRunQueue(cap,t);
    } else {
        pushOnRunQueue(cap,t);
    }

    IF_DEBUG(sanity,
	     //debugBelch("&& Doing sanity check on yielding TSO %ld.", t->id);
	     checkTSO(t));

    return rtsFalse;
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadBlocked
 * -------------------------------------------------------------------------- */

static void
scheduleHandleThreadBlocked( StgTSO *t
#if !defined(DEBUG)
    STG_UNUSED
#endif
    )
{

      // We don't need to do anything.  The thread is blocked, and it
      // has tidied up its stack and placed itself on whatever queue
      // it needs to be on.

    // ASSERT(t->why_blocked != NotBlocked);
    // Not true: for example,
    //    - the thread may have woken itself up already, because
    //      threadPaused() might have raised a blocked throwTo
    //      exception, see maybePerformBlockedException().

#ifdef DEBUG
    traceThreadStatus(DEBUG_sched, t);
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

    // blocked exceptions can now complete, even if the thread was in
    // blocked mode (see #2910).
    awakenBlockedExceptionQueue (cap, t);

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

	  if (t->bound != task->incall) {
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

	  ASSERT(task->incall->tso == t);

	  if (t->what_next == ThreadComplete) {
	      if (task->incall->ret) {
                  // NOTE: return val is stack->sp[1] (see StgStartup.hc)
                  *(task->incall->ret) = (StgClosure *)task->incall->tso->stackobj->sp[1];
	      }
	      task->incall->stat = Success;
	  } else {
	      if (task->incall->ret) {
		  *(task->incall->ret) = NULL;
	      }
	      if (sched_state >= SCHED_INTERRUPTING) {
                  if (heap_overflow) {
                      task->incall->stat = HeapExhausted;
                  } else {
                      task->incall->stat = Interrupted;
                  }
	      } else {
		  task->incall->stat = Killed;
	      }
	  }
#ifdef DEBUG
	  removeThreadLabel((StgWord)task->incall->tso->id);
#endif

          // We no longer consider this thread and task to be bound to
          // each other.  The TSO lives on until it is GC'd, but the
          // task is about to be released by the caller, and we don't
          // want anyone following the pointer from the TSO to the
          // defunct task (which might have already been
          // re-used). This was a real bug: the GC updated
          // tso->bound->tso which lead to a deadlock.
          t->bound = NULL;
          task->incall->tso = NULL;

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
        (RtsFlags.ProfFlags.heapProfileInterval==0 &&
	 RtsFlags.ProfFlags.doHeapProfile && ready_to_gc)) {
        return rtsTrue;
    } else {
        return rtsFalse;
    }
}

/* -----------------------------------------------------------------------------
 * Start a synchronisation of all capabilities
 * -------------------------------------------------------------------------- */

// Returns:
//    0      if we successfully got a sync
//    non-0  if there was another sync request in progress,
//           and we yielded to it.  The value returned is the
//           type of the other sync request.
//
#if defined(THREADED_RTS)
static nat requestSync (Capability **pcap, Task *task, nat sync_type)
{
    nat prev_pending_sync;

    prev_pending_sync = cas(&pending_sync, 0, sync_type);

    if (prev_pending_sync)
    {
	do {
            debugTrace(DEBUG_sched, "someone else is trying to sync (%d)...",
                       prev_pending_sync);
            ASSERT(*pcap);
            yieldCapability(pcap,task,rtsTrue);
        } while (pending_sync);
        return prev_pending_sync; // NOTE: task->cap might have changed now
    }
    else
    {
        return 0;
    }
}

//
// Grab all the capabilities except the one we already hold.  Used
// when synchronising before a single-threaded GC (SYNC_SEQ_GC), and
// before a fork (SYNC_OTHER).
//
// Only call this after requestSync(), otherwise a deadlock might
// ensue if another thread is trying to synchronise.
//
static void acquireAllCapabilities(Capability *cap, Task *task)
{
    Capability *tmpcap;
    nat i;

    for (i=0; i < n_capabilities; i++) {
        debugTrace(DEBUG_sched, "grabbing all the capabilies (%d/%d)", i, n_capabilities);
        tmpcap = &capabilities[i];
        if (tmpcap != cap) {
            // we better hope this task doesn't get migrated to
            // another Capability while we're waiting for this one.
            // It won't, because load balancing happens while we have
            // all the Capabilities, but even so it's a slightly
            // unsavoury invariant.
            task->cap = tmpcap;
            waitForReturnCapability(&tmpcap, task);
            if (tmpcap->no != i) {
                barf("acquireAllCapabilities: got the wrong capability");
            }
        }
    }
    task->cap = cap;
}

static void releaseAllCapabilities(nat n, Capability *cap, Task *task)
{
    nat i;

    for (i = 0; i < n; i++) {
        if (cap->no != i) {
            task->cap = &capabilities[i];
            releaseCapability(&capabilities[i]);
        }
    }
    task->cap = cap;
}
#endif

/* -----------------------------------------------------------------------------
 * Perform a garbage collection if necessary
 * -------------------------------------------------------------------------- */

static void
scheduleDoGC (Capability **pcap, Task *task USED_IF_THREADS,
              rtsBool force_major)
{
    Capability *cap = *pcap;
    rtsBool heap_census;
    nat collect_gen;
#ifdef THREADED_RTS
    StgWord8 gc_type;
    nat i, sync;
    StgTSO *tso;
#endif

    if (sched_state == SCHED_SHUTTING_DOWN) {
        // The final GC has already been done, and the system is
        // shutting down.  We'll probably deadlock if we try to GC
        // now.
        return;
    }

    heap_census = scheduleNeedHeapProfile(rtsTrue);

    // Figure out which generation we are collecting, so that we can
    // decide whether this is a parallel GC or not.
    collect_gen = calcNeeded(force_major || heap_census, NULL);

#ifdef THREADED_RTS
    if (sched_state < SCHED_INTERRUPTING
        && RtsFlags.ParFlags.parGcEnabled
        && collect_gen >= RtsFlags.ParFlags.parGcGen
        && ! oldest_gen->mark)
    {
        gc_type = SYNC_GC_PAR;
    } else {
        gc_type = SYNC_GC_SEQ;
    }

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
        threads if pending_sync is set. Tested inside
	yieldCapability() and releaseCapability() in Capability.c */

    do {
        sync = requestSync(pcap, task, gc_type);
        cap = *pcap;
        if (sync == SYNC_GC_SEQ || sync == SYNC_GC_PAR) {
            // someone else had a pending sync request for a GC, so
            // let's assume GC has been done and we don't need to GC
            // again.
            return;
        }
        if (sched_state == SCHED_SHUTTING_DOWN) {
            // The scheduler might now be shutting down.  We tested
            // this above, but it might have become true since then as
            // we yielded the capability in requestSync().
            return;
        }
    } while (sync);

    // don't declare this until after we have sync'd, because
    // n_capabilities may change.
    rtsBool idle_cap[n_capabilities];
#ifdef DEBUG
    unsigned int old_n_capabilities = n_capabilities;
#endif

    interruptAllCapabilities();

    // The final shutdown GC is always single-threaded, because it's
    // possible that some of the Capabilities have no worker threads.
    
    if (gc_type == SYNC_GC_SEQ)
    {
        traceEventRequestSeqGc(cap);
    }
    else
    {
        traceEventRequestParGc(cap);
        debugTrace(DEBUG_sched, "ready_to_gc, grabbing GC threads");
    }

    if (gc_type == SYNC_GC_SEQ)
    {
        // single-threaded GC: grab all the capabilities
        acquireAllCapabilities(cap,task);
    }
    else
    {
        // If we are load-balancing collections in this
        // generation, then we require all GC threads to participate
        // in the collection.  Otherwise, we only require active
        // threads to participate, and we set gc_threads[i]->idle for
        // any idle capabilities.  The rationale here is that waking
        // up an idle Capability takes much longer than just doing any
        // GC work on its behalf.

        if (RtsFlags.ParFlags.parGcNoSyncWithIdle == 0
            || (RtsFlags.ParFlags.parGcLoadBalancingEnabled &&
                collect_gen >= RtsFlags.ParFlags.parGcLoadBalancingGen)) {
            for (i=0; i < n_capabilities; i++) {
                if (capabilities[i].disabled) {
                    idle_cap[i] = tryGrabCapability(&capabilities[i], task);
                } else {
                    idle_cap[i] = rtsFalse;
                }
            }
        } else {
            for (i=0; i < n_capabilities; i++) {
                if (capabilities[i].disabled) {
                    idle_cap[i] = tryGrabCapability(&capabilities[i], task);
                } else if (i == cap->no ||
                           capabilities[i].idle < RtsFlags.ParFlags.parGcNoSyncWithIdle) {
                    idle_cap[i] = rtsFalse;
                } else {
                    idle_cap[i] = tryGrabCapability(&capabilities[i], task);
                    if (!idle_cap[i]) {
                        n_failed_trygrab_idles++;
                    } else {
                        n_idle_caps++;
                    }
                }
            }
        }

        // We set the gc_thread[i]->idle flag if that
        // capability/thread is not participating in this collection.
        // We also keep a local record of which capabilities are idle
        // in idle_cap[], because scheduleDoGC() is re-entrant:
        // another thread might start a GC as soon as we've finished
        // this one, and thus the gc_thread[]->idle flags are invalid
        // as soon as we release any threads after GC.  Getting this
        // wrong leads to a rare and hard to debug deadlock!

        for (i=0; i < n_capabilities; i++) {
            gc_threads[i]->idle = idle_cap[i];
            capabilities[i].idle++;
        }

        // For all capabilities participating in this GC, wait until
        // they have stopped mutating and are standing by for GC.
        waitForGcThreads(cap);
        
#if defined(THREADED_RTS)
        // Stable point where we can do a global check on our spark counters
        ASSERT(checkSparkCountInvariant());
#endif
    }

#endif

    IF_DEBUG(scheduler, printAllThreads());

delete_threads_and_gc:
    /*
     * We now have all the capabilities; if we're in an interrupting
     * state, then we should take the opportunity to delete all the
     * threads in the system.
     */
    if (sched_state == SCHED_INTERRUPTING) {
	deleteAllThreads(cap);
#if defined(THREADED_RTS)
        // Discard all the sparks from every Capability.  Why?
        // They'll probably be GC'd anyway since we've killed all the
        // threads.  It just avoids the GC having to do any work to
        // figure out that any remaining sparks are garbage.
        for (i = 0; i < n_capabilities; i++) {
            capabilities[i].spark_stats.gcd +=
                sparkPoolSize(capabilities[i].sparks);
            // No race here since all Caps are stopped.
            discardSparksCap(&capabilities[i]);
        }
#endif
        sched_state = SCHED_SHUTTING_DOWN;
    }
    
    /*
     * When there are disabled capabilities, we want to migrate any
     * threads away from them.  Normally this happens in the
     * scheduler's loop, but only for unbound threads - it's really
     * hard for a bound thread to migrate itself.  So we have another
     * go here.
     */
#if defined(THREADED_RTS)
    for (i = enabled_capabilities; i < n_capabilities; i++) {
        Capability *tmp_cap, *dest_cap;
        tmp_cap = &capabilities[i];
        ASSERT(tmp_cap->disabled);
        if (i != cap->no) {
            dest_cap = &capabilities[i % enabled_capabilities];
            while (!emptyRunQueue(tmp_cap)) {
                tso = popRunQueue(tmp_cap);
                migrateThread(tmp_cap, tso, dest_cap);
                if (tso->bound) {
                  traceTaskMigrate(tso->bound->task,
                                   tso->bound->task->cap,
                                   dest_cap);
                  tso->bound->task->cap = dest_cap;
                }
            }
        }
    }
#endif

#if defined(THREADED_RTS)
    // reset pending_sync *before* GC, so that when the GC threads
    // emerge they don't immediately re-enter the GC.
    pending_sync = 0;
    GarbageCollect(collect_gen, heap_census, gc_type, cap);
#else
    GarbageCollect(collect_gen, heap_census, 0, cap);
#endif

    traceSparkCounters(cap);

    switch (recent_activity) {
    case ACTIVITY_INACTIVE:
        if (force_major) {
            // We are doing a GC because the system has been idle for a
            // timeslice and we need to check for deadlock.  Record the
            // fact that we've done a GC and turn off the timer signal;
            // it will get re-enabled if we run any threads after the GC.
            recent_activity = ACTIVITY_DONE_GC;
#ifndef PROFILING
            stopTimer();
#endif
            break;
        }
        // fall through...

    case ACTIVITY_MAYBE_NO:
        // the GC might have taken long enough for the timer to set
        // recent_activity = ACTIVITY_MAYBE_NO or ACTIVITY_INACTIVE,
        // but we aren't necessarily deadlocked:
        recent_activity = ACTIVITY_YES;
        break;

    case ACTIVITY_DONE_GC:
        // If we are actually active, the scheduler will reset the
        // recent_activity flag and re-enable the timer.
        break;
    }

#if defined(THREADED_RTS)
    // Stable point where we can do a global check on our spark counters
    ASSERT(checkSparkCountInvariant());
#endif

    // The heap census itself is done during GarbageCollect().
    if (heap_census) {
        performHeapProfile = rtsFalse;
    }

#if defined(THREADED_RTS)

    // If n_capabilities has changed during GC, we're in trouble.
    ASSERT(n_capabilities == old_n_capabilities);

    if (gc_type == SYNC_GC_PAR)
    {
        releaseGCThreads(cap);
        for (i = 0; i < n_capabilities; i++) {
            if (i != cap->no) {
                if (idle_cap[i]) {
                    ASSERT(capabilities[i].running_task == task);
                    task->cap = &capabilities[i];
                    releaseCapability(&capabilities[i]);
                } else {
                    ASSERT(capabilities[i].running_task != task);
                }
            }
        }
        task->cap = cap;
    }
#endif

    if (heap_overflow && sched_state < SCHED_INTERRUPTING) {
        // GC set the heap_overflow flag, so we should proceed with
        // an orderly shutdown now.  Ultimately we want the main
        // thread to return to its caller with HeapExhausted, at which
        // point the caller should call hs_exit().  The first step is
        // to delete all the threads.
        //
        // Another way to do this would be to raise an exception in
        // the main thread, which we really should do because it gives
        // the program a chance to clean up.  But how do we find the
        // main thread?  It should presumably be the same one that
        // gets ^C exceptions, but that's all done on the Haskell side
        // (GHC.TopHandler).
	sched_state = SCHED_INTERRUPTING;
        goto delete_threads_and_gc;
    }

#ifdef SPARKBALANCE
    /* JB 
       Once we are all together... this would be the place to balance all
       spark pools. No concurrent stealing or adding of new sparks can
       occur. Should be defined in Sparks.c. */
    balanceSparkPoolsCaps(n_capabilities, capabilities);
#endif

#if defined(THREADED_RTS)
    if (gc_type == SYNC_GC_SEQ) {
        // release our stash of capabilities.
        releaseAllCapabilities(n_capabilities, cap, task);
    }
#endif

    return;
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
    pid_t pid;
    StgTSO* t,*next;
    Capability *cap;
    nat g;
    Task *task = NULL;
    nat i;
#ifdef THREADED_RTS
    nat sync;
#endif

    debugTrace(DEBUG_sched, "forking!");
    
    task = newBoundTask();

    cap = NULL;
    waitForReturnCapability(&cap, task);

#ifdef THREADED_RTS
    do {
        sync = requestSync(&cap, task, SYNC_OTHER);
    } while (sync);

    acquireAllCapabilities(cap,task);

    pending_sync = 0;
#endif

    // no funny business: hold locks while we fork, otherwise if some
    // other thread is holding a lock when the fork happens, the data
    // structure protected by the lock will forever be in an
    // inconsistent state in the child.  See also #1391.
    ACQUIRE_LOCK(&sched_mutex);
    ACQUIRE_LOCK(&sm_mutex);
    ACQUIRE_LOCK(&stable_mutex);
    ACQUIRE_LOCK(&task->lock);

    for (i=0; i < n_capabilities; i++) {
        ACQUIRE_LOCK(&capabilities[i].lock);
    }

    stopTimer(); // See #4074

#if defined(TRACING)
    flushEventLog(); // so that child won't inherit dirty file buffers
#endif

    pid = fork();
    
    if (pid) { // parent
	
        startTimer(); // #4074

        RELEASE_LOCK(&sched_mutex);
        RELEASE_LOCK(&sm_mutex);
        RELEASE_LOCK(&stable_mutex);
        RELEASE_LOCK(&task->lock);

        for (i=0; i < n_capabilities; i++) {
            releaseCapability_(&capabilities[i],rtsFalse);
            RELEASE_LOCK(&capabilities[i].lock);
        }
        boundTaskExiting(task);

	// just return the pid
        return pid;
	
    } else { // child
	
#if defined(THREADED_RTS)
        initMutex(&sched_mutex);
        initMutex(&sm_mutex);
        initMutex(&stable_mutex);
        initMutex(&task->lock);

        for (i=0; i < n_capabilities; i++) {
            initMutex(&capabilities[i].lock);
        }
#endif

#ifdef TRACING
        resetTracing();
#endif

        // Now, all OS threads except the thread that forked are
	// stopped.  We need to stop all Haskell threads, including
	// those involved in foreign calls.  Also we need to delete
	// all Tasks, because they correspond to OS threads that are
	// now gone.

        for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
          for (t = generations[g].threads; t != END_TSO_QUEUE; t = next) {
                next = t->global_link;
		// don't allow threads to catch the ThreadKilled
		// exception, but we do want to raiseAsync() because these
		// threads may be evaluating thunks that we need later.
                deleteThread_(t->cap,t);

                // stop the GC from updating the InCall to point to
                // the TSO.  This is only necessary because the
                // OSThread bound to the TSO has been killed, and
                // won't get a chance to exit in the usual way (see
                // also scheduleHandleThreadFinished).
                t->bound = NULL;
          }
	}
	
        discardTasksExcept(task);

        for (i=0; i < n_capabilities; i++) {
            cap = &capabilities[i];

            // Empty the run queue.  It seems tempting to let all the
            // killed threads stay on the run queue as zombies to be
            // cleaned up later, but some of them may correspond to
            // bound threads for which the corresponding Task does not
            // exist.
            truncateRunQueue(cap);

            // Any suspended C-calling Tasks are no more, their OS threads
            // don't exist now:
            cap->suspended_ccalls = NULL;

#if defined(THREADED_RTS)
            // Wipe our spare workers list, they no longer exist.  New
            // workers will be created if necessary.
            cap->spare_workers = NULL;
            cap->n_spare_workers = 0;
            cap->returning_tasks_hd = NULL;
            cap->returning_tasks_tl = NULL;
#endif

            // Release all caps except 0, we'll use that for starting
            // the IO manager and running the client action below.
            if (cap->no != 0) {
                task->cap = cap;
                releaseCapability(cap);
            }
        }
        cap = &capabilities[0];
        task->cap = cap;

        // Empty the threads lists.  Otherwise, the garbage
	// collector may attempt to resurrect some of these threads.
        for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
            generations[g].threads = END_TSO_QUEUE;
        }

        // On Unix, all timers are reset in the child, so we need to start
        // the timer again.
        initTimer();
        startTimer();

        // TODO: need to trace various other things in the child
        // like startup event, capabilities, process info etc
        traceTaskCreate(task, cap);

#if defined(THREADED_RTS)
        ioManagerStartCap(&cap);
#endif

        rts_evalStableIO(&cap, entry, NULL);  // run the action
	rts_checkSchedStatus("forkProcess",cap);
	
	rts_unlock(cap);
	hs_exit();                      // clean up and exit
	stg_exit(EXIT_SUCCESS);
    }
#else /* !FORKPROCESS_PRIMOP_SUPPORTED */
    barf("forkProcess#: primop not supported on this platform, sorry!\n");
#endif
}

/* ---------------------------------------------------------------------------
 * Changing the number of Capabilities
 *
 * Changing the number of Capabilities is very tricky!  We can only do
 * it with the system fully stopped, so we do a full sync with
 * requestSync(SYNC_OTHER) and grab all the capabilities.
 *
 * Then we resize the appropriate data structures, and update all
 * references to the old data structures which have now moved.
 * Finally we release the Capabilities we are holding, and start
 * worker Tasks on the new Capabilities we created.
 *
 * ------------------------------------------------------------------------- */
   
void
setNumCapabilities (nat new_n_capabilities USED_IF_THREADS)
{
#if !defined(THREADED_RTS)
    if (new_n_capabilities != 1) {
        errorBelch("setNumCapabilities: not supported in the non-threaded RTS");
    }
    return;
#elif defined(NOSMP)
    if (new_n_capabilities != 1) {
        errorBelch("setNumCapabilities: not supported on this platform");
    }
    return;
#else
    Task *task;
    Capability *cap;
    nat sync;
    StgTSO* t;
    nat g, n;
    Capability *old_capabilities = NULL;
    nat old_n_capabilities = n_capabilities;

    if (new_n_capabilities == enabled_capabilities) return;

    debugTrace(DEBUG_sched, "changing the number of Capabilities from %d to %d",
               enabled_capabilities, new_n_capabilities);
    
    cap = rts_lock();
    task = cap->running_task;

    do {
        sync = requestSync(&cap, task, SYNC_OTHER);
    } while (sync);

    acquireAllCapabilities(cap,task);

    pending_sync = 0;

    if (new_n_capabilities < enabled_capabilities)
    {
        // Reducing the number of capabilities: we do not actually
        // remove the extra capabilities, we just mark them as
        // "disabled". This has the following effects:
        //
        //   - threads on a disabled capability are migrated away by the
        //     scheduler loop
        //
        //   - disabled capabilities do not participate in GC
        //     (see scheduleDoGC())
        //
        //   - No spark threads are created on this capability
        //     (see scheduleActivateSpark())
        //
        //   - We do not attempt to migrate threads *to* a disabled
        //     capability (see schedulePushWork()).
        //
        // but in other respects, a disabled capability remains
        // alive.  Threads may be woken up on a disabled capability,
        // but they will be immediately migrated away.
        //
        // This approach is much easier than trying to actually remove
        // the capability; we don't have to worry about GC data
        // structures, the nursery, etc.
        //
        for (n = new_n_capabilities; n < enabled_capabilities; n++) {
            capabilities[n].disabled = rtsTrue;
            traceCapDisable(&capabilities[n]);
        }
        enabled_capabilities = new_n_capabilities;
    }
    else
    {
        // Increasing the number of enabled capabilities.
        //
        // enable any disabled capabilities, up to the required number
        for (n = enabled_capabilities;
             n < new_n_capabilities && n < n_capabilities; n++) {
            capabilities[n].disabled = rtsFalse;
            traceCapEnable(&capabilities[n]);
        }
        enabled_capabilities = n;

        if (new_n_capabilities > n_capabilities) {
#if defined(TRACING)
            // Allocate eventlog buffers for the new capabilities.  Note this
            // must be done before calling moreCapabilities(), because that
            // will emit events about creating the new capabilities and adding
            // them to existing capsets.
            tracingAddCapapilities(n_capabilities, new_n_capabilities);
#endif

            // Resize the capabilities array
            // NB. after this, capabilities points somewhere new.  Any pointers
            // of type (Capability *) are now invalid.
            old_capabilities = moreCapabilities(n_capabilities, new_n_capabilities);

            // update our own cap pointer
            cap = &capabilities[cap->no];

            // Resize and update storage manager data structures
            storageAddCapabilities(n_capabilities, new_n_capabilities);

            // Update (Capability *) refs in the Task manager.
            updateCapabilityRefs();

            // Update (Capability *) refs from TSOs
            for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
                for (t = generations[g].threads; t != END_TSO_QUEUE; t = t->global_link) {
                    t->cap = &capabilities[t->cap->no];
                }
            }
        }
    }

    // update n_capabilities before things start running
    if (new_n_capabilities > n_capabilities) {
        n_capabilities = enabled_capabilities = new_n_capabilities;
    }

    // Start worker tasks on the new Capabilities
    startWorkerTasks(old_n_capabilities, new_n_capabilities);

    // We're done: release the original Capabilities
    releaseAllCapabilities(old_n_capabilities, cap,task);

    // We can't free the old array until now, because we access it
    // while updating pointers in updateCapabilityRefs().
    if (old_capabilities) {
        stgFree(old_capabilities);
    }

    // Notify IO manager that the number of capabilities has changed.
    rts_evalIO(&cap, ioManagerCapabilitiesChanged_closure, NULL);

    rts_unlock(cap);

#endif // THREADED_RTS
}



/* ---------------------------------------------------------------------------
 * Delete all the threads in the system
 * ------------------------------------------------------------------------- */
   
static void
deleteAllThreads ( Capability *cap )
{
    // NOTE: only safe to call if we own all capabilities.

    StgTSO* t, *next;
    nat g;

    debugTrace(DEBUG_sched,"deleting all threads");
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (t = generations[g].threads; t != END_TSO_QUEUE; t = next) {
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
   Managing the suspended_ccalls list.
   Locks required: sched_mutex
   -------------------------------------------------------------------------- */

STATIC_INLINE void
suspendTask (Capability *cap, Task *task)
{
    InCall *incall;
    
    incall = task->incall;
    ASSERT(incall->next == NULL && incall->prev == NULL);
    incall->next = cap->suspended_ccalls;
    incall->prev = NULL;
    if (cap->suspended_ccalls) {
	cap->suspended_ccalls->prev = incall;
    }
    cap->suspended_ccalls = incall;
}

STATIC_INLINE void
recoverSuspendedTask (Capability *cap, Task *task)
{
    InCall *incall;

    incall = task->incall;
    if (incall->prev) {
	incall->prev->next = incall->next;
    } else {
	ASSERT(cap->suspended_ccalls == incall);
	cap->suspended_ccalls = incall->next;
    }
    if (incall->next) {
	incall->next->prev = incall->prev;
    }
    incall->next = incall->prev = NULL;
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
 * duration of the call, on the suspended_ccalling_threads queue.  We
 * give out a token to the task, which it can use to resume the thread
 * on return from the C function.
 *
 * If this is an interruptible C call, this means that the FFI call may be
 * unceremoniously terminated and should be scheduled on an
 * unbound worker thread.
 * ------------------------------------------------------------------------- */
   
void *
suspendThread (StgRegTable *reg, rtsBool interruptible)
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

  traceEventStopThread(cap, tso, THREAD_SUSPENDED_FOREIGN_CALL, 0);

  // XXX this might not be necessary --SDM
  tso->what_next = ThreadRunGHC;

  threadPaused(cap,tso);

  if (interruptible) {
    tso->why_blocked = BlockedOnCCall_Interruptible;
  } else {
    tso->why_blocked = BlockedOnCCall;
  }

  // Hand back capability
  task->incall->suspended_tso = tso;
  task->incall->suspended_cap = cap;

  ACQUIRE_LOCK(&cap->lock);

  suspendTask(cap,task);
  cap->in_haskell = rtsFalse;
  releaseCapability_(cap,rtsFalse);
  
  RELEASE_LOCK(&cap->lock);

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
    InCall *incall;
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

    incall = task->incall;
    cap = incall->suspended_cap;
    task->cap = cap;

    // Wait for permission to re-enter the RTS with the result.
    waitForReturnCapability(&cap,task);
    // we might be on a different capability now... but if so, our
    // entry on the suspended_ccalls list will also have been
    // migrated.

    // Remove the thread from the suspended list
    recoverSuspendedTask(cap,task);

    tso = incall->suspended_tso;
    incall->suspended_tso = NULL;
    incall->suspended_cap = NULL;
    tso->_link = END_TSO_QUEUE; // no write barrier reqd

    traceEventRunThread(cap, tso);
    
    /* Reset blocking status */
    tso->why_blocked  = NotBlocked;

    if ((tso->flags & TSO_BLOCKEX) == 0) {
        // avoid locking the TSO if we don't have to
        if (tso->blocked_exceptions != END_BLOCKED_EXCEPTIONS_QUEUE) {
            maybePerformBlockedException(cap,tso);
        }
    }
    
    cap->r.rCurrentTSO = tso;
    cap->in_haskell = rtsTrue;
    errno = saved_errno;
#if mingw32_HOST_OS
    SetLastError(saved_winerror);
#endif

    /* We might have GC'd, mark the TSO dirty again */
    dirty_TSO(cap,tso);
    dirty_STACK(cap,tso->stackobj);

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
    tso->flags |= TSO_LOCKED; // we requested explicit affinity; don't
			      // move this thread from now on.
#if defined(THREADED_RTS)
    cpu %= enabled_capabilities;
    if (cpu == cap->no) {
	appendToRunQueue(cap,tso);
    } else {
        migrateThread(cap, tso, &capabilities[cpu]);
    }
#else
    appendToRunQueue(cap,tso);
#endif
}

void
scheduleWaitThread (StgTSO* tso, /*[out]*/HaskellObj* ret, Capability **pcap)
{
    Task *task;
    DEBUG_ONLY( StgThreadID id );
    Capability *cap;

    cap = *pcap;

    // We already created/initialised the Task
    task = cap->running_task;

    // This TSO is now a bound thread; make the Task and TSO
    // point to each other.
    tso->bound = task->incall;
    tso->cap = cap;

    task->incall->tso = tso;
    task->incall->ret = ret;
    task->incall->stat = NoStatus;

    appendToRunQueue(cap,tso);

    DEBUG_ONLY( id = tso->id );
    debugTrace(DEBUG_sched, "new bound thread (%lu)", (unsigned long)id);

    cap = schedule(cap,task);

    ASSERT(task->incall->stat != NoStatus);
    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    debugTrace(DEBUG_sched, "bound thread (%lu) finished", (unsigned long)id);
    *pcap = cap;
}

/* ----------------------------------------------------------------------------
 * Starting Tasks
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
void scheduleWorker (Capability *cap, Task *task)
{
    // schedule() runs without a lock.
    cap = schedule(cap,task);

    // On exit from schedule(), we have a Capability, but possibly not
    // the same one we started with.

    // During shutdown, the requirement is that after all the
    // Capabilities are shut down, all workers that are shutting down
    // have finished workerTaskStop().  This is why we hold on to
    // cap->lock until we've finished workerTaskStop() below.
    //
    // There may be workers still involved in foreign calls; those
    // will just block in waitForReturnCapability() because the
    // Capability has been shut down.
    //
    ACQUIRE_LOCK(&cap->lock);
    releaseCapability_(cap,rtsFalse);
    workerTaskStop(task);
    RELEASE_LOCK(&cap->lock);
}
#endif

/* ---------------------------------------------------------------------------
 * Start new worker tasks on Capabilities from--to
 * -------------------------------------------------------------------------- */

static void
startWorkerTasks (nat from USED_IF_THREADS, nat to USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;
    Capability *cap;

    for (i = from; i < to; i++) {
        cap = &capabilities[i];
        ACQUIRE_LOCK(&cap->lock);
        startWorkerTask(cap);
        RELEASE_LOCK(&cap->lock);
    }
#endif
}

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

  /*
   * Eagerly start one worker to run each Capability, except for
   * Capability 0.  The idea is that we're probably going to start a
   * bound thread on Capability 0 pretty soon, so we don't want a
   * worker task hogging it.
   */
  startWorkerTasks(1, n_capabilities);

  RELEASE_LOCK(&sched_mutex);

}

void
exitScheduler (rtsBool wait_foreign USED_IF_THREADS)
               /* see Capability.c, shutdownCapability() */
{
    Task *task = NULL;

    task = newBoundTask();

    // If we haven't killed all the threads yet, do it now.
    if (sched_state < SCHED_SHUTTING_DOWN) {
	sched_state = SCHED_INTERRUPTING;
        Capability *cap = task->cap;
        waitForReturnCapability(&cap,task);
        scheduleDoGC(&cap,task,rtsTrue);
        ASSERT(task->incall->tso == NULL);
        releaseCapability(cap);
    }
    sched_state = SCHED_SHUTTING_DOWN;

    shutdownCapabilities(task, wait_foreign);

    // debugBelch("n_failed_trygrab_idles = %d, n_idle_caps = %d\n",
    //            n_failed_trygrab_idles, n_idle_caps);

    boundTaskExiting(task);
}

void
freeScheduler( void )
{
    nat still_running;

    ACQUIRE_LOCK(&sched_mutex);
    still_running = freeTaskManager();
    // We can only free the Capabilities if there are no Tasks still
    // running.  We might have a Task about to return from a foreign
    // call into waitForReturnCapability(), for example (actually,
    // this should be the *only* thing that a still-running Task can
    // do at this point, and it will block waiting for the
    // Capability).
    if (still_running == 0) {
        freeCapabilities();
        if (n_capabilities != 1) {
            stgFree(capabilities);
        }
    }
    RELEASE_LOCK(&sched_mutex);
#if defined(THREADED_RTS)
    closeMutex(&sched_mutex);
#endif
}

void markScheduler (evac_fn evac USED_IF_NOT_THREADS, 
                    void *user USED_IF_NOT_THREADS)
{
#if !defined(THREADED_RTS)
    evac(user, (StgClosure **)(void *)&blocked_queue_hd);
    evac(user, (StgClosure **)(void *)&blocked_queue_tl);
    evac(user, (StgClosure **)(void *)&sleeping_queue);
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
    Capability *cap = NULL;

    // We must grab a new Task here, because the existing Task may be
    // associated with a particular Capability, and chained onto the 
    // suspended_ccalls queue.
    task = newBoundTask();
    
    // TODO: do we need to traceTask*() here?

    waitForReturnCapability(&cap,task);
    scheduleDoGC(&cap,task,force_major);
    releaseCapability(cap);
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

/* ---------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   ------------------------------------------------------------------------ */

void
interruptStgRts(void)
{
    sched_state = SCHED_INTERRUPTING;
    interruptAllCapabilities();
#if defined(THREADED_RTS)
    wakeUpRts();
#endif
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

#if defined(THREADED_RTS)
void wakeUpRts(void)
{
    // This forces the IO Manager thread to wakeup, which will
    // in turn ensure that some OS thread wakes up and runs the
    // scheduler loop, which will cause a GC and deadlock check.
    ioManagerWakeup();
}
#endif

/* -----------------------------------------------------------------------------
   Deleting threads

   This is used for interruption (^C) and forking, and corresponds to
   raising an exception but without letting the thread catch the
   exception.
   -------------------------------------------------------------------------- */

static void
deleteThread (Capability *cap STG_UNUSED, StgTSO *tso)
{
    // NOTE: must only be called on a TSO that we have exclusive
    // access to, because we will call throwToSingleThreaded() below.
    // The TSO must be on the run queue of the Capability we own, or 
    // we must own all Capabilities.

    if (tso->why_blocked != BlockedOnCCall &&
	tso->why_blocked != BlockedOnCCall_Interruptible) {
        throwToSingleThreaded(tso->cap,tso,NULL);
    }
}

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void
deleteThread_(Capability *cap, StgTSO *tso)
{ // for forkProcess only:
  // like deleteThread(), but we delete threads in foreign calls, too.

    if (tso->why_blocked == BlockedOnCCall ||
	tso->why_blocked == BlockedOnCCall_Interruptible) {
	tso->what_next = ThreadKilled;
	appendToRunQueue(tso->cap, tso);
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
    p = tso->stackobj->sp;
    while(1) {
	info = get_ret_itbl((StgClosure *)p);
	next = p + stack_frame_sizeW((StgClosure *)p);
	switch (info->i.type) {
	    
	case UPDATE_FRAME:
	    // Only create raise_closure if we need to.
	    if (raise_closure == NULL) {
		raise_closure = 
		    (StgThunk *)allocate(cap,sizeofW(StgThunk)+1);
                SET_HDR(raise_closure, &stg_raise_info, cap->r.rCCCS);
		raise_closure->payload[0] = exception;
	    }
            updateThunk(cap, tso, ((StgUpdateFrame *)p)->updatee,
                        (StgClosure *)raise_closure);
	    p = next;
	    continue;

        case ATOMICALLY_FRAME:
	    debugTrace(DEBUG_stm, "found ATOMICALLY_FRAME at %p", p);
            tso->stackobj->sp = p;
            return ATOMICALLY_FRAME;
	    
	case CATCH_FRAME:
            tso->stackobj->sp = p;
	    return CATCH_FRAME;

        case CATCH_STM_FRAME:
	    debugTrace(DEBUG_stm, "found CATCH_STM_FRAME at %p", p);
            tso->stackobj->sp = p;
            return CATCH_STM_FRAME;
	    
        case UNDERFLOW_FRAME:
            tso->stackobj->sp = p;
            threadStackUnderflow(cap,tso);
            p = tso->stackobj->sp;
            continue;

        case STOP_FRAME:
            tso->stackobj->sp = p;
	    return STOP_FRAME;

        case CATCH_RETRY_FRAME: {
            StgTRecHeader *trec = tso -> trec;
            StgTRecHeader *outer = trec -> enclosing_trec;
            debugTrace(DEBUG_stm,
                       "found CATCH_RETRY_FRAME at %p during raise", p);
            debugTrace(DEBUG_stm, "trec=%p outer=%p", trec, outer);
            stmAbortTransaction(cap, trec);
            stmFreeAbortedTRec(cap, trec);
            tso -> trec = outer;
            p = next;
            continue;
        }

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
findRetryFrameHelper (Capability *cap, StgTSO *tso)
{
  StgPtr           p, next;
  StgRetInfoTable *info;

  p = tso->stackobj->sp;
  while (1) {
    info = get_ret_itbl((StgClosure *)p);
    next = p + stack_frame_sizeW((StgClosure *)p);
    switch (info->i.type) {
      
    case ATOMICALLY_FRAME:
	debugTrace(DEBUG_stm,
		   "found ATOMICALLY_FRAME at %p during retry", p);
        tso->stackobj->sp = p;
	return ATOMICALLY_FRAME;
      
    case CATCH_RETRY_FRAME:
	debugTrace(DEBUG_stm,
                   "found CATCH_RETRY_FRAME at %p during retry", p);
        tso->stackobj->sp = p;
	return CATCH_RETRY_FRAME;
      
    case CATCH_STM_FRAME: {
        StgTRecHeader *trec = tso -> trec;
	StgTRecHeader *outer = trec -> enclosing_trec;
        debugTrace(DEBUG_stm,
		   "found CATCH_STM_FRAME at %p during retry", p);
        debugTrace(DEBUG_stm, "trec=%p outer=%p", trec, outer);
        stmAbortTransaction(cap, trec);
        stmFreeAbortedTRec(cap, trec);
	tso -> trec = outer;
        p = next; 
        continue;
    }
      
    case UNDERFLOW_FRAME:
        tso->stackobj->sp = p;
        threadStackUnderflow(cap,tso);
        p = tso->stackobj->sp;
        continue;

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
    generation *gen;

    for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
	next = tso->global_link;

        gen = Bdescr((P_)tso)->gen;
	tso->global_link = gen->threads;
	gen->threads = tso;

	debugTrace(DEBUG_sched, "resurrecting thread %lu", (unsigned long)tso->id);
	
	// Wake up the thread on the Capability it was last on
	cap = tso->cap;
	
	switch (tso->why_blocked) {
	case BlockedOnMVar:
	case BlockedOnMVarRead:
	    /* Called by GC - sched_mutex lock is currently held. */
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)blockedIndefinitelyOnMVar_closure);
	    break;
	case BlockedOnBlackHole:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)nonTermination_closure);
	    break;
	case BlockedOnSTM:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)blockedIndefinitelyOnSTM_closure);
	    break;
	case NotBlocked:
	    /* This might happen if the thread was blocked on a black hole
	     * belonging to a thread that we've just woken up (raiseAsync
	     * can wake up threads, remember...).
	     */
	    continue;
        case BlockedOnMsgThrowTo:
            // This can happen if the target is masking, blocks on a
            // black hole, and then is found to be unreachable.  In
            // this case, we want to let the target wake up and carry
            // on, and do nothing to this thread.
            continue;
	default:
	    barf("resurrectThreads: thread blocked in a strange way: %d",
                 tso->why_blocked);
	}
    }
}
