/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Schedule.c
 * (RTS internal scheduler interface)
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "rts/OSThreads.h"
#include "Capability.h"
#include "Trace.h"

#include "BeginPrivate.h"

/* initScheduler(), exitScheduler()
 * Called from STG :  no
 * Locks assumed   :  none
 */
void initScheduler (void);
void exitScheduler (bool wait_foreign);
void freeScheduler (void);

// Place a new thread on the run queue of the current Capability
void scheduleThread (Capability *cap, StgTSO *tso);

// Place a new thread on the run queue of the current Capability
// at the front of the queue.
void scheduleThreadNow (Capability *cap, StgTSO *tso);

// Place a new thread on the run queue of a specified Capability
// (cap is the currently owned Capability, cpu is the number of
// the desired Capability).
void scheduleThreadOn(Capability *cap, StgWord cpu, StgTSO *tso);

/* wakeUpRts()
 *
 * Causes an OS thread to wake up and run the scheduler, if necessary.
 */
#if defined(THREADED_RTS)
void wakeUpRts(void);
#endif

/* raiseExceptionHelper */
StgWord raiseExceptionHelper (StgRegTable *reg, StgTSO *tso, StgClosure *exception);

/* findRetryFrameHelper */
StgWord findRetryFrameHelper (Capability *cap, StgTSO *tso);

/* findAtomicallyFrameHelper */
StgWord findAtomicallyFrameHelper (Capability *cap, StgTSO *tso);

/* Entry point for a new worker */
void scheduleWorker (Capability *cap, Task *task);

#if defined(THREADED_RTS)
void stopAllCapabilitiesWith (Capability **pCap, Task *task, SyncType sync_type);
void stopAllCapabilities (Capability **pCap, Task *task);
void releaseAllCapabilities(uint32_t n, Capability *keep_cap, Task *task);
#endif

/* The state of the scheduler.  This is used to control the sequence
 * of events during shutdown.  See Note [shutdown] in Schedule.c.
 */
enum SchedState {
    SCHED_RUNNING       = 0,  /* running as normal */
    SCHED_INTERRUPTING  = 1,  /* before threads are deleted */
    SCHED_SHUTTING_DOWN = 2,  /* final shutdown */
};

extern StgWord sched_state;

INLINE_HEADER void setSchedState(enum SchedState ss)
{
    SEQ_CST_STORE_ALWAYS(&sched_state, (StgWord) ss);
}

INLINE_HEADER enum SchedState getSchedState(void)
{
    return (enum SchedState) SEQ_CST_LOAD_ALWAYS(&sched_state);
}

/* Note [Recent activity]
  ~~~~~~~~~~~~~~~~~~~~~~~

Initial state: ACTIVITY_YES, in initScheduler

In state ACTIVITY_YES:
 - transition to ACTIVITY_MAYBE_NO, in Timer.c handle_tick()
 - transition to ACTIVITY_YES if scheduler runs thread
   (This transition uses setRecentActivity unnecessarily!)

In state ACTIVITY_MAYBE_NO:
 - transition to ACTIVITY_INACTIVE, if idle GC is on, in Timer.c handle_tick()
   and also calls wakeUpRts() to interrupt I/O manager sleeping
 - transition to ACTIVITY_DONE_GC, if idle GC is off, in Timer.c handle_tick()
   and also calls stopTimer()
 - transition to ACTIVITY_YES if scheduler runs thread
 - transition to ACTIVITY_YES is GC completes
   (perhaps the timer went off during GC and transitioned to ACTIVITY_MAYBE_NO
    but still threads would be runnable)

In state ACTIVITY_INACTIVE:
 - remain in ACTIVITY_INACTIVE if the scheduler runs thread
   (this is for behaviour in the library I/O managers, and could be avoided
    for the in-RTS ones)
 - transition to ACTIVITY_DONE_GC if scheduler has no runnable threads
   performs deadlock detection GC, which calls stopTimer()
   done in scheduleDoGC

In state ACTIVITY_DONE_GC:
 - transition to ACTIVITY_YES if scheduler runs thread
   and call startTimer()

Note that in the state ACTIVITY_DONE_GC, the timer is off. It is turned off on
the transition to ACTIVITY_DONE_GC, in handle_tick() and turned on again in
the transition to ACTIVITY_YES in the scheduler.


/*
 * flag that tracks whether we have done any execution in this time
 * slice, and controls the disabling of the interval timer.
 *
 * The timer interrupt transitions ACTIVITY_YES into
 * ACTIVITY_MAYBE_NO, waits for RtsFlags.GcFlags.idleGCDelayTime,
 * and then:
 *   - if idle GC is on, set ACTIVITY_INACTIVE and wakeUpRts()
 *   - if idle GC is off, set ACTIVITY_DONE_GC and stopTimer()
 *
 * If the scheduler finds ACTIVITY_INACTIVE, then it sets
 * ACTIVITY_DONE_GC, performs the GC and calls stopTimer().
 *
 * If the scheduler finds ACTIVITY_DONE_GC and it has a thread to run,
 * it enables the timer again with startTimer().
 */
enum RecentActivity {
    // the RTS is active
    ACTIVITY_YES      = 0,
    // no activity since the last timer signal
    ACTIVITY_MAYBE_NO = 1,
    // RtsFlags.GcFlags.idleGCDelayTime has passed with no activity
    ACTIVITY_INACTIVE = 2,
    // like ACTIVITY_INACTIVE, but we've done a GC too (if idle GC is
    // enabled) and the interval timer is now turned off.
    ACTIVITY_DONE_GC  = 3,
};

/* Recent activity flag.
 * Locks required  : Transition from MAYBE_NO to INACTIVE
 * happens in the timer signal, so it is atomic.  Transition from
 * INACTIVE to DONE_GC happens under sched_mutex.  No lock required
 * to set it to ACTIVITY_YES.
 *
 * N.B. we must always use atomics here since even in the non-threaded runtime
 * the timer may be provided via a signal.
 */
extern StgWord recent_activity;

INLINE_HEADER enum RecentActivity
setRecentActivity(enum RecentActivity new_value)
{
    StgWord old = SEQ_CST_XCHG_ALWAYS((StgPtr) &recent_activity, (StgWord) new_value);
    return (enum RecentActivity) old;
}

INLINE_HEADER enum RecentActivity
getRecentActivity(void)
{
    return (enum RecentActivity) RELAXED_LOAD_ALWAYS(&recent_activity);
}

extern bool heap_overflow;

#if defined(THREADED_RTS)
extern Mutex sched_mutex;
#endif

/* Called by shutdown_handler(). */
void interruptStgRts (void);

void resurrectThreads (StgTSO *);

/* -----------------------------------------------------------------------------
 * Some convenient macros/inline functions...
 */

#if !IN_STG_CODE

/* END_TSO_QUEUE and friends now defined in rts/include/stg/MiscClosures.h */

/* Add a thread to the end of the run queue.
 * NOTE: tso->link should be END_TSO_QUEUE before calling this macro.
 * ASSUMES: cap->running_task is the current task.
 */
void appendToRunQueue (Capability *cap, StgTSO *tso);

/* Push a thread on the beginning of the run queue.
 * ASSUMES: cap->running_task is the current task.
 */
void pushOnRunQueue (Capability *cap, StgTSO *tso);

/* Pop the first thread off the runnable queue.
 */
StgTSO *popRunQueue (Capability *cap);

INLINE_HEADER StgTSO *
peekRunQueue (Capability *cap)
{
    return cap->run_queue_hd;
}

void promoteInRunQueue (Capability *cap, StgTSO *tso);

INLINE_HEADER bool
emptyRunQueue(Capability *cap)
{
    // Can only be called by the task owning the capability.
    TSAN_ANNOTATE_BENIGN_RACE(&cap->n_run_queue, "emptyRunQueue");
    return cap->n_run_queue == 0;
}

INLINE_HEADER void
truncateRunQueue(Capability *cap)
{
    // Can only be called by the task owning the capability.
    TSAN_ANNOTATE_BENIGN_RACE(&cap->run_queue_hd, "truncateRunQueue");
    TSAN_ANNOTATE_BENIGN_RACE(&cap->run_queue_tl, "truncateRunQueue");
    TSAN_ANNOTATE_BENIGN_RACE(&cap->n_run_queue, "truncateRunQueue");
    cap->run_queue_hd = END_TSO_QUEUE;
    cap->run_queue_tl = END_TSO_QUEUE;
    cap->n_run_queue = 0;
}

#endif /* !IN_STG_CODE */

#include "EndPrivate.h"
