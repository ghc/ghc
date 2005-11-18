/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2003-2005
 *
 * Capabilities
 *
 * A Capability represent the token required to execute STG code,
 * and all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a
 * register (BaseReg; actually it is a pointer to cap->r).
 *
 * Only in an SMP build will there be multiple capabilities, for
 * the threaded RTS and other non-threaded builds, there is only
 * one global capability, namely MainCapability.
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "OSThreads.h"
#include "Capability.h"
#include "Schedule.h"
#include "Sparks.h"

#if !defined(SMP)
Capability MainCapability;     // for non-SMP, we have one global capability
#endif

nat n_capabilities;
Capability *capabilities = NULL;

// Holds the Capability which last became free.  This is used so that
// an in-call has a chance of quickly finding a free Capability.
// Maintaining a global free list of Capabilities would require global
// locking, so we don't do that.
Capability *last_free_capability;

#ifdef SMP
#define UNUSED_IF_NOT_SMP
#else
#define UNUSED_IF_NOT_SMP STG_UNUSED
#endif

#ifdef RTS_USER_SIGNALS
#define UNUSED_IF_NOT_THREADS
#else
#define UNUSED_IF_NOT_THREADS STG_UNUSED
#endif


STATIC_INLINE rtsBool
globalWorkToDo (void)
{
    return blackholes_need_checking
	|| interrupted
#if defined(RTS_USER_SIGNALS)
	|| signals_pending()
#endif
	;
}

#if defined(THREADED_RTS)
STATIC_INLINE rtsBool
anyWorkForMe( Capability *cap, Task *task )
{
    // If the run queue is not empty, then we only wake up the guy who
    // can run the thread at the head, even if there is some other
    // reason for this task to run (eg. interrupted=rtsTrue).
    if (!emptyRunQueue(cap)) {
	if (cap->run_queue_hd->bound == NULL) {
	    return (task->tso == NULL);
	} else {
	    return (cap->run_queue_hd->bound == task);
	}
    } else if (task->tso == NULL && !emptySparkPoolCap(cap)) {
	return rtsTrue;
    }
    return globalWorkToDo();
}
#endif

/* -----------------------------------------------------------------------------
 * Manage the returning_tasks lists.
 *
 * These functions require cap->lock
 * -------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
STATIC_INLINE void
newReturningTask (Capability *cap, Task *task)
{
    ASSERT_LOCK_HELD(&cap->lock);
    ASSERT(task->return_link == NULL);
    if (cap->returning_tasks_hd) {
	ASSERT(cap->returning_tasks_tl->return_link == NULL);
	cap->returning_tasks_tl->return_link = task;
    } else {
	cap->returning_tasks_hd = task;
    }
    cap->returning_tasks_tl = task;
}

STATIC_INLINE Task *
popReturningTask (Capability *cap)
{
    ASSERT_LOCK_HELD(&cap->lock);
    Task *task;
    task = cap->returning_tasks_hd;
    ASSERT(task);
    cap->returning_tasks_hd = task->return_link;
    if (!cap->returning_tasks_hd) {
	cap->returning_tasks_tl = NULL;
    }
    task->return_link = NULL;
    return task;
}
#endif

/* ----------------------------------------------------------------------------
 * Initialisation
 *
 * The Capability is initially marked not free.
 * ------------------------------------------------------------------------- */

static void
initCapability( Capability *cap, nat i )
{
    nat g;

    cap->no = i;
    cap->in_haskell        = rtsFalse;

    cap->run_queue_hd      = END_TSO_QUEUE;
    cap->run_queue_tl      = END_TSO_QUEUE;

#if defined(THREADED_RTS)
    initMutex(&cap->lock);
    cap->running_task      = NULL; // indicates cap is free
    cap->spare_workers     = NULL;
    cap->suspended_ccalling_tasks = NULL;
    cap->returning_tasks_hd = NULL;
    cap->returning_tasks_tl = NULL;
#endif

    cap->f.stgGCEnter1     = (F_)__stg_gc_enter_1;
    cap->f.stgGCFun        = (F_)__stg_gc_fun;

    cap->mut_lists  = stgMallocBytes(sizeof(bdescr *) *
				     RtsFlags.GcFlags.generations,
				     "initCapability");

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	cap->mut_lists[g] = NULL;
    }
}

/* ---------------------------------------------------------------------------
 * Function:  initCapabilities()
 *
 * Purpose:   set up the Capability handling. For the SMP build,
 *            we keep a table of them, the size of which is
 *            controlled by the user via the RTS flag -N.
 *
 * ------------------------------------------------------------------------- */
void
initCapabilities( void )
{
#if defined(SMP)
    nat i,n;

    n_capabilities = n = RtsFlags.ParFlags.nNodes;
    capabilities = stgMallocBytes(n * sizeof(Capability), "initCapabilities");

    for (i = 0; i < n; i++) {
	initCapability(&capabilities[i], i);
    }

    IF_DEBUG(scheduler, sched_belch("allocated %d capabilities", n));
#else
    n_capabilities = 1;
    capabilities = &MainCapability;
    initCapability(&MainCapability, 0);
#endif

    // There are no free capabilities to begin with.  We will start
    // a worker Task to each Capability, which will quickly put the
    // Capability on the free list when it finds nothing to do.
    last_free_capability = &capabilities[0];
}

/* ----------------------------------------------------------------------------
 * Give a Capability to a Task.  The task must currently be sleeping
 * on its condition variable.
 *
 * Requires cap->lock (modifies cap->running_task).
 *
 * When migrating a Task, the migrater must take task->lock before
 * modifying task->cap, to synchronise with the waking up Task.
 * Additionally, the migrater should own the Capability (when
 * migrating the run queue), or cap->lock (when migrating
 * returning_workers).
 *
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
STATIC_INLINE void
giveCapabilityToTask (Capability *cap, Task *task)
{
    ASSERT_LOCK_HELD(&cap->lock);
    ASSERT(task->cap == cap);
    // We are not modifying task->cap, so we do not need to take task->lock.
    IF_DEBUG(scheduler,
	     sched_belch("passing capability %d to %s %p",
			 cap->no, task->tso ? "bound task" : "worker",
			 (void *)task->id));
    ACQUIRE_LOCK(&task->lock);
    task->wakeup = rtsTrue;
    // the wakeup flag is needed because signalCondition() doesn't
    // flag the condition if the thread is already runniing, but we want
    // it to be sticky.
    signalCondition(&task->cond);
    RELEASE_LOCK(&task->lock);
}
#endif

/* ----------------------------------------------------------------------------
 * Function:  releaseCapability(Capability*)
 *
 * Purpose:   Letting go of a capability. Causes a
 *            'returning worker' thread or a 'waiting worker'
 *            to wake up, in that order.
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
void
releaseCapability_ (Capability* cap)
{
    Task *task;

    task = cap->running_task;

    ASSERT_CAPABILITY_INVARIANTS(cap,task);

    cap->running_task = NULL;

    // Check to see whether a worker thread can be given
    // the go-ahead to return the result of an external call..
    if (cap->returning_tasks_hd != NULL) {
	giveCapabilityToTask(cap,cap->returning_tasks_hd);
	// The Task pops itself from the queue (see waitForReturnCapability())
	return;
    }

    // If the next thread on the run queue is a bound thread,
    // give this Capability to the appropriate Task.
    if (!emptyRunQueue(cap) && cap->run_queue_hd->bound) {
	// Make sure we're not about to try to wake ourselves up
	ASSERT(task != cap->run_queue_hd->bound);
	task = cap->run_queue_hd->bound;
	giveCapabilityToTask(cap,task);
	return;
    }

    // If we have an unbound thread on the run queue, or if there's
    // anything else to do, give the Capability to a worker thread.
    if (!emptyRunQueue(cap) || !emptySparkPoolCap(cap) || globalWorkToDo()) {
	if (cap->spare_workers) {
	    giveCapabilityToTask(cap,cap->spare_workers);
	    // The worker Task pops itself from the queue;
	    return;
	}

	// Create a worker thread if we don't have one.  If the system
	// is interrupted, we only create a worker task if there
	// are threads that need to be completed.  If the system is
	// shutting down, we never create a new worker.
	if (!shutting_down_scheduler) {
	    IF_DEBUG(scheduler,
		     sched_belch("starting new worker on capability %d", cap->no));
	    startWorkerTask(cap, workerStart);
	    return;
	}
    }

    last_free_capability = cap;
    IF_DEBUG(scheduler, sched_belch("freeing capability %d", cap->no));
}

void
releaseCapability (Capability* cap UNUSED_IF_NOT_THREADS)
{
    ACQUIRE_LOCK(&cap->lock);
    releaseCapability_(cap);
    RELEASE_LOCK(&cap->lock);
}

static void
releaseCapabilityAndQueueWorker (Capability* cap UNUSED_IF_NOT_THREADS)
{
    Task *task;

    ACQUIRE_LOCK(&cap->lock);

    task = cap->running_task;

    // If the current task is a worker, save it on the spare_workers
    // list of this Capability.  A worker can mark itself as stopped,
    // in which case it is not replaced on the spare_worker queue.
    // This happens when the system is shutting down (see
    // Schedule.c:workerStart()).
    // Also, be careful to check that this task hasn't just exited
    // Haskell to do a foreign call (task->suspended_tso).
    if (!isBoundTask(task) && !task->stopped && !task->suspended_tso) {
	task->next = cap->spare_workers;
	cap->spare_workers = task;
    }
    // Bound tasks just float around attached to their TSOs.

    releaseCapability_(cap);

    RELEASE_LOCK(&cap->lock);
}
#endif

/* ----------------------------------------------------------------------------
 * waitForReturnCapability( Task *task )
 *
 * Purpose:  when an OS thread returns from an external call,
 * it calls waitForReturnCapability() (via Schedule.resumeThread())
 * to wait for permission to enter the RTS & communicate the
 * result of the external call back to the Haskell thread that
 * made it.
 *
 * ------------------------------------------------------------------------- */
void
waitForReturnCapability (Capability **pCap,
			 Task *task UNUSED_IF_NOT_THREADS)
{
#if !defined(THREADED_RTS)

    MainCapability.running_task = task;
    task->cap = &MainCapability;
    *pCap = &MainCapability;

#else
    Capability *cap = *pCap;

    if (cap == NULL) {
	// Try last_free_capability first
	cap = last_free_capability;
	if (!cap->running_task) {
	    nat i;
	    // otherwise, search for a free capability
	    for (i = 0; i < n_capabilities; i++) {
		cap = &capabilities[i];
		if (!cap->running_task) {
		    break;
		}
	    }
	    // Can't find a free one, use last_free_capability.
	    cap = last_free_capability;
	}

	// record the Capability as the one this Task is now assocated with.
	task->cap = cap;

    } else {
	ASSERT(task->cap == cap);
    }

    ACQUIRE_LOCK(&cap->lock);

    IF_DEBUG(scheduler,
	     sched_belch("returning; I want capability %d", cap->no));

    if (!cap->running_task) {
	// It's free; just grab it
	cap->running_task = task;
	RELEASE_LOCK(&cap->lock);
    } else {
	newReturningTask(cap,task);
	RELEASE_LOCK(&cap->lock);

	for (;;) {
	    ACQUIRE_LOCK(&task->lock);
	    // task->lock held, cap->lock not held
	    if (!task->wakeup) waitCondition(&task->cond, &task->lock);
	    cap = task->cap;
	    task->wakeup = rtsFalse;
	    RELEASE_LOCK(&task->lock);

	    // now check whether we should wake up...
	    ACQUIRE_LOCK(&cap->lock);
	    if (cap->running_task == NULL) {
		if (cap->returning_tasks_hd != task) {
		    giveCapabilityToTask(cap,cap->returning_tasks_hd);
		    RELEASE_LOCK(&cap->lock);
		    continue;
		}
		cap->running_task = task;
		popReturningTask(cap);
		RELEASE_LOCK(&cap->lock);
		break;
	    }
	    RELEASE_LOCK(&cap->lock);
	}

    }

    ASSERT_CAPABILITY_INVARIANTS(cap,task);

    IF_DEBUG(scheduler,
	     sched_belch("returning; got capability %d", cap->no));

    *pCap = cap;
#endif
}

#if defined(THREADED_RTS)
/* ----------------------------------------------------------------------------
 * yieldCapability
 * ------------------------------------------------------------------------- */

void
yieldCapability (Capability** pCap, Task *task)
{
    Capability *cap = *pCap;

    // The fast path; no locking
    if ( cap->returning_tasks_hd == NULL && anyWorkForMe(cap,task) )
	return;

    while ( cap->returning_tasks_hd != NULL || !anyWorkForMe(cap,task) ) {
	IF_DEBUG(scheduler, sched_belch("giving up capability %d", cap->no));

	// We must now release the capability and wait to be woken up
	// again.
	releaseCapabilityAndQueueWorker(cap);

	for (;;) {
	    ACQUIRE_LOCK(&task->lock);
	    // task->lock held, cap->lock not held
	    if (!task->wakeup) waitCondition(&task->cond, &task->lock);
	    cap = task->cap;
	    task->wakeup = rtsFalse;
	    RELEASE_LOCK(&task->lock);

	    IF_DEBUG(scheduler, sched_belch("woken up on capability %d", cap->no));
	    ACQUIRE_LOCK(&cap->lock);
	    if (cap->running_task != NULL) {
		RELEASE_LOCK(&cap->lock);
		continue;
	    }

	    if (task->tso == NULL) {
		ASSERT(cap->spare_workers != NULL);
		// if we're not at the front of the queue, release it
		// again.  This is unlikely to happen.
		if (cap->spare_workers != task) {
		    giveCapabilityToTask(cap,cap->spare_workers);
		    RELEASE_LOCK(&cap->lock);
		    continue;
		}
		cap->spare_workers = task->next;
		task->next = NULL;
	    }
	    cap->running_task = task;
	    RELEASE_LOCK(&cap->lock);
	    break;
	}

	IF_DEBUG(scheduler, sched_belch("got capability %d", cap->no));
	ASSERT(cap->running_task == task);
    }

    *pCap = cap;

    ASSERT_CAPABILITY_INVARIANTS(cap,task);

    return;
}

/* ----------------------------------------------------------------------------
 * prodCapabilities
 *
 * Used to indicate that the interrupted flag is now set, or some
 * other global condition that might require waking up a Task on each
 * Capability.
 * ------------------------------------------------------------------------- */

static void
prodCapabilities(rtsBool all)
{
    nat i;
    Capability *cap;
    Task *task;

    for (i=0; i < n_capabilities; i++) {
	cap = &capabilities[i];
	ACQUIRE_LOCK(&cap->lock);
	if (!cap->running_task) {
	    if (cap->spare_workers) {
		task = cap->spare_workers;
		ASSERT(!task->stopped);
		giveCapabilityToTask(cap,task);
		if (!all) {
		    RELEASE_LOCK(&cap->lock);
		    return;
		}
	    }
	}
	RELEASE_LOCK(&cap->lock);
    }
}

void
prodAllCapabilities (void)
{
    prodCapabilities(rtsTrue);
}

/* ----------------------------------------------------------------------------
 * prodOneCapability
 *
 * Like prodAllCapabilities, but we only require a single Task to wake
 * up in order to service some global event, such as checking for
 * deadlock after some idle time has passed.
 * ------------------------------------------------------------------------- */

void
prodOneCapability (void)
{
    prodCapabilities(rtsFalse);
}

/* ----------------------------------------------------------------------------
 * shutdownCapability
 *
 * At shutdown time, we want to let everything exit as cleanly as
 * possible.  For each capability, we let its run queue drain, and
 * allow the workers to stop.
 *
 * This function should be called when interrupted and
 * shutting_down_scheduler = rtsTrue, thus any worker that wakes up
 * will exit the scheduler and call taskStop(), and any bound thread
 * that wakes up will return to its caller.  Runnable threads are
 * killed.
 *
 * ------------------------------------------------------------------------- */

void
shutdownCapability (Capability *cap, Task *task)
{
    nat i;

    ASSERT(interrupted && shutting_down_scheduler);

    task->cap = cap;

    for (i = 0; i < 50; i++) {
	IF_DEBUG(scheduler, sched_belch("shutting down capability %d, attempt %d", cap->no, i));
	ACQUIRE_LOCK(&cap->lock);
	if (cap->running_task) {
	    RELEASE_LOCK(&cap->lock);
	    IF_DEBUG(scheduler, sched_belch("not owner, yielding"));
	    yieldThread();
	    continue;
	}
	cap->running_task = task;
	if (!emptyRunQueue(cap) || cap->spare_workers) {
	    IF_DEBUG(scheduler, sched_belch("runnable threads or workers still alive, yielding"));
	    releaseCapability_(cap); // this will wake up a worker
	    RELEASE_LOCK(&cap->lock);
	    yieldThread();
	    continue;
	}
	IF_DEBUG(scheduler, sched_belch("capability %d is stopped.", cap->no));
	RELEASE_LOCK(&cap->lock);
	break;
    }
    // we now have the Capability, its run queue and spare workers
    // list are both empty.
}

/* ----------------------------------------------------------------------------
 * tryGrabCapability
 *
 * Attempt to gain control of a Capability if it is free.
 *
 * ------------------------------------------------------------------------- */

rtsBool
tryGrabCapability (Capability *cap, Task *task)
{
    if (cap->running_task != NULL) return rtsFalse;
    ACQUIRE_LOCK(&cap->lock);
    if (cap->running_task != NULL) {
	RELEASE_LOCK(&cap->lock);
	return rtsFalse;
    }
    task->cap = cap;
    cap->running_task = task;
    RELEASE_LOCK(&cap->lock);
    return rtsTrue;
}


#endif /* THREADED_RTS */


