/* ---------------------------------------------------------------------------
 * (c) The GHC Team, 2003
 *
 * Capabilities
 *
 * A Capability represent the token required to execute STG code,
 * and all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a
 * register (BaseReg).
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
#include "Schedule.h"  /* to get at EMPTY_RUN_QUEUE() */
#if defined(SMP)
#include "Hash.h"
#endif

#if !defined(SMP)
Capability MainCapability;     /* for non-SMP, we have one global capability */
#endif

Capability *capabilities = NULL;
nat rts_n_free_capabilities;

#if defined(RTS_SUPPORTS_THREADS)

/* returning_worker_cond: when a worker thread returns from executing an
 * external call, it needs to wait for an RTS Capability before passing
 * on the result of the call to the Haskell thread that made it.
 * 
 * returning_worker_cond is signalled in Capability.releaseCapability().
 *
 */
Condition returning_worker_cond = INIT_COND_VAR;

/*
 * To avoid starvation of threads blocked on worker_thread_cond,
 * the task(s) that enter the Scheduler will check to see whether
 * there are one or more worker threads blocked waiting on
 * returning_worker_cond.
 */
nat rts_n_waiting_workers = 0;

/* thread_ready_cond: when signalled, a thread has become runnable for a
 * task to execute.
 *
 * In the non-SMP case, it also implies that the thread that is woken up has
 * exclusive access to the RTS and all its data structures (that are not
 * locked by the Scheduler's mutex).
 *
 * thread_ready_cond is signalled whenever
 *      !noCapabilities && !EMPTY_RUN_QUEUE().
 */
Condition thread_ready_cond = INIT_COND_VAR;

/*
 * To be able to make an informed decision about whether or not 
 * to create a new task when making an external call, keep track of
 * the number of tasks currently blocked waiting on thread_ready_cond.
 * (if > 0 => no need for a new task, just unblock an existing one).
 *
 * waitForWorkCapability() takes care of keeping it up-to-date;
 * Task.startTask() uses its current value.
 */
nat rts_n_waiting_tasks = 0;
#endif

#if defined(SMP)
/*
 * Free capability list. 
 */
Capability *free_capabilities;

/* 
 * Maps OSThreadId to Capability *
 */
HashTable *capability_hash;
#endif

#ifdef SMP
#define UNUSED_IF_NOT_SMP
#else
#define UNUSED_IF_NOT_SMP STG_UNUSED
#endif


#if defined(RTS_SUPPORTS_THREADS)
INLINE_HEADER rtsBool
ANY_WORK_FOR_ME( Condition *cond )
{
    // If the run queue is not empty, then we only wake up the guy who
    // can run the thread at the head, even if there is some other
    // reason for this task to run (eg. interrupted=rtsTrue).
    if (!EMPTY_RUN_QUEUE()) {
	if (run_queue_hd->main == NULL) {
	    return (cond == NULL);
	} else {
	    return (&run_queue_hd->main->bound_thread_cond == cond);
	}
    }

    return blackholes_need_checking
	|| interrupted
#if defined(RTS_USER_SIGNALS)
	|| signals_pending()
#endif
	;
}
#endif

INLINE_HEADER rtsBool
ANY_WORK_TO_DO(void) 
{
    return (!EMPTY_RUN_QUEUE() 
	    || interrupted
	    || blackholes_need_checking
#if defined(RTS_USER_SIGNALS)
	    || signals_pending()
#endif
	);
}

/* ----------------------------------------------------------------------------
   Initialisation
   ------------------------------------------------------------------------- */

static void
initCapability( Capability *cap )
{
    cap->r.rInHaskell      = rtsFalse;
    cap->f.stgGCEnter1     = (F_)__stg_gc_enter_1;
    cap->f.stgGCFun        = (F_)__stg_gc_fun;
}

/* ---------------------------------------------------------------------------
 * Function:  initCapabilities()
 *
 * Purpose:   set up the Capability handling. For the SMP build,
 *            we keep a table of them, the size of which is
 *            controlled by the user via the RTS flag RtsFlags.ParFlags.nNodes
 *
 * ------------------------------------------------------------------------- */
void
initCapabilities( void )
{
#if defined(SMP)
    nat i,n;

    n = RtsFlags.ParFlags.nNodes;
    capabilities = stgMallocBytes(n * sizeof(Capability), "initCapabilities");

    for (i = 0; i < n; i++) {
	initCapability(&capabilities[i]);
	capabilities[i].link = &capabilities[i+1];
    }
    capabilities[n-1].link = NULL;
    
    free_capabilities = &capabilities[0];
    rts_n_free_capabilities = n;

    capability_hash = allocHashTable();

    IF_DEBUG(scheduler, sched_belch("allocated %d capabilities", n));
#else
    capabilities = &MainCapability;
    initCapability(&MainCapability);
    rts_n_free_capabilities = 1;
#endif

#if defined(RTS_SUPPORTS_THREADS)
    initCondition(&returning_worker_cond);
    initCondition(&thread_ready_cond);
#endif
}

/* ----------------------------------------------------------------------------
   grabCapability( Capability** )

   (only externally visible when !RTS_SUPPORTS_THREADS.  In the
   threaded RTS, clients must use waitFor*Capability()).
   ------------------------------------------------------------------------- */

#if defined(RTS_SUPPORTS_THREADS)
static
#endif
void
grabCapability( Capability** cap )
{
#if defined(SMP)
  ASSERT(rts_n_free_capabilities > 0);
  *cap = free_capabilities;
  free_capabilities = (*cap)->link;
  rts_n_free_capabilities--;
  insertHashTable(capability_hash, osThreadId(), *cap);
#else
# if defined(RTS_SUPPORTS_THREADS)
  ASSERT(rts_n_free_capabilities == 1);
  rts_n_free_capabilities = 0;
# endif
  *cap = &MainCapability;
#endif
#if defined(RTS_SUPPORTS_THREADS)
  IF_DEBUG(scheduler, sched_belch("worker: got capability"));
#endif
}

/* ----------------------------------------------------------------------------
 * Function:  myCapability(void)
 *
 * Purpose:   Return the capability owned by the current thread.
 *            Should not be used if the current thread does not 
 *            hold a Capability.
 * ------------------------------------------------------------------------- */
Capability *
myCapability (void)
{
#if defined(SMP)
    return lookupHashTable(capability_hash, osThreadId());
#else
    return &MainCapability;
#endif
}

/* ----------------------------------------------------------------------------
 * Function:  releaseCapability(Capability*)
 *
 * Purpose:   Letting go of a capability. Causes a
 *            'returning worker' thread or a 'waiting worker'
 *            to wake up, in that order.
 * ------------------------------------------------------------------------- */

void
releaseCapability( Capability* cap UNUSED_IF_NOT_SMP )
{
    // Precondition: sched_mutex is held.
#if defined(RTS_SUPPORTS_THREADS)
#if !defined(SMP)
    ASSERT(rts_n_free_capabilities == 0);
#endif
#if defined(SMP)
    cap->link = free_capabilities;
    free_capabilities = cap;
    ASSERT(myCapability() == cap);
    removeHashTable(capability_hash, osThreadId(), NULL);
#endif
    // Check to see whether a worker thread can be given
    // the go-ahead to return the result of an external call..
    if (rts_n_waiting_workers > 0) {
	// Decrement the counter here to avoid livelock where the
	// thread that is yielding its capability will repeatedly
	// signal returning_worker_cond.
	rts_n_waiting_workers--;
	signalCondition(&returning_worker_cond);
	IF_DEBUG(scheduler, 
		 sched_belch("worker: released capability to returning worker"));
    } else {
	rts_n_free_capabilities++;
	IF_DEBUG(scheduler, sched_belch("worker: released capability"));
	threadRunnable();
    }
#endif
    return;
}

#if defined(RTS_SUPPORTS_THREADS)
/*
 * When a native thread has completed the execution of an external
 * call, it needs to communicate the result back. This is done
 * as follows:
 *
 *  - in resumeThread(), the thread calls waitForReturnCapability().
 *  - If no capabilities are readily available, waitForReturnCapability()
 *    increments a counter rts_n_waiting_workers, and blocks
 *    waiting for the condition returning_worker_cond to become
 *    signalled.
 *  - upon entry to the Scheduler, a worker thread checks the
 *    value of rts_n_waiting_workers. If > 0, the worker thread
 *    will yield its capability to let a returning worker thread
 *    proceed with returning its result -- this is done via
 *    yieldToReturningWorker().
 *  - the worker thread that yielded its capability then tries
 *    to re-grab a capability and re-enter the Scheduler.
 */

/* ----------------------------------------------------------------------------
 * waitForReturnCapability( Mutext *pMutex, Capability** )
 *
 * Purpose:  when an OS thread returns from an external call,
 * it calls grabReturnCapability() (via Schedule.resumeThread())
 * to wait for permissions to enter the RTS & communicate the
 * result of the external call back to the Haskell thread that
 * made it.
 *
 * ------------------------------------------------------------------------- */

void
waitForReturnCapability( Mutex* pMutex, Capability** pCap )
{
    // Pre-condition: pMutex is held.

    IF_DEBUG(scheduler, 
	     sched_belch("worker: returning; workers waiting: %d",
			 rts_n_waiting_workers));

    if ( noCapabilities() ) {
	rts_n_waiting_workers++;
	context_switch = 1;	// make sure it's our turn soon
	waitCondition(&returning_worker_cond, pMutex);
#if defined(SMP)
	*pCap = free_capabilities;
	free_capabilities = (*pCap)->link;
	ASSERT(pCap != NULL);
	insertHashTable(capability_hash, osThreadId(), *pCap);
#else
	*pCap = &MainCapability;
	ASSERT(rts_n_free_capabilities == 0);
#endif
    } else {
	grabCapability(pCap);
    }

    // Post-condition: pMutex is held, pCap points to a capability
    // which is now held by the current thread.
    return;
}


/* ----------------------------------------------------------------------------
 * yieldCapability( Mutex* pMutex, Capability** pCap )
 * ------------------------------------------------------------------------- */

void
yieldCapability( Capability** pCap, Condition *cond )
{
    // Pre-condition:  pMutex is assumed held, the current thread
    // holds the capability pointed to by pCap.

    if ( rts_n_waiting_workers > 0 || !ANY_WORK_FOR_ME(cond)) {
	IF_DEBUG(scheduler, 
		 if (rts_n_waiting_workers > 0) {
		     sched_belch("worker: giving up capability (returning wkr)");
		 } else if (!EMPTY_RUN_QUEUE()) {
		     sched_belch("worker: giving up capability (passing capability)");
		 } else {
		     sched_belch("worker: giving up capability (no threads to run)");
		 }
	    );
	releaseCapability(*pCap);
	*pCap = NULL;
    }

    // Post-condition:  either:
    //
    //  1. *pCap is NULL, in which case the current thread does not
    //     hold a capability now, or
    //  2. *pCap is not NULL, in which case the current thread still
    //     holds the capability.
    //
    return;
}


/* ----------------------------------------------------------------------------
 * waitForCapability( Mutex*, Capability**, Condition* )
 *
 * Purpose:  wait for a Capability to become available. In
 *           the process of doing so, updates the number
 *           of tasks currently blocked waiting for a capability/more
 *           work. That counter is used when deciding whether or
 *           not to create a new worker thread when an external
 *           call is made.
 *           If pThreadCond is not NULL, a capability can be specifically
 *           passed to this thread.
 * ------------------------------------------------------------------------- */
 
void
waitForCapability( Mutex* pMutex, Capability** pCap, Condition* pThreadCond )
{
    // Pre-condition: pMutex is held.

    while ( noCapabilities() || !ANY_WORK_FOR_ME(pThreadCond)) {
	IF_DEBUG(scheduler,
		 sched_belch("worker: wait for capability (cond: %p)",
			     pThreadCond));

	if (pThreadCond != NULL) {
	    waitCondition(pThreadCond, pMutex);
	    IF_DEBUG(scheduler, sched_belch("worker: get passed capability"));
	} else {
	    rts_n_waiting_tasks++;
	    waitCondition(&thread_ready_cond, pMutex);
	    rts_n_waiting_tasks--;
	    IF_DEBUG(scheduler, sched_belch("worker: get normal capability"));
	}
    }
    grabCapability(pCap);

    // Post-condition: pMutex is held and *pCap is held by the current thread
    return;
}

#endif /* RTS_SUPPORTS_THREADS */

/* ----------------------------------------------------------------------------
   threadRunnable()

   Signals that a thread has been placed on the run queue, so a worker
   might need to be woken up to run it.

   ToDo: should check whether the thread at the front of the queue is
   bound, and if so wake up the appropriate worker.
   -------------------------------------------------------------------------- */
void
threadRunnable ( void )
{
#if defined(RTS_SUPPORTS_THREADS)
    if ( !noCapabilities() && ANY_WORK_TO_DO() ) {
	if (!EMPTY_RUN_QUEUE() && run_queue_hd->main != NULL) {
	    signalCondition(&run_queue_hd->main->bound_thread_cond);
	    return;
	}
	if (rts_n_waiting_tasks > 0) {
	    signalCondition(&thread_ready_cond);
	} else {
	    startSchedulerTaskIfNecessary();
	}
    }
#endif
}


/* ----------------------------------------------------------------------------
   prodWorker()

   Wake up... time to die.
   -------------------------------------------------------------------------- */
void
prodWorker ( void )
{
#if defined(RTS_SUPPORTS_THREADS)
    signalCondition(&thread_ready_cond);
#endif
}
