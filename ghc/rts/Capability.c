/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2002
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
#include "OSThreads.h"
#include "Capability.h"
#include "Schedule.h"  /* to get at EMPTY_RUN_QUEUE() */

#if !defined(SMP)
Capability MainCapability;     /* for non-SMP, we have one global capability */
#endif

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
static nat rts_n_waiting_workers = 0;

/* thread_ready_cond: when signalled, a thread has become runnable for a
 * task to execute.
 *
 * In the non-SMP case, it also implies that the thread that is woken up has
 * exclusive access to the RTS and all its data structures (that are not
 * locked by the Scheduler's mutex).
 *
 * thread_ready_cond is signalled whenever COND_NO_THREADS_READY doesn't hold.
 *
 */
Condition thread_ready_cond = INIT_COND_VAR;
#if 0
/* For documentation purposes only */
#define COND_NO_THREADS_READY() (noCapabilities() || EMPTY_RUN_QUEUE())
#endif

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

/* -----------------------------------------------------------------------------
   Initialisation
   -------------------------------------------------------------------------- */
static
void
initCapability( Capability *cap )
{
    cap->f.stgChk0         = (F_)__stg_chk_0;
    cap->f.stgChk1         = (F_)__stg_chk_1;
    cap->f.stgGCEnter1     = (F_)__stg_gc_enter_1;
    cap->f.stgUpdatePAP    = (F_)__stg_update_PAP;
}

#if defined(SMP)
static void initCapabilities_(nat n);
#endif

/* 
 * Function:  initCapabilities()
 *
 * Purpose:   set up the Capability handling. For the SMP build,
 *            we keep a table of them, the size of which is
 *            controlled by the user via the RTS flag RtsFlags.ParFlags.nNodes
 *
 * Pre-conditions: no locks assumed held.
 */
void
initCapabilities()
{
#if defined(RTS_SUPPORTS_THREADS)
  initCondition(&returning_worker_cond);
  initCondition(&thread_ready_cond);
#endif

#if defined(SMP)
  initCapabilities_(RtsFlags.ParFlags.nNodes);
#else
  initCapability(&MainCapability);
  rts_n_free_capabilities = 1;
#endif

  return;
}

#if defined(SMP)
/* Free capability list. */
static Capability *free_capabilities; /* Available capabilities for running threads */
#endif

/* -----------------------------------------------------------------------------
   Acquiring capabilities
   -------------------------------------------------------------------------- */

/*
 * Function:  grabCapability(Capability**)
 * 
 * Purpose:   the act of grabbing a capability is easy; just 
 *            remove one from the free capabilities list (which
 *            may just have one entry). In threaded builds, worker
 *            threads are prevented from doing so willy-nilly
 *            via the condition variables thread_ready_cond and
 *            returning_worker_cond.
 *
 */ 
void grabCapability(Capability** cap)
{
#if !defined(SMP)
  rts_n_free_capabilities = 0;
  *cap = &MainCapability;
#else
  *cap = free_capabilities;
  free_capabilities = (*cap)->link;
  rts_n_free_capabilities--;
#endif
}

/*
 * Function:  releaseCapability(Capability*)
 *
 * Purpose:   Letting go of a capability. Causes a
 *            'returning worker' thread or a 'waiting worker'
 *            to wake up, in that order.
 *
 */
void releaseCapability(Capability* cap
#if !defined(SMP)
		       STG_UNUSED
#endif
)
{
#if defined(SMP)
  cap->link = free_capabilities;
  free_capabilities = cap;
  rts_n_free_capabilities++;
#else
  rts_n_free_capabilities = 1;
#endif

#if defined(RTS_SUPPORTS_THREADS)
  /* Check to see whether a worker thread can be given
     the go-ahead to return the result of an external call..*/
  if (rts_n_waiting_workers > 0) {
    /* Decrement the counter here to avoid livelock where the
     * thread that is yielding its capability will repeatedly
     * signal returning_worker_cond.
     */
    rts_n_waiting_workers--;
    signalCondition(&returning_worker_cond);
  } else if ( !EMPTY_RUN_QUEUE() ) {
    /* Signal that work is available */
    signalCondition(&thread_ready_cond);
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
 *  - in resumeThread(), the thread calls grabReturnCapability().
 *  - If no capabilities are readily available, grabReturnCapability()
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

/*
 * Function: grabReturnCapability(Capability**)
 *
 * Purpose:  when an OS thread returns from an external call,
 * it calls grabReturnCapability() (via Schedule.resumeThread())
 * to wait for permissions to enter the RTS & communicate the
 * result of the external call back to the Haskell thread that
 * made it.
 *
 * Pre-condition:  pMutex is held.
 * Post-condition: pMutex is still held and a capability has
 *                 been assigned to the worker thread.
 */
void
grabReturnCapability(Mutex* pMutex, Capability** pCap)
{
  IF_DEBUG(scheduler,
	   fprintf(stderr,"worker (%ld): returning, waiting for lock.\n", osThreadId()));
  rts_n_waiting_workers++;
  IF_DEBUG(scheduler,
	   fprintf(stderr,"worker (%ld): returning; workers waiting: %d\n",
		   osThreadId(), rts_n_waiting_workers));
  while ( noCapabilities() ) {
    waitCondition(&returning_worker_cond, pMutex);
  }
  
  grabCapability(pCap);
  return;
}


/* -----------------------------------------------------------------------------
   Yielding/waiting for capabilities
   -------------------------------------------------------------------------- */

/*
 * Function: yieldToReturningWorker(Mutex*,Capability*)
 *
 * Purpose:  when, upon entry to the Scheduler, an OS worker thread
 *           spots that one or more threads are blocked waiting for
 *           permission to return back their result, it gives up
 *           its Capability. 
 *
 * Pre-condition:  pMutex is assumed held and the thread possesses
 *                 a Capability.
 * Post-condition: pMutex isn't held and the Capability has
 *                 been given back.
 */
void
yieldToReturningWorker(Mutex* pMutex, Capability* cap)
{
  if ( rts_n_waiting_workers > 0 && noCapabilities() ) {
    IF_DEBUG(scheduler,
	     fprintf(stderr,"worker thread (%ld): giving up RTS token\n", osThreadId()));
    releaseCapability(cap);
    RELEASE_LOCK(pMutex);
    yieldThread();
    /* At this point, pMutex has been given up & we've 
     * forced a thread context switch. Guaranteed to be
     * enough for the signalled worker thread to race
     * ahead of us?
     */

    /* Re-grab the mutex */
    ACQUIRE_LOCK(pMutex);
    /* And wait for work */
    waitForWorkCapability(pMutex, cap, rtsFalse);
  }
  return;
}


/*
 * Function: waitForWorkCapability(Mutex*, Capability**, rtsBool)
 *
 * Purpose:  wait for a Capability to become available. In
 *           the process of doing so, updates the number
 *           of tasks currently blocked waiting for a capability/more
 *           work. That counter is used when deciding whether or
 *           not to create a new worker thread when an external
 *           call is made.
 *
 * Pre-condition: pMutex is held.
 */
void 
waitForWorkCapability(Mutex* pMutex, Capability** pCap, rtsBool runnable)
{
  while ( noCapabilities() || (runnable && EMPTY_RUN_QUEUE()) ) {
    rts_n_waiting_tasks++;
    waitCondition(&thread_ready_cond, pMutex);
    rts_n_waiting_tasks--;
  }
  grabCapability(pCap);
  return;
}
#endif /* RTS_SUPPORTS_THREADS */

#if defined(SMP)
/*
 * Function: initCapabilities_(nat)
 *
 * Purpose:  upon startup, allocate and fill in table
 *           holding 'n' Capabilities. Only for SMP, since
 *           it is the only build that supports multiple
 *           capabilities within the RTS.
 */
static void
initCapabilities_(nat n)
{
  nat i;
  Capability *cap, *prev;
  cap  = NULL;
  prev = NULL;
  for (i = 0; i < n; i++) {
    cap = stgMallocBytes(sizeof(Capability), "initCapabilities");
    initCapability(cap);
    cap->link = prev;
    prev = cap;
  }
  free_capabilities = cap;
  rts_n_free_capabilities = n;
  IF_DEBUG(scheduler,fprintf(stderr,"scheduler: Allocated %d capabilities\n", n_free_capabilities););
}
#endif /* SMP */

