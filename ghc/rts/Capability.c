/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2002
 *
 * Capabilities
 *
 * A Capability represent the token required to execute STG code,
 * and all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its  TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a
 * register (BaseReg).
 *
 * Only in an SMP build will there be multiple capabilities, the threaded
 * RTS and other non-threaded builds, there is one global capability,
 * namely MainRegTable.
 * 
 * --------------------------------------------------------------------------*/
#include "PosixSource.h"
#include "Rts.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "Capability.h"

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
 *
 * Locks needed: sched_mutex
 */
nat rts_n_waiting_workers = 0;
#endif

static
void
initCapability( Capability *cap )
{
    cap->f.stgChk0         = (F_)__stg_chk_0;
    cap->f.stgChk1         = (F_)__stg_chk_1;
    cap->f.stgGCEnter1     = (F_)__stg_gc_enter_1;
    cap->f.stgUpdatePAP    = (F_)__stg_update_PAP;
}

#ifdef SMP
static void initCapabilities_(nat n);
#endif

/* 
 */
void
initCapabilities()
{
#if defined(RTS_SUPPORTS_THREADS)
  initCondition(returning_worker_cond);
#endif

#if defined(SMP)
  initCapabilities_(RtsFlags.ParFlags.nNodes);
#else
  initCapability(&MainCapability);
  rts_n_free_capabilities = 1;
#endif

  return;
}

/* Free capability list.
 * Locks required: sched_mutex.
 */
#if defined(SMP)
static Capability *free_capabilities; /* Available capabilities for running threads */
#endif

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
 * Purpose:   Letting go of a capability.
 *
 * Pre-condition: sched_mutex is assumed held by current thread.
 * Post-condition:
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
 *    yieldCapability().
 *  - the worker thread that yielded its capability then tries
 *    to re-grab a capability and re-enter the Scheduler.
 */

/*
 * Function: grabReturnCapability(Capability**)
 *
 * Purpose:  when an OS thread returns from an external call,
 * it calls grabReturningCapability() (via Schedule.resumeThread())
 * to wait for permissions to enter the RTS & communicate the
 * result of the ext. call back to the Haskell thread that
 * made it.
 *
 * Pre-condition:  sched_mutex isn't held.
 * Post-condition: sched_mutex is held and a capability has
 *                 been assigned to the worker thread.
 */
void
grabReturnCapability(Capability** pCap)
{
  IF_DEBUG(scheduler,
	   sched_belch("thread %d: returning, waiting for sched. lock.\n", osThreadId()));
  ACQUIRE_LOCK(&sched_mutex);
  rts_n_waiting_workers++;
  IF_DEBUG(scheduler,
	   sched_belch("worker (%d,%d): returning; workers waiting: %d\n",
		       tok, osThreadId(), rts_n_waiting_workers));
  while ( noCapabilities() ) {
    waitCondition(&returning_worker_cond, &sched_mutex);
  }
  
  grabCapability(pCap);
  return;
}

/*
 * Function: yieldCapability(Capability**)
 *
 * Purpose:  when, upon entry to the Scheduler, an OS worker thread
 *           spots that one or more threads are blocked waiting for
 *           permission to return back their result, it gives up
 *           its Capability. 
 *
 * Pre-condition:  sched_mutex is held and the thread possesses
 *                 a Capability.
 * Post-condition: sched_mutex isn't held and the Capability has
 *                 been given back.
 */
void
yieldCapability(Capability* cap)
{
    IF_DEBUG(scheduler,
	     sched_belch("worker thread (%d): giving up RTS token\n", osThreadId()));
    releaseCapability(cap);
    RELEASE_LOCK(&sched_mutex);
    yieldThread();
    /* At this point, sched_mutex has been given up & we've 
     * forced a thread context switch. Guaranteed to be
     * enough for the signalled worker thread to race
     * ahead?
     */
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
 * 
 * Pre-condition: sched_mutex is held.
 *
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

