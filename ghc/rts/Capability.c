/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 *
 * Capabilities
 *
 * The notion of a capability is used when operating in multi-threaded
 * environments (which the SMP and Threads builds of the RTS do), to
 * hold all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its  TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a 
 * register (BaseReg).
 *
 * Only in an SMP build will there be multiple capabilities, the threaded
 * RTS and other non-threaded builds, there is one global capability,
 * namely MainRegTable.
 *
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
 * Letting go of a capability
 *
 * Locks required: sched_mutex
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
    /* The worker is responsible for grabbing the capability and
     * decrementing the rts_n_returning_workers count
     */
    signalCondition(&returning_worker_cond);
  } else if ( !EMPTY_RUN_QUEUE() ) {
    /* Signal that work is available */
    signalCondition(&thread_ready_cond);
  }
#endif
  return;
}

#if defined(SMP)
/* Allocate 'n' capabilities */
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

