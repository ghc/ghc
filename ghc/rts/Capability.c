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
#include "RtsUtils.h"
#include "Capability.h"


void
initCapability( Capability *cap )
{
    cap->f.stgChk0         = (F_)__stg_chk_0;
    cap->f.stgChk1         = (F_)__stg_chk_1;
    cap->f.stgGCEnter1     = (F_)__stg_gc_enter_1;
    cap->f.stgUpdatePAP    = (F_)__stg_update_PAP;
}

/* Free capability list.
 * Locks required: sched_mutex.
 */
#if defined(SMP)
static Capability *free_capabilities; /* Available capabilities for running threads */


void grabCapability(Capability** cap)
{
  *cap = free_capabilities;
  free_capabilities = (*cap)->link;
  rts_n_free_capabilities--;
}

void releaseCapability(Capability** cap)
{
  (*cap)->link = free_capabilities;
  free_capabilities = *cap;
  rts_n_free_capabilities++;
  return;
}

/* Allocate 'n' capabilities */
void
initCapabilities(nat n)
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

