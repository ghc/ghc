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
 * This header file contains the functions for working with capabilities.
 * (the main, and only, consumer of this interface is the scheduler).
 * 
 * --------------------------------------------------------------------------*/
#ifndef __CAPABILITY_H__
#define __CAPABILITY_H__
#include "RtsFlags.h"
/* ToDo: assume that RtsFlags.h has been included at usage sites of Capability.h? */

#if !defined(SMP)
extern Capability MainCapability;
#endif

extern void initCapabilities(void);
extern void grabCapability(Capability** pCap);
extern void releaseCapability(Capability* cap);

extern nat rts_n_free_capabilities;  
#if defined(RTS_SUPPORTS_THREADS)
/* number of worker threads waiting for a return capability
 */
extern nat rts_n_waiting_workers;

extern void grabReturnCapability(Mutex* pMutex, Capability** pCap);
extern void yieldToReturningWorker(Mutex* pMutex, Capability** pCap, Condition *pThreadCond);
extern void waitForWorkCapability(Mutex* pMutex, Capability** pCap, Condition *pThreadCond);
extern void passCapability(Mutex* pMutex, Capability* cap, Condition *pTargetThreadCond);
extern void passCapabilityToWorker(Mutex* pMutex, Capability* cap);

static inline rtsBool needToYieldToReturningWorker(void)
{
	return rts_n_waiting_workers > 0;
}

static inline nat getFreeCapabilities (void)
{
  return rts_n_free_capabilities;
}

static inline rtsBool noCapabilities (void)
{
  return (rts_n_free_capabilities == 0);
}

static inline rtsBool allFreeCapabilities (void)
{
# if defined(SMP)
  return (rts_n_free_capabilities == RtsFlags.ParFlags.nNodes);
# else
  return (rts_n_free_capabilities == 1);
# endif 
}

#endif /* RTS_SUPPORTS_THREADS */

#endif /* __CAPABILITY_H__ */
