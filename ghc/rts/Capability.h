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
extern void grabCapability(Capability** cap);
extern void releaseCapability(Capability* cap);

#if defined(SMP)
extern nat rts_n_free_capabilities;  /* total number of available capabilities */

static inline nat getFreeCapabilities()
{
  return rts_n_free_capabilities;
}

static inline rtsBool noFreeCapabilities()
{
  return (rts_n_free_capabilities == 0);
}

static inline rtsBool allFreeCapabilities()
{
  return (rts_n_free_capabilities == RtsFlags.ParFlags.nNodes);
}

#endif /* SMP */

#endif /* __CAPABILITY_H__ */
