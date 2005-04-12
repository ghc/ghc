/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2003
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

// All the capabilities
extern Capability *capabilities;

// Initialised the available capabilities.
//
extern void initCapabilities( void );

// Releases a capability
//
extern void releaseCapability( Capability* cap );

// Signal that a thread has become runnable
//
extern void threadRunnable ( void );

extern void prodWorker ( void );

#ifdef RTS_SUPPORTS_THREADS
// Gives up the current capability IFF there is a higher-priority
// thread waiting for it.  This happens in one of two ways:
//
//   (a) we are passing the capability to another OS thread, so
//       that it can run a bound Haskell thread, or
//
//   (b) there is an OS thread waiting to return from a foreign call
//
// On return: *pCap is NULL if the capability was released.  The
// current worker thread should then re-acquire it using
// waitForCapability().
//
extern void yieldCapability( Capability **pCap );

// Acquires a capability for doing some work.
//
// If the current OS thread is bound to a particular Haskell thread,
// then pThreadCond points to a condition variable for waking up this
// OS thread when its Haskell thread is ready to run.
//
// On return: pCap points to the capability.
extern void waitForCapability( Mutex* pMutex, Capability** pCap, 
			       Condition *pThreadCond );

// Acquires a capability at a return point.  
//
// OS threads waiting in this function get priority over those waiting
// in waitForWorkCapability().
//
// On return: pCap points to the capability.
extern void waitForReturnCapability(Mutex* pMutex, Capability** pCap);

// Signals that the next time a capability becomes free, it should
// be transfered to a particular OS thread, identified by the
// condition variable pTargetThreadCond.
//
extern void passCapability(Condition *pTargetThreadCond);

// Signals that the next time a capability becomes free, it should
// be transfered to an ordinary worker thread.
//
extern void passCapabilityToWorker( void );

extern nat rts_n_free_capabilities;  

extern Capability *free_capabilities;

/* number of worker threads waiting for a return capability
 */
extern nat rts_n_waiting_workers;

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
#if defined(SMP)
  return (rts_n_free_capabilities == RTS_DEREF(RtsFlags).ParFlags.nNodes);
#else
  return (rts_n_free_capabilities == 1);
#endif
}

#else // !RTS_SUPPORTS_THREADS

// Grab a capability.  (Only in the non-threaded RTS; in the threaded
// RTS one of the waitFor*Capability() functions must be used).
//
extern void grabCapability( Capability **pCap );

#endif /* !RTS_SUPPORTS_THREADS */

#endif /* __CAPABILITY_H__ */
