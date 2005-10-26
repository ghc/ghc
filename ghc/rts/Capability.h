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

#ifndef CAPABILITY_H
#define CAPABILITY_H

#include "RtsFlags.h"
#include "Task.h"

struct Capability_ {
    // State required by the STG virtual machine when running Haskell
    // code.  During STG execution, the BaseReg register always points
    // to the StgRegTable of the current Capability (&cap->r).
    StgFunTable f;
    StgRegTable r;

    nat no;  // capability number.

    // The Task currently holding this Capability.  This task has
    // exclusive access to the contents of this Capability (apart from
    // returning_tasks_hd/returning_tasks_tl).
    // Locks required: cap->lock.
    Task *running_task;

    // true if this Capability is running Haskell code, used for
    // catching unsafe call-ins.
    rtsBool in_haskell;

    // The run queue.  The Task owning this Capability has exclusive
    // access to its run queue, so can wake up threads without
    // taking a lock, and the common path through the scheduler is
    // also lock-free.
    StgTSO *run_queue_hd;
    StgTSO *run_queue_tl;

    // Tasks currently making safe foreign calls.  Doubly-linked.
    // When returning, a task first acquires the Capability before
    // removing itself from this list, so that the GC can find all
    // the suspended TSOs easily.  Hence, when migrating a Task from
    // the returning_tasks list, we must also migrate its entry from
    // this list.
    Task *suspended_ccalling_tasks;

#if defined(THREADED_RTS)
    // Worker Tasks waiting in the wings.  Singly-linked.
    Task *spare_workers;

    // This lock protects running_task and returning_tasks_{hd,tl}.
    Mutex lock;

    // Tasks waiting to return from a foreign call, or waiting to make
    // a new call-in using this Capability (NULL if empty).
    // NB. this field needs to be modified by tasks other than the
    // running_task, so it requires cap->lock to modify.  A task can
    // check whether it is NULL without taking the lock, however.
    Task *returning_tasks_hd; // Singly-linked, with head/tail
    Task *returning_tasks_tl;
#endif
}; // typedef Capability, defined in RtsAPI.h

// Converts a *StgRegTable into a *Capability.
//
INLINE_HEADER Capability *
regTableToCapability (StgRegTable *reg)
{
    return (Capability *)((void *)((unsigned char*)reg - sizeof(StgFunTable)));
}

// Initialise the available capabilities.
//
void initCapabilities (void);

// Release a capability.  This is called by a Task that is exiting
// Haskell to make a foreign call, or in various other cases when we
// want to relinquish a Capability that we currently hold.
//
// ASSUMES: cap->running_task is the current Task.
//
#if defined(THREADED_RTS)
void releaseCapability  (Capability* cap);
void releaseCapability_ (Capability* cap); // assumes cap->lock is held
#else
// releaseCapability() is empty in non-threaded RTS
INLINE_HEADER void releaseCapability  (Capability* cap STG_UNUSED) {};
INLINE_HEADER void releaseCapability_ (Capability* cap STG_UNUSED) {};
#endif

#if !IN_STG_CODE && !defined(SMP)
// for non-SMP, we have one global capability
extern Capability MainCapability; 
#endif

// Array of all the capabilities
//
extern nat n_capabilities;
extern Capability *capabilities;

// The Capability that was last free.  Used as a good guess for where
// to assign new threads.
//
extern Capability *last_free_capability;

// Acquires a capability at a return point.  If *cap is non-NULL, then
// this is taken as a preference for the Capability we wish to
// acquire.
//
// OS threads waiting in this function get priority over those waiting
// in waitForCapability().
//
// On return, *cap is non-NULL, and points to the Capability acquired.
//
void waitForReturnCapability (Capability **cap/*in/out*/, Task *task);

#if defined(THREADED_RTS)

// Gives up the current capability IFF there is a higher-priority
// thread waiting for it.  This happens in one of two ways:
//
//   (a) we are passing the capability to another OS thread, so
//       that it can run a bound Haskell thread, or
//
//   (b) there is an OS thread waiting to return from a foreign call
//
// On return: *pCap is NULL if the capability was released.  The
// current task should then re-acquire it using waitForCapability().
//
void yieldCapability (Capability** pCap, Task *task);

// Acquires a capability for doing some work.
//
// On return: pCap points to the capability.
//
void waitForCapability (Task *task, Mutex *mutex, Capability **pCap);

// Wakes up a worker thread on just one Capability, used when we
// need to service some global event.
//
void prodOneCapability (void);

// Similar to prodOneCapability(), but prods all of them.
//
void prodAllCapabilities (void);

// Waits for a capability to drain of runnable threads and workers,
// and then acquires it.  Used at shutdown time.
//
void shutdownCapability (Capability *cap, Task *task);

#else // !THREADED_RTS

// Grab a capability.  (Only in the non-threaded RTS; in the threaded
// RTS one of the waitFor*Capability() functions must be used).
//
extern void grabCapability (Capability **pCap);

#endif /* !THREADED_RTS */

#endif /* CAPABILITY_H */
