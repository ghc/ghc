/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2006
 *
 * Capabilities
 *
 * For details on the high-level design, see
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/scheduler
 *
 * A Capability holds all the state an OS thread/task needs to run
 * Haskell code: its STG registers, a pointer to its TSO, a nursery
 * etc. During STG execution, a pointer to the Capabilitity is kept in
 * a register (BaseReg).
 *
 * Only in a THREADED_RTS build will there be multiple capabilities,
 * in the non-threaded RTS there is one global capability, called
 * MainCapability.
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "sm/GC.h" // for evac_fn
#include "Task.h"
#include "Sparks.h"

#include "BeginPrivate.h"

struct Capability_ {
    // State required by the STG virtual machine when running Haskell
    // code.  During STG execution, the BaseReg register always points
    // to the StgRegTable of the current Capability (&cap->r).
    StgFunTable f;
    StgRegTable r;

    uint32_t no;  // capability number.

    // The NUMA node on which this capability resides.  This is used to allocate
    // node-local memory in allocate().
    //
    // Note: this is always equal to cap->no % n_numa_nodes.
    // The reason we slice it this way is that if we add or remove capabilities
    // via setNumCapabilities(), then we keep the number of capabilities on each
    // NUMA node balanced.
    uint32_t node;

    // The Task currently holding this Capability.  This task has
    // exclusive access to the contents of this Capability (apart from
    // returning_tasks_hd/returning_tasks_tl).
    // Locks required: cap->lock.
    Task *running_task;

    // true if this Capability is running Haskell code, used for
    // catching unsafe call-ins.
    bool in_haskell;

    // Has there been any activity on this Capability since the last GC?
    uint32_t idle;

    bool disabled;

    // The run queue.  The Task owning this Capability has exclusive
    // access to its run queue, so can wake up threads without
    // taking a lock, and the common path through the scheduler is
    // also lock-free.
    StgTSO *run_queue_hd;
    StgTSO *run_queue_tl;
    uint32_t n_run_queue;

    // Tasks currently making safe foreign calls.  Doubly-linked.
    // When returning, a task first acquires the Capability before
    // removing itself from this list, so that the GC can find all
    // the suspended TSOs easily.  Hence, when migrating a Task from
    // the returning_tasks list, we must also migrate its entry from
    // this list.
    InCall *suspended_ccalls;
    uint32_t n_suspended_ccalls;

    // One mutable list per generation, so we don't need to take any
    // locks when updating an old-generation thunk.  This also lets us
    // keep track of which closures this CPU has been mutating, so we
    // can traverse them using the right thread during GC and avoid
    // unnecessarily moving the data from one cache to another.
    bdescr **mut_lists;
    bdescr **saved_mut_lists; // tmp use during GC

    // block for allocating pinned objects into
    bdescr *pinned_object_block;
    // full pinned object blocks allocated since the last GC
    bdescr *pinned_object_blocks;

    // per-capability weak pointer list associated with nursery (older
    // lists stored in generation object)
    StgWeak *weak_ptr_list_hd;
    StgWeak *weak_ptr_list_tl;

    // Context switch flag.  When non-zero, this means: stop running
    // Haskell code, and switch threads.
    int context_switch;

    // Interrupt flag.  Like the context_switch flag, this also
    // indicates that we should stop running Haskell code, but we do
    // *not* switch threads.  This is used to stop a Capability in
    // order to do GC, for example.
    //
    // The interrupt flag is always reset before we start running
    // Haskell code, unlike the context_switch flag which is only
    // reset after we have executed the context switch.
    int interrupt;

    // Total words allocated by this cap since rts start
    // See Note [allocation accounting] in Storage.c
    W_ total_allocated;

#if defined(THREADED_RTS)
    // Worker Tasks waiting in the wings.  Singly-linked.
    Task *spare_workers;
    uint32_t n_spare_workers; // count of above

    // This lock protects:
    //    running_task
    //    returning_tasks_{hd,tl}
    //    wakeup_queue
    //    inbox
    //    putMVars
    Mutex lock;

    // Tasks waiting to return from a foreign call, or waiting to make
    // a new call-in using this Capability (NULL if empty).
    // NB. this field needs to be modified by tasks other than the
    // running_task, so it requires cap->lock to modify.  A task can
    // check whether it is NULL without taking the lock, however.
    Task *returning_tasks_hd; // Singly-linked, with head/tail
    Task *returning_tasks_tl;
    uint32_t n_returning_tasks;

    // Messages, or END_TSO_QUEUE.
    // Locks required: cap->lock
    Message *inbox;

    // putMVars are really messages, but they're allocated with malloc() so they
    // can't go on the inbox queue: the GC would get confused.
    struct PutMVar_ *putMVars;

    SparkPool *sparks;

    // Stats on spark creation/conversion
    SparkCounters spark_stats;
#if !defined(mingw32_HOST_OS)
    // IO manager for this cap
    int io_manager_control_wr_fd;
#endif
#endif

    // Per-capability STM-related data
    StgTVarWatchQueue *free_tvar_watch_queues;
    StgTRecChunk *free_trec_chunks;
    StgTRecHeader *free_trec_headers;
    uint32_t transaction_tokens;
} // typedef Capability is defined in RtsAPI.h
  // We never want a Capability to overlap a cache line with anything
  // else, so round it up to a cache line size:
#ifndef mingw32_HOST_OS
  ATTRIBUTE_ALIGNED(64)
#endif
  ;

#if defined(THREADED_RTS)
#define ASSERT_TASK_ID(task) ASSERT(task->id == osThreadId())
#else
#define ASSERT_TASK_ID(task) /*empty*/
#endif

// These properties should be true when a Task is holding a Capability
#define ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task)                     \
  ASSERT(cap->running_task != NULL && cap->running_task == task);       \
  ASSERT(task->cap == cap);                                             \
  ASSERT_PARTIAL_CAPABILITY_INVARIANTS(cap,task)

// This assert requires cap->lock to be held, so it can't be part of
// ASSERT_PARTIAL_CAPABILITY_INVARIANTS()
#if defined(THREADED_RTS)
#define ASSERT_RETURNING_TASKS(cap,task)                                \
  ASSERT(cap->returning_tasks_hd == NULL ?                              \
           cap->returning_tasks_tl == NULL && cap->n_returning_tasks == 0 \
         : 1);
#else
#define ASSERT_RETURNING_TASKS(cap,task) /* nothing */
#endif

// Sometimes a Task holds a Capability, but the Task is not associated
// with that Capability (ie. task->cap != cap).  This happens when
// (a) a Task holds multiple Capabilities, and (b) when the current
// Task is bound, its thread has just blocked, and it may have been
// moved to another Capability.
#define ASSERT_PARTIAL_CAPABILITY_INVARIANTS(cap,task)                  \
  ASSERT(cap->run_queue_hd == END_TSO_QUEUE ?                           \
            cap->run_queue_tl == END_TSO_QUEUE && cap->n_run_queue == 0 \
         : 1);                                                          \
  ASSERT(cap->suspended_ccalls == NULL ? cap->n_suspended_ccalls == 0 : 1); \
  ASSERT(myTask() == task);                                             \
  ASSERT_TASK_ID(task);

#if defined(THREADED_RTS)
bool checkSparkCountInvariant (void);
#endif

// Converts a *StgRegTable into a *Capability.
//
INLINE_HEADER Capability *
regTableToCapability (StgRegTable *reg)
{
    return (Capability *)((void *)((unsigned char*)reg - STG_FIELD_OFFSET(Capability,r)));
}

// Initialise the available capabilities.
//
void initCapabilities (void);

// Add and initialise more Capabilities
//
void moreCapabilities (uint32_t from, uint32_t to);

// Release a capability.  This is called by a Task that is exiting
// Haskell to make a foreign call, or in various other cases when we
// want to relinquish a Capability that we currently hold.
//
// ASSUMES: cap->running_task is the current Task.
//
#if defined(THREADED_RTS)
void releaseCapability           (Capability* cap);
void releaseAndWakeupCapability  (Capability* cap);
void releaseCapability_ (Capability* cap, bool always_wakeup);
// assumes cap->lock is held
#else
// releaseCapability() is empty in non-threaded RTS
INLINE_HEADER void releaseCapability  (Capability* cap STG_UNUSED) {};
INLINE_HEADER void releaseAndWakeupCapability  (Capability* cap STG_UNUSED) {};
INLINE_HEADER void releaseCapability_ (Capability* cap STG_UNUSED,
                                       bool always_wakeup STG_UNUSED) {};
#endif

// declared in includes/rts/Threads.h:
// extern Capability MainCapability;

// declared in includes/rts/Threads.h:
// extern uint32_t n_capabilities;
// extern uint32_t enabled_capabilities;

// Array of all the capabilities
extern Capability **capabilities;

//
// Types of global synchronisation
//
typedef enum {
    SYNC_OTHER,
    SYNC_GC_SEQ,
    SYNC_GC_PAR
} SyncType;

//
// Details about a global synchronisation
//
typedef struct {
    SyncType type;              // The kind of synchronisation
    bool *idle;                 // Array of size n_capabilities. idle[i] is true
                                // if capability i will be idle during this GC
                                // cycle. Only available when doing GC (when
                                // type is SYNC_GC_*).
    Task *task;                 // The Task performing the sync
} PendingSync;

//
// Indicates that the RTS wants to synchronise all the Capabilities
// for some reason.  All Capabilities should stop and return to the
// scheduler.
//
extern PendingSync * volatile pending_sync;

// Acquires a capability at a return point.  If *cap is non-NULL, then
// this is taken as a preference for the Capability we wish to
// acquire.
//
// OS threads waiting in this function get priority over those waiting
// in waitForCapability().
//
// On return, *cap is non-NULL, and points to the Capability acquired.
//
void waitForCapability (Capability **cap/*in/out*/, Task *task);

EXTERN_INLINE void recordMutableCap (const StgClosure *p, Capability *cap,
                                        uint32_t gen);

EXTERN_INLINE void recordClosureMutated (Capability *cap, StgClosure *p);

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
bool yieldCapability (Capability** pCap, Task *task, bool gcAllowed);

// Wakes up a worker thread on just one Capability, used when we
// need to service some global event.
//
void prodOneCapability (void);
void prodCapability (Capability *cap, Task *task);

// Similar to prodOneCapability(), but prods all of them.
//
void prodAllCapabilities (void);

// Attempt to gain control of a Capability if it is free.
//
bool tryGrabCapability (Capability *cap, Task *task);

// Try to find a spark to run
//
StgClosure *findSpark (Capability *cap);

// True if any capabilities have sparks
//
bool anySparks (void);

INLINE_HEADER bool emptySparkPoolCap (Capability *cap);
INLINE_HEADER uint32_t sparkPoolSizeCap  (Capability *cap);
INLINE_HEADER void    discardSparksCap  (Capability *cap);

#else // !THREADED_RTS

// Grab a capability.  (Only in the non-threaded RTS; in the threaded
// RTS one of the waitFor*Capability() functions must be used).
//
extern void grabCapability (Capability **pCap);

#endif /* !THREADED_RTS */

// Shut down all capabilities.
//
void shutdownCapabilities(Task *task, bool wait_foreign);

// cause all capabilities to context switch as soon as possible.
void contextSwitchAllCapabilities(void);
INLINE_HEADER void contextSwitchCapability(Capability *cap);

// cause all capabilities to stop running Haskell code and return to
// the scheduler as soon as possible.
void interruptAllCapabilities(void);
INLINE_HEADER void interruptCapability(Capability *cap);

// Free all capabilities
void freeCapabilities (void);

// For the GC:
void markCapability (evac_fn evac, void *user, Capability *cap,
                     bool no_mark_sparks USED_IF_THREADS);

void markCapabilities (evac_fn evac, void *user);

void traverseSparkQueues (evac_fn evac, void *user);

/* -----------------------------------------------------------------------------
   NUMA
   -------------------------------------------------------------------------- */

/* Number of logical NUMA nodes */
extern uint32_t n_numa_nodes;

/* Map logical NUMA node to OS node numbers */
extern uint32_t numa_map[MAX_NUMA_NODES];

#define capNoToNumaNode(n) ((n) % n_numa_nodes)

/* -----------------------------------------------------------------------------
   Messages
   -------------------------------------------------------------------------- */

typedef struct PutMVar_ {
    StgStablePtr mvar;
    struct PutMVar_ *link;
} PutMVar;

#if defined(THREADED_RTS)

INLINE_HEADER bool emptyInbox(Capability *cap);

#endif // THREADED_RTS

/* -----------------------------------------------------------------------------
 * INLINE functions... private below here
 * -------------------------------------------------------------------------- */

EXTERN_INLINE void
recordMutableCap (const StgClosure *p, Capability *cap, uint32_t gen)
{
    bdescr *bd;

    // We must own this Capability in order to modify its mutable list.
    //    ASSERT(cap->running_task == myTask());
    // NO: assertion is violated by performPendingThrowTos()
    bd = cap->mut_lists[gen];
    if (bd->free >= bd->start + BLOCK_SIZE_W) {
        bdescr *new_bd;
        new_bd = allocBlockOnNode_lock(cap->node);
        new_bd->link = bd;
        bd = new_bd;
        cap->mut_lists[gen] = bd;
    }
    *bd->free++ = (StgWord)p;
}

EXTERN_INLINE void
recordClosureMutated (Capability *cap, StgClosure *p)
{
    bdescr *bd;
    bd = Bdescr((StgPtr)p);
    if (bd->gen_no != 0) recordMutableCap(p,cap,bd->gen_no);
}


#if defined(THREADED_RTS)
INLINE_HEADER bool
emptySparkPoolCap (Capability *cap)
{ return looksEmpty(cap->sparks); }

INLINE_HEADER uint32_t
sparkPoolSizeCap (Capability *cap)
{ return sparkPoolSize(cap->sparks); }

INLINE_HEADER void
discardSparksCap (Capability *cap)
{ discardSparks(cap->sparks); }
#endif

INLINE_HEADER void
stopCapability (Capability *cap)
{
    // setting HpLim to NULL tries to make the next heap check will
    // fail, which will cause the thread to return to the scheduler.
    // It may not work - the thread might be updating HpLim itself
    // at the same time - so we also have the context_switch/interrupted
    // flags as a sticky way to tell the thread to stop.
    cap->r.rHpLim = NULL;
}

INLINE_HEADER void
interruptCapability (Capability *cap)
{
    stopCapability(cap);
    cap->interrupt = 1;
}

INLINE_HEADER void
contextSwitchCapability (Capability *cap)
{
    stopCapability(cap);
    cap->context_switch = 1;
}

#if defined(THREADED_RTS)

INLINE_HEADER bool emptyInbox(Capability *cap)
{
    return (cap->inbox == (Message*)END_TSO_QUEUE &&
            cap->putMVars == NULL);
}

#endif

#include "EndPrivate.h"
