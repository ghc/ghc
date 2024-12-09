/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2003-2012
 *
 * Capabilities
 *
 * A Capability represents the token required to execute STG code,
 * and all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its TSO, a nursery etc. During
 * STG execution, a pointer to the capability is kept in a
 * register (BaseReg; actually it is a pointer to cap->r).
 *
 * Only in a THREADED_RTS build will there be multiple capabilities,
 * for non-threaded builds there is only one global capability, namely
 * MainCapability.
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Schedule.h"
#include "Sparks.h"
#include "Trace.h"
#include "eventlog/EventLog.h" // for flushLocalEventsBuf
#include "sm/GC.h" // for gcWorkerThread()
#include "STM.h"
#include "RtsUtils.h"
#include "sm/OSMem.h"
#include "sm/BlockAlloc.h" // for countBlocks()
#include "IOManager.h"

#include <string.h>

// one global capability, this is the Capability for non-threaded
// builds, and for +RTS -N1
Capability MainCapability;

uint32_t n_capabilities = 0;
uint32_t enabled_capabilities = 0;

// The array of Capabilities.  It's important that when we need
// to allocate more Capabilities we don't have to move the existing
// Capabilities, because there may be pointers to them in use
// (e.g. threads in waitForCapability(), see #8209), so this is
// an array of Capability* rather than an array of Capability.
Capability *capabilities[MAX_N_CAPABILITIES];

// Holds the Capability which last became free.  This is used so that
// an in-call has a chance of quickly finding a free Capability.
// Maintaining a global free list of Capabilities would require global
// locking, so we don't do that.
static Capability *last_free_capability[MAX_NUMA_NODES];

/*
 * Indicates that the RTS wants to synchronise all the Capabilities
 * for some reason.  All Capabilities should yieldCapability().
 */
PendingSync * volatile pending_sync = 0;

// Number of logical NUMA nodes
uint32_t n_numa_nodes;

// Map logical NUMA node to OS node numbers
uint32_t numa_map[MAX_NUMA_NODES];

/* Let foreign code get the current Capability -- assuming there is one!
 * This is useful for unsafe foreign calls because they are called with
 * the current Capability held, but they are not passed it.
 */
Capability * rts_unsafeGetMyCapability (void)
{
#if defined(THREADED_RTS)
  return myTask()->cap;
#else
  return &MainCapability;
#endif
}

#if defined(THREADED_RTS)
STATIC_INLINE bool
globalWorkToDo (void)
{
    return getSchedState() >= SCHED_INTERRUPTING
      || getRecentActivity() == ACTIVITY_INACTIVE; // need to check for deadlock
}
#endif

#if defined(THREADED_RTS)
StgClosure *
findSpark (Capability *cap)
{
  Capability *robbed;
  StgClosurePtr spark;
  bool retry;
  uint32_t i = 0;

  // This is an approximate check so relaxed load is acceptable here.
  if (!emptyRunQueue(cap) || RELAXED_LOAD(&cap->n_returning_tasks) != 0) {
      // If there are other threads, don't try to run any new
      // sparks: sparks might be speculative, we don't want to take
      // resources away from the main computation.
      return 0;
  }

  do {
      retry = false;

      // first try to get a spark from our own pool.
      // We should be using reclaimSpark(), because it works without
      // needing any atomic instructions:
      //   spark = reclaimSpark(cap->sparks);
      // However, measurements show that this makes at least one benchmark
      // slower (prsa) and doesn't affect the others.
      spark = tryStealSpark(cap->sparks);
      while (spark != NULL && fizzledSpark(spark)) {
          cap->spark_stats.fizzled++;
          traceEventSparkFizzle(cap);
          spark = tryStealSpark(cap->sparks);
      }
      if (spark != NULL) {
          cap->spark_stats.converted++;

          // Post event for running a spark from capability's own pool.
          traceEventSparkRun(cap);

          return spark;
      }
      if (!emptySparkPoolCap(cap)) {
          retry = true;
      }

      if (getNumCapabilities() == 1) { return NULL; } // makes no sense...

      debugTrace(DEBUG_sched,
                 "cap %d: Trying to steal work from other capabilities",
                 cap->no);

      /* visit cap.s 0..n-1 in sequence until a theft succeeds. We could
      start at a random place instead of 0 as well.  */
      for ( i=0 ; i < getNumCapabilities() ; i++ ) {
          robbed = getCapability(i);
          if (cap == robbed)  // ourselves...
              continue;

          if (emptySparkPoolCap(robbed)) // nothing to steal here
              continue;

          spark = tryStealSpark(robbed->sparks);
          while (spark != NULL && fizzledSpark(spark)) {
              cap->spark_stats.fizzled++;
              traceEventSparkFizzle(cap);
              spark = tryStealSpark(robbed->sparks);
          }
          if (spark == NULL && !emptySparkPoolCap(robbed)) {
              // we conflicted with another thread while trying to steal;
              // try again later.
              retry = true;
          }

          if (spark != NULL) {
              cap->spark_stats.converted++;
              traceEventSparkSteal(cap, robbed->no);

              return spark;
          }
          // otherwise: no success, try next one
      }
  } while (retry);

  debugTrace(DEBUG_sched, "No sparks stolen");
  return NULL;
}

// Returns True if any spark pool is non-empty at this moment in time
// The result is only valid for an instant, of course, so in a sense
// is immediately invalid, and should not be relied upon for
// correctness.
bool
anySparks (void)
{
    uint32_t i;

    for (i=0; i < getNumCapabilities(); i++) {
        if (!emptySparkPoolCap(getCapability(i))) {
            return true;
        }
    }
    return false;
}
#endif

/* -----------------------------------------------------------------------------
 * Manage the returning_tasks lists.
 *
 * These functions require cap->lock
 * -------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
STATIC_INLINE void
newReturningTask (Capability *cap, Task *task)
{
    ASSERT_LOCK_HELD(&cap->lock);
    ASSERT(task->next == NULL);
    if (cap->returning_tasks_hd) {
        ASSERT(cap->returning_tasks_tl->next == NULL);
        cap->returning_tasks_tl->next = task;
    } else {
        cap->returning_tasks_hd = task;
    }
    cap->returning_tasks_tl = task;

    // See Note [Data race in shouldYieldCapability] in Schedule.c.
    RELAXED_ADD(&cap->n_returning_tasks, 1);

    ASSERT_RETURNING_TASKS(cap,task);
}

STATIC_INLINE Task *
popReturningTask (Capability *cap)
{
    ASSERT_LOCK_HELD(&cap->lock);
    Task *task;
    task = cap->returning_tasks_hd;
    ASSERT(task);
    cap->returning_tasks_hd = task->next;
    if (!cap->returning_tasks_hd) {
        cap->returning_tasks_tl = NULL;
    }
    task->next = NULL;

    // See Note [Data race in shouldYieldCapability] in Schedule.c.
    RELAXED_ADD(&cap->n_returning_tasks, -1);

    ASSERT_RETURNING_TASKS(cap,task);
    return task;
}
#endif

/* ----------------------------------------------------------------------------
 * Initialisation
 *
 * The Capability is initially marked not free.
 * ------------------------------------------------------------------------- */

static void
initCapability (Capability *cap, uint32_t i)
{
    uint32_t g;

    cap->no = i;
    cap->node = capNoToNumaNode(i);
    cap->in_haskell        = false;
    cap->idle              = 0;
    cap->disabled          = false;

    cap->run_queue_hd      = END_TSO_QUEUE;
    cap->run_queue_tl      = END_TSO_QUEUE;
    cap->n_run_queue       = 0;

#if defined(THREADED_RTS)
    initMutex(&cap->lock);
    cap->running_task      = NULL; // indicates cap is free
    cap->spare_workers     = NULL;
    cap->n_spare_workers   = 0;
    cap->suspended_ccalls  = NULL;
    cap->n_suspended_ccalls = 0;
    cap->returning_tasks_hd = NULL;
    cap->returning_tasks_tl = NULL;
    cap->n_returning_tasks  = 0;
    cap->inbox              = (Message*)END_TSO_QUEUE;
    cap->putMVars           = NULL;
    cap->sparks             = allocSparkPool();
    cap->spark_stats.created    = 0;
    cap->spark_stats.dud        = 0;
    cap->spark_stats.overflowed = 0;
    cap->spark_stats.converted  = 0;
    cap->spark_stats.gcd        = 0;
    cap->spark_stats.fizzled    = 0;
#endif
    cap->total_allocated        = 0;

    initCapabilityIOManager(&cap->iomgr);

    cap->f.stgEagerBlackholeInfo = (W_)&__stg_EAGER_BLACKHOLE_info;
    cap->f.stgGCEnter1     = (StgFunPtr)__stg_gc_enter_1;
    cap->f.stgGCFun        = (StgFunPtr)__stg_gc_fun;

    cap->mut_lists  = stgMallocBytes(sizeof(bdescr *) *
                                     RtsFlags.GcFlags.generations,
                                     "initCapability");
    cap->saved_mut_lists = stgMallocBytes(sizeof(bdescr *) *
                                          RtsFlags.GcFlags.generations,
                                          "initCapability");
    cap->current_segments = NULL;


    // At this point storage manager is not initialized yet, so this will be
    // initialized in initStorage().
    cap->upd_rem_set.queue.blocks = NULL;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        cap->mut_lists[g] = NULL;
    }

    cap->weak_ptr_list_hd = NULL;
    cap->weak_ptr_list_tl = NULL;
    cap->free_tvar_watch_queues = END_STM_WATCH_QUEUE;
    cap->free_trec_chunks = END_STM_CHUNK_LIST;
    cap->free_trec_headers = NO_TREC;
    cap->transaction_tokens = 0;
    cap->context_switch = 0;
    cap->interrupt = 0;
    cap->pinned_object_block = NULL;
    cap->pinned_object_blocks = NULL;
    cap->pinned_object_empty = NULL;

#if defined(PROFILING)
    cap->r.rCCCS = CCS_SYSTEM;
#else
    cap->r.rCCCS = NULL;
#endif

    // cap->r.rCurrentTSO is charged for calls to allocate(), so we
    // don't want it set when not running a Haskell thread.
    cap->r.rCurrentTSO = NULL;

    traceCapCreate(cap);
    traceCapsetAssignCap(CAPSET_OSPROCESS_DEFAULT, i);
    traceCapsetAssignCap(CAPSET_CLOCKDOMAIN_DEFAULT, i);
#if defined(THREADED_RTS)
    traceSparkCounters(cap);
#endif
}

/* ---------------------------------------------------------------------------
 * Function:  initCapabilities()
 *
 * Purpose:   set up the Capability handling. For the THREADED_RTS build,
 *            we keep a table of them, the size of which is
 *            controlled by the user via the RTS flag -N.
 *
 * ------------------------------------------------------------------------- */
void initCapabilities (void)
{
    /* Declare a couple capability sets representing the process and
       clock domain. Each capability will get added to these capsets. */
    traceCapsetCreate(CAPSET_OSPROCESS_DEFAULT, CapsetTypeOsProcess);
    traceCapsetCreate(CAPSET_CLOCKDOMAIN_DEFAULT, CapsetTypeClockdomain);

    // Initialise NUMA
    if (!RtsFlags.GcFlags.numa) {
        n_numa_nodes = 1;
        for (uint32_t i = 0; i < MAX_NUMA_NODES; i++) {
            numa_map[i] = 0;
        }
    } else if (RtsFlags.DebugFlags.numa) {
        // n_numa_nodes was set by RtsFlags.c
    } else {
        uint32_t nNodes = osNumaNodes();
        if (nNodes > MAX_NUMA_NODES) {
            barf("Too many NUMA nodes (max %d)", MAX_NUMA_NODES);
        }
        StgWord mask = RtsFlags.GcFlags.numaMask & osNumaMask();
        uint32_t logical = 0, physical = 0;
        for (; physical < MAX_NUMA_NODES; physical++) {
            if (mask & 1) {
                numa_map[logical++] = physical;
            }
            mask = mask >> 1;
        }
        n_numa_nodes = logical;
        if (logical == 0) {
            barf("available NUMA node set is empty");
        }
    }

#if defined(THREADED_RTS)

#if !defined(REG_Base)
    // We can't support multiple CPUs if BaseReg is not a register
    if (RtsFlags.ParFlags.nCapabilities > 1) {
        errorBelch("warning: multiple CPUs not supported in this build, reverting to 1");
        RtsFlags.ParFlags.nCapabilities = 1;
    }
#endif

    if (RtsFlags.ParFlags.nCapabilities > MAX_N_CAPABILITIES) {
        errorBelch("warning: this GHC runtime system only supports up to %d capabilities",
                   MAX_N_CAPABILITIES);
        RtsFlags.ParFlags.nCapabilities = MAX_N_CAPABILITIES;
    }

    n_capabilities = 0;
    moreCapabilities(0, RtsFlags.ParFlags.nCapabilities);
    n_capabilities = RtsFlags.ParFlags.nCapabilities;

#else /* !THREADED_RTS */

    n_capabilities = 1;
    capabilities[0] = &MainCapability;

    initCapability(&MainCapability, 0);

#endif

    enabled_capabilities = n_capabilities;

    // There are no free capabilities to begin with.  We will start
    // a worker Task to each Capability, which will quickly put the
    // Capability on the free list when it finds nothing to do.
    for (uint32_t i = 0; i < n_numa_nodes; i++) {
        last_free_capability[i] = getCapability(0);
    }
}

void
moreCapabilities (uint32_t from USED_IF_THREADS, uint32_t to USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    // We must disable the timer while we do this since the tick handler may
    // call contextSwitchAllCapabilities, which may see the capabilities array
    // as we free it. The alternative would be to protect the capabilities
    // array with a lock but this seems more expensive than necessary.
    // See #17289.
    stopTimer();

    if (to == 1) {
        // THREADED_RTS must work on builds that don't have a mutable
        // BaseReg (eg. unregisterised), so in this case
        // capabilities[0] must coincide with &MainCapability.
        capabilities[0] = &MainCapability;
        initCapability(&MainCapability, 0);
    }
    else
    {
        for (uint32_t i = 0; i < to; i++) {
            if (i >= from) {
                capabilities[i] = stgMallocAlignedBytes(sizeof(Capability),
                                                        CAPABILITY_ALIGNMENT,
                                                        "moreCapabilities");
                initCapability(capabilities[i], i);
            }
        }
    }

    debugTrace(DEBUG_sched, "allocated %d more capabilities", to - from);

    startTimer();
#endif
}

/* ----------------------------------------------------------------------------
 * setContextSwitches: cause all capabilities to context switch as
 * soon as possible.
 * ------------------------------------------------------------------------- */

void contextSwitchAllCapabilities(void)
{
    uint32_t i;
    for (i=0; i < getNumCapabilities(); i++) {
        contextSwitchCapability(getCapability(i), true);
    }
}

void interruptAllCapabilities(void)
{
    uint32_t i;
    for (i=0; i < getNumCapabilities(); i++) {
        interruptCapability(getCapability(i));
    }
}

/* ----------------------------------------------------------------------------
 * Give a Capability to a Task.  The task must currently be sleeping
 * on its condition variable.
 *
 * Requires cap->lock (modifies cap->running_task).
 *
 * When migrating a Task, the migrater must take task->lock before
 * modifying task->cap, to synchronise with the waking up Task.
 * Additionally, the migrater should own the Capability (when
 * migrating the run queue), or cap->lock (when migrating
 * returning_workers).
 *
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
static void
giveCapabilityToTask (Capability *cap USED_IF_DEBUG, Task *task)
{
    ASSERT_LOCK_HELD(&cap->lock);
    ASSERT(task->cap == cap);
    debugTrace(DEBUG_sched, "passing capability %d to %s %#" FMT_HexWord64,
               cap->no, task->incall && task->incall->tso ? "bound task" : "worker",
               serialisableTaskId(task));
    ACQUIRE_LOCK(&task->lock);
    if (task->wakeup == false) {
        task->wakeup = true;
        // the wakeup flag is needed because signalCondition() doesn't
        // flag the condition if the thread is already running, but we want
        // it to be sticky.
        signalCondition(&task->cond);
    }
    RELEASE_LOCK(&task->lock);
}
#endif

/* ----------------------------------------------------------------------------
 * releaseCapability
 *
 * The current Task (cap->task) releases the Capability.  The Capability is
 * marked free, and if there is any work to do, an appropriate Task is woken up.
 *
 * The caller must hold cap->lock and will still hold it after
 * releaseCapability returns.
 *
 * N.B. May need to take all_tasks_mutex.
 *
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
void
releaseCapability_ (Capability* cap,
                    bool always_wakeup)
{
    Task *task;

    task = cap->running_task;

    ASSERT_PARTIAL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT_RETURNING_TASKS(cap,task);
    ASSERT_LOCK_HELD(&cap->lock);

    RELAXED_STORE(&cap->running_task, NULL);

    // Check to see whether a worker thread can be given
    // the go-ahead to return the result of an external call..
    if (cap->n_returning_tasks != 0) {
        giveCapabilityToTask(cap,cap->returning_tasks_hd);
        // The Task pops itself from the queue (see waitForCapability())
        return;
    }

    // If there is a pending sync, then we should just leave the Capability
    // free.  The thread trying to sync will be about to call
    // waitForCapability().
    //
    // Note: this is *after* we check for a returning task above,
    // because the task attempting to acquire all the capabilities may
    // be currently in waitForCapability() waiting for this
    // capability, in which case simply setting it as free would not
    // wake up the waiting task.
    PendingSync *sync = SEQ_CST_LOAD(&pending_sync);
    if (sync && (sync->type != SYNC_GC_PAR || sync->idle[cap->no])) {
        debugTrace(DEBUG_sched, "sync pending, freeing capability %d", cap->no);
        return;
    }

    // If the next thread on the run queue is a bound thread,
    // give this Capability to the appropriate Task.
    if (!emptyRunQueue(cap) && peekRunQueue(cap)->bound) {
        // Make sure we're not about to try to wake ourselves up
        // ASSERT(task != cap->run_queue_hd->bound);
        // assertion is false: in schedule() we force a yield after
        // ThreadBlocked, but the thread may be back on the run queue
        // by now.
        task = peekRunQueue(cap)->bound->task;
        giveCapabilityToTask(cap, task);
        return;
    }

    if (!cap->spare_workers) {
        // Create a worker thread if we don't have one.  If the system
        // is interrupted, we only create a worker task if there
        // are threads that need to be completed.  If the system is
        // shutting down, we never create a new worker.
        if (getSchedState() < SCHED_SHUTTING_DOWN || !emptyRunQueue(cap)) {
            debugTrace(DEBUG_sched,
                       "starting new worker on capability %d", cap->no);
            startWorkerTask(cap);
            return;
        }
    }

    // If we have an unbound thread on the run queue, or if there's
    // anything else to do, give the Capability to a worker thread.
    if (always_wakeup ||
        !emptyRunQueue(cap) || !emptyInbox(cap) ||
        (!cap->disabled && !emptySparkPoolCap(cap)) || globalWorkToDo()) {
        if (cap->spare_workers) {
            giveCapabilityToTask(cap, cap->spare_workers);
            // The worker Task pops itself from the queue;
            return;
        }
    }

#if defined(PROFILING)
    cap->r.rCCCS = CCS_IDLE;
#endif
    RELAXED_STORE(&last_free_capability[cap->node], cap);
    debugTrace(DEBUG_sched, "freeing capability %d", cap->no);
}

void
releaseCapability (Capability* cap USED_IF_THREADS)
{
    ACQUIRE_LOCK(&cap->lock);
    releaseCapability_(cap, false);
    RELEASE_LOCK(&cap->lock);
}

void
releaseAndWakeupCapability (Capability* cap USED_IF_THREADS)
{
    ACQUIRE_LOCK(&cap->lock);
    releaseCapability_(cap, true);
    RELEASE_LOCK(&cap->lock);
}

static void
enqueueWorker (Capability* cap USED_IF_THREADS)
{
    Task *task;

    task = cap->running_task;

    // If the Task is stopped, we shouldn't be yielding, we should
    // be just exiting.
    ASSERT(!task->stopped);
    ASSERT(task->worker);

    if (cap->n_spare_workers < MAX_SPARE_WORKERS)
    {
        task->next = cap->spare_workers;
        cap->spare_workers = task;
        cap->n_spare_workers++;
    }
    else
    {
        debugTrace(DEBUG_sched, "%d spare workers already, exiting",
                   cap->n_spare_workers);
        releaseCapability_(cap,false);
        // hold the lock until after workerTaskStop; c.f. scheduleWorker()
        workerTaskStop(task);
        RELEASE_LOCK(&cap->lock);
        shutdownThread();
    }
}

#endif

/*
 * Note [Benign data race due to work-pushing]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * #17276 points out a tricky data race (noticed by ThreadSanitizer) between
 * waitForWorkerCapability and schedulePushWork. In short, schedulePushWork
 * works as follows:
 *
 *  1. collect the set of all idle capabilities, take cap->lock of each.
 *
 *  2. sort through each TSO on the calling capability's run queue and push
 *     some to idle capabilities. This may (if the TSO is a bound thread)
 *     involve setting tso->bound->task->cap despite not holding
 *     tso->bound->task->lock.
 *
 *  3. release cap->lock of all idle capabilities.
 *
 * Now, step 2 is in principle safe since the capability of the caller of
 * schedulePushWork *owns* the TSO and therefore the Task to which it is bound.
 * Furthermore, step 3 ensures that the write in step (2) will be visible to
 * any core which starts execution of the previously-idle capability.
 *
 * However, this argument doesn't quite work for waitForWorkerCapability, which
 * reads task->cap *without* first owning the capability which owns `task`.
 * For this reason, we check again whether the task has been migrated to
 * another capability after taking task->cap->lock. See Note [migrated bound
 * threads] above.
 *
 */

/* ----------------------------------------------------------------------------
 * waitForWorkerCapability(task)
 *
 * waits to be given a Capability, and then returns the Capability.  The task
 * must be either a worker (and on a cap->spare_workers queue), or a bound Task.
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

static Capability * waitForWorkerCapability (Task *task)
{
    Capability *cap;

    for (;;) {
        ACQUIRE_LOCK(&task->lock);
        // task->lock held, cap->lock not held
        if (!task->wakeup) waitCondition(&task->cond, &task->lock);
        // The happens-after matches the happens-before in
        // schedulePushWork, which does owns 'task' when it sets 'task->cap'.
        TSAN_ANNOTATE_HAPPENS_AFTER(&task->cap);
        cap = task->cap;

        // See Note [Benign data race due to work-pushing].
        TSAN_ANNOTATE_BENIGN_RACE(&task->cap, "we will double-check this below");
        task->wakeup = false;
        RELEASE_LOCK(&task->lock);

        debugTrace(DEBUG_sched, "woken up on capability %d", cap->no);

        ACQUIRE_LOCK(&cap->lock);
        if (cap->running_task != NULL) {
            debugTrace(DEBUG_sched,
                       "capability %d is owned by another task", cap->no);
            RELEASE_LOCK(&cap->lock);
            continue;
        }

        if (task->cap != cap) {
            // see Note [migrated bound threads]
            debugTrace(DEBUG_sched,
                       "task has been migrated to cap %d", task->cap->no);
            RELEASE_LOCK(&cap->lock);
            continue;
        }

        if (task->incall->tso == NULL) {
            ASSERT(cap->spare_workers != NULL);
            // if we're not at the front of the queue, release it
                // again.  This is unlikely to happen.
            if (cap->spare_workers != task) {
                giveCapabilityToTask(cap,cap->spare_workers);
                RELEASE_LOCK(&cap->lock);
                continue;
            }
            cap->spare_workers = task->next;
            task->next = NULL;
            cap->n_spare_workers--;
        }

        RELAXED_STORE(&cap->running_task, task);
        RELEASE_LOCK(&cap->lock);
        break;
    }

    return cap;
}

#endif /* THREADED_RTS */

/* ----------------------------------------------------------------------------
 * waitForReturnCapability (Task *task)
 *
 * The Task should be on the cap->returning_tasks queue of a Capability.  This
 * function waits for the Task to be woken up, and returns the Capability that
 * it was woken up on.
 *
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

static Capability * waitForReturnCapability (Task *task)
{
    Capability *cap;

    for (;;) {
        ACQUIRE_LOCK(&task->lock);
        // task->lock held, cap->lock not held
        if (!task->wakeup) waitCondition(&task->cond, &task->lock);
        cap = task->cap;
        task->wakeup = false;
        RELEASE_LOCK(&task->lock);

        // now check whether we should wake up...
        ACQUIRE_LOCK(&cap->lock);
        if (cap->running_task == NULL) {
            if (cap->returning_tasks_hd != task) {
                giveCapabilityToTask(cap,cap->returning_tasks_hd);
                RELEASE_LOCK(&cap->lock);
                continue;
            }
            RELAXED_STORE(&cap->running_task, task);
            popReturningTask(cap);
            RELEASE_LOCK(&cap->lock);
            break;
        }
        RELEASE_LOCK(&cap->lock);
    }

    return cap;
}

#endif /* THREADED_RTS */

#if defined(THREADED_RTS)

/* ----------------------------------------------------------------------------
 * capability_is_busy (Capability *cap)
 *
 * A predicate for determining whether the given Capability is currently running
 * a Task. This can be safely called without holding the Capability's lock
 * although the result may be inaccurate if it races with the scheduler.
 * Consequently there is a TSAN suppression for it.
 *
 * ------------------------------------------------------------------------- */
static bool capability_is_busy(const Capability * cap)
{
    return RELAXED_LOAD(&cap->running_task) != NULL;
}


/* ----------------------------------------------------------------------------
 * find_capability_for_task
 *
 * Given a Task, identify a reasonable Capability to run it on. We try to
 * find an idle capability if possible.
 *
 * ------------------------------------------------------------------------- */

static Capability * find_capability_for_task(const Task * task)
{
    if (task->preferred_capability != -1) {
        // Does the task have a preferred capability? If so, use it
        return getCapability(task->preferred_capability % enabled_capabilities);
    } else {
        // Try last_free_capability first
        Capability *cap = RELAXED_LOAD(&last_free_capability[task->node]);

        // N.B. There is a data race here since we are loking at
        // cap->running_task without taking cap->lock. However, this is
        // benign since the result is merely guiding our search heuristic.
        if (!capability_is_busy(cap)) {
            return cap;
        } else {
            // The last_free_capability is already busy, search for a free
            // capability on this node.
            for (uint32_t i = task->node; i < enabled_capabilities;
                  i += n_numa_nodes) {
                // visits all the capabilities on this node, because
                // cap[i]->node == i % n_numa_nodes
                if (!RELAXED_LOAD(&getCapability(i)->running_task)) {
                    return getCapability(i);
                }
            }

            // Can't find a free one, use last_free_capability.
            return RELAXED_LOAD(&last_free_capability[task->node]);
        }
    }
}
#endif /* THREADED_RTS */

/* ----------------------------------------------------------------------------
 * waitForCapability (Capability **pCap, Task *task)
 *
 * Purpose:  when an OS thread returns from an external call,
 * it calls waitForCapability() (via Schedule.resumeThread())
 * to wait for permission to enter the RTS & communicate the
 * result of the external call back to the Haskell thread that
 * made it.
 *
 * pCap is strictly an output.
 *
 * ------------------------------------------------------------------------- */

void waitForCapability (Capability **pCap, Task *task)
{
#if !defined(THREADED_RTS)

    MainCapability.running_task = task;
    task->cap = &MainCapability;
    *pCap = &MainCapability;

#else
    Capability *cap = *pCap;

    if (cap == NULL) {
        cap = find_capability_for_task(task);

        // record the Capability as the one this Task is now associated with.
        task->cap = cap;
    } else {
        ASSERT(task->cap == cap);
    }

    debugTrace(DEBUG_sched, "returning; I want capability %d", cap->no);

    ACQUIRE_LOCK(&cap->lock);
    if (!cap->running_task) {
        // It's free; just grab it
        RELAXED_STORE(&cap->running_task, task);
        RELEASE_LOCK(&cap->lock);
    } else {
        newReturningTask(cap,task);
        RELEASE_LOCK(&cap->lock);
        cap = waitForReturnCapability(task);
    }

#if defined(PROFILING)
    cap->r.rCCCS = CCS_SYSTEM;
#endif

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap, task);

    debugTrace(DEBUG_sched, "resuming capability %d", cap->no);

    *pCap = cap;
#endif
}

/* ----------------------------------------------------------------------------
 * yieldCapability
 *
 * Give up the Capability, and return when we have it again.  This is called
 * when either we know that the Capability should be given to another Task, or
 * there is nothing to do right now.  One of the following is true:
 *
 *    - The current Task is a worker, and there's a bound thread at the head of
 *      the run queue (or vice versa)
 *
 *    - The run queue is empty.  We'll be woken up again when there's work to
 *      do.
 *
 *    - Another Task is trying to do parallel GC (pending_sync == SYNC_GC_PAR).
 *      We should become a GC worker for a while.
 *
 *    - Another Task is trying to acquire all the Capabilities (pending_sync !=
 *      SYNC_GC_PAR), either to do a sequential GC, forkProcess, or
 *      setNumCapabilities.  We should give up the Capability temporarily.
 *
 * When yieldCapability returns *pCap will have been updated to the new
 * capability held by the caller.
 *
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

/* See Note [GC livelock] in Schedule.c for why we have gcAllowed
   and return the bool */
bool /* Did we GC? */
yieldCapability
    ( Capability** pCap     // [in/out] Task's owned capability. Set to the
                            //          newly owned capability on return.
                            //          Precondition:
                            //              pCap != NULL
                            //              && *pCap != NULL
    , Task *task            // [in] This thread's task.
    , bool gcAllowed
    )
{
    Capability *cap = *pCap;

    if (gcAllowed)
    {
        PendingSync *sync = SEQ_CST_LOAD(&pending_sync);

        if (sync) {
            switch (sync->type) {
            case SYNC_GC_PAR:
                if (! sync->idle[cap->no]) {
                    traceEventGcStart(cap);
                    gcWorkerThread(cap);
                    traceEventGcEnd(cap);
                    traceSparkCounters(cap);
                    // See Note [migrated bound threads 2]
                    if (task->cap == cap) {
                        return true;
                    }
                }
                break;

            case SYNC_FLUSH_UPD_REM_SET:
                debugTrace(DEBUG_nonmoving_gc, "Flushing update remembered set blocks...");
                break;

            case SYNC_FLUSH_EVENT_LOG:
                /* N.B. the actual flushing is performed by flushEventLog */
                break;

            default:
                break;
            }
        }
    }

    debugTrace(DEBUG_sched, "giving up capability %d", cap->no);

    // We must now release the capability and wait to be woken up again.
    task->wakeup = false;

    ACQUIRE_LOCK(&cap->lock);

    // If this is a worker thread, put it on the spare_workers queue
    if (isWorker(task)) {
        enqueueWorker(cap);
    }

    releaseCapability_(cap, false);

    if (isWorker(task) || isBoundTask(task)) {
        RELEASE_LOCK(&cap->lock);
        cap = waitForWorkerCapability(task);
    } else {
        // Not a worker Task, or a bound Task.  The only way we can be woken up
        // again is to put ourselves on the returning_tasks queue, so that's
        // what we do.  We still hold cap->lock at this point
        // The Task waiting for this Capability does not have it
        // yet, so we can be sure to be woken up later. (see #10545)
        newReturningTask(cap,task);
        RELEASE_LOCK(&cap->lock);
        cap = waitForReturnCapability(task);
    }

    debugTrace(DEBUG_sched, "resuming capability %d", cap->no);
    ASSERT(cap->running_task == task);

#if defined(PROFILING)
    cap->r.rCCCS = CCS_SYSTEM;
#endif

    *pCap = cap;

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    return false;
}

#endif /* THREADED_RTS */

/*
 * Note [migrated bound threads]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * There's a tricky case where:
 *    - cap A is running an unbound thread T1
 *    - there is a bound thread T2 at the head of the run queue on cap A
 *    - T1 makes a safe foreign call, the task bound to T2 is woken up on cap A
 *    - T1 returns quickly grabbing A again (T2 is still waking up on A)
 *    - T1 blocks, the scheduler migrates T2 to cap B
 *    - the task bound to T2 wakes up on cap B
 *
 * We take advantage of the following invariant:
 *
 *  - A bound thread can only be migrated by the holder of the
 *    Capability on which the bound thread currently lives.  So, if we
 *    hold Capability C, and task->cap == C, then task cannot be
 *    migrated under our feet.
 *
 * See also Note [Benign data race due to work-pushing].
 *
 *
 * Note [migrated bound threads 2]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Second tricky case;
 *   - A bound Task becomes a GC thread
 *   - scheduleDoGC() migrates the thread belonging to this Task,
 *     because the Capability it is on is disabled
 *   - after GC, gcWorkerThread() returns, but now we are
 *     holding a Capability that is not the same as task->cap
 *   - Hence we must check for this case and immediately give up the
 *     cap we hold.
 *
 */

/* ----------------------------------------------------------------------------
 * prodCapability
 *
 * If a Capability is currently idle, wake up a Task on it.  Used to
 * get every Capability into the GC.
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

void
prodCapability (Capability *cap, Task *task)
{
    ACQUIRE_LOCK(&cap->lock);
    if (!cap->running_task) {
        cap->running_task = task;
        releaseCapability_(cap,true);
    }
    RELEASE_LOCK(&cap->lock);
}

#endif /* THREADED_RTS */

/* ----------------------------------------------------------------------------
 * tryGrabCapability
 *
 * Attempt to gain control of a Capability if it is free.
 *
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

bool
tryGrabCapability (Capability *cap, Task *task)
{
    int r;
    // N.B. This is benign as we will check again after taking the lock.
    TSAN_ANNOTATE_BENIGN_RACE(&cap->running_task, "tryGrabCapability (cap->running_task)");
    if (RELAXED_LOAD(&cap->running_task) != NULL) return false;

    r = TRY_ACQUIRE_LOCK(&cap->lock);
    if (r != 0) return false;
    if (cap->running_task != NULL) {
        RELEASE_LOCK(&cap->lock);
        return false;
    }
    task->cap = cap;
    RELAXED_STORE(&cap->running_task, task);
    RELEASE_LOCK(&cap->lock);
    return true;
}


#endif /* THREADED_RTS */

/* ----------------------------------------------------------------------------
 * shutdownCapability
 *
 * At shutdown time, we want to let everything exit as cleanly as
 * possible.  For each capability, we let its run queue drain, and
 * allow the workers to stop.
 *
 * This function should be called when interrupted and
 * sched_state = SCHED_SHUTTING_DOWN, thus any worker that wakes up
 * will exit the scheduler and call taskStop(), and any bound thread
 * that wakes up will return to its caller.  Runnable threads are
 * killed.
 *
 * ------------------------------------------------------------------------- */

static void
shutdownCapability (Capability *cap USED_IF_THREADS,
                    Task *task USED_IF_THREADS,
                    bool safe USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    uint32_t i;

    task->cap = cap;

    // Loop indefinitely until all the workers have exited and there
    // are no Haskell threads left.  We used to bail out after 50
    // iterations of this loop, but that occasionally left a worker
    // running which caused problems later (the closeMutex() below
    // isn't safe, for one thing).

    for (i = 0; /* i < 50 */; i++) {
        ASSERT(getSchedState() == SCHED_SHUTTING_DOWN);

        debugTrace(DEBUG_sched,
                   "shutting down capability %d, attempt %d", cap->no, i);
        ACQUIRE_LOCK(&cap->lock);
        if (cap->running_task) {
            RELEASE_LOCK(&cap->lock);
            debugTrace(DEBUG_sched, "not owner, yielding");
            yieldThread();
            continue;
        }
        cap->running_task = task;

        if (cap->spare_workers) {
            // Look for workers that have died without removing
            // themselves from the list; this could happen if the OS
            // summarily killed the thread, for example.  This
            // actually happens on Windows when the system is
            // terminating the program, and the RTS is running in a
            // DLL.
            Task *t, *prev;
            prev = NULL;
            for (t = cap->spare_workers; t != NULL; t = t->next) {
                if (!osThreadIsAlive(t->id)) {
                    debugTrace(DEBUG_sched,
                               "worker thread %p has died unexpectedly", (void *)(size_t)t->id);
                    cap->n_spare_workers--;
                    if (!prev) {
                        cap->spare_workers = t->next;
                    } else {
                        prev->next = t->next;
                    }
                    prev = t;
                }
            }
        }

        if (!emptyRunQueue(cap) || cap->spare_workers) {
            debugTrace(DEBUG_sched,
                       "runnable threads or workers still alive, yielding");
            releaseCapability_(cap,false); // this will wake up a worker
            RELEASE_LOCK(&cap->lock);
            yieldThread();
            continue;
        }

        // If "safe", then busy-wait for any threads currently doing
        // foreign calls.  If we're about to unload this DLL, for
        // example, we need to be sure that there are no OS threads
        // that will try to return to code that has been unloaded.
        // We can be a bit more relaxed when this is a standalone
        // program that is about to terminate, and let safe=false.
        if (cap->suspended_ccalls && safe) {
            debugTrace(DEBUG_sched,
                       "thread(s) are involved in foreign calls, yielding");
            cap->running_task = NULL;
            RELEASE_LOCK(&cap->lock);
            // The IO manager thread might have been slow to start up,
            // so the first attempt to kill it might not have
            // succeeded.  Just in case, try again - the kill message
            // will only be sent once.
            //
            // To reproduce this deadlock: run ffi002(threaded1)
            // repeatedly on a loaded machine.
            //
            // FIXME: stopIOManager is not a per-capability action. It shuts
            // down the I/O subsystem for all capabilities, but here we call
            // it once per cap, so this is accidentally quadratic, but mainly
            // it is confusing. Replace this with a per-capability stop, and
            // perhaps make it synchronous so it works the first time and we
            // don't have to come back and try again here.
            //
            stopIOManager();
            yieldThread();
            continue;
        }

        traceSparkCounters(cap);
        RELEASE_LOCK(&cap->lock);
        break;
    }
    // we now have the Capability, its run queue and spare workers
    // list are both empty.

    // ToDo: we can't drop this mutex, because there might still be
    // threads performing foreign calls that will eventually try to
    // return via resumeThread() and attempt to grab cap->lock.
    // closeMutex(&cap->lock);
#endif
}

void
shutdownCapabilities(Task *task, bool safe)
{
    uint32_t i;
    for (i=0; i < getNumCapabilities(); i++) {
        ASSERT(task->incall->tso == NULL);
        shutdownCapability(getCapability(i), task, safe);
    }
#if defined(THREADED_RTS)
    ASSERT(checkSparkCountInvariant());
#endif
}

static void
freeCapability (Capability *cap)
{
    stgFree(cap->mut_lists);
    stgFree(cap->saved_mut_lists);
    if (cap->current_segments) {
        stgFree(cap->current_segments);
    }
#if defined(THREADED_RTS)
    freeSparkPool(cap->sparks);
#endif
    traceCapsetRemoveCap(CAPSET_OSPROCESS_DEFAULT, cap->no);
    traceCapsetRemoveCap(CAPSET_CLOCKDOMAIN_DEFAULT, cap->no);
    traceCapDelete(cap);
}

void
freeCapabilities (void)
{
#if defined(THREADED_RTS)
    uint32_t i;
    for (i=0; i < getNumCapabilities(); i++) {
        Capability *cap = getCapability(i);
        freeCapability(cap);
        if (cap != &MainCapability) {
            stgFreeAligned(cap);
        }
    }
#else
    freeCapability(&MainCapability);
#endif
    traceCapsetDelete(CAPSET_OSPROCESS_DEFAULT);
    traceCapsetDelete(CAPSET_CLOCKDOMAIN_DEFAULT);
}

/* ---------------------------------------------------------------------------
   Mark everything directly reachable from the Capabilities.  When
   using multiple GC threads, each GC thread marks all Capabilities
   for which (c `mod` n == 0), for Capability c and thread n.
   ------------------------------------------------------------------------ */

void
markCapability (evac_fn evac, void *user, Capability *cap,
                bool no_mark_sparks USED_IF_THREADS)
{
    InCall *incall;

    // Each GC thread is responsible for following roots from the
    // Capability of the same number.  There will usually be the same
    // or fewer Capabilities as GC threads, but just in case there
    // are more, we mark every Capability whose number is the GC
    // thread's index plus a multiple of the number of GC threads.
    evac(user, (StgClosure **)(void *)&cap->run_queue_hd);
    evac(user, (StgClosure **)(void *)&cap->run_queue_tl);
#if defined(THREADED_RTS)
    evac(user, (StgClosure **)(void *)&cap->inbox);
#endif
    for (incall = cap->suspended_ccalls; incall != NULL;
         incall=incall->next) {
        evac(user, (StgClosure **)(void *)&incall->suspended_tso);
    }

#if defined(THREADED_RTS)
    if (!no_mark_sparks) {
        traverseSparkQueue (evac, user, cap);
    }
#endif

    markCapabilityIOManager(evac, user, cap->iomgr);

    // Free STM structures for this Capability
    stmPreGCHook(cap);
}

void
markCapabilities (evac_fn evac, void *user)
{
    uint32_t n;
    for (n = 0; n < getNumCapabilities(); n++) {
        markCapability(evac, user, getCapability(n), false);
    }
}

#if defined(THREADED_RTS)
bool checkSparkCountInvariant (void)
{
    SparkCounters sparks = { 0, 0, 0, 0, 0, 0 };
    StgWord64 remaining = 0;
    uint32_t i;

    for (i = 0; i < getNumCapabilities(); i++) {
        Capability *cap = getCapability(i);
        sparks.created   += cap->spark_stats.created;
        sparks.dud       += cap->spark_stats.dud;
        sparks.overflowed+= cap->spark_stats.overflowed;
        sparks.converted += cap->spark_stats.converted;
        sparks.gcd       += cap->spark_stats.gcd;
        sparks.fizzled   += cap->spark_stats.fizzled;
        remaining        += sparkPoolSize(cap->sparks);
    }

    /* The invariant is
     *   created = converted + remaining + gcd + fizzled
     */
    debugTrace(DEBUG_sparks,"spark invariant: %ld == %ld + %ld + %ld + %ld "
                            "(created == converted + remaining + gcd + fizzled)",
                            sparks.created, sparks.converted, remaining,
                            sparks.gcd, sparks.fizzled);

    return (sparks.created ==
              sparks.converted + remaining + sparks.gcd + sparks.fizzled);

}
#endif
