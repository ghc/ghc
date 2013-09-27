/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2003-2012
 *
 * Capabilities
 *
 * A Capability represents the token required to execute STG code,
 * and all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a
 * register (BaseReg; actually it is a pointer to cap->r).
 *
 * Only in an THREADED_RTS build will there be multiple capabilities,
 * for non-threaded builds there is only one global capability, namely
 * MainCapability.
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Schedule.h"
#include "Sparks.h"
#include "Trace.h"
#include "sm/GC.h" // for gcWorkerThread()
#include "STM.h"
#include "RtsUtils.h"

#include <string.h>

// one global capability, this is the Capability for non-threaded
// builds, and for +RTS -N1
Capability MainCapability;

nat n_capabilities = 0;
nat enabled_capabilities = 0;

// The array of Capabilities.  It's important that when we need
// to allocate more Capabilities we don't have to move the existing
// Capabilities, because there may be pointers to them in use
// (e.g. threads in waitForReturnCapability(), see #8209), so this is
// an array of Capability* rather than an array of Capability.
Capability **capabilities = NULL;

// Holds the Capability which last became free.  This is used so that
// an in-call has a chance of quickly finding a free Capability.
// Maintaining a global free list of Capabilities would require global
// locking, so we don't do that.
Capability *last_free_capability = NULL;

/*
 * Indicates that the RTS wants to synchronise all the Capabilities
 * for some reason.  All Capabilities should stop and return to the
 * scheduler.
 */
volatile StgWord pending_sync = 0;

/* Let foreign code get the current Capability -- assuming there is one!
 * This is useful for unsafe foreign calls because they are called with
 * the current Capability held, but they are not passed it. For example,
 * see see the integer-gmp package which calls allocate() in its
 * stgAllocForGMP() function (which gets called by gmp functions).
 * */
Capability * rts_unsafeGetMyCapability (void)
{
#if defined(THREADED_RTS)
  return myTask()->cap;
#else
  return &MainCapability;
#endif
}

#if defined(THREADED_RTS)
STATIC_INLINE rtsBool
globalWorkToDo (void)
{
  return sched_state >= SCHED_INTERRUPTING
    || recent_activity == ACTIVITY_INACTIVE; // need to check for deadlock
}
#endif

#if defined(THREADED_RTS)
StgClosure *
findSpark (Capability *cap)
{
  Capability *robbed;
  StgClosurePtr spark;
  rtsBool retry;
  nat i = 0;

  if (!emptyRunQueue(cap) || cap->returning_tasks_hd != NULL) {
    // If there are other threads, don't try to run any new
    // sparks: sparks might be speculative, we don't want to take
    // resources away from the main computation.
    return 0;
  }

  do {
    retry = rtsFalse;

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
      retry = rtsTrue;
    }

      if (n_capabilities == 1) { return NULL; } // makes no sense...

      debugTrace(DEBUG_sched,
                 "cap %d: Trying to steal work from other capabilities",
                 cap->no);

      /* visit cap.s 0..n-1 in sequence until a theft succeeds. We could
      start at a random place instead of 0 as well.  */
      for ( i=0 ; i < n_capabilities ; i++ ) {
          robbed = capabilities[i];
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
              retry = rtsTrue;
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
rtsBool
anySparks (void)
{
  nat i;

    for (i=0; i < n_capabilities; i++) {
        if (!emptySparkPoolCap(capabilities[i])) {
            return rtsTrue;
        }
    }
  return rtsFalse;
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
  return task;
}
#endif

/* ----------------------------------------------------------------------------
 * Initialisation
 *
 * The Capability is initially marked not free.
 * ------------------------------------------------------------------------- */

static void
initCapability( Capability *cap, nat i )
{
  nat g;

  cap->no = i;
  cap->in_haskell        = rtsFalse;
  cap->idle              = 0;
  cap->disabled          = rtsFalse;

  cap->run_queue_hd      = END_TSO_QUEUE;
  cap->run_queue_tl      = END_TSO_QUEUE;

#if defined(THREADED_RTS)
  initMutex(&cap->lock);
  cap->running_task           = NULL; // indicates cap is free
  cap->spare_workers          = NULL;
  cap->n_spare_workers        = 0;
  cap->suspended_ccalls_hd    = NULL;
  cap->suspended_ccalls_tl    = NULL;
  cap->returning_tasks_hd     = NULL;
  cap->returning_tasks_tl     = NULL;
  cap->inbox                  = (Message*)END_TSO_QUEUE;
  cap->sparks                 = allocSparkPool();
  cap->spark_stats.created    = 0;
  cap->spark_stats.dud        = 0;
  cap->spark_stats.overflowed = 0;
  cap->spark_stats.converted  = 0;
  cap->spark_stats.gcd        = 0;
  cap->spark_stats.fizzled    = 0;
  cap->picked_up_by_ULS       = rtsFalse;
#endif
  cap->total_allocated        = 0;

  cap->f.stgEagerBlackholeInfo = (W_)&__stg_EAGER_BLACKHOLE_info;
  cap->f.stgGCEnter1     = (StgFunPtr)__stg_gc_enter_1;
  cap->f.stgGCFun        = (StgFunPtr)__stg_gc_fun;

  cap->mut_lists  = stgMallocBytes(sizeof(bdescr *) *
                                   RtsFlags.GcFlags.generations,
                                   "initCapability");
  cap->saved_mut_lists = stgMallocBytes(sizeof(bdescr *) *
                                        RtsFlags.GcFlags.generations,
                                        "initCapability");

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    cap->mut_lists[g] = NULL;
  }

  cap->free_tvar_watch_queues = END_STM_WATCH_QUEUE;
  cap->free_invariant_check_queues = END_INVARIANT_CHECK_QUEUE;
  cap->free_trec_chunks = END_STM_CHUNK_LIST;
  cap->free_trec_headers = NO_TREC;
  cap->transaction_tokens = 0;
  cap->context_switch = 0;
  cap->pinned_object_block = NULL;
  cap->upcall_thread = (StgTSO*)END_TSO_QUEUE;
  cap->upcall_queue_returning = allocUpcallQueue();
  cap->upcall_queue_non_returning = allocUpcallQueue();
  cap->pinned_object_blocks = NULL;

#ifdef PROFILING
  cap->r.rCCCS = CCS_SYSTEM;
#else
  cap->r.rCCCS = NULL;
#endif

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
void
initCapabilities( void )
{
  /* Declare a couple capability sets representing the process and
     clock domain. Each capability will get added to these capsets. */
  traceCapsetCreate(CAPSET_OSPROCESS_DEFAULT, CapsetTypeOsProcess);
  traceCapsetCreate(CAPSET_CLOCKDOMAIN_DEFAULT, CapsetTypeClockdomain);

#if defined(THREADED_RTS)

#ifndef REG_Base
  // We can't support multiple CPUs if BaseReg is not a register
  if (RtsFlags.ParFlags.nNodes > 1) {
    errorBelch("warning: multiple CPUs not supported in this build, reverting to 1");
    RtsFlags.ParFlags.nNodes = 1;
  }
#endif

  n_capabilities = 0;
  moreCapabilities(0, RtsFlags.ParFlags.nNodes);
  n_capabilities = RtsFlags.ParFlags.nNodes;

#else /* !THREADED_RTS */

    n_capabilities = 1;
    capabilities = stgMallocBytes(sizeof(Capability*), "initCapabilities");
    capabilities[0] = &MainCapability;
    initCapability(&MainCapability, 0);

#endif

  enabled_capabilities = n_capabilities;

    // There are no free capabilities to begin with.  We will start
    // a worker Task to each Capability, which will quickly put the
    // Capability on the free list when it finds nothing to do.
    last_free_capability = capabilities[0];
}

void
moreCapabilities (nat from USED_IF_THREADS, nat to USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;
    Capability **old_capabilities = capabilities;

    capabilities = stgMallocBytes(to * sizeof(Capability*), "moreCapabilities");

    if (to == 1) {
        // THREADED_RTS must work on builds that don't have a mutable
        // BaseReg (eg. unregisterised), so in this case
  // capabilities[0] must coincide with &MainCapability.
        capabilities[0] = &MainCapability;
    }

    for (i = 0; i < to; i++) {
        if (i < from) {
            capabilities[i] = old_capabilities[i];
        } else {
            capabilities[i] = stgMallocBytes(sizeof(Capability),
                                             "moreCapabilities");
            initCapability(capabilities[i], i);
        }
    }

    debugTrace(DEBUG_sched, "allocated %d more capabilities", to - from);

    if (old_capabilities != NULL) {
        stgFree(old_capabilities);
    }
#endif
}


/* ----------------------------------------------------------------------------
 * Initialize the upcall threads for each of the capability.
 * ------------------------------------------------------------------------- */

void initUpcallThreadOnCapability (Capability* cap) {
  cap->upcall_thread = createThread (cap, RtsFlags.GcFlags.initialStkSize);
  cap->upcall_thread->what_next = ThreadComplete; //Default state of upcall
  cap->upcall_thread->is_upcall_thread = rtsTrue;
  //labelThread (cap, cap->upcall_thread, "Upcall thread");
  debugTrace (DEBUG_sched, "allocated upcall thread (%d) for capability %d",
              cap->upcall_thread->id, cap->no);
}

void initUpcallThreads (void) {
  nat i;
  Capability* cap;
  for (i=0; i < n_capabilities; i++) {
    cap = capabilities[i];
    initUpcallThreadOnCapability (cap);
  }
}

/* ----------------------------------------------------------------------------
 * setContextSwitches: cause all capabilities to context switch as
 * soon as possible.
 * ------------------------------------------------------------------------- */

void contextSwitchAllCapabilities(void)
{
    nat i;
    for (i=0; i < n_capabilities; i++) {
        contextSwitchCapability(capabilities[i]);
    }
}

void interruptAllCapabilities(void)
{
    nat i;
    for (i=0; i < n_capabilities; i++) {
        interruptCapability(capabilities[i]);
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
             cap->no, task->incall->tso ? "bound task" : "worker",
             serialisableTaskId(task));
  ACQUIRE_LOCK(&task->lock);
  if (task->wakeup == rtsFalse) {
    task->wakeup = rtsTrue;
    // the wakeup flag is needed because signalCondition() doesn't
    // flag the condition if the thread is already runniing, but we want
    // it to be sticky.
    signalCondition(&task->cond);
  }
  RELEASE_LOCK(&task->lock);
}
#endif

/* ----------------------------------------------------------------------------
 * Function:  releaseCapability(Capability*)
 *
 * Purpose:   Letting go of a capability. Causes a
 *            'returning worker' thread or a 'waiting worker'
 *            to wake up, in that order.
 * ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)
void
releaseCapability_ (Capability* cap,
                    rtsBool always_wakeup)
{
  Task *task;

  task = cap->running_task;

  ASSERT_PARTIAL_CAPABILITY_INVARIANTS(cap,task);

  cap->running_task = NULL;

  // Check to see whether a worker thread can be given
  // the go-ahead to return the result of an external call..
  if (cap->returning_tasks_hd != NULL) {
    giveCapabilityToTask(cap,cap->returning_tasks_hd);
    // The Task pops itself from the queue (see waitForReturnCapability())
    return;
  }

  // If there is a pending sync, then we should just leave the
  // Capability free.  The thread trying to sync will be about to
  // call waitForReturnCapability().
  if (pending_sync != 0 && pending_sync != SYNC_GC_PAR) {
    last_free_capability = cap; // needed?
    debugTrace(DEBUG_sched, "sync pending, set capability %d free", cap->no);
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
    if (sched_state < SCHED_SHUTTING_DOWN || !emptyRunQueue(cap)) {
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

#ifdef PROFILING
  cap->r.rCCCS = CCS_IDLE;
#endif
  last_free_capability = cap;
  debugTrace(DEBUG_sched, "freeing capability %d", cap->no);
}

void
releaseCapability (Capability* cap USED_IF_THREADS)
{
  ACQUIRE_LOCK(&cap->lock);
  releaseCapability_(cap, rtsFalse);
  RELEASE_LOCK(&cap->lock);
}

void
releaseAndWakeupCapability (Capability* cap USED_IF_THREADS)
{
  ACQUIRE_LOCK(&cap->lock);
  releaseCapability_(cap, rtsTrue);
  RELEASE_LOCK(&cap->lock);
}

static void
releaseCapabilityAndQueueWorker (Capability* cap USED_IF_THREADS)
{
  Task *task;

  ACQUIRE_LOCK(&cap->lock);

  task = cap->running_task;

  // If the Task is stopped, we shouldn't be yielding, we should
  // be just exiting.
  ASSERT(!task->stopped);

  // If the current task is a worker, save it on the spare_workers
  // list of this Capability.  A worker can mark itself as stopped,
  // in which case it is not replaced on the spare_worker queue.
  // This happens when the system is shutting down (see
  // Schedule.c:workerStart()).
  if (!isBoundTask(task))
  {
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
      releaseCapability_(cap,rtsFalse);
      // hold the lock until after workerTaskStop; c.f. scheduleWorker()
      workerTaskStop(task);
      RELEASE_LOCK(&cap->lock);
      shutdownThread();
    }
  }
  // Bound tasks just float around attached to their TSOs.

  releaseCapability_(cap,rtsFalse);

  RELEASE_LOCK(&cap->lock);
}
#endif

/* ----------------------------------------------------------------------------
 * waitForReturnCapability (Capability **pCap, Task *task)
 *
 * Purpose:  when an OS thread returns from an external call,
 * it calls waitForReturnCapability() (via Schedule.resumeThread())
 * to wait for permission to enter the RTS & communicate the
 * result of the external call back to the Haskell thread that
 * made it.
 *
 * ------------------------------------------------------------------------- */
void
waitForReturnCapability (Capability **pCap, Task *task)
{
#if !defined(THREADED_RTS)

  MainCapability.running_task = task;
  task->cap = &MainCapability;
  *pCap = &MainCapability;

#else
    Capability *cap = *pCap;

    if (cap == NULL) {
  // Try last_free_capability first
  cap = last_free_capability;
  if (cap->running_task) {
      nat i;
      // otherwise, search for a free capability
            cap = NULL;
      for (i = 0; i < n_capabilities; i++) {
                if (!capabilities[i]->running_task) {
                    cap = capabilities[i];
        break;
    }
      }
            if (cap == NULL) {
                // Can't find a free one, use last_free_capability.
                cap = last_free_capability;
            }
  }

  // record the Capability as the one this Task is now assocated with.
  task->cap = cap;

    } else {
  ASSERT(task->cap == cap);
    }

  ACQUIRE_LOCK(&cap->lock);

  debugTrace(DEBUG_sched, "returning; I want capability %d", cap->no);

  if (!cap->running_task) {
    // It's free; just grab it
    cap->running_task = task;
    RELEASE_LOCK(&cap->lock);
  } else {
    newReturningTask(cap,task);
    RELEASE_LOCK(&cap->lock);

    for (;;) {
      ACQUIRE_LOCK(&task->lock);
      // task->lock held, cap->lock not held
      if (!task->wakeup) waitCondition(&task->cond, &task->lock);
      cap = task->cap;
      task->wakeup = rtsFalse;
      RELEASE_LOCK(&task->lock);

      // now check whether we should wake up...
      ACQUIRE_LOCK(&cap->lock);
      if (cap->running_task == NULL) {
        if (cap->returning_tasks_hd != task) {
          giveCapabilityToTask(cap,cap->returning_tasks_hd);
          RELEASE_LOCK(&cap->lock);
          continue;
        }
        cap->running_task = task;
        popReturningTask(cap);
        RELEASE_LOCK(&cap->lock);
        break;
      }
      RELEASE_LOCK(&cap->lock);
    }

  }

#ifdef PROFILING
  cap->r.rCCCS = CCS_SYSTEM;
#endif

  ASSERT_FULL_CAPABILITY_INVARIANTS(cap, task);

  debugTrace(DEBUG_sched, "resuming capability %d", cap->no);

  *pCap = cap;
#endif
}

#if defined(THREADED_RTS)
/* ----------------------------------------------------------------------------
 * yieldCapability
 * ------------------------------------------------------------------------- */

/* See Note [GC livelock] in Schedule.c for why we have gcAllowed
   and return the rtsBool */
rtsBool /* Did we GC? */
yieldCapability (Capability** pCap, Task *task, rtsBool gcAllowed)
{
  Capability *cap = *pCap;

  if ((pending_sync == SYNC_GC_PAR) && gcAllowed) {
    traceEventGcStart(cap);
    gcWorkerThread(cap);
    traceEventGcEnd(cap);
    traceSparkCounters(cap);
    // See Note [migrated bound threads 2]
    if (task->cap == cap) {
      return rtsTrue;
    }
  }

  debugTrace(DEBUG_sched, "giving up capability %d", cap->no);

  // We must now release the capability and wait to be woken up
  // again.
  task->wakeup = rtsFalse;
  releaseCapabilityAndQueueWorker(cap);

  for (;;) {
    ACQUIRE_LOCK(&task->lock);
    // task->lock held, cap->lock not held
    if (!task->wakeup) waitCondition(&task->cond, &task->lock);
    cap = task->cap;
    task->wakeup = rtsFalse;
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

    cap->running_task = task;
    RELEASE_LOCK(&cap->lock);
    break;
  }

  debugTrace(DEBUG_sched, "resuming capability %d", cap->no);
  ASSERT(cap->running_task == task);

#ifdef PROFILING
  cap->r.rCCCS = CCS_SYSTEM;
#endif

  *pCap = cap;

  ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

  return rtsFalse;
}

// Note [migrated bound threads]
//
// There's a tricky case where:
//    - cap A is running an unbound thread T1
//    - there is a bound thread T2 at the head of the run queue on cap A
//    - T1 makes a safe foreign call, the task bound to T2 is woken up on cap A
//    - T1 returns quickly grabbing A again (T2 is still waking up on A)
//    - T1 blocks, the scheduler migrates T2 to cap B
//    - the task bound to T2 wakes up on cap B
//
// We take advantage of the following invariant:
//
//  - A bound thread can only be migrated by the holder of the
//    Capability on which the bound thread currently lives.  So, if we
//    hold Capabilty C, and task->cap == C, then task cannot be
//    migrated under our feet.

// Note [migrated bound threads 2]
//
// Second tricky case;
//   - A bound Task becomes a GC thread
//   - scheduleDoGC() migrates the thread belonging to this Task,
//     because the Capability it is on is disabled
//   - after GC, gcWorkerThread() returns, but now we are
//     holding a Capability that is not the same as task->cap
//   - Hence we must check for this case and immediately give up the
//     cap we hold.


/* ----------------------------------------------------------------------------
 * prodCapability
 *
 * If a Capability is currently idle, wake up a Task on it.  Used to
 * get every Capability into the GC.
 * ------------------------------------------------------------------------- */

void
prodCapability (Capability *cap, Task *task)
{
  ACQUIRE_LOCK(&cap->lock);
  if (!cap->running_task) {
    cap->running_task = task;
    releaseCapability_(cap,rtsTrue);
  }
  RELEASE_LOCK(&cap->lock);
}

/* ----------------------------------------------------------------------------
 * tryGrabCapability
 *
 * Attempt to gain control of a Capability if it is free.
 *
 * ------------------------------------------------------------------------- */

rtsBool
tryGrabCapability (Capability *cap, Task *task)
{
  if (cap->running_task != NULL) return rtsFalse;
  ACQUIRE_LOCK(&cap->lock);
  if (cap->running_task != NULL) {
    RELEASE_LOCK(&cap->lock);
    return rtsFalse;
  }
  task->cap = cap;
  cap->running_task = task;
  RELEASE_LOCK(&cap->lock);
  return rtsTrue;
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

void
shutdownCapability (Capability *cap USED_IF_THREADS,
                    Task *task USED_IF_THREADS,
                    rtsBool safe USED_IF_THREADS)
{
#if defined(THREADED_RTS)
  nat i;

  task->cap = cap;

  // Loop indefinitely until all the workers have exited and there
  // are no Haskell threads left.  We used to bail out after 50
  // iterations of this loop, but that occasionally left a worker
  // running which caused problems later (the closeMutex() below
  // isn't safe, for one thing).

  for (i = 0; /* i < 50 */; i++) {
    ASSERT(sched_state == SCHED_SHUTTING_DOWN);

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
      releaseCapability_(cap,rtsFalse); // this will wake up a worker
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
    if (cap->suspended_ccalls_hd && safe) {
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
      ioManagerDie();
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
shutdownCapabilities(Task *task, rtsBool safe)
{
    nat i;
    for (i=0; i < n_capabilities; i++) {
        ASSERT(task->incall->tso == NULL);
        shutdownCapability(capabilities[i], task, safe);
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
    nat i;
    for (i=0; i < n_capabilities; i++) {
        freeCapability(capabilities[i]);
        stgFree(capabilities[i]);
    }
#else
  freeCapability(&MainCapability);
#endif
    stgFree(capabilities);
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
                rtsBool no_mark_sparks USED_IF_THREADS)
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
  for (incall = cap->suspended_ccalls_hd; incall != NULL;
       incall=incall->next) {
    evac(user, (StgClosure **)(void *)&incall->suspended_tso);
  }

#if defined(THREADED_RTS)
  if (!no_mark_sparks) {
    traverseSparkQueue (evac, user, cap);
  }
#endif

  evac (user, (StgClosure **)(void*)&cap->upcall_thread);
  traverseUpcallQueue (evac, user, cap);

  // Free STM structures for this Capability
  stmPreGCHook(cap);
}

void
markCapabilities (evac_fn evac, void *user)
{
    nat n;
    for (n = 0; n < n_capabilities; n++) {
        markCapability(evac, user, capabilities[n], rtsFalse);
    }
}

#if defined(THREADED_RTS)
rtsBool checkSparkCountInvariant (void)
{
    SparkCounters sparks = { 0, 0, 0, 0, 0, 0 };
    StgWord64 remaining = 0;
    nat i;

    for (i = 0; i < n_capabilities; i++) {
        sparks.created   += capabilities[i]->spark_stats.created;
        sparks.dud       += capabilities[i]->spark_stats.dud;
        sparks.overflowed+= capabilities[i]->spark_stats.overflowed;
        sparks.converted += capabilities[i]->spark_stats.converted;
        sparks.gcd       += capabilities[i]->spark_stats.gcd;
        sparks.fizzled   += capabilities[i]->spark_stats.fizzled;
        remaining        += sparkPoolSize(capabilities[i]->sparks);
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
