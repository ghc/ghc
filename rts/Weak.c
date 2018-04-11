/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "RtsUtils.h"
#include "Weak.h"
#include "Schedule.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#include "Trace.h"

// List of dead weak pointers collected by the last GC
static StgWeak *finalizer_list = NULL;

// Count of the above list.
static uint32_t n_finalizers = 0;

void
runCFinalizers(StgCFinalizerList *list)
{
    StgCFinalizerList *head;
    for (head = list;
        (StgClosure *)head != &stg_NO_FINALIZER_closure;
        head = (StgCFinalizerList *)head->link)
    {
        if (head->flag)
            ((void (*)(void *, void *))head->fptr)(head->eptr, head->ptr);
        else
            ((void (*)(void *))head->fptr)(head->ptr);
    }
}

void
runAllCFinalizers(StgWeak *list)
{
    StgWeak *w;
    Task *task;

    task = myTask();
    if (task != NULL) {
        task->running_finalizers = true;
    }

    for (w = list; w; w = w->link) {
        // We need to filter out DEAD_WEAK objects, because it's not guaranteed
        // that the list will not have them when shutting down.
        // They only get filtered out during GC for the generation they
        // belong to.
        // If there's no major GC between the time that the finalizer for the
        // object from the oldest generation is manually called and shutdown
        // we end up running the same finalizer twice. See #7170.
        if (w->header.info != &stg_DEAD_WEAK_info) {
            runCFinalizers((StgCFinalizerList *)w->cfinalizers);
        }
    }

    if (task != NULL) {
        task->running_finalizers = false;
    }
}

/*
 * scheduleFinalizers() is called on the list of weak pointers found
 * to be dead after a garbage collection.  It overwrites each object
 * with DEAD_WEAK, and creates a new thread to run the pending finalizers.
 *
 * This function is called just after GC.  The weak pointers on the
 * argument list are those whose keys were found to be not reachable,
 * however the value and finalizer fields have by now been marked live.
 * The weak pointer object itself may not be alive - i.e. we may be
 * looking at either an object in from-space or one in to-space.  It
 * doesn't really matter either way.
 *
 * Pre-condition: sched_mutex _not_ held.
 */

void
scheduleFinalizers(Capability *cap, StgWeak *list)
{
    StgWeak *w;
    StgTSO *t;
    StgMutArrPtrs *arr;
    StgWord size;
    uint32_t n, i;

    ASSERT(n_finalizers == 0);

    finalizer_list = list;

    // Traverse the list and
    //  * count the number of Haskell finalizers
    //  * overwrite all the weak pointers with DEAD_WEAK
    n = 0;
    i = 0;
    for (w = list; w; w = w->link) {
        // Better not be a DEAD_WEAK at this stage; the garbage
        // collector removes DEAD_WEAKs from the weak pointer list.
        ASSERT(w->header.info != &stg_DEAD_WEAK_info);

        if (w->finalizer != &stg_NO_FINALIZER_closure) {
            n++;
        }

        // Remember the length of the list, for runSomeFinalizers() below
        i++;

#if defined(PROFILING)
        // A weak pointer is inherently used, so we do not need to call
        // LDV_recordDead().
        //
        // Furthermore, when PROFILING is turned on, dead weak
        // pointers are exactly as large as weak pointers, so there is
        // no need to fill the slop, either.  See stg_DEAD_WEAK_info
        // in StgMiscClosures.cmm.
#endif

        // We must overwrite the header with DEAD_WEAK, so that if
        // there's a later call to finalizeWeak# on this weak pointer,
        // we don't run the finalizer again.
        SET_HDR(w, &stg_DEAD_WEAK_info, w->header.prof.ccs);
    }

    n_finalizers = i;

    // No Haskell finalizers to run?
    if (n == 0) return;

    debugTrace(DEBUG_weak, "weak: batching %d finalizers", n);

    size = n + mutArrPtrsCardTableSize(n);
    arr = (StgMutArrPtrs *)allocate(cap, sizeofW(StgMutArrPtrs) + size);
    TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), n, 0);
    SET_HDR(arr, &stg_MUT_ARR_PTRS_FROZEN_info, CCS_SYSTEM);
    arr->ptrs = n;
    arr->size = size;

    n = 0;
    for (w = list; w; w = w->link) {
        if (w->finalizer != &stg_NO_FINALIZER_closure) {
            arr->payload[n] = w->finalizer;
            n++;
        }
    }
    // set all the cards to 1
    for (i = n; i < size; i++) {
        arr->payload[i] = (StgClosure *)(W_)(-1);
    }

    t = createIOThread(cap,
                       RtsFlags.GcFlags.initialStkSize,
                       rts_apply(cap,
                           rts_apply(cap,
                               (StgClosure *)runFinalizerBatch_closure,
                               rts_mkInt(cap,n)),
                           (StgClosure *)arr)
        );

    scheduleThread(cap,t);
    labelThread(cap, t, "weak finalizer thread");
}

/* -----------------------------------------------------------------------------
   Incrementally running C finalizers

   The GC detects all the dead finalizers, but we don't want to run
   them during the GC because that increases the time that the runtime
   is paused.

   What options are there?

   1. Parallelise running the C finalizers across the GC threads
      - doesn't solve the pause problem, just reduces it (maybe by a lot)

   2. Make a Haskell thread to run the C finalizers, like we do for
      Haskell finalizers.
      + scheduling is handled for us
      - no guarantee that we'll process finalizers in a timely manner

   3. Run finalizers when any capability is idle.
      + reduces pause to 0
      - requires scheduler modifications
      - if the runtime is busy, finalizers wait until the next GC

   4. like (3), but also run finalizers incrementally between GCs.
      - reduces the delay to run finalizers compared with (3)

   For now we do (3). It would be easy to do (4) later by adding a
   call to doIdleGCWork() in the scheduler loop, but I haven't found
   that necessary so far.

   -------------------------------------------------------------------------- */

// Run this many finalizers before returning from
// runSomeFinalizers(). This is so that we only tie up the capability
// for a short time, and respond quickly if new work becomes
// available.
static const int32_t finalizer_chunk = 100;

// non-zero if a thread is already in runSomeFinalizers(). This
// protects the globals finalizer_list and n_finalizers.
static volatile StgWord finalizer_lock = 0;

//
// Run some C finalizers.  Returns true if there's more work to do.
//
bool runSomeFinalizers(bool all)
{
    if (n_finalizers == 0)
        return false;

    if (cas(&finalizer_lock, 0, 1) != 0) {
        // another capability is doing the work, it's safe to say
        // there's nothing to do, because the thread already in
        // runSomeFinalizers() will call in again.
        return false;
    }

    debugTrace(DEBUG_sched, "running C finalizers, %d remaining", n_finalizers);

    Task *task = myTask();
    if (task != NULL) {
        task->running_finalizers = true;
    }

    StgWeak *w = finalizer_list;
    int32_t count = 0;
    while (w != NULL) {
        runCFinalizers((StgCFinalizerList *)w->cfinalizers);
        w = w->link;
        ++count;
        if (!all && count >= finalizer_chunk) break;
    }

    finalizer_list = w;
    n_finalizers -= count;

    if (task != NULL) {
        task->running_finalizers = false;
    }

    debugTrace(DEBUG_sched, "ran %d C finalizers", count);

    write_barrier();
    finalizer_lock = 0;

    return n_finalizers != 0;
}
