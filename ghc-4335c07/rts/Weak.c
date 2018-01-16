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
    Task *task;

    task = myTask();
    if (task != NULL) {
        task->running_finalizers = true;
    }

    // count number of finalizers, and kill all the weak pointers first...
    n = 0;
    for (w = list; w; w = w->link) {
        // Better not be a DEAD_WEAK at this stage; the garbage
        // collector removes DEAD_WEAKs from the weak pointer list.
        ASSERT(w->header.info != &stg_DEAD_WEAK_info);

        if (w->finalizer != &stg_NO_FINALIZER_closure) {
            n++;
        }

        runCFinalizers((StgCFinalizerList *)w->cfinalizers);

#if defined(PROFILING)
        // A weak pointer is inherently used, so we do not need to call
        // LDV_recordDead().
        //
        // Furthermore, when PROFILING is turned on, dead weak
        // pointers are exactly as large as weak pointers, so there is
        // no need to fill the slop, either.  See stg_DEAD_WEAK_info
        // in StgMiscClosures.cmm.
#endif
        SET_HDR(w, &stg_DEAD_WEAK_info, w->header.prof.ccs);
    }

    if (task != NULL) {
        task->running_finalizers = false;
    }

    // No finalizers to run?
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
