/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#define COMPILING_RTS_MAIN
#include "Rts.h"
#include "RtsUtils.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "Weak.h"
#include "Storage.h"
#include "Schedule.h"
#include "Prelude.h"
#include "RtsAPI.h"

StgWeak *weak_ptr_list;

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
    nat n;

    // count number of finalizers, and kill all the weak pointers first...
    n = 0;
    for (w = list; w; w = w->link) { 

	// Better not be a DEAD_WEAK at this stage; the garbage
	// collector removes DEAD_WEAKs from the weak pointer list.
	ASSERT(w->header.info != &stg_DEAD_WEAK_info);

	if (w->finalizer != &stg_NO_FINALIZER_closure) {
	    n++;
	}

#ifdef PROFILING
        // A weak pointer is inherently used, so we do not need to call
        // LDV_recordDead().
	//
        // Furthermore, when PROFILING is turned on, dead weak
        // pointers are exactly as large as weak pointers, so there is
        // no need to fill the slop, either.  See stg_DEAD_WEAK_info
        // in StgMiscClosures.hc.
#endif
	SET_HDR(w, &stg_DEAD_WEAK_info, w->header.prof.ccs);
    }
	
    // No finalizers to run?
    if (n == 0) return;

    IF_DEBUG(weak,debugBelch("weak: batching %d finalizers\n", n));

    arr = (StgMutArrPtrs *)allocateLocal(cap, sizeofW(StgMutArrPtrs) + n);
    TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), n, 0);
    SET_HDR(arr, &stg_MUT_ARR_PTRS_FROZEN_info, CCS_SYSTEM);
    arr->ptrs = n;

    n = 0;
    for (w = list; w; w = w->link) {
	if (w->finalizer != &stg_NO_FINALIZER_closure) {
	    arr->payload[n] = w->finalizer;
	    n++;
	}
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
}
