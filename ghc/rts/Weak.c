/* -----------------------------------------------------------------------------
 * $Id: Weak.c,v 1.21 2002/02/18 13:26:13 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "Weak.h"
#include "Storage.h"
#include "Prelude.h"

StgWeak *weak_ptr_list;

/*
 * finalizeWeakPointersNow() is called just before the system is shut
 * down.  It runs the finalizer for each weak pointer still in the
 * system.
 *
 * Careful here - rts_evalIO might cause a garbage collection, which
 * might change weak_ptr_list.  Must re-load weak_ptr_list each time
 * around the loop.
 */

void
finalizeWeakPointersNow(void)
{
  StgWeak *w;
  
  while ((w = weak_ptr_list)) {
    weak_ptr_list = w->link;
    if (w->header.info != &stg_DEAD_WEAK_info) {
	w->header.info = &stg_DEAD_WEAK_info;
	IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p -> %p\n", w, w->key));
	if (w->finalizer != &stg_NO_FINALIZER_closure) {
	    rts_evalIO(w->finalizer,NULL);
	}
    }
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
scheduleFinalizers(StgWeak *list)
{
    StgWeak *w;
    StgTSO *t;
    StgMutArrPtrs *arr;
    nat n;

    /* count number of finalizers first... */
    for (n = 0, w = list; w; w = w->link) { 
	if (w->finalizer != &stg_NO_FINALIZER_closure)
	    n++;
    }
	
    if (n == 0) return;

    IF_DEBUG(weak,fprintf(stderr,"weak: batching %d finalizers\n", n));

    arr = (StgMutArrPtrs *)allocate(sizeofW(StgMutArrPtrs) + n);
    SET_HDR(arr, &stg_MUT_ARR_PTRS_FROZEN_info, CCS_SYSTEM);
    arr->ptrs = n;

    for (n = 0, w = list; w; w = w->link) {
	if (w->finalizer != &stg_NO_FINALIZER_closure) {
	    arr->payload[n] = w->finalizer;
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

    t = createIOThread(RtsFlags.GcFlags.initialStkSize, 
		       rts_apply(
			   rts_apply(
			       (StgClosure *)runFinalizerBatch_closure,
			       rts_mkInt(n)), 
			   (StgClosure *)arr)
	);
    scheduleThread(t);
}
