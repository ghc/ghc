/* -----------------------------------------------------------------------------
 * $Id: Weak.c,v 1.11 1999/07/06 15:05:49 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "Weak.h"
#include "Storage.h"

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
    IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p -> %p\n", w, w->key));
    w->header.info = &DEAD_WEAK_info;
    if (w->finalizer != &NO_FINALIZER_closure) {
      rts_evalIO(w->finalizer,NULL);
    }
  }
} 

/*
 * scheduleFinalizers() is called on the list of weak pointers found
 * to be dead after a garbage collection.  It overwrites each object
 * with DEAD_WEAK, and creates a new thread for the finalizer.
 *
 * This function is called just after GC.  The weak pointers on the
 * argument list are those whose keys were found to be not reachable,
 * however the value and finalizer fields have by now been marked live.
 * The weak pointer object itself may not be alive - i.e. we may be
 * looking at either an object in from-space or one in to-space.  It
 * doesn't really matter either way.
 */

void
scheduleFinalizers(StgWeak *list)
{
  StgWeak *w;
  
  for (w = list; w; w = w->link) {
    IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p -> %p\n", w, w->key));
    if (w->finalizer != &NO_FINALIZER_closure) {
#ifdef INTERPRETER
      createGenThread(RtsFlags.GcFlags.initialStkSize, w->finalizer);
#else
      createIOThread(RtsFlags.GcFlags.initialStkSize, w->finalizer);
#endif
    }
    w->header.info = &DEAD_WEAK_info;
  }
}
