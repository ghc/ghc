/* -----------------------------------------------------------------------------
 * $Id: Weak.c,v 1.3 1999/01/13 17:25:49 simonm Exp $
 *
 * Weak pointers / finalisers
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsFlags.h"
#include "Weak.h"
#include "Storage.h"

StgWeak *weak_ptr_list;

/*
 * finaliseWeakPointersNow() is called just before the system is shut
 * down.  It runs the finaliser for each weak pointer still in the
 * system.
 */

void
finaliseWeakPointersNow(void)
{
  StgWeak *w;

  for (w = weak_ptr_list; w; w = w->link) {
    IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p\n", w));
    w->header.info = &DEAD_WEAK_info;
    rts_evalIO(w->finaliser,NULL);
  }
} 

/*
 * scheduleFinalisers() is called on the list of weak pointers found
 * to be dead after a garbage collection.  It overwrites each object
 * with DEAD_WEAK, and creates a new thread for the finaliser.
 */

void
scheduleFinalisers(StgWeak *list)
{
  StgWeak *w;
  
  for (w = list; w; w = w->link) {
    IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p\n", w));
#ifdef INTERPRETER
    createGenThread(RtsFlags.GcFlags.initialStkSize, w->finaliser);
#else
    createIOThread(RtsFlags.GcFlags.initialStkSize, w->finaliser);
#endif
    w->header.info = &DEAD_WEAK_info;

    /* need to fill the slop with zeros if we're sanity checking */
    IF_DEBUG(sanity, {
      nat dw_size = sizeW_fromITBL(get_itbl(w));
      memset((P_)w + dw_size, 0, (sizeofW(StgWeak) - dw_size) * sizeof(W_));
    });
  }
}

void
markWeakList(void)
{
  StgWeak *w, **last_w;

  last_w = &weak_ptr_list;
  for (w = weak_ptr_list; w; w = w->link) {
    w = (StgWeak *)MarkRoot((StgClosure *)w);
    *last_w = w;
    last_w = &(w->link);
  }
}

