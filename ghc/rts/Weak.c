/* -----------------------------------------------------------------------------
 * $Id: Weak.c,v 1.4 1999/01/26 11:12:53 simonm Exp $
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
    IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p -> %p\n", w, w->key));
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
    IF_DEBUG(weak,fprintf(stderr,"Finalising weak pointer at %p -> %p\n", w, w->key));
#ifdef INTERPRETER
    createGenThread(RtsFlags.GcFlags.initialStkSize, w->finaliser);
#else
    createIOThread(RtsFlags.GcFlags.initialStkSize, w->finaliser);
#endif
    w->header.info = &DEAD_WEAK_info;
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

