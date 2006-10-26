/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Weak pointers and weak-like things in the GC
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"
#include "MarkWeak.h"
#include "GC.h"
#include "Evac.h"
#include "Trace.h"
#include "Schedule.h"

/* -----------------------------------------------------------------------------
   Weak Pointers

   traverse_weak_ptr_list is called possibly many times during garbage
   collection.  It returns a flag indicating whether it did any work
   (i.e. called evacuate on any live pointers).

   Invariant: traverse_weak_ptr_list is called when the heap is in an
   idempotent state.  That means that there are no pending
   evacuate/scavenge operations.  This invariant helps the weak
   pointer code decide which weak pointers are dead - if there are no
   new live weak pointers, then all the currently unreachable ones are
   dead.

   For generational GC: we just don't try to finalize weak pointers in
   older generations than the one we're collecting.  This could
   probably be optimised by keeping per-generation lists of weak
   pointers, but for a few weak pointers this scheme will work.

   There are three distinct stages to processing weak pointers:

   - weak_stage == WeakPtrs

     We process all the weak pointers whos keys are alive (evacuate
     their values and finalizers), and repeat until we can find no new
     live keys.  If no live keys are found in this pass, then we
     evacuate the finalizers of all the dead weak pointers in order to
     run them.

   - weak_stage == WeakThreads

     Now, we discover which *threads* are still alive.  Pointers to
     threads from the all_threads and main thread lists are the
     weakest of all: a pointers from the finalizer of a dead weak
     pointer can keep a thread alive.  Any threads found to be unreachable
     are evacuated and placed on the resurrected_threads list so we 
     can send them a signal later.

   - weak_stage == WeakDone

     No more evacuation is done.

   -------------------------------------------------------------------------- */

/* Which stage of processing various kinds of weak pointer are we at?
 * (see traverse_weak_ptr_list() below for discussion).
 */
typedef enum { WeakPtrs, WeakThreads, WeakDone } WeakStage;
static WeakStage weak_stage;

/* Weak pointers
 */
StgWeak *old_weak_ptr_list; // also pending finaliser list

/* List of all threads during GC
 */
StgTSO *resurrected_threads;
static StgTSO *old_all_threads;

void
initWeakForGC(void)
{
    old_weak_ptr_list = weak_ptr_list;
    weak_ptr_list = NULL;
    weak_stage = WeakPtrs;

    /* The all_threads list is like the weak_ptr_list.  
     * See traverseWeakPtrList() for the details.
     */
    old_all_threads = all_threads;
    all_threads = END_TSO_QUEUE;
    resurrected_threads = END_TSO_QUEUE;
}

rtsBool 
traverseWeakPtrList(void)
{
  StgWeak *w, **last_w, *next_w;
  StgClosure *new;
  rtsBool flag = rtsFalse;

  switch (weak_stage) {

  case WeakDone:
      return rtsFalse;

  case WeakPtrs:
      /* doesn't matter where we evacuate values/finalizers to, since
       * these pointers are treated as roots (iff the keys are alive).
       */
      evac_gen = 0;
      
      last_w = &old_weak_ptr_list;
      for (w = old_weak_ptr_list; w != NULL; w = next_w) {
	  
	  /* There might be a DEAD_WEAK on the list if finalizeWeak# was
	   * called on a live weak pointer object.  Just remove it.
	   */
	  if (w->header.info == &stg_DEAD_WEAK_info) {
	      next_w = ((StgDeadWeak *)w)->link;
	      *last_w = next_w;
	      continue;
	  }
	  
	  switch (get_itbl(w)->type) {

	  case EVACUATED:
	      next_w = (StgWeak *)((StgEvacuated *)w)->evacuee;
	      *last_w = next_w;
	      continue;

	  case WEAK:
	      /* Now, check whether the key is reachable.
	       */
	      new = isAlive(w->key);
	      if (new != NULL) {
		  w->key = new;
		  // evacuate the value and finalizer 
		  w->value = evacuate(w->value);
		  w->finalizer = evacuate(w->finalizer);
		  // remove this weak ptr from the old_weak_ptr list 
		  *last_w = w->link;
		  // and put it on the new weak ptr list 
		  next_w  = w->link;
		  w->link = weak_ptr_list;
		  weak_ptr_list = w;
		  flag = rtsTrue;

		  debugTrace(DEBUG_weak, 
			     "weak pointer still alive at %p -> %p",
			     w, w->key);
		  continue;
	      }
	      else {
		  last_w = &(w->link);
		  next_w = w->link;
		  continue;
	      }

	  default:
	      barf("traverseWeakPtrList: not WEAK");
	  }
      }
      
      /* If we didn't make any changes, then we can go round and kill all
       * the dead weak pointers.  The old_weak_ptr list is used as a list
       * of pending finalizers later on.
       */
      if (flag == rtsFalse) {
	  for (w = old_weak_ptr_list; w; w = w->link) {
	      w->finalizer = evacuate(w->finalizer);
	  }

	  // Next, move to the WeakThreads stage after fully
	  // scavenging the finalizers we've just evacuated.
	  weak_stage = WeakThreads;
      }

      return rtsTrue;

  case WeakThreads:
      /* Now deal with the all_threads list, which behaves somewhat like
       * the weak ptr list.  If we discover any threads that are about to
       * become garbage, we wake them up and administer an exception.
       */
      {
	  StgTSO *t, *tmp, *next, **prev;
	  
	  prev = &old_all_threads;
	  for (t = old_all_threads; t != END_TSO_QUEUE; t = next) {
	      
	      tmp = (StgTSO *)isAlive((StgClosure *)t);
	      
	      if (tmp != NULL) {
		  t = tmp;
	      }
	      
	      ASSERT(get_itbl(t)->type == TSO);
	      switch (t->what_next) {
	      case ThreadRelocated:
		  next = t->link;
		  *prev = next;
		  continue;
	      case ThreadKilled:
	      case ThreadComplete:
		  // finshed or died.  The thread might still be alive, but we
		  // don't keep it on the all_threads list.  Don't forget to
		  // stub out its global_link field.
		  next = t->global_link;
		  t->global_link = END_TSO_QUEUE;
		  *prev = next;
		  continue;
	      default:
		  ;
	      }
	      
	      if (tmp == NULL) {
		  // not alive (yet): leave this thread on the
		  // old_all_threads list.
		  prev = &(t->global_link);
		  next = t->global_link;
	      } 
	      else {
		  // alive: move this thread onto the all_threads list.
		  next = t->global_link;
		  t->global_link = all_threads;
		  all_threads  = t;
		  *prev = next;
	      }
	  }
      }
      
      /* If we evacuated any threads, we need to go back to the scavenger.
       */
      if (flag) return rtsTrue;

      /* And resurrect any threads which were about to become garbage.
       */
      {
	  StgTSO *t, *tmp, *next;
	  for (t = old_all_threads; t != END_TSO_QUEUE; t = next) {
	      next = t->global_link;
	      tmp = (StgTSO *)evacuate((StgClosure *)t);
	      tmp->global_link = resurrected_threads;
	      resurrected_threads = tmp;
	  }
      }
      
      /* Finally, we can update the blackhole_queue.  This queue
       * simply strings together TSOs blocked on black holes, it is
       * not intended to keep anything alive.  Hence, we do not follow
       * pointers on the blackhole_queue until now, when we have
       * determined which TSOs are otherwise reachable.  We know at
       * this point that all TSOs have been evacuated, however.
       */
      { 
	  StgTSO **pt;
	  for (pt = &blackhole_queue; *pt != END_TSO_QUEUE; pt = &((*pt)->link)) {
	      *pt = (StgTSO *)isAlive((StgClosure *)*pt);
	      ASSERT(*pt != NULL);
	  }
      }

      weak_stage = WeakDone;  // *now* we're done,
      return rtsTrue;         // but one more round of scavenging, please

  default:
      barf("traverse_weak_ptr_list");
      return rtsTrue;
  }

}

/* -----------------------------------------------------------------------------
   The blackhole queue
   
   Threads on this list behave like weak pointers during the normal
   phase of garbage collection: if the blackhole is reachable, then
   the thread is reachable too.
   -------------------------------------------------------------------------- */
rtsBool
traverseBlackholeQueue (void)
{
    StgTSO *prev, *t, *tmp;
    rtsBool flag;

    flag = rtsFalse;
    prev = NULL;

    for (t = blackhole_queue; t != END_TSO_QUEUE; prev=t, t = t->link) {
	if (! (tmp = (StgTSO *)isAlive((StgClosure*)t))) {
	    if (isAlive(t->block_info.closure)) {
		t = (StgTSO *)evacuate((StgClosure *)t);
		if (prev) prev->link = t;
		flag = rtsTrue;
	    }
	}
    }
    return flag;
}

/* -----------------------------------------------------------------------------
   After GC, the live weak pointer list may have forwarding pointers
   on it, because a weak pointer object was evacuated after being
   moved to the live weak pointer list.  We remove those forwarding
   pointers here.

   Also, we don't consider weak pointer objects to be reachable, but
   we must nevertheless consider them to be "live" and retain them.
   Therefore any weak pointer objects which haven't as yet been
   evacuated need to be evacuated now.
   -------------------------------------------------------------------------- */

void
markWeakPtrList ( void )
{
  StgWeak *w, **last_w;

  last_w = &weak_ptr_list;
  for (w = weak_ptr_list; w; w = w->link) {
      // w might be WEAK, EVACUATED, or DEAD_WEAK (actually CON_STATIC) here
      ASSERT(w->header.info == &stg_DEAD_WEAK_info 
	     || get_itbl(w)->type == WEAK || get_itbl(w)->type == EVACUATED);
      w = (StgWeak *)evacuate((StgClosure *)w);
      *last_w = w;
      last_w = &(w->link);
  }
}

