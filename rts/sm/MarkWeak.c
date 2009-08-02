/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Weak pointers and weak-like things in the GC
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "MarkWeak.h"
#include "GC.h"
#include "GCThread.h"
#include "Evac.h"
#include "Trace.h"
#include "Schedule.h"
#include "Weak.h"

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

// List of threads found to be unreachable
StgTSO *resurrected_threads;

// List of blocked threads found to have pending throwTos
StgTSO *exception_threads;

void
initWeakForGC(void)
{
    old_weak_ptr_list = weak_ptr_list;
    weak_ptr_list = NULL;
    weak_stage = WeakPtrs;
    resurrected_threads = END_TSO_QUEUE;
    exception_threads = END_TSO_QUEUE;
}

rtsBool 
traverseWeakPtrList(void)
{
  StgWeak *w, **last_w, *next_w;
  StgClosure *new;
  rtsBool flag = rtsFalse;
  const StgInfoTable *info;

  switch (weak_stage) {

  case WeakDone:
      return rtsFalse;

  case WeakPtrs:
      /* doesn't matter where we evacuate values/finalizers to, since
       * these pointers are treated as roots (iff the keys are alive).
       */
      gct->evac_step = 0;
      
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
	  
          info = w->header.info;
          if (IS_FORWARDING_PTR(info)) {
	      next_w = (StgWeak *)UN_FORWARDING_PTR(info);
	      *last_w = next_w;
	      continue;
          }

	  switch (INFO_PTR_TO_STRUCT(info)->type) {

	  case WEAK:
	      /* Now, check whether the key is reachable.
	       */
	      new = isAlive(w->key);
	      if (new != NULL) {
		  w->key = new;
		  // evacuate the value and finalizer 
		  evacuate(&w->value);
		  evacuate(&w->finalizer);
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
	      evacuate(&w->finalizer);
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
          nat g, s;
          step *stp;
	  
          // Traverse thread lists for generations we collected...
          for (g = 0; g <= N; g++) {
              for (s = 0; s < generations[g].n_steps; s++) {
                  stp = &generations[g].steps[s];

                  prev = &stp->old_threads;

                  for (t = stp->old_threads; t != END_TSO_QUEUE; t = next) {
	      
                      tmp = (StgTSO *)isAlive((StgClosure *)t);
	      
                      if (tmp != NULL) {
                          t = tmp;
                      }

                      ASSERT(get_itbl(t)->type == TSO);
                      if (t->what_next == ThreadRelocated) {
                          next = t->_link;
                          *prev = next;
                          continue;
                      }

                      next = t->global_link;

                      // This is a good place to check for blocked
                      // exceptions.  It might be the case that a thread is
                      // blocked on delivering an exception to a thread that
                      // is also blocked - we try to ensure that this
                      // doesn't happen in throwTo(), but it's too hard (or
                      // impossible) to close all the race holes, so we
                      // accept that some might get through and deal with
                      // them here.  A GC will always happen at some point,
                      // even if the system is otherwise deadlocked.
                      //
                      // If an unreachable thread has blocked
                      // exceptions, we really want to perform the
                      // blocked exceptions rather than throwing
                      // BlockedIndefinitely exceptions.  This is the
                      // only place we can discover such threads.
                      // The target thread might even be
                      // ThreadFinished or ThreadKilled.  Bugs here
                      // will only be seen when running on a
                      // multiprocessor.
                      if (t->blocked_exceptions != END_TSO_QUEUE) {
                          if (tmp == NULL) {
                              evacuate((StgClosure **)&t);
                              flag = rtsTrue;
                          }
                          t->global_link = exception_threads;
                          exception_threads = t;
                          *prev = next;
                          continue;
                      }

                      if (tmp == NULL) {
                          // not alive (yet): leave this thread on the
                          // old_all_threads list.
                          prev = &(t->global_link);
                      } 
                      else {
                          // alive
                          *prev = next;

                          // move this thread onto the correct threads list.
                          step *new_step;
                          new_step = Bdescr((P_)t)->step;
                          t->global_link = new_step->threads;
                          new_step->threads  = t;
                      }
                  }
              }
          }
      }

      /* If we evacuated any threads, we need to go back to the scavenger.
       */
      if (flag) return rtsTrue;

      /* And resurrect any threads which were about to become garbage.
       */
      {
          nat g, s;
          step *stp;
	  StgTSO *t, *tmp, *next;

          for (g = 0; g <= N; g++) {
              for (s = 0; s < generations[g].n_steps; s++) {
                  stp = &generations[g].steps[s];

                  for (t = stp->old_threads; t != END_TSO_QUEUE; t = next) {
                      next = t->global_link;

                      // ThreadFinished and ThreadComplete: we have to keep
                      // these on the all_threads list until they
                      // become garbage, because they might get
                      // pending exceptions.
                      switch (t->what_next) {
                      case ThreadKilled:
                      case ThreadComplete:
                          continue;
                      default:
                          tmp = t;
                          evacuate((StgClosure **)&tmp);
                          tmp->global_link = resurrected_threads;
                          resurrected_threads = tmp;
                      }
                  }
              }
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
	  for (pt = &blackhole_queue; *pt != END_TSO_QUEUE; pt = &((*pt)->_link)) {
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
    nat type;

    flag = rtsFalse;
    prev = NULL;

    for (t = blackhole_queue; t != END_TSO_QUEUE; prev=t, t = t->_link) {
        // if the thread is not yet alive...
	if (! (tmp = (StgTSO *)isAlive((StgClosure*)t))) {
            // if the closure it is blocked on is either (a) a
            // reachable BLAKCHOLE or (b) not a BLACKHOLE, then we
            // make the thread alive.
	    if (!isAlive(t->block_info.closure)) {
                type = get_itbl(t->block_info.closure)->type;
                if (type == BLACKHOLE || type == CAF_BLACKHOLE) {
                    continue;
                }
            }
            evacuate((StgClosure **)&t);
            if (prev) {
                prev->_link = t;
            } else {
                blackhole_queue = t;
            }
                 // no write barrier when on the blackhole queue,
                 // because we traverse the whole queue on every GC.
            flag = rtsTrue;
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
  StgWeak *w, **last_w, *tmp;

  last_w = &weak_ptr_list;
  for (w = weak_ptr_list; w; w = w->link) {
      // w might be WEAK, EVACUATED, or DEAD_WEAK (actually CON_STATIC) here
      ASSERT(IS_FORWARDING_PTR(w->header.info)
             || w->header.info == &stg_DEAD_WEAK_info 
	     || get_itbl(w)->type == WEAK);
      tmp = w;
      evacuate((StgClosure **)&tmp);
      *last_w = w;
      last_w = &(w->link);
  }
}

