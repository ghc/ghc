/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2010
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
#include "GCTDecl.h"
#include "GCUtils.h"
#include "Evac.h"
#include "Trace.h"
#include "Schedule.h"
#include "Weak.h"
#include "Storage.h"
#include "Threads.h"

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

static void resurrectUnreachableThreads (generation *gen);
static rtsBool tidyThreadList (generation *gen);

void
initWeakForGC(void)
{
    gct->old_weak_ptrs = NULL;
    gct->weak_stage = WeakPtrs;
    gct->resurrected_threads = END_TSO_QUEUE;
}

rtsBool 
traverseWeakPtrList(void)
{
  StgWeak *w, **last_w, *next_w;
  rtsBool flag = rtsFalse;
  generation *gen;
  bdescr *bd;
  const StgInfoTable *info;
  StgClosure *new;
  nat g;

  switch (gct->weak_stage) {

  case WeakDone:
      return rtsFalse;

  case WeakPtrs:

      last_w = &gct->old_weak_ptrs;
      for (w = gct->old_weak_ptrs; w != NULL; w = next_w) {
          
          /* There might be a DEAD_WEAK on the list if finalizeWeak# was
           * called on a live weak pointer object.  Just remove it.
           */
          if (w->header.info == &stg_DEAD_WEAK_info) {
              next_w = ((StgDeadWeak *)w)->link;
              *last_w = next_w;
              continue;
          }
          
          info = get_itbl(w);
          switch (info->type) {
              
          case WEAK:
              /* Now, check whether the key is reachable.
               */
              new = isAlive(w->key);
              if (new != NULL) {
                  w->key = new;

                  bd = Bdescr((P_)w);
                  gct->evac_gen_ix = bd->gen_ix;
              
                  // evacuate the value and finalizer 
                  evacuate(&w->value);
                  evacuate(&w->finalizer);
                  // remove this weak ptr from the old_weak_ptrs list 
                  *last_w = w->link;
                  // and put it on the new weak ptr list
                  next_w  = w->link;
                  w->link = bd->gen->weak_ptrs;
                  bd->gen->weak_ptrs = w;
                  flag = rtsTrue;
                  
                  debugTrace(DEBUG_weak, 
                             "weak pointer still alive at %p -> %p",
                             w, w->key);
                  
                  if (gct->failed_to_evac || 
                      Bdescr((P_)w->key)->gen_ix < bd->gen_ix) {
                      gct->failed_to_evac = rtsFalse;
                      recordMutableGen_GC((StgClosure *)w,bd->gen_no);
                  }
                  
                  continue;
              }
              else {
                  last_w = &(w->link);
                  next_w = w->link;
                  continue;
              }
              
          default:
              barf("traverseWeakPtrListGen: not WEAK");
          }
      }
        
      /* If we didn't make any changes, then we can go round and kill all
       * the dead weak pointers.  The old_weak_ptr list is used as a list
       * of pending finalizers later on.
       */
      if (flag == rtsFalse) {
          for (w = gct->old_weak_ptrs; w; w = w->link) {
              bd = Bdescr((P_)w);
              gct->evac_gen_ix = bd->gen_ix;
              evacuate(&w->finalizer);
              if (gct->failed_to_evac) {
                  gct->failed_to_evac = rtsFalse;
                  recordMutableGen_GC((StgClosure *)w,Bdescr((P_)w)->gen_no);
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
              SET_INFO(w, &stg_DEAD_WEAK_info);
          }
                
          // Next, move to the WeakThreads stage after fully
          // scavenging the finalizers we've just evacuated.
          gct->weak_stage = WeakThreads;
      }

      return rtsTrue;

  case WeakThreads:
      /* Now deal with the gen->threads lists, which behave somewhat like
       * the weak ptr list.  If we discover any threads that are about to
       * become garbage, we wake them up and administer an exception.
       *
       * We can't do in the same phase as WeakPtrs, because we want
       * the property that a reachable finalizer can keep a thread
       * alive (see test conc031).
       */
  {
	  
      // Traverse thread lists for generations we collected...
      for (g = 0; g < total_generations; g++) {
          gen = &all_generations[g];
          if (gen->no > gct->collect_gen) break;
          if (gct->gc_type == GC_LOCAL && isNonLocalGen(gen)) continue;
          if (tidyThreadList(gen)) {
              flag = rtsTrue;
          }
      }
        
      /* If we evacuated any threads, we need to go back to the scavenger.
       */
      if (flag) return rtsTrue;

      /* And resurrect any threads which were about to become garbage.
       */
      for (g = 0; g < total_generations; g++) {
          gen = &all_generations[g];
          if (gen->no > gct->collect_gen) break;
          if (gct->gc_type == GC_LOCAL && isNonLocalGen(gen)) continue;
          resurrectUnreachableThreads(gen);
      }

      gct->weak_stage = WeakDone;  // *now* we're done,
      return rtsTrue;         // but one more round of scavenging, please
  }
      
  default:
      barf("traverse_weak_ptr_list");
      return rtsTrue;
  }
}
  
static void resurrectUnreachableThreads (generation *gen)
{
    StgTSO *t, *tmp, *next;

    for (t = gen->old_threads; t != END_TSO_QUEUE; t = next) {
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

            if (gct->gc_type == GC_LOCAL) {
                // We don't currently resurrect threads with
                // exceptions in GC_LOCAL.  It's hard to get right,
                // becuase we would have to do real throwTo rather
                // than throwToSingleThhreaded.
                generation *gen = Bdescr((P_)tmp)->gen;
                tmp->global_link = gen->threads;
                gen->threads = tmp;
            } else {
                tmp->global_link = gct->resurrected_threads;
                gct->resurrected_threads = tmp;
            }
        }
    }
}


static rtsBool tidyThreadList (generation *gen)
{
    StgTSO *t, *tmp, *next, **prev;
    rtsBool flag = rtsFalse;

    prev = &gen->old_threads;

    for (t = gen->old_threads; t != END_TSO_QUEUE; t = next) {
	      
        if (gct->gc_type == GC_LOCAL) {
            tmp = t;
            evacuate((StgClosure **)&tmp);
        } else {
            tmp = (StgTSO *)isAlive((StgClosure *)t);
        }
        
        if (tmp != NULL) {
            t = tmp;
        }

        next = t->global_link;
        
        ASSERT(get_itbl(t)->type == TSO);

        // if the thread is not masking exceptions but there are
        // pending exceptions on its queue, then something has gone
        // wrong.  However, pending exceptions are OK if there is an
        // FFI call.
        ASSERT(t->blocked_exceptions == END_BLOCKED_EXCEPTIONS_QUEUE
               || t->why_blocked == BlockedOnCCall
               || t->why_blocked == BlockedOnCCall_Interruptible
               || (t->flags & TSO_BLOCKEX));
        
        if (tmp == NULL) {
            // not alive (yet): leave this thread on the
            // old_all_threads list.
            prev = &(t->global_link);
        } 
        else {
            // alive
            *prev = next;
            
            // move this thread onto the correct threads list.
            generation *new_gen;
            new_gen = Bdescr((P_)t)->gen;
            t->global_link = new_gen->threads;
            new_gen->threads  = t;
        }
    }

    return flag;
}

/* -----------------------------------------------------------------------------
   Every weak pointer object (WEAK) is treated as implicitly alive to
   begin with.  Only when w->key is found to be unreachable does the
   WEAK turn into a DEAD_WEAK which may be subsequently GC'd.

   All WEAK objects are chained together by their w->link fields.
   Since we're doing generational GC, it makes sense to divide this
   list per-generation, so that we don't have to traverse the weak
   pointers of old generations when collecting young generations, so
   each generation has its own gen->weak_ptrs list.

   In markWeakPtr list we traverse the gen->weak_ptrs lists of
   generations we are collecting, and evacuate each one.  The
   resulting weak pointer objects are chained together on a temporary
   list gct->old_weak_ptrs, which we will traverse again (possibly
   multiple times) in traverseWeakPtrList().  Eventually WEAK objects
   will be placed on the appropriate gen->weak_ptrs list of the
   generation that they now belong to.
  -------------------------------------------------------------------------- */

void
markWeakPtrList ( void )
{
    StgWeak *w, *next;
    generation *gen;
    nat g;

    for (g = 0; g < total_generations; g++) {
        gen = &all_generations[g];
        if (gen->no > gct->collect_gen) break;
        if (gct->gc_type == GC_LOCAL && isNonLocalGen(gen)) continue;
        
        for (w = gen->weak_ptrs; w; w = next) {
            // w might be WEAK, a forwarding ptr, or DEAD_WEAK (actually
            // CONSTR_STATIC) here
            
#ifdef DEBUG
            {   // careful to do this assertion only reading the info ptr
                // once, because during parallel GC it might change under our feet.
                const StgInfoTable *info;
                info = w->header.info;
                ASSERT(IS_FORWARDING_PTR(info)
                       || info == &stg_DEAD_WEAK_info 
                       || INFO_PTR_TO_STRUCT(info)->type == WEAK);
            }
#endif
            
            evacuate((StgClosure**)&w);

            next = w->link;
            // We drop DEAD_WEAK objects from the list here.
            if (w->header.info != &stg_DEAD_WEAK_info) {
                w->link = gct->old_weak_ptrs;
                gct->old_weak_ptrs = w;
            }
        }

        gen->weak_ptrs = NULL;
        // we'll re-populate these lists later in traverseWeakPtrList()
    }
}
