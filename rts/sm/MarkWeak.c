/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Weak pointers and weak-like things in the GC
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "MarkWeak.h"
#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"
#include "Evac.h"
#include "Trace.h"
#include "Schedule.h"
#include "Weak.h"
#include "Storage.h"
#include "Threads.h"

#include "sm/GCUtils.h"
#include "sm/MarkWeak.h"
#include "sm/Sanity.h"

/* -----------------------------------------------------------------------------
   Weak Pointers

   traverseWeakPtrList is called possibly many times during garbage
   collection.  It returns a flag indicating whether it did any work
   (i.e. called evacuate on any live pointers).

   Invariant: traverseWeakPtrList is called when the heap is in an
   idempotent state.  That means that there are no pending
   evacuate/scavenge operations.  This invariant helps the weak
   pointer code decide which weak pointers are dead - if there are no
   new live weak pointers, then all the currently unreachable ones are
   dead.

   For generational GC: we don't try to finalize weak pointers in
   older generations than the one we're collecting.

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
     weakest of all: a pointer from the finalizer of a dead weak
     pointer can keep a thread alive.  Any threads found to be unreachable
     are evacuated and placed on the resurrected_threads list so we
     can send them a signal later.

   - weak_stage == WeakDone

     No more evacuation is done.

   -------------------------------------------------------------------------- */

/* Which stage of processing various kinds of weak pointer are we at?
 * (see traverseWeakPtrList() below for discussion).
 */
typedef enum { WeakPtrs, WeakThreads, WeakDone } WeakStage;
static WeakStage weak_stage;

// List of weak pointers whose key is dead
StgWeak *dead_weak_ptr_list;

// List of threads found to be unreachable
StgTSO *resurrected_threads;

static void    collectDeadWeakPtrs (generation *gen);
static bool tidyWeakList (generation *gen);
static bool resurrectUnreachableThreads (generation *gen);
static void    tidyThreadList (generation *gen);

void
initWeakForGC(void)
{
    uint32_t g;

    for (g = 0; g <= N; g++) {
        generation *gen = &generations[g];
        gen->old_weak_ptr_list = gen->weak_ptr_list;
        gen->weak_ptr_list = NULL;
    }

    weak_stage = WeakThreads;
    dead_weak_ptr_list = NULL;
    resurrected_threads = END_TSO_QUEUE;
}

bool
traverseWeakPtrList(void)
{
  bool flag = false;

  switch (weak_stage) {

  case WeakDone:
      return false;

  case WeakThreads:
      /* Now deal with the gen->threads lists, which behave somewhat like
       * the weak ptr list.  If we discover any threads that are about to
       * become garbage, we wake them up and administer an exception.
       */
  {
      uint32_t g;

      for (g = 0; g <= N; g++) {
          tidyThreadList(&generations[g]);
      }

      // Use weak pointer relationships (value is reachable if
      // key is reachable):
      for (g = 0; g <= N; g++) {
          if (tidyWeakList(&generations[g])) {
              flag = true;
          }
      }

      // if we evacuated anything new, we must scavenge thoroughly
      // before we can determine which threads are unreachable.
      if (flag) return true;

      // Resurrect any threads which were unreachable
      for (g = 0; g <= N; g++) {
          if (resurrectUnreachableThreads(&generations[g])) {
              flag = true;
          }
      }

      // Next, move to the WeakPtrs stage after fully
      // scavenging the finalizers we've just evacuated.
      weak_stage = WeakPtrs;

      // if we evacuated anything new, we must scavenge thoroughly
      // before entering the WeakPtrs stage.
      if (flag) return true;

      // otherwise, fall through...
  }
  /* fallthrough */

  case WeakPtrs:
  {
      uint32_t g;

      // resurrecting threads might have made more weak pointers
      // alive, so traverse those lists again:
      for (g = 0; g <= N; g++) {
          if (tidyWeakList(&generations[g])) {
              flag = true;
          }
      }

      /* If we didn't make any changes, then we can go round and kill all
       * the dead weak pointers.  The dead_weak_ptr list is used as a list
       * of pending finalizers later on.
       */
      if (flag == false) {
          for (g = 0; g <= N; g++) {
              collectDeadWeakPtrs(&generations[g]);
          }

          weak_stage = WeakDone;  // *now* we're done,
      }

      return true;         // but one more round of scavenging, please
  }

  default:
      barf("traverseWeakPtrList");
      return true;
  }
}

static void collectDeadWeakPtrs (generation *gen)
{
    StgWeak *w, *next_w;
    for (w = gen->old_weak_ptr_list; w != NULL; w = next_w) {
        // If we have C finalizers, keep the value alive for this GC.
        // See Note [MallocPtr finalizers] in GHC.ForeignPtr, and #10904
        if (w->cfinalizers != &stg_NO_FINALIZER_closure) {
            evacuate(&w->value);
        }
        evacuate(&w->finalizer);
        next_w = w->link;
        w->link = dead_weak_ptr_list;
        dead_weak_ptr_list = w;
    }
}

static bool resurrectUnreachableThreads (generation *gen)
{
    StgTSO *t, *tmp, *next;
    bool flag = false;

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
            tmp->global_link = resurrected_threads;
            resurrected_threads = tmp;
            flag = true;
        }
    }
    return flag;
}

static bool tidyWeakList(generation *gen)
{
    StgWeak *w, **last_w, *next_w;
    const StgInfoTable *info;
    StgClosure *new;
    bool flag = false;
    last_w = &gen->old_weak_ptr_list;
    for (w = gen->old_weak_ptr_list; w != NULL; w = next_w) {

        /* There might be a DEAD_WEAK on the list if finalizeWeak# was
         * called on a live weak pointer object.  Just remove it.
         */
        if (w->header.info == &stg_DEAD_WEAK_info) {
            next_w = w->link;
            *last_w = next_w;
            continue;
        }

        info = get_itbl((StgClosure *)w);
        switch (info->type) {

        case WEAK:
            /* Now, check whether the key is reachable.
             */
            new = isAlive(w->key);
            if (new != NULL) {
                generation *new_gen;

                w->key = new;

                // Find out which generation this weak ptr is in, and
                // move it onto the weak ptr list of that generation.

                new_gen = Bdescr((P_)w)->gen;
                gct->evac_gen_no = new_gen->no;
                gct->failed_to_evac = false;

                // evacuate the fields of the weak ptr
                scavengeLiveWeak(w);

                if (gct->failed_to_evac) {
                    debugTrace(DEBUG_weak,
                               "putting weak pointer %p into mutable list",
                               w);
                    gct->failed_to_evac = false;
                    recordMutableGen_GC((StgClosure *)w, new_gen->no);
                }

                // remove this weak ptr from the old_weak_ptr list
                *last_w = w->link;
                next_w  = w->link;

                // and put it on the correct weak ptr list.
                w->link = new_gen->weak_ptr_list;
                new_gen->weak_ptr_list = w;
                flag = true;

                if (gen->no != new_gen->no) {
                    debugTrace(DEBUG_weak,
                      "moving weak pointer %p from %d to %d",
                      w, gen->no, new_gen->no);
                }


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
            barf("tidyWeakList: not WEAK: %d, %p", info->type, w);
        }
    }

    return flag;
}

static void tidyThreadList (generation *gen)
{
    StgTSO *t, *tmp, *next, **prev;

    prev = &gen->old_threads;

    for (t = gen->old_threads; t != END_TSO_QUEUE; t = next) {

        tmp = (StgTSO *)isAlive((StgClosure *)t);

        if (tmp != NULL) {
            t = tmp;
        }

        ASSERT(get_itbl((StgClosure *)t)->type == TSO);
        next = t->global_link;

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
}

#if defined(DEBUG)
static void checkWeakPtrSanity(StgWeak *hd, StgWeak *tl)
{
    StgWeak *w, *prev;
    for (prev = NULL, w = hd; w != NULL; prev = w, w = w->link) {
        ASSERT(INFO_PTR_TO_STRUCT(UNTAG_CLOSURE((StgClosure*)w)->header.info)->type == WEAK
            || UNTAG_CLOSURE((StgClosure*)w)->header.info == &stg_DEAD_WEAK_info);
        checkClosure((StgClosure*)w);
    }
    if (tl != NULL) {
        ASSERT(prev == tl);
    }
}
#endif

void collectFreshWeakPtrs()
{
    uint32_t i;
    // move recently allocated weak_ptr_list to the old list as well
    for (i = 0; i < n_capabilities; i++) {
        Capability *cap = capabilities[i];
        if (cap->weak_ptr_list_tl != NULL) {
            IF_DEBUG(sanity, checkWeakPtrSanity(cap->weak_ptr_list_hd, cap->weak_ptr_list_tl));
            cap->weak_ptr_list_tl->link = g0->weak_ptr_list;
            g0->weak_ptr_list = cap->weak_ptr_list_hd;
            cap->weak_ptr_list_tl = NULL;
            cap->weak_ptr_list_hd = NULL;
        } else {
            ASSERT(cap->weak_ptr_list_hd == NULL);
        }
    }
}

/* -----------------------------------------------------------------------------
   Evacuate every weak pointer object on the weak_ptr_list, and update
   the link fields.
   -------------------------------------------------------------------------- */

void
markWeakPtrList ( void )
{
    uint32_t g;

    for (g = 0; g <= N; g++) {
        generation *gen = &generations[g];
        StgWeak *w, **last_w;

        last_w = &gen->weak_ptr_list;
        for (w = gen->weak_ptr_list; w != NULL; w = w->link) {
            // w might be WEAK, EVACUATED, or DEAD_WEAK (actually CON_STATIC) here

#if defined(DEBUG)
            {   // careful to do this assertion only reading the info ptr
                // once, because during parallel GC it might change under our feet.
                const StgInfoTable *info;
                info = w->header.info;
                ASSERT(IS_FORWARDING_PTR(info)
                       || info == &stg_DEAD_WEAK_info
                       || INFO_PTR_TO_STRUCT(info)->type == WEAK);
            }
#endif

            evacuate((StgClosure **)last_w);
            w = *last_w;
            last_w = &(w->link);
        }
    }
}

/* -----------------------------------------------------------------------------
   Fully scavenge a known-to-be-alive weak pointer.

   In scavenge_block, we only partially scavenge a weak pointer because it may
   turn out to be dead. This function should be called when we decide that the
   weak pointer is alive after this GC.
   -------------------------------------------------------------------------- */

void
scavengeLiveWeak(StgWeak *w)
{
    evacuate(&w->value);
    evacuate(&w->key);
    evacuate(&w->finalizer);
    evacuate(&w->cfinalizers);
}
