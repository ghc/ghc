/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: evacuation functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"
#include "MBlock.h"
#include "Evac.h"
#include "GC.h"
#include "GCUtils.h"
#include "Compact.h"
#include "Prelude.h"
#include "LdvProfile.h"

/* Used to avoid long recursion due to selector thunks
 */
#define MAX_THUNK_SELECTOR_DEPTH 16

static void eval_thunk_selector (StgClosure **q, StgSelector * p, rtsBool);
STATIC_INLINE void evacuate_large(StgPtr p);

/* -----------------------------------------------------------------------------
   Allocate some space in which to copy an object.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
alloc_for_copy (nat size, step *stp)
{
    StgPtr to;
    step_workspace *ws;

    /* Find out where we're going, using the handy "to" pointer in 
     * the step of the source object.  If it turns out we need to
     * evacuate to an older generation, adjust it here (see comment
     * by evacuate()).
     */
    if (stp < gct->evac_step) {
	if (gct->eager_promotion) {
	    stp = gct->evac_step;
	} else {
	    gct->failed_to_evac = rtsTrue;
	}
    }
    
    ws = &gct->steps[stp->gen_no][stp->no];
    
    /* chain a new block onto the to-space for the destination step if
     * necessary.
     */
    
    ASSERT(ws->todo_free >= ws->todo_bd->free && ws->todo_free <= ws->todo_lim);
    to = ws->todo_free;
    if (to + size >= ws->todo_lim) {
	to = gc_alloc_todo_block(ws);
    }
    ws->todo_free = to + size;
    ASSERT(ws->todo_free >= ws->todo_bd->free && ws->todo_free <= ws->todo_lim);

    return to;
}

/* -----------------------------------------------------------------------------
   The evacuate() code
   -------------------------------------------------------------------------- */

#define MINOR_GC
#include "Evac.c-inc"

#undef MINOR_GC
#include "Evac.c-inc"

/* -----------------------------------------------------------------------------
   Evacuate a large object

   This just consists of removing the object from the (doubly-linked)
   step->large_objects list, and linking it on to the (singly-linked)
   step->new_large_objects list, from where it will be scavenged later.

   Convention: bd->flags has BF_EVACUATED set for a large object
   that has been evacuated, or unset otherwise.
   -------------------------------------------------------------------------- */

STATIC_INLINE void
evacuate_large(StgPtr p)
{
  bdescr *bd = Bdescr(p);
  step *stp;
  step_workspace *ws;

  // object must be at the beginning of the block (or be a ByteArray)
  ASSERT(get_itbl((StgClosure *)p)->type == ARR_WORDS ||
	 (((W_)p & BLOCK_MASK) == 0));

  // already evacuated? 
  if (bd->flags & BF_EVACUATED) { 
    /* Don't forget to set the gct->failed_to_evac flag if we didn't get
     * the desired destination (see comments in evacuate()).
     */
    if (bd->step < gct->evac_step) {
      gct->failed_to_evac = rtsTrue;
      TICK_GC_FAILED_PROMOTION();
    }
    return;
  }

  stp = bd->step;

  ACQUIRE_SPIN_LOCK(&stp->sync_large_objects);
  // remove from large_object list 
  if (bd->u.back) {
    bd->u.back->link = bd->link;
  } else { // first object in the list 
    stp->large_objects = bd->link;
  }
  if (bd->link) {
    bd->link->u.back = bd->u.back;
  }
  RELEASE_SPIN_LOCK(&stp->sync_large_objects);
  
  /* link it on to the evacuated large object list of the destination step
   */
  stp = bd->step->to;
  if (stp < gct->evac_step) {
      if (gct->eager_promotion) {
	  stp = gct->evac_step;
      } else {
	  gct->failed_to_evac = rtsTrue;
      }
  }

  ws = &gct->steps[stp->gen_no][stp->no];
  bd->step = stp;
  bd->gen_no = stp->gen_no;
  bd->link = ws->todo_large_objects;
  ws->todo_large_objects = bd;
  bd->flags |= BF_EVACUATED;
}

/* -----------------------------------------------------------------------------
   Evaluate a THUNK_SELECTOR if possible.

   p points to a THUNK_SELECTOR that we want to evaluate.  The
   result of "evaluating" it will be evacuated and a pointer to the
   to-space closure will be returned.

   If the THUNK_SELECTOR could not be evaluated (its selectee is still
   a THUNK, for example), then the THUNK_SELECTOR itself will be
   evacuated.
   -------------------------------------------------------------------------- */
static void
unchain_thunk_selectors(StgSelector *p, StgClosure *val)
{
    StgSelector *prev;

    prev = NULL;
    while (p)
    {
#ifdef THREADED_RTS
        ASSERT(p->header.info == &stg_WHITEHOLE_info);
#else
        ASSERT(p->header.info == &stg_BLACKHOLE_info);
#endif
        // val must be in to-space.  Not always: when we recursively
        // invoke eval_thunk_selector(), the recursive calls will not 
        // evacuate the value (because we want to select on the value,
        // not evacuate it), so in this case val is in from-space.
        // ASSERT(!HEAP_ALLOCED(val) || Bdescr((P_)val)->gen_no > N || (Bdescr((P_)val)->flags & BF_EVACUATED));

        prev = (StgSelector*)((StgClosure *)p)->payload[0];

        // Update the THUNK_SELECTOR with an indirection to the
        // EVACUATED closure now at p.  Why do this rather than
        // upd_evacuee(q,p)?  Because we have an invariant that an
        // EVACUATED closure always points to an object in the
        // same or an older generation (required by the short-cut
        // test in the EVACUATED case, below).
        ((StgInd *)p)->indirectee = val;
        write_barrier();
        SET_INFO(p, &stg_IND_info);

        // For the purposes of LDV profiling, we have created an
        // indirection.
        LDV_RECORD_CREATE(p);

        p = prev;
    }
}

static void
eval_thunk_selector (StgClosure **q, StgSelector * p, rtsBool evac)
                 // NB. for legacy reasons, p & q are swapped around :(
{
    nat field;
    StgInfoTable *info;
    StgWord info_ptr;
    StgClosure *selectee;
    StgSelector *prev_thunk_selector;
    bdescr *bd;
    StgClosure *val;
    
    prev_thunk_selector = NULL;
    // this is a chain of THUNK_SELECTORs that we are going to update
    // to point to the value of the current THUNK_SELECTOR.  Each
    // closure on the chain is a BLACKHOLE, and points to the next in the
    // chain with payload[0].

selector_chain:

    bd = Bdescr((StgPtr)p);
    if (HEAP_ALLOCED(p)) {
        // If the THUNK_SELECTOR is in to-space or in a generation that we
        // are not collecting, then bale out early.  We won't be able to
        // save any space in any case, and updating with an indirection is
        // trickier in a non-collected gen: we would have to update the
        // mutable list.
        if ((bd->gen_no > N) || (bd->flags & BF_EVACUATED)) {
            unchain_thunk_selectors(prev_thunk_selector, (StgClosure *)p);
            *q = (StgClosure *)p;
            return;
        }
        // we don't update THUNK_SELECTORS in the compacted
        // generation, because compaction does not remove the INDs
        // that result, this causes confusion later
        // (scavenge_mark_stack doesn't deal with IND).  BEWARE!  This
        // bit is very tricky to get right.  If you make changes
        // around here, test by compiling stage 3 with +RTS -c -RTS.
        if (bd->flags & BF_COMPACTED) {
            // must call evacuate() to mark this closure if evac==rtsTrue
            *q = (StgClosure *)p;
            if (evac) evacuate(q);
            unchain_thunk_selectors(prev_thunk_selector, (StgClosure *)p);
            return;
        }
    }


    // BLACKHOLE the selector thunk, since it is now under evaluation.
    // This is important to stop us going into an infinite loop if
    // this selector thunk eventually refers to itself.
#if defined(THREADED_RTS)
    // In threaded mode, we'll use WHITEHOLE to lock the selector
    // thunk while we evaluate it.
    {
        do {
            info_ptr = xchg((StgPtr)&p->header.info, (W_)&stg_WHITEHOLE_info);
        } while (info_ptr == (W_)&stg_WHITEHOLE_info);

        // make sure someone else didn't get here first...
        if (INFO_PTR_TO_STRUCT(info_ptr)->type != THUNK_SELECTOR) {
            // v. tricky now.  The THUNK_SELECTOR has been evacuated
            // by another thread, and is now either EVACUATED or IND.
            // We need to extract ourselves from the current situation
            // as cleanly as possible.
            //   - unlock the closure
            //   - update *q, we may have done *some* evaluation
            //   - if evac, we need to call evacuate(), because we
            //     need the write-barrier stuff.
            //   - undo the chain we've built to point to p.
            SET_INFO(p, (const StgInfoTable *)info_ptr);
            *q = (StgClosure *)p;
            if (evac) evacuate(q);
            unchain_thunk_selectors(prev_thunk_selector, (StgClosure *)p);
            return;
        }
    }
#else
    // Save the real info pointer (NOTE: not the same as get_itbl()).
    info_ptr = (StgWord)p->header.info;
    SET_INFO(p,&stg_BLACKHOLE_info);
#endif

    field = INFO_PTR_TO_STRUCT(info_ptr)->layout.selector_offset;

    // The selectee might be a constructor closure,
    // so we untag the pointer.
    selectee = UNTAG_CLOSURE(p->selectee);

selector_loop:
    // selectee now points to the closure that we're trying to select
    // a field from.  It may or may not be in to-space: we try not to
    // end up in to-space, but it's impractical to avoid it in
    // general.  The compacting GC scatters to-space pointers in
    // from-space during marking, for example.  We rely on the property
    // that evacuate() doesn't mind if it gets passed a to-space pointer.

    info = get_itbl(selectee);
    switch (info->type) {
      case WHITEHOLE:
	  goto bale_out; // about to be evacuated by another thread (or a loop).
	
      case CONSTR:
      case CONSTR_1_0:
      case CONSTR_0_1:
      case CONSTR_2_0:
      case CONSTR_1_1:
      case CONSTR_0_2:
      case CONSTR_STATIC:
      case CONSTR_NOCAF_STATIC:
          {
              // check that the size is in range 
              ASSERT(field <  (StgWord32)(info->layout.payload.ptrs + 
                                          info->layout.payload.nptrs));
	  
              // Select the right field from the constructor
              val = selectee->payload[field];
              
#ifdef PROFILING
              // For the purposes of LDV profiling, we have destroyed
              // the original selector thunk, p.
              SET_INFO(p, (StgInfoTable *)info_ptr);
              LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC((StgClosure *)p);
              SET_INFO(p, &stg_BLACKHOLE_info);
#endif

              // the closure in val is now the "value" of the
              // THUNK_SELECTOR in p.  However, val may itself be a
              // THUNK_SELECTOR, in which case we want to continue
              // evaluating until we find the real value, and then
              // update the whole chain to point to the value.
          val_loop:
              info = get_itbl(UNTAG_CLOSURE(val));
              switch (info->type) {
              case IND:
              case IND_PERM:
              case IND_OLDGEN:
              case IND_OLDGEN_PERM:
              case IND_STATIC:
                  val = ((StgInd *)val)->indirectee;
                  goto val_loop;
              case THUNK_SELECTOR:
                  ((StgClosure*)p)->payload[0] = (StgClosure *)prev_thunk_selector;
                  prev_thunk_selector = p;
                  p = (StgSelector*)val;
                  goto selector_chain;
              default:
                  ((StgClosure*)p)->payload[0] = (StgClosure *)prev_thunk_selector;
                  prev_thunk_selector = p;

                  *q = val;
                  if (evac) evacuate(q);
                  val = *q;
                  // evacuate() cannot recurse through
                  // eval_thunk_selector(), because we know val is not
                  // a THUNK_SELECTOR.
                  unchain_thunk_selectors(prev_thunk_selector, val);
                  return;
              }
          }

      case IND:
      case IND_PERM:
      case IND_OLDGEN:
      case IND_OLDGEN_PERM:
      case IND_STATIC:
          // Again, we might need to untag a constructor.
          selectee = UNTAG_CLOSURE( ((StgInd *)selectee)->indirectee );
	  goto selector_loop;

      case EVACUATED:
	  // We don't follow pointers into to-space; the constructor
	  // has already been evacuated, so we won't save any space
	  // leaks by evaluating this selector thunk anyhow.
	  goto bale_out;

      case THUNK_SELECTOR:
      {
	  StgClosure *val;

          // recursively evaluate this selector.  We don't want to
          // recurse indefinitely, so we impose a depth bound.
	  if (gct->thunk_selector_depth >= MAX_THUNK_SELECTOR_DEPTH) {
	      goto bale_out;
	  }

	  gct->thunk_selector_depth++;
          // rtsFalse says "don't evacuate the result".  It will,
          // however, update any THUNK_SELECTORs that are evaluated
          // along the way.
	  eval_thunk_selector(&val, (StgSelector*)selectee, rtsFalse);
	  gct->thunk_selector_depth--;

          // did we actually manage to evaluate it?
          if (val == selectee) goto bale_out;

          // Of course this pointer might be tagged...
          selectee = UNTAG_CLOSURE(val);
          goto selector_loop;
      }

      case AP:
      case AP_STACK:
      case THUNK:
      case THUNK_1_0:
      case THUNK_0_1:
      case THUNK_2_0:
      case THUNK_1_1:
      case THUNK_0_2:
      case THUNK_STATIC:
      case CAF_BLACKHOLE:
      case SE_CAF_BLACKHOLE:
      case SE_BLACKHOLE:
      case BLACKHOLE:
	  // not evaluated yet 
	  goto bale_out;
    
      default:
	barf("eval_thunk_selector: strange selectee %d",
	     (int)(info->type));
    }

bale_out:
    // We didn't manage to evaluate this thunk; restore the old info
    // pointer.  But don't forget: we still need to evacuate the thunk itself.
    SET_INFO(p, (const StgInfoTable *)info_ptr);
    // THREADED_RTS: we just unlocked the thunk, so another thread
    // might get in and update it.  copy() will lock it again and
    // check whether it was updated in the meantime.
    *q = (StgClosure *)p;
    if (evac) {
        copy(q,(StgClosure *)p,THUNK_SELECTOR_sizeW(),bd->step->to);
    }
    unchain_thunk_selectors(prev_thunk_selector, *q);
    return;
}

/* -----------------------------------------------------------------------------
   move_TSO is called to update the TSO structure after it has been
   moved from one place to another.
   -------------------------------------------------------------------------- */

void
move_TSO (StgTSO *src, StgTSO *dest)
{
    ptrdiff_t diff;

    // relocate the stack pointer... 
    diff = (StgPtr)dest - (StgPtr)src; // In *words* 
    dest->sp = (StgPtr)dest->sp + diff;
}

