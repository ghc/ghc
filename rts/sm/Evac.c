/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: evacuation functions
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
lnat thunk_selector_depth = 0;
#define MAX_THUNK_SELECTOR_DEPTH 8

static StgClosure * eval_thunk_selector ( nat field, StgSelector * p );

STATIC_INLINE void 
upd_evacuee(StgClosure *p, StgClosure *dest)
{
    // not true: (ToDo: perhaps it should be)
    // ASSERT(Bdescr((P_)dest)->flags & BF_EVACUATED);
    SET_INFO(p, &stg_EVACUATED_info);
    ((StgEvacuated *)p)->evacuee = dest;
}


STATIC_INLINE StgClosure *
copy(StgClosure *src, nat size, step *stp)
{
  StgPtr to, from;
  nat i;
#ifdef PROFILING
  // @LDV profiling
  nat size_org = size;
#endif

  TICK_GC_WORDS_COPIED(size);
  /* Find out where we're going, using the handy "to" pointer in 
   * the step of the source object.  If it turns out we need to
   * evacuate to an older generation, adjust it here (see comment
   * by evacuate()).
   */
  if (stp->gen_no < evac_gen) {
      if (eager_promotion) {
	  stp = &generations[evac_gen].steps[0];
      } else {
	  failed_to_evac = rtsTrue;
      }
  }

  /* chain a new block onto the to-space for the destination step if
   * necessary.
   */
  if (stp->hp + size >= stp->hpLim) {
    gc_alloc_block(stp);
  }

  to = stp->hp;
  from = (StgPtr)src;
  stp->hp = to + size;
  for (i = 0; i < size; i++) { // unroll for small i
      to[i] = from[i];
  }
  upd_evacuee((StgClosure *)from,(StgClosure *)to);

#ifdef PROFILING
  // We store the size of the just evacuated object in the LDV word so that
  // the profiler can guess the position of the next object later.
  SET_EVACUAEE_FOR_LDV(from, size_org);
#endif
  return (StgClosure *)to;
}

// Same as copy() above, except the object will be allocated in memory
// that will not be scavenged.  Used for object that have no pointer
// fields.
STATIC_INLINE StgClosure *
copy_noscav(StgClosure *src, nat size, step *stp)
{
  StgPtr to, from;
  nat i;
#ifdef PROFILING
  // @LDV profiling
  nat size_org = size;
#endif

  TICK_GC_WORDS_COPIED(size);
  /* Find out where we're going, using the handy "to" pointer in 
   * the step of the source object.  If it turns out we need to
   * evacuate to an older generation, adjust it here (see comment
   * by evacuate()).
   */
  if (stp->gen_no < evac_gen) {
      if (eager_promotion) {
	  stp = &generations[evac_gen].steps[0];
      } else {
	  failed_to_evac = rtsTrue;
      }
  }

  /* chain a new block onto the to-space for the destination step if
   * necessary.
   */
  if (stp->scavd_hp + size >= stp->scavd_hpLim) {
    gc_alloc_scavd_block(stp);
  }

  to = stp->scavd_hp;
  from = (StgPtr)src;
  stp->scavd_hp = to + size;
  for (i = 0; i < size; i++) { // unroll for small i
      to[i] = from[i];
  }
  upd_evacuee((StgClosure *)from,(StgClosure *)to);

#ifdef PROFILING
  // We store the size of the just evacuated object in the LDV word so that
  // the profiler can guess the position of the next object later.
  SET_EVACUAEE_FOR_LDV(from, size_org);
#endif
  return (StgClosure *)to;
}

/* Special version of copy() for when we only want to copy the info
 * pointer of an object, but reserve some padding after it.  This is
 * used to optimise evacuation of BLACKHOLEs.
 */


static StgClosure *
copyPart(StgClosure *src, nat size_to_reserve, nat size_to_copy, step *stp)
{
  P_ dest, to, from;
#ifdef PROFILING
  // @LDV profiling
  nat size_to_copy_org = size_to_copy;
#endif

  TICK_GC_WORDS_COPIED(size_to_copy);
  if (stp->gen_no < evac_gen) {
      if (eager_promotion) {
	  stp = &generations[evac_gen].steps[0];
      } else {
	  failed_to_evac = rtsTrue;
      }
  }

  if (stp->hp + size_to_reserve >= stp->hpLim) {
    gc_alloc_block(stp);
  }

  for(to = stp->hp, from = (P_)src; size_to_copy>0; --size_to_copy) {
    *to++ = *from++;
  }
  
  dest = stp->hp;
  stp->hp += size_to_reserve;
  upd_evacuee(src,(StgClosure *)dest);
#ifdef PROFILING
  // We store the size of the just evacuated object in the LDV word so that
  // the profiler can guess the position of the next object later.
  // size_to_copy_org is wrong because the closure already occupies size_to_reserve
  // words.
  SET_EVACUAEE_FOR_LDV(src, size_to_reserve);
  // fill the slop
  if (size_to_reserve - size_to_copy_org > 0)
    LDV_FILL_SLOP(stp->hp - 1, (int)(size_to_reserve - size_to_copy_org)); 
#endif
  return (StgClosure *)dest;
}


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

  // object must be at the beginning of the block (or be a ByteArray)
  ASSERT(get_itbl((StgClosure *)p)->type == ARR_WORDS ||
	 (((W_)p & BLOCK_MASK) == 0));

  // already evacuated? 
  if (bd->flags & BF_EVACUATED) { 
    /* Don't forget to set the failed_to_evac flag if we didn't get
     * the desired destination (see comments in evacuate()).
     */
    if (bd->gen_no < evac_gen) {
      failed_to_evac = rtsTrue;
      TICK_GC_FAILED_PROMOTION();
    }
    return;
  }

  stp = bd->step;
  // remove from large_object list 
  if (bd->u.back) {
    bd->u.back->link = bd->link;
  } else { // first object in the list 
    stp->large_objects = bd->link;
  }
  if (bd->link) {
    bd->link->u.back = bd->u.back;
  }
  
  /* link it on to the evacuated large object list of the destination step
   */
  stp = bd->step->to;
  if (stp->gen_no < evac_gen) {
      if (eager_promotion) {
	  stp = &generations[evac_gen].steps[0];
      } else {
	  failed_to_evac = rtsTrue;
      }
  }

  bd->step = stp;
  bd->gen_no = stp->gen_no;
  bd->link = stp->new_large_objects;
  stp->new_large_objects = bd;
  bd->flags |= BF_EVACUATED;
}

/* -----------------------------------------------------------------------------
   Evacuate

   This is called (eventually) for every live object in the system.

   The caller to evacuate specifies a desired generation in the
   evac_gen global variable.  The following conditions apply to
   evacuating an object which resides in generation M when we're
   collecting up to generation N

   if  M >= evac_gen 
           if  M > N     do nothing
	   else          evac to step->to

   if  M < evac_gen      evac to evac_gen, step 0

   if the object is already evacuated, then we check which generation
   it now resides in.

   if  M >= evac_gen     do nothing
   if  M <  evac_gen     set failed_to_evac flag to indicate that we
                         didn't manage to evacuate this object into evac_gen.


   OPTIMISATION NOTES:

   evacuate() is the single most important function performance-wise
   in the GC.  Various things have been tried to speed it up, but as
   far as I can tell the code generated by gcc 3.2 with -O2 is about
   as good as it's going to get.  We pass the argument to evacuate()
   in a register using the 'regparm' attribute (see the prototype for
   evacuate() near the top of this file).

   Changing evacuate() to take an (StgClosure **) rather than
   returning the new pointer seems attractive, because we can avoid
   writing back the pointer when it hasn't changed (eg. for a static
   object, or an object in a generation > N).  However, I tried it and
   it doesn't help.  One reason is that the (StgClosure **) pointer
   gets spilled to the stack inside evacuate(), resulting in far more
   extra reads/writes than we save.
   -------------------------------------------------------------------------- */

REGPARM1 StgClosure *
evacuate(StgClosure *q)
{
  bdescr *bd = NULL;
  step *stp;
  const StgInfoTable *info;

loop:
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

  if (!HEAP_ALLOCED(q)) {

      if (!major_gc) return q;

      info = get_itbl(q);
      switch (info->type) {

      case THUNK_STATIC:
	  if (info->srt_bitmap != 0 && 
	      *THUNK_STATIC_LINK((StgClosure *)q) == NULL) {
	      *THUNK_STATIC_LINK((StgClosure *)q) = static_objects;
	      static_objects = (StgClosure *)q;
	  }
	  return q;
	  
      case FUN_STATIC:
	  if (info->srt_bitmap != 0 && 
	      *FUN_STATIC_LINK((StgClosure *)q) == NULL) {
	      *FUN_STATIC_LINK((StgClosure *)q) = static_objects;
	      static_objects = (StgClosure *)q;
	  }
	  return q;
	  
      case IND_STATIC:
	  /* If q->saved_info != NULL, then it's a revertible CAF - it'll be
	   * on the CAF list, so don't do anything with it here (we'll
	   * scavenge it later).
	   */
	  if (((StgIndStatic *)q)->saved_info == NULL
	      && *IND_STATIC_LINK((StgClosure *)q) == NULL) {
	      *IND_STATIC_LINK((StgClosure *)q) = static_objects;
	      static_objects = (StgClosure *)q;
	  }
	  return q;
	  
      case CONSTR_STATIC:
	  if (*STATIC_LINK(info,(StgClosure *)q) == NULL) {
	      *STATIC_LINK(info,(StgClosure *)q) = static_objects;
	      static_objects = (StgClosure *)q;
	  }
	  return q;
	  
      case CONSTR_NOCAF_STATIC:
	  /* no need to put these on the static linked list, they don't need
	   * to be scavenged.
	   */
	  return q;
	  
      default:
	  barf("evacuate(static): strange closure type %d", (int)(info->type));
      }
  }

  bd = Bdescr((P_)q);

  if (bd->gen_no > N) {
      /* Can't evacuate this object, because it's in a generation
       * older than the ones we're collecting.  Let's hope that it's
       * in evac_gen or older, or we will have to arrange to track
       * this pointer using the mutable list.
       */
      if (bd->gen_no < evac_gen) {
	  // nope 
	  failed_to_evac = rtsTrue;
	  TICK_GC_FAILED_PROMOTION();
      }
      return q;
  }

  if ((bd->flags & (BF_LARGE | BF_COMPACTED | BF_EVACUATED)) != 0) {

      /* pointer into to-space: just return it.  This normally
       * shouldn't happen, but alllowing it makes certain things
       * slightly easier (eg. the mutable list can contain the same
       * object twice, for example).
       */
      if (bd->flags & BF_EVACUATED) {
	  if (bd->gen_no < evac_gen) {
	      failed_to_evac = rtsTrue;
	      TICK_GC_FAILED_PROMOTION();
	  }
	  return q;
      }

      /* evacuate large objects by re-linking them onto a different list.
       */
      if (bd->flags & BF_LARGE) {
	  info = get_itbl(q);
	  if (info->type == TSO && 
	      ((StgTSO *)q)->what_next == ThreadRelocated) {
	      q = (StgClosure *)((StgTSO *)q)->link;
	      goto loop;
	  }
	  evacuate_large((P_)q);
	  return q;
      }
      
      /* If the object is in a step that we're compacting, then we
       * need to use an alternative evacuate procedure.
       */
      if (bd->flags & BF_COMPACTED) {
	  if (!is_marked((P_)q,bd)) {
	      mark((P_)q,bd);
	      if (mark_stack_full()) {
		  mark_stack_overflowed = rtsTrue;
		  reset_mark_stack();
	      }
	      push_mark_stack((P_)q);
	  }
	  return q;
      }
  }
      
  stp = bd->step->to;

  info = get_itbl(q);
  
  switch (info->type) {

  case MUT_VAR_CLEAN:
  case MUT_VAR_DIRTY:
  case MVAR:
      return copy(q,sizeW_fromITBL(info),stp);

  case CONSTR_0_1:
  { 
      StgWord w = (StgWord)q->payload[0];
      if (q->header.info == Czh_con_info &&
	  // unsigned, so always true:  (StgChar)w >= MIN_CHARLIKE &&  
	  (StgChar)w <= MAX_CHARLIKE) {
	  return (StgClosure *)CHARLIKE_CLOSURE((StgChar)w);
      }
      if (q->header.info == Izh_con_info &&
	  (StgInt)w >= MIN_INTLIKE && (StgInt)w <= MAX_INTLIKE) {
	  return (StgClosure *)INTLIKE_CLOSURE((StgInt)w);
      }
      // else
      return copy_noscav(q,sizeofW(StgHeader)+1,stp);
  }

  case FUN_0_1:
  case FUN_1_0:
  case CONSTR_1_0:
    return copy(q,sizeofW(StgHeader)+1,stp);

  case THUNK_1_0:
  case THUNK_0_1:
    return copy(q,sizeofW(StgThunk)+1,stp);

  case THUNK_1_1:
  case THUNK_2_0:
  case THUNK_0_2:
#ifdef NO_PROMOTE_THUNKS
    if (bd->gen_no == 0 && 
	bd->step->no != 0 &&
	bd->step->no == generations[bd->gen_no].n_steps-1) {
      stp = bd->step;
    }
#endif
    return copy(q,sizeofW(StgThunk)+2,stp);

  case FUN_1_1:
  case FUN_2_0:
  case CONSTR_1_1:
  case CONSTR_2_0:
  case FUN_0_2:
    return copy(q,sizeofW(StgHeader)+2,stp);

  case CONSTR_0_2:
    return copy_noscav(q,sizeofW(StgHeader)+2,stp);

  case THUNK:
    return copy(q,thunk_sizeW_fromITBL(info),stp);

  case FUN:
  case CONSTR:
  case IND_PERM:
  case IND_OLDGEN_PERM:
  case WEAK:
  case STABLE_NAME:
    return copy(q,sizeW_fromITBL(info),stp);

  case BCO:
      return copy(q,bco_sizeW((StgBCO *)q),stp);

  case CAF_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case SE_BLACKHOLE:
  case BLACKHOLE:
    return copyPart(q,BLACKHOLE_sizeW(),sizeofW(StgHeader),stp);

  case THUNK_SELECTOR:
    {
	StgClosure *p;
	const StgInfoTable *info_ptr;

	if (thunk_selector_depth > MAX_THUNK_SELECTOR_DEPTH) {
	    return copy(q,THUNK_SELECTOR_sizeW(),stp);
	}

	// stashed away for LDV profiling, see below
	info_ptr = q->header.info;

	p = eval_thunk_selector(info->layout.selector_offset,
				(StgSelector *)q);

	if (p == NULL) {
	    return copy(q,THUNK_SELECTOR_sizeW(),stp);
	} else {
	    StgClosure *val;
	    // q is still BLACKHOLE'd.
	    thunk_selector_depth++;
	    val = evacuate(p);
	    thunk_selector_depth--;

#ifdef PROFILING
	    // For the purposes of LDV profiling, we have destroyed
	    // the original selector thunk.
	    SET_INFO(q, info_ptr);
	    LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(q);
#endif

	    // Update the THUNK_SELECTOR with an indirection to the
	    // EVACUATED closure now at p.  Why do this rather than
	    // upd_evacuee(q,p)?  Because we have an invariant that an
	    // EVACUATED closure always points to an object in the
	    // same or an older generation (required by the short-cut
	    // test in the EVACUATED case, below).
	    SET_INFO(q, &stg_IND_info);
	    ((StgInd *)q)->indirectee = p;

	    // For the purposes of LDV profiling, we have created an
	    // indirection.
	    LDV_RECORD_CREATE(q);

	    return val;
	}
    }

  case IND:
  case IND_OLDGEN:
    // follow chains of indirections, don't evacuate them 
    q = ((StgInd*)q)->indirectee;
    goto loop;

  case RET_BCO:
  case RET_SMALL:
  case RET_VEC_SMALL:
  case RET_BIG:
  case RET_VEC_BIG:
  case RET_DYN:
  case UPDATE_FRAME:
  case STOP_FRAME:
  case CATCH_FRAME:
  case CATCH_STM_FRAME:
  case CATCH_RETRY_FRAME:
  case ATOMICALLY_FRAME:
    // shouldn't see these 
    barf("evacuate: stack frame at %p\n", q);

  case PAP:
      return copy(q,pap_sizeW((StgPAP*)q),stp);

  case AP:
      return copy(q,ap_sizeW((StgAP*)q),stp);

  case AP_STACK:
      return copy(q,ap_stack_sizeW((StgAP_STACK*)q),stp);

  case EVACUATED:
    /* Already evacuated, just return the forwarding address.
     * HOWEVER: if the requested destination generation (evac_gen) is
     * older than the actual generation (because the object was
     * already evacuated to a younger generation) then we have to
     * set the failed_to_evac flag to indicate that we couldn't 
     * manage to promote the object to the desired generation.
     */
    /* 
     * Optimisation: the check is fairly expensive, but we can often
     * shortcut it if either the required generation is 0, or the
     * current object (the EVACUATED) is in a high enough generation.
     * We know that an EVACUATED always points to an object in the
     * same or an older generation.  stp is the lowest step that the
     * current object would be evacuated to, so we only do the full
     * check if stp is too low.
     */
    if (evac_gen > 0 && stp->gen_no < evac_gen) {  // optimisation 
      StgClosure *p = ((StgEvacuated*)q)->evacuee;
      if (HEAP_ALLOCED(p) && Bdescr((P_)p)->gen_no < evac_gen) {
	failed_to_evac = rtsTrue;
	TICK_GC_FAILED_PROMOTION();
      }
    }
    return ((StgEvacuated*)q)->evacuee;

  case ARR_WORDS:
      // just copy the block 
      return copy_noscav(q,arr_words_sizeW((StgArrWords *)q),stp);

  case MUT_ARR_PTRS_CLEAN:
  case MUT_ARR_PTRS_DIRTY:
  case MUT_ARR_PTRS_FROZEN:
  case MUT_ARR_PTRS_FROZEN0:
      // just copy the block 
      return copy(q,mut_arr_ptrs_sizeW((StgMutArrPtrs *)q),stp);

  case TSO:
    {
      StgTSO *tso = (StgTSO *)q;

      /* Deal with redirected TSOs (a TSO that's had its stack enlarged).
       */
      if (tso->what_next == ThreadRelocated) {
	q = (StgClosure *)tso->link;
	goto loop;
      }

      /* To evacuate a small TSO, we need to relocate the update frame
       * list it contains.  
       */
      {
	  StgTSO *new_tso;
	  StgPtr p, q;

	  new_tso = (StgTSO *)copyPart((StgClosure *)tso,
				       tso_sizeW(tso),
				       sizeofW(StgTSO), stp);
	  move_TSO(tso, new_tso);
	  for (p = tso->sp, q = new_tso->sp;
	       p < tso->stack+tso->stack_size;) {
	      *q++ = *p++;
	  }
	  
	  return (StgClosure *)new_tso;
      }
    }

  case TREC_HEADER: 
    return copy(q,sizeofW(StgTRecHeader),stp);

  case TVAR_WATCH_QUEUE:
    return copy(q,sizeofW(StgTVarWatchQueue),stp);

  case TVAR:
    return copy(q,sizeofW(StgTVar),stp);
    
  case TREC_CHUNK:
    return copy(q,sizeofW(StgTRecChunk),stp);

  case ATOMIC_INVARIANT:
    return copy(q,sizeofW(StgAtomicInvariant),stp);

  case INVARIANT_CHECK_QUEUE:
    return copy(q,sizeofW(StgInvariantCheckQueue),stp);

  default:
    barf("evacuate: strange closure type %d", (int)(info->type));
  }

  barf("evacuate");
}

/* -----------------------------------------------------------------------------
   Evaluate a THUNK_SELECTOR if possible.

   returns: NULL if we couldn't evaluate this THUNK_SELECTOR, or
   a closure pointer if we evaluated it and this is the result.  Note
   that "evaluating" the THUNK_SELECTOR doesn't necessarily mean
   reducing it to HNF, just that we have eliminated the selection.
   The result might be another thunk, or even another THUNK_SELECTOR.

   If the return value is non-NULL, the original selector thunk has
   been BLACKHOLE'd, and should be updated with an indirection or a
   forwarding pointer.  If the return value is NULL, then the selector
   thunk is unchanged.

   ***
   ToDo: the treatment of THUNK_SELECTORS could be improved in the
   following way (from a suggestion by Ian Lynagh):

   We can have a chain like this:

      sel_0 --> (a,b)
                 |
                 |-----> sel_0 --> (a,b)
                                    |
                                    |-----> sel_0 --> ...

   and the depth limit means we don't go all the way to the end of the
   chain, which results in a space leak.  This affects the recursive
   call to evacuate() in the THUNK_SELECTOR case in evacuate(): *not*
   the recursive call to eval_thunk_selector() in
   eval_thunk_selector().

   We could eliminate the depth bound in this case, in the following
   way:

      - traverse the chain once to discover the *value* of the 
        THUNK_SELECTOR.  Mark all THUNK_SELECTORS that we
        visit on the way as having been visited already (somehow).

      - in a second pass, traverse the chain again updating all
        THUNK_SEELCTORS that we find on the way with indirections to
        the value.

      - if we encounter a "marked" THUNK_SELECTOR in a normal 
        evacuate(), we konw it can't be updated so just evac it.

   Program that illustrates the problem:

	foo [] = ([], [])
	foo (x:xs) = let (ys, zs) = foo xs
	             in if x >= 0 then (x:ys, zs) else (ys, x:zs)

	main = bar [1..(100000000::Int)]
	bar xs = (\(ys, zs) -> print ys >> print zs) (foo xs)

   -------------------------------------------------------------------------- */

static inline rtsBool
is_to_space ( StgClosure *p )
{
    bdescr *bd;

    bd = Bdescr((StgPtr)p);
    if (HEAP_ALLOCED(p) &&
	((bd->flags & BF_EVACUATED) 
	 || ((bd->flags & BF_COMPACTED) &&
	     is_marked((P_)p,bd)))) {
	return rtsTrue;
    } else {
	return rtsFalse;
    }
}    

static StgClosure *
eval_thunk_selector( nat field, StgSelector * p )
{
    StgInfoTable *info;
    const StgInfoTable *info_ptr;
    StgClosure *selectee;
    
    selectee = p->selectee;

    // Save the real info pointer (NOTE: not the same as get_itbl()).
    info_ptr = p->header.info;

    // If the THUNK_SELECTOR is in a generation that we are not
    // collecting, then bail out early.  We won't be able to save any
    // space in any case, and updating with an indirection is trickier
    // in an old gen.
    if (Bdescr((StgPtr)p)->gen_no > N) {
	return NULL;
    }

    // BLACKHOLE the selector thunk, since it is now under evaluation.
    // This is important to stop us going into an infinite loop if
    // this selector thunk eventually refers to itself.
    SET_INFO(p,&stg_BLACKHOLE_info);

selector_loop:

    // We don't want to end up in to-space, because this causes
    // problems when the GC later tries to evacuate the result of
    // eval_thunk_selector().  There are various ways this could
    // happen:
    //
    // 1. following an IND_STATIC
    //
    // 2. when the old generation is compacted, the mark phase updates
    //    from-space pointers to be to-space pointers, and we can't
    //    reliably tell which we're following (eg. from an IND_STATIC).
    // 
    // 3. compacting GC again: if we're looking at a constructor in
    //    the compacted generation, it might point directly to objects
    //    in to-space.  We must bale out here, otherwise doing the selection
    //    will result in a to-space pointer being returned.
    //
    //  (1) is dealt with using a BF_EVACUATED test on the
    //  selectee. (2) and (3): we can tell if we're looking at an
    //  object in the compacted generation that might point to
    //  to-space objects by testing that (a) it is BF_COMPACTED, (b)
    //  the compacted generation is being collected, and (c) the
    //  object is marked.  Only a marked object may have pointers that
    //  point to to-space objects, because that happens when
    //  scavenging.
    //
    //  The to-space test is now embodied in the in_to_space() inline
    //  function, as it is re-used below.
    //
    if (is_to_space(selectee)) {
	goto bale_out;
    }

    info = get_itbl(selectee);
    switch (info->type) {
      case CONSTR:
      case CONSTR_1_0:
      case CONSTR_0_1:
      case CONSTR_2_0:
      case CONSTR_1_1:
      case CONSTR_0_2:
      case CONSTR_STATIC:
      case CONSTR_NOCAF_STATIC:
	  // check that the size is in range 
	  ASSERT(field <  (StgWord32)(info->layout.payload.ptrs + 
				      info->layout.payload.nptrs));
	  
	  // Select the right field from the constructor, and check
	  // that the result isn't in to-space.  It might be in
	  // to-space if, for example, this constructor contains
	  // pointers to younger-gen objects (and is on the mut-once
	  // list).
	  //
	  { 
	      StgClosure *q;
	      q = selectee->payload[field];
	      if (is_to_space(q)) {
		  goto bale_out;
	      } else {
		  return q;
	      }
	  }

      case IND:
      case IND_PERM:
      case IND_OLDGEN:
      case IND_OLDGEN_PERM:
      case IND_STATIC:
	  selectee = ((StgInd *)selectee)->indirectee;
	  goto selector_loop;

      case EVACUATED:
	  // We don't follow pointers into to-space; the constructor
	  // has already been evacuated, so we won't save any space
	  // leaks by evaluating this selector thunk anyhow.
	  break;

      case THUNK_SELECTOR:
      {
	  StgClosure *val;

	  // check that we don't recurse too much, re-using the
	  // depth bound also used in evacuate().
	  if (thunk_selector_depth >= MAX_THUNK_SELECTOR_DEPTH) {
	      break;
	  }
	  thunk_selector_depth++;

	  val = eval_thunk_selector(info->layout.selector_offset, 
				    (StgSelector *)selectee);

	  thunk_selector_depth--;

	  if (val == NULL) { 
	      break;
	  } else {
	      // We evaluated this selector thunk, so update it with
	      // an indirection.  NOTE: we don't use UPD_IND here,
	      // because we are guaranteed that p is in a generation
	      // that we are collecting, and we never want to put the
	      // indirection on a mutable list.
#ifdef PROFILING
	      // For the purposes of LDV profiling, we have destroyed
	      // the original selector thunk.
	      SET_INFO(p, info_ptr);
	      LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(selectee);
#endif
	      ((StgInd *)selectee)->indirectee = val;
	      SET_INFO(selectee,&stg_IND_info);

	      // For the purposes of LDV profiling, we have created an
	      // indirection.
	      LDV_RECORD_CREATE(selectee);

	      selectee = val;
	      goto selector_loop;
	  }
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
	  break;
    
      default:
	barf("eval_thunk_selector: strange selectee %d",
	     (int)(info->type));
    }

bale_out:
    // We didn't manage to evaluate this thunk; restore the old info pointer
    SET_INFO(p, info_ptr);
    return NULL;
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

