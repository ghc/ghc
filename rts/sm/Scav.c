/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: scavenging functions
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
#include "GC.h"
#include "Compact.h"
#include "Evac.h"
#include "Scav.h"
#include "Apply.h"
#include "Trace.h"
#include "LdvProfile.h"

static void scavenge_stack (StgPtr p, StgPtr stack_end);

static void scavenge_large_bitmap (StgPtr p, 
				   StgLargeBitmap *large_bitmap, 
				   nat size );

/* Similar to scavenge_large_bitmap(), but we don't write back the
 * pointers we get back from evacuate().
 */
static void
scavenge_large_srt_bitmap( StgLargeSRT *large_srt )
{
    nat i, b, size;
    StgWord bitmap;
    StgClosure **p;
    
    b = 0;
    bitmap = large_srt->l.bitmap[b];
    size   = (nat)large_srt->l.size;
    p      = (StgClosure **)large_srt->srt;
    for (i = 0; i < size; ) {
	if ((bitmap & 1) != 0) {
	    evacuate(*p);
	}
	i++;
	p++;
	if (i % BITS_IN(W_) == 0) {
	    b++;
	    bitmap = large_srt->l.bitmap[b];
	} else {
	    bitmap = bitmap >> 1;
	}
    }
}

/* evacuate the SRT.  If srt_bitmap is zero, then there isn't an
 * srt field in the info table.  That's ok, because we'll
 * never dereference it.
 */
STATIC_INLINE void
scavenge_srt (StgClosure **srt, nat srt_bitmap)
{
  nat bitmap;
  StgClosure **p;

  bitmap = srt_bitmap;
  p = srt;

  if (bitmap == (StgHalfWord)(-1)) {  
      scavenge_large_srt_bitmap( (StgLargeSRT *)srt );
      return;
  }

  while (bitmap != 0) {
      if ((bitmap & 1) != 0) {
#if defined(__PIC__) && defined(mingw32_TARGET_OS)
	  // Special-case to handle references to closures hiding out in DLLs, since
	  // double indirections required to get at those. The code generator knows
	  // which is which when generating the SRT, so it stores the (indirect)
	  // reference to the DLL closure in the table by first adding one to it.
	  // We check for this here, and undo the addition before evacuating it.
	  // 
	  // If the SRT entry hasn't got bit 0 set, the SRT entry points to a
	  // closure that's fixed at link-time, and no extra magic is required.
	  if ( (unsigned long)(*srt) & 0x1 ) {
	      evacuate(*stgCast(StgClosure**,(stgCast(unsigned long, *srt) & ~0x1)));
	  } else {
	      evacuate(*p);
	  }
#else
	  evacuate(*p);
#endif
      }
      p++;
      bitmap = bitmap >> 1;
  }
}


STATIC_INLINE void
scavenge_thunk_srt(const StgInfoTable *info)
{
    StgThunkInfoTable *thunk_info;

    if (!major_gc) return;

    thunk_info = itbl_to_thunk_itbl(info);
    scavenge_srt((StgClosure **)GET_SRT(thunk_info), thunk_info->i.srt_bitmap);
}

STATIC_INLINE void
scavenge_fun_srt(const StgInfoTable *info)
{
    StgFunInfoTable *fun_info;

    if (!major_gc) return;
  
    fun_info = itbl_to_fun_itbl(info);
    scavenge_srt((StgClosure **)GET_FUN_SRT(fun_info), fun_info->i.srt_bitmap);
}

/* -----------------------------------------------------------------------------
   Scavenge a TSO.
   -------------------------------------------------------------------------- */

static void
scavengeTSO (StgTSO *tso)
{
    if (   tso->why_blocked == BlockedOnMVar
	|| tso->why_blocked == BlockedOnBlackHole
	|| tso->why_blocked == BlockedOnException
	) {
	tso->block_info.closure = evacuate(tso->block_info.closure);
    }
    tso->blocked_exceptions = 
	(StgTSO *)evacuate((StgClosure *)tso->blocked_exceptions);
    
    // We don't always chase the link field: TSOs on the blackhole
    // queue are not automatically alive, so the link field is a
    // "weak" pointer in that case.
    if (tso->why_blocked != BlockedOnBlackHole) {
	tso->link = (StgTSO *)evacuate((StgClosure *)tso->link);
    }

    // scavange current transaction record
    tso->trec = (StgTRecHeader *)evacuate((StgClosure *)tso->trec);
    
    // scavenge this thread's stack 
    scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
}

/* -----------------------------------------------------------------------------
   Blocks of function args occur on the stack (at the top) and
   in PAPs.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
scavenge_arg_block (StgFunInfoTable *fun_info, StgClosure **args)
{
    StgPtr p;
    StgWord bitmap;
    nat size;

    p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
	size = BITMAP_SIZE(fun_info->f.b.bitmap);
	goto small_bitmap;
    case ARG_GEN_BIG:
	size = GET_FUN_LARGE_BITMAP(fun_info)->size;
	scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
	size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

STATIC_INLINE StgPtr
scavenge_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size)
{
    StgPtr p;
    StgWord bitmap;
    StgFunInfoTable *fun_info;
    
    fun_info = get_fun_itbl(UNTAG_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
	goto small_bitmap;
    case ARG_GEN_BIG:
	scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    case ARG_BCO:
	scavenge_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

STATIC_INLINE StgPtr
scavenge_PAP (StgPAP *pap)
{
    pap->fun = evacuate(pap->fun);
    return scavenge_PAP_payload (pap->fun, pap->payload, pap->n_args);
}

STATIC_INLINE StgPtr
scavenge_AP (StgAP *ap)
{
    ap->fun = evacuate(ap->fun);
    return scavenge_PAP_payload (ap->fun, ap->payload, ap->n_args);
}

/* -----------------------------------------------------------------------------
   Scavenge a given step until there are no more objects in this step
   to scavenge.

   evac_gen is set by the caller to be either zero (for a step in a
   generation < N) or G where G is the generation of the step being
   scavenged.  

   We sometimes temporarily change evac_gen back to zero if we're
   scavenging a mutable object where early promotion isn't such a good
   idea.  
   -------------------------------------------------------------------------- */

void
scavenge(step *stp)
{
  StgPtr p, q;
  StgInfoTable *info;
  bdescr *bd;
  nat saved_evac_gen = evac_gen;

  p = stp->scan;
  bd = stp->scan_bd;

  failed_to_evac = rtsFalse;

  /* scavenge phase - standard breadth-first scavenging of the
   * evacuated objects 
   */

  while (bd != stp->hp_bd || p < stp->hp) {

    // If we're at the end of this block, move on to the next block 
    if (bd != stp->hp_bd && p == bd->free) {
      bd = bd->link;
      p = bd->start;
      continue;
    }

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);
    
    ASSERT(thunk_selector_depth == 0);

    q = p;
    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    { 
	rtsBool saved_eager_promotion = eager_promotion;

	StgMVar *mvar = ((StgMVar *)p);
	eager_promotion = rtsFalse;
	mvar->head = (StgTSO *)evacuate((StgClosure *)mvar->head);
	mvar->tail = (StgTSO *)evacuate((StgClosure *)mvar->tail);
	mvar->value = evacuate((StgClosure *)mvar->value);
	eager_promotion = saved_eager_promotion;

	if (failed_to_evac) {
	    mvar->header.info = &stg_MVAR_DIRTY_info;
	} else {
	    mvar->header.info = &stg_MVAR_CLEAN_info;
	}
	p += sizeofW(StgMVar);
	break;
    }

    case FUN_2_0:
	scavenge_fun_srt(info);
	((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;

    case THUNK_2_0:
	scavenge_thunk_srt(info);
	((StgThunk *)p)->payload[1] = evacuate(((StgThunk *)p)->payload[1]);
	((StgThunk *)p)->payload[0] = evacuate(((StgThunk *)p)->payload[0]);
	p += sizeofW(StgThunk) + 2;
	break;

    case CONSTR_2_0:
	((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case THUNK_1_0:
	scavenge_thunk_srt(info);
	((StgThunk *)p)->payload[0] = evacuate(((StgThunk *)p)->payload[0]);
	p += sizeofW(StgThunk) + 1;
	break;
	
    case FUN_1_0:
	scavenge_fun_srt(info);
    case CONSTR_1_0:
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 1;
	break;
	
    case THUNK_0_1:
	scavenge_thunk_srt(info);
	p += sizeofW(StgThunk) + 1;
	break;
	
    case FUN_0_1:
	scavenge_fun_srt(info);
    case CONSTR_0_1:
	p += sizeofW(StgHeader) + 1;
	break;
	
    case THUNK_0_2:
	scavenge_thunk_srt(info);
	p += sizeofW(StgThunk) + 2;
	break;
	
    case FUN_0_2:
	scavenge_fun_srt(info);
    case CONSTR_0_2:
	p += sizeofW(StgHeader) + 2;
	break;
	
    case THUNK_1_1:
	scavenge_thunk_srt(info);
	((StgThunk *)p)->payload[0] = evacuate(((StgThunk *)p)->payload[0]);
	p += sizeofW(StgThunk) + 2;
	break;

    case FUN_1_1:
	scavenge_fun_srt(info);
    case CONSTR_1_1:
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case FUN:
	scavenge_fun_srt(info);
	goto gen_obj;

    case THUNK:
    {
	StgPtr end;

	scavenge_thunk_srt(info);
	end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	p += info->layout.payload.nptrs;
	break;
    }
	
    gen_obj:
    case CONSTR:
    case WEAK:
    case STABLE_NAME:
    {
	StgPtr end;

	end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	p += info->layout.payload.nptrs;
	break;
    }

    case BCO: {
	StgBCO *bco = (StgBCO *)p;
	bco->instrs = (StgArrWords *)evacuate((StgClosure *)bco->instrs);
	bco->literals = (StgArrWords *)evacuate((StgClosure *)bco->literals);
	bco->ptrs = (StgMutArrPtrs *)evacuate((StgClosure *)bco->ptrs);
	p += bco_sizeW(bco);
	break;
    }

    case IND_PERM:
      if (stp->gen->no != 0) {
#ifdef PROFILING
        // @LDV profiling
        // No need to call LDV_recordDead_FILL_SLOP_DYNAMIC() because an 
        // IND_OLDGEN_PERM closure is larger than an IND_PERM closure.
        LDV_recordDead((StgClosure *)p, sizeofW(StgInd));
#endif        
        // 
        // Todo: maybe use SET_HDR() and remove LDV_RECORD_CREATE()?
        //
	SET_INFO(((StgClosure *)p), &stg_IND_OLDGEN_PERM_info);

        // We pretend that p has just been created.
        LDV_RECORD_CREATE((StgClosure *)p);
      }
	// fall through 
    case IND_OLDGEN_PERM:
	((StgInd *)p)->indirectee = evacuate(((StgInd *)p)->indirectee);
	p += sizeofW(StgInd);
	break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
	rtsBool saved_eager_promotion = eager_promotion;

	eager_promotion = rtsFalse;
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	eager_promotion = saved_eager_promotion;

	if (failed_to_evac) {
	    ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
	} else {
	    ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
	}
	p += sizeofW(StgMutVar);
	break;
    }

    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
    case SE_BLACKHOLE:
    case BLACKHOLE:
	p += BLACKHOLE_sizeW();
	break;

    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	s->selectee = evacuate(s->selectee);
	p += THUNK_SELECTOR_sizeW();
	break;
    }

    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
	StgAP_STACK *ap = (StgAP_STACK *)p;

	ap->fun = evacuate(ap->fun);
	scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	p = (StgPtr)ap->payload + ap->size;
	break;
    }

    case PAP:
	p = scavenge_PAP((StgPAP *)p);
	break;

    case AP:
	p = scavenge_AP((StgAP *)p);
	break;

    case ARR_WORDS:
	// nothing to follow 
	p += arr_words_sizeW((StgArrWords *)p);
	break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
	// follow everything 
    {
	StgPtr next;
	rtsBool saved_eager;

	// We don't eagerly promote objects pointed to by a mutable
	// array, but if we find the array only points to objects in
	// the same or an older generation, we mark it "clean" and
	// avoid traversing it during minor GCs.
	saved_eager = eager_promotion;
	eager_promotion = rtsFalse;
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	eager_promotion = saved_eager;

	if (failed_to_evac) {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
	} else {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
	}

	failed_to_evac = rtsTrue; // always put it on the mutable list.
	break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
	// follow everything 
    {
	StgPtr next;

	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}

	// If we're going to put this object on the mutable list, then
	// set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
	if (failed_to_evac) {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
	} else {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
	}
	break;
    }

    case TSO:
    { 
	StgTSO *tso = (StgTSO *)p;
	rtsBool saved_eager = eager_promotion;

	eager_promotion = rtsFalse;
	scavengeTSO(tso);
	eager_promotion = saved_eager;

	if (failed_to_evac) {
	    tso->flags |= TSO_DIRTY;
	} else {
	    tso->flags &= ~TSO_DIRTY;
	}

	failed_to_evac = rtsTrue; // always on the mutable list
	p += tso_sizeW(tso);
	break;
    }

    case TVAR_WATCH_QUEUE:
      {
	StgTVarWatchQueue *wq = ((StgTVarWatchQueue *) p);
	evac_gen = 0;
	wq->closure = (StgClosure*)evacuate((StgClosure*)wq->closure);
	wq->next_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)wq->next_queue_entry);
	wq->prev_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)wq->prev_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTVarWatchQueue);
	break;
      }

    case TVAR:
      {
	StgTVar *tvar = ((StgTVar *) p);
	evac_gen = 0;
	tvar->current_value = evacuate((StgClosure*)tvar->current_value);
	tvar->first_watch_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)tvar->first_watch_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTVar);
	break;
      }

    case TREC_HEADER:
      {
        StgTRecHeader *trec = ((StgTRecHeader *) p);
        evac_gen = 0;
	trec->enclosing_trec = (StgTRecHeader *)evacuate((StgClosure*)trec->enclosing_trec);
	trec->current_chunk = (StgTRecChunk *)evacuate((StgClosure*)trec->current_chunk);
	trec->invariants_to_check = (StgInvariantCheckQueue *)evacuate((StgClosure*)trec->invariants_to_check);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTRecHeader);
        break;
      }

    case TREC_CHUNK:
      {
	StgWord i;
	StgTRecChunk *tc = ((StgTRecChunk *) p);
	TRecEntry *e = &(tc -> entries[0]);
	evac_gen = 0;
	tc->prev_chunk = (StgTRecChunk *)evacuate((StgClosure*)tc->prev_chunk);
	for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	  e->tvar = (StgTVar *)evacuate((StgClosure*)e->tvar);
	  e->expected_value = evacuate((StgClosure*)e->expected_value);
	  e->new_value = evacuate((StgClosure*)e->new_value);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTRecChunk);
	break;
      }

    case ATOMIC_INVARIANT:
      {
        StgAtomicInvariant *invariant = ((StgAtomicInvariant *) p);
        evac_gen = 0;
	invariant->code = (StgClosure *)evacuate(invariant->code);
	invariant->last_execution = (StgTRecHeader *)evacuate((StgClosure*)invariant->last_execution);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgAtomicInvariant);
        break;
      }

    case INVARIANT_CHECK_QUEUE:
      {
        StgInvariantCheckQueue *queue = ((StgInvariantCheckQueue *) p);
        evac_gen = 0;
	queue->invariant = (StgAtomicInvariant *)evacuate((StgClosure*)queue->invariant);
	queue->my_execution = (StgTRecHeader *)evacuate((StgClosure*)queue->my_execution);
	queue->next_queue_entry = (StgInvariantCheckQueue *)evacuate((StgClosure*)queue->next_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgInvariantCheckQueue);
        break;
      }

    default:
	barf("scavenge: unimplemented/strange closure type %d @ %p", 
	     info->type, p);
    }

    /*
     * We need to record the current object on the mutable list if
     *  (a) It is actually mutable, or 
     *  (b) It contains pointers to a younger generation.
     * Case (b) arises if we didn't manage to promote everything that
     * the current object points to into the current generation.
     */
    if (failed_to_evac) {
	failed_to_evac = rtsFalse;
	if (stp->gen_no > 0) {
	    recordMutableGen((StgClosure *)q, stp->gen);
	}
    }
  }

  stp->scan_bd = bd;
  stp->scan = p;
}    

/* -----------------------------------------------------------------------------
   Scavenge everything on the mark stack.

   This is slightly different from scavenge():
      - we don't walk linearly through the objects, so the scavenger
        doesn't need to advance the pointer on to the next object.
   -------------------------------------------------------------------------- */

void
scavenge_mark_stack(void)
{
    StgPtr p, q;
    StgInfoTable *info;
    nat saved_evac_gen;

    evac_gen = oldest_gen->no;
    saved_evac_gen = evac_gen;

linear_scan:
    while (!mark_stack_empty()) {
	p = pop_mark_stack();

	ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
	info = get_itbl((StgClosure *)p);
	
	q = p;
	switch (info->type) {
	    
        case MVAR_CLEAN:
        case MVAR_DIRTY:
        { 
            rtsBool saved_eager_promotion = eager_promotion;
            
            StgMVar *mvar = ((StgMVar *)p);
            eager_promotion = rtsFalse;
            mvar->head = (StgTSO *)evacuate((StgClosure *)mvar->head);
            mvar->tail = (StgTSO *)evacuate((StgClosure *)mvar->tail);
            mvar->value = evacuate((StgClosure *)mvar->value);
            eager_promotion = saved_eager_promotion;
            
            if (failed_to_evac) {
                mvar->header.info = &stg_MVAR_DIRTY_info;
            } else {
                mvar->header.info = &stg_MVAR_CLEAN_info;
            }
            break;
        }

	case FUN_2_0:
	    scavenge_fun_srt(info);
	    ((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;

	case THUNK_2_0:
	    scavenge_thunk_srt(info);
	    ((StgThunk *)p)->payload[1] = evacuate(((StgThunk *)p)->payload[1]);
	    ((StgThunk *)p)->payload[0] = evacuate(((StgThunk *)p)->payload[0]);
	    break;

	case CONSTR_2_0:
	    ((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;
	
	case FUN_1_0:
	case FUN_1_1:
	    scavenge_fun_srt(info);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;

	case THUNK_1_0:
	case THUNK_1_1:
	    scavenge_thunk_srt(info);
	    ((StgThunk *)p)->payload[0] = evacuate(((StgThunk *)p)->payload[0]);
	    break;

	case CONSTR_1_0:
	case CONSTR_1_1:
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;
	
	case FUN_0_1:
	case FUN_0_2:
	    scavenge_fun_srt(info);
	    break;

	case THUNK_0_1:
	case THUNK_0_2:
	    scavenge_thunk_srt(info);
	    break;

	case CONSTR_0_1:
	case CONSTR_0_2:
	    break;
	
	case FUN:
	    scavenge_fun_srt(info);
	    goto gen_obj;

	case THUNK:
	{
	    StgPtr end;
	    
	    scavenge_thunk_srt(info);
	    end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
	    for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    break;
	}
	
	gen_obj:
	case CONSTR:
	case WEAK:
	case STABLE_NAME:
	{
	    StgPtr end;
	    
	    end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	    for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    break;
	}

	case BCO: {
	    StgBCO *bco = (StgBCO *)p;
	    bco->instrs = (StgArrWords *)evacuate((StgClosure *)bco->instrs);
	    bco->literals = (StgArrWords *)evacuate((StgClosure *)bco->literals);
	    bco->ptrs = (StgMutArrPtrs *)evacuate((StgClosure *)bco->ptrs);
	    break;
	}

	case IND_PERM:
	    // don't need to do anything here: the only possible case
	    // is that we're in a 1-space compacting collector, with
	    // no "old" generation.
	    break;

	case IND_OLDGEN:
	case IND_OLDGEN_PERM:
	    ((StgInd *)p)->indirectee = 
		evacuate(((StgInd *)p)->indirectee);
	    break;

	case MUT_VAR_CLEAN:
	case MUT_VAR_DIRTY: {
	    rtsBool saved_eager_promotion = eager_promotion;
	    
	    eager_promotion = rtsFalse;
	    ((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	    eager_promotion = saved_eager_promotion;
	    
	    if (failed_to_evac) {
		((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
	    } else {
		((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
	    }
	    break;
	}

	case CAF_BLACKHOLE:
	case SE_CAF_BLACKHOLE:
	case SE_BLACKHOLE:
	case BLACKHOLE:
	case ARR_WORDS:
	    break;

	case THUNK_SELECTOR:
	{ 
	    StgSelector *s = (StgSelector *)p;
	    s->selectee = evacuate(s->selectee);
	    break;
	}

	// A chunk of stack saved in a heap object
	case AP_STACK:
	{
	    StgAP_STACK *ap = (StgAP_STACK *)p;
	    
	    ap->fun = evacuate(ap->fun);
	    scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	    break;
	}

	case PAP:
	    scavenge_PAP((StgPAP *)p);
	    break;

	case AP:
	    scavenge_AP((StgAP *)p);
	    break;
      
	case MUT_ARR_PTRS_CLEAN:
	case MUT_ARR_PTRS_DIRTY:
	    // follow everything 
	{
	    StgPtr next;
	    rtsBool saved_eager;

	    // We don't eagerly promote objects pointed to by a mutable
	    // array, but if we find the array only points to objects in
	    // the same or an older generation, we mark it "clean" and
	    // avoid traversing it during minor GCs.
	    saved_eager = eager_promotion;
	    eager_promotion = rtsFalse;
	    next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	    for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    eager_promotion = saved_eager;

	    if (failed_to_evac) {
		((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
	    } else {
		((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
	    }

	    failed_to_evac = rtsTrue; // mutable anyhow.
	    break;
	}

	case MUT_ARR_PTRS_FROZEN:
	case MUT_ARR_PTRS_FROZEN0:
	    // follow everything 
	{
	    StgPtr next, q = p;
	    
	    next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	    for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }

	    // If we're going to put this object on the mutable list, then
	    // set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
	    if (failed_to_evac) {
		((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
	    } else {
		((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
	    }
	    break;
	}

	case TSO:
	{ 
	    StgTSO *tso = (StgTSO *)p;
	    rtsBool saved_eager = eager_promotion;

	    eager_promotion = rtsFalse;
	    scavengeTSO(tso);
	    eager_promotion = saved_eager;
	    
	    if (failed_to_evac) {
		tso->flags |= TSO_DIRTY;
	    } else {
		tso->flags &= ~TSO_DIRTY;
	    }
	    
	    failed_to_evac = rtsTrue; // always on the mutable list
	    break;
	}

	case TVAR_WATCH_QUEUE:
	  {
	    StgTVarWatchQueue *wq = ((StgTVarWatchQueue *) p);
	    evac_gen = 0;
            wq->closure = (StgClosure*)evacuate((StgClosure*)wq->closure);
	    wq->next_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)wq->next_queue_entry);
	    wq->prev_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)wq->prev_queue_entry);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }
	  
	case TVAR:
	  {
	    StgTVar *tvar = ((StgTVar *) p);
	    evac_gen = 0;
	    tvar->current_value = evacuate((StgClosure*)tvar->current_value);
	    tvar->first_watch_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)tvar->first_watch_queue_entry);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }
	  
	case TREC_CHUNK:
	  {
	    StgWord i;
	    StgTRecChunk *tc = ((StgTRecChunk *) p);
	    TRecEntry *e = &(tc -> entries[0]);
	    evac_gen = 0;
	    tc->prev_chunk = (StgTRecChunk *)evacuate((StgClosure*)tc->prev_chunk);
	    for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	      e->tvar = (StgTVar *)evacuate((StgClosure*)e->tvar);
	      e->expected_value = evacuate((StgClosure*)e->expected_value);
	      e->new_value = evacuate((StgClosure*)e->new_value);
	    }
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }

	case TREC_HEADER:
	  {
	    StgTRecHeader *trec = ((StgTRecHeader *) p);
	    evac_gen = 0;
	    trec->enclosing_trec = (StgTRecHeader *)evacuate((StgClosure*)trec->enclosing_trec);
	    trec->current_chunk = (StgTRecChunk *)evacuate((StgClosure*)trec->current_chunk);
  	    trec->invariants_to_check = (StgInvariantCheckQueue *)evacuate((StgClosure*)trec->invariants_to_check);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }

        case ATOMIC_INVARIANT:
          {
            StgAtomicInvariant *invariant = ((StgAtomicInvariant *) p);
            evac_gen = 0;
	    invariant->code = (StgClosure *)evacuate(invariant->code);
    	    invariant->last_execution = (StgTRecHeader *)evacuate((StgClosure*)invariant->last_execution);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
            break;
          }

        case INVARIANT_CHECK_QUEUE:
          {
            StgInvariantCheckQueue *queue = ((StgInvariantCheckQueue *) p);
            evac_gen = 0;
    	    queue->invariant = (StgAtomicInvariant *)evacuate((StgClosure*)queue->invariant);
	    queue->my_execution = (StgTRecHeader *)evacuate((StgClosure*)queue->my_execution);
            queue->next_queue_entry = (StgInvariantCheckQueue *)evacuate((StgClosure*)queue->next_queue_entry);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
            break;
          }

	default:
	    barf("scavenge_mark_stack: unimplemented/strange closure type %d @ %p", 
		 info->type, p);
	}

	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    if (evac_gen > 0) {
		recordMutableGen((StgClosure *)q, &generations[evac_gen]);
	    }
	}
	
	// mark the next bit to indicate "scavenged"
	mark(q+1, Bdescr(q));

    } // while (!mark_stack_empty())

    // start a new linear scan if the mark stack overflowed at some point
    if (mark_stack_overflowed && oldgen_scan_bd == NULL) {
	debugTrace(DEBUG_gc, "scavenge_mark_stack: starting linear scan");
	mark_stack_overflowed = rtsFalse;
	oldgen_scan_bd = oldest_gen->steps[0].old_blocks;
	oldgen_scan = oldgen_scan_bd->start;
    }

    if (oldgen_scan_bd) {
	// push a new thing on the mark stack
    loop:
	// find a closure that is marked but not scavenged, and start
	// from there.
	while (oldgen_scan < oldgen_scan_bd->free 
	       && !is_marked(oldgen_scan,oldgen_scan_bd)) {
	    oldgen_scan++;
	}

	if (oldgen_scan < oldgen_scan_bd->free) {

	    // already scavenged?
	    if (is_marked(oldgen_scan+1,oldgen_scan_bd)) {
		oldgen_scan += sizeofW(StgHeader) + MIN_PAYLOAD_SIZE;
		goto loop;
	    }
	    push_mark_stack(oldgen_scan);
	    // ToDo: bump the linear scan by the actual size of the object
	    oldgen_scan += sizeofW(StgHeader) + MIN_PAYLOAD_SIZE;
	    goto linear_scan;
	}

	oldgen_scan_bd = oldgen_scan_bd->link;
	if (oldgen_scan_bd != NULL) {
	    oldgen_scan = oldgen_scan_bd->start;
	    goto loop;
	}
    }
}

/* -----------------------------------------------------------------------------
   Scavenge one object.

   This is used for objects that are temporarily marked as mutable
   because they contain old-to-new generation pointers.  Only certain
   objects can have this property.
   -------------------------------------------------------------------------- */

static rtsBool
scavenge_one(StgPtr p)
{
    const StgInfoTable *info;
    nat saved_evac_gen = evac_gen;
    rtsBool no_luck;
    
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);
    
    switch (info->type) {
	
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    { 
	rtsBool saved_eager_promotion = eager_promotion;

	StgMVar *mvar = ((StgMVar *)p);
	eager_promotion = rtsFalse;
	mvar->head = (StgTSO *)evacuate((StgClosure *)mvar->head);
	mvar->tail = (StgTSO *)evacuate((StgClosure *)mvar->tail);
	mvar->value = evacuate((StgClosure *)mvar->value);
	eager_promotion = saved_eager_promotion;

	if (failed_to_evac) {
	    mvar->header.info = &stg_MVAR_DIRTY_info;
	} else {
	    mvar->header.info = &stg_MVAR_CLEAN_info;
	}
	break;
    }

    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    {
	StgPtr q, end;
	
	end = (StgPtr)((StgThunk *)p)->payload + info->layout.payload.ptrs;
	for (q = (StgPtr)((StgThunk *)p)->payload; q < end; q++) {
	    *q = (StgWord)(StgPtr)evacuate((StgClosure *)*q);
	}
	break;
    }

    case FUN:
    case FUN_1_0:			// hardly worth specialising these guys
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case WEAK:
    case IND_PERM:
    {
	StgPtr q, end;
	
	end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (q = (StgPtr)((StgClosure *)p)->payload; q < end; q++) {
	    *q = (StgWord)(StgPtr)evacuate((StgClosure *)*q);
	}
	break;
    }
    
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
	StgPtr q = p;
	rtsBool saved_eager_promotion = eager_promotion;

	eager_promotion = rtsFalse;
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	eager_promotion = saved_eager_promotion;

	if (failed_to_evac) {
	    ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
	} else {
	    ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
	}
	break;
    }

    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
    case SE_BLACKHOLE:
    case BLACKHOLE:
	break;
	
    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	s->selectee = evacuate(s->selectee);
	break;
    }
    
    case AP_STACK:
    {
	StgAP_STACK *ap = (StgAP_STACK *)p;

	ap->fun = evacuate(ap->fun);
	scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	p = (StgPtr)ap->payload + ap->size;
	break;
    }

    case PAP:
	p = scavenge_PAP((StgPAP *)p);
	break;

    case AP:
	p = scavenge_AP((StgAP *)p);
	break;

    case ARR_WORDS:
	// nothing to follow 
	break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
	StgPtr next, q;
	rtsBool saved_eager;

	// We don't eagerly promote objects pointed to by a mutable
	// array, but if we find the array only points to objects in
	// the same or an older generation, we mark it "clean" and
	// avoid traversing it during minor GCs.
	saved_eager = eager_promotion;
	eager_promotion = rtsFalse;
	q = p;
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	eager_promotion = saved_eager;

	if (failed_to_evac) {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
	} else {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
	}

	failed_to_evac = rtsTrue;
	break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
    {
	// follow everything 
	StgPtr next, q=p;
      
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}

	// If we're going to put this object on the mutable list, then
	// set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
	if (failed_to_evac) {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
	} else {
	    ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
	}
	break;
    }

    case TSO:
    {
	StgTSO *tso = (StgTSO *)p;
	rtsBool saved_eager = eager_promotion;

	eager_promotion = rtsFalse;
	scavengeTSO(tso);
	eager_promotion = saved_eager;

	if (failed_to_evac) {
	    tso->flags |= TSO_DIRTY;
	} else {
	    tso->flags &= ~TSO_DIRTY;
	}

	failed_to_evac = rtsTrue; // always on the mutable list
	break;
    }
  
    case TVAR_WATCH_QUEUE:
      {
	StgTVarWatchQueue *wq = ((StgTVarWatchQueue *) p);
	evac_gen = 0;
        wq->closure = (StgClosure*)evacuate((StgClosure*)wq->closure);
        wq->next_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)wq->next_queue_entry);
        wq->prev_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)wq->prev_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	break;
      }

    case TVAR:
      {
	StgTVar *tvar = ((StgTVar *) p);
	evac_gen = 0;
	tvar->current_value = evacuate((StgClosure*)tvar->current_value);
        tvar->first_watch_queue_entry = (StgTVarWatchQueue *)evacuate((StgClosure*)tvar->first_watch_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	break;
      }

    case TREC_HEADER:
      {
        StgTRecHeader *trec = ((StgTRecHeader *) p);
        evac_gen = 0;
	trec->enclosing_trec = (StgTRecHeader *)evacuate((StgClosure*)trec->enclosing_trec);
	trec->current_chunk = (StgTRecChunk *)evacuate((StgClosure*)trec->current_chunk);
        trec->invariants_to_check = (StgInvariantCheckQueue *)evacuate((StgClosure*)trec->invariants_to_check);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
        break;
      }

    case TREC_CHUNK:
      {
	StgWord i;
	StgTRecChunk *tc = ((StgTRecChunk *) p);
	TRecEntry *e = &(tc -> entries[0]);
	evac_gen = 0;
	tc->prev_chunk = (StgTRecChunk *)evacuate((StgClosure*)tc->prev_chunk);
	for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	  e->tvar = (StgTVar *)evacuate((StgClosure*)e->tvar);
	  e->expected_value = evacuate((StgClosure*)e->expected_value);
	  e->new_value = evacuate((StgClosure*)e->new_value);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	break;
      }

    case ATOMIC_INVARIANT:
    {
      StgAtomicInvariant *invariant = ((StgAtomicInvariant *) p);
      evac_gen = 0;
      invariant->code = (StgClosure *)evacuate(invariant->code);
      invariant->last_execution = (StgTRecHeader *)evacuate((StgClosure*)invariant->last_execution);
      evac_gen = saved_evac_gen;
      failed_to_evac = rtsTrue; // mutable
      break;
    }

    case INVARIANT_CHECK_QUEUE:
    {
      StgInvariantCheckQueue *queue = ((StgInvariantCheckQueue *) p);
      evac_gen = 0;
      queue->invariant = (StgAtomicInvariant *)evacuate((StgClosure*)queue->invariant);
      queue->my_execution = (StgTRecHeader *)evacuate((StgClosure*)queue->my_execution);
      queue->next_queue_entry = (StgInvariantCheckQueue *)evacuate((StgClosure*)queue->next_queue_entry);
      evac_gen = saved_evac_gen;
      failed_to_evac = rtsTrue; // mutable
      break;
    }

    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
    {
	/* Careful here: a THUNK can be on the mutable list because
	 * it contains pointers to young gen objects.  If such a thunk
	 * is updated, the IND_OLDGEN will be added to the mutable
	 * list again, and we'll scavenge it twice.  evacuate()
	 * doesn't check whether the object has already been
	 * evacuated, so we perform that check here.
	 */
	StgClosure *q = ((StgInd *)p)->indirectee;
	if (HEAP_ALLOCED(q) && Bdescr((StgPtr)q)->flags & BF_EVACUATED) {
	    break;
	}
	((StgInd *)p)->indirectee = evacuate(q);
    }

#if 0 && defined(DEBUG)
      if (RtsFlags.DebugFlags.gc) 
      /* Debugging code to print out the size of the thing we just
       * promoted 
       */
      { 
	StgPtr start = gen->steps[0].scan;
	bdescr *start_bd = gen->steps[0].scan_bd;
	nat size = 0;
	scavenge(&gen->steps[0]);
	if (start_bd != gen->steps[0].scan_bd) {
	  size += (P_)BLOCK_ROUND_UP(start) - start;
	  start_bd = start_bd->link;
	  while (start_bd != gen->steps[0].scan_bd) {
	    size += BLOCK_SIZE_W;
	    start_bd = start_bd->link;
	  }
	  size += gen->steps[0].scan -
	    (P_)BLOCK_ROUND_DOWN(gen->steps[0].scan);
	} else {
	  size = gen->steps[0].scan - start;
	}
	debugBelch("evac IND_OLDGEN: %ld bytes", size * sizeof(W_));
      }
#endif
      break;

    default:
	barf("scavenge_one: strange object %d", (int)(info->type));
    }    

    no_luck = failed_to_evac;
    failed_to_evac = rtsFalse;
    return (no_luck);
}

/* -----------------------------------------------------------------------------
   Scavenging mutable lists.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

void
scavenge_mutable_list(generation *gen)
{
    bdescr *bd;
    StgPtr p, q;

    bd = gen->saved_mut_list;

    evac_gen = gen->no;
    for (; bd != NULL; bd = bd->link) {
	for (q = bd->start; q < bd->free; q++) {
	    p = (StgPtr)*q;
	    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

#ifdef DEBUG	    
	    switch (get_itbl((StgClosure *)p)->type) {
	    case MUT_VAR_CLEAN:
		barf("MUT_VAR_CLEAN on mutable list");
	    case MUT_VAR_DIRTY:
		mutlist_MUTVARS++; break;
	    case MUT_ARR_PTRS_CLEAN:
	    case MUT_ARR_PTRS_DIRTY:
	    case MUT_ARR_PTRS_FROZEN:
	    case MUT_ARR_PTRS_FROZEN0:
		mutlist_MUTARRS++; break;
	    case MVAR_CLEAN:
		barf("MVAR_CLEAN on mutable list");
	    case MVAR_DIRTY:
		mutlist_MVARS++; break;
	    default:
		mutlist_OTHERS++; break;
	    }
#endif

	    // Check whether this object is "clean", that is it
	    // definitely doesn't point into a young generation.
	    // Clean objects don't need to be scavenged.  Some clean
	    // objects (MUT_VAR_CLEAN) are not kept on the mutable
	    // list at all; others, such as MUT_ARR_PTRS_CLEAN and
	    // TSO, are always on the mutable list.
	    //
	    switch (get_itbl((StgClosure *)p)->type) {
	    case MUT_ARR_PTRS_CLEAN:
		recordMutableGen((StgClosure *)p,gen);
		continue;
	    case TSO: {
		StgTSO *tso = (StgTSO *)p;
		if ((tso->flags & TSO_DIRTY) == 0) {
		    // A clean TSO: we don't have to traverse its
		    // stack.  However, we *do* follow the link field:
		    // we don't want to have to mark a TSO dirty just
		    // because we put it on a different queue.
		    if (tso->why_blocked != BlockedOnBlackHole) {
			tso->link = (StgTSO *)evacuate((StgClosure *)tso->link);
		    }
		    recordMutableGen((StgClosure *)p,gen);
		    continue;
		}
	    }
	    default:
		;
	    }

	    if (scavenge_one(p)) {
		// didn't manage to promote everything, so put the
		// object back on the list.
		recordMutableGen((StgClosure *)p,gen);
	    }
	}
    }

    // free the old mut_list
    freeChain(gen->saved_mut_list);
    gen->saved_mut_list = NULL;
}

/* -----------------------------------------------------------------------------
   Scavenging the static objects.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

void
scavenge_static(void)
{
  StgClosure* p = static_objects;
  const StgInfoTable *info;

  /* Always evacuate straight to the oldest generation for static
   * objects */
  evac_gen = oldest_gen->no;

  /* keep going until we've scavenged all the objects on the linked
     list... */
  while (p != END_OF_STATIC_LIST) {

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl(p);
    /*
    if (info->type==RBH)
      info = REVERT_INFOPTR(info); // if it's an RBH, look at the orig closure
    */
    // make sure the info pointer is into text space 
    
    /* Take this object *off* the static_objects list,
     * and put it on the scavenged_static_objects list.
     */
    static_objects = *STATIC_LINK(info,p);
    *STATIC_LINK(info,p) = scavenged_static_objects;
    scavenged_static_objects = p;
    
    switch (info -> type) {
      
    case IND_STATIC:
      {
	StgInd *ind = (StgInd *)p;
	ind->indirectee = evacuate(ind->indirectee);

	/* might fail to evacuate it, in which case we have to pop it
	 * back on the mutable list of the oldest generation.  We
	 * leave it *on* the scavenged_static_objects list, though,
	 * in case we visit this object again.
	 */
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  recordMutableGen((StgClosure *)p,oldest_gen);
	}
	break;
      }
      
    case THUNK_STATIC:
      scavenge_thunk_srt(info);
      break;

    case FUN_STATIC:
      scavenge_fun_srt(info);
      break;
      
    case CONSTR_STATIC:
      {	
	StgPtr q, next;
	
	next = (P_)p->payload + info->layout.payload.ptrs;
	// evacuate the pointers 
	for (q = (P_)p->payload; q < next; q++) {
	    *q = (StgWord)(StgPtr)evacuate((StgClosure *)*q);
	}
	break;
      }
      
    default:
      barf("scavenge_static: strange closure %d", (int)(info->type));
    }

    ASSERT(failed_to_evac == rtsFalse);

    /* get the next static object from the list.  Remember, there might
     * be more stuff on this list now that we've done some evacuating!
     * (static_objects is a global)
     */
    p = static_objects;
  }
}

/* -----------------------------------------------------------------------------
   scavenge a chunk of memory described by a bitmap
   -------------------------------------------------------------------------- */

static void
scavenge_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, nat size )
{
    nat i, b;
    StgWord bitmap;
    
    b = 0;
    bitmap = large_bitmap->bitmap[b];
    for (i = 0; i < size; ) {
	if ((bitmap & 1) == 0) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	i++;
	p++;
	if (i % BITS_IN(W_) == 0) {
	    b++;
	    bitmap = large_bitmap->bitmap[b];
	} else {
	    bitmap = bitmap >> 1;
	}
    }
}

STATIC_INLINE StgPtr
scavenge_small_bitmap (StgPtr p, nat size, StgWord bitmap)
{
    while (size > 0) {
	if ((bitmap & 1) == 0) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	p++;
	bitmap = bitmap >> 1;
	size--;
    }
    return p;
}

/* -----------------------------------------------------------------------------
   scavenge_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   AP_STACK_UPDs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */

static void
scavenge_stack(StgPtr p, StgPtr stack_end)
{
  const StgRetInfoTable* info;
  StgWord bitmap;
  nat size;

  /* 
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with an activation record. 
   */

  while (p < stack_end) {
    info  = get_ret_itbl((StgClosure *)p);
      
    switch (info->i.type) {
	
    case UPDATE_FRAME:
	// In SMP, we can get update frames that point to indirections
	// when two threads evaluate the same thunk.  We do attempt to
	// discover this situation in threadPaused(), but it's
	// possible that the following sequence occurs:
	//
	//        A             B
	//                  enter T
	//     enter T
	//     blackhole T
	//                  update T
	//     GC
	//
	// Now T is an indirection, and the update frame is already
	// marked on A's stack, so we won't traverse it again in
	// threadPaused().  We could traverse the whole stack again
	// before GC, but that seems like overkill.
	//
	// Scavenging this update frame as normal would be disastrous;
	// the updatee would end up pointing to the value.  So we turn
	// the indirection into an IND_PERM, so that evacuate will
	// copy the indirection into the old generation instead of
	// discarding it.
	if (get_itbl(((StgUpdateFrame *)p)->updatee)->type == IND) {
	    ((StgUpdateFrame *)p)->updatee->header.info = 
		(StgInfoTable *)&stg_IND_PERM_info;
	}
	((StgUpdateFrame *)p)->updatee 
	    = evacuate(((StgUpdateFrame *)p)->updatee);
	p += sizeofW(StgUpdateFrame);
	continue;

      // small bitmap (< 32 entries, or 64 on a 64-bit machine) 
    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
    case STOP_FRAME:
    case CATCH_FRAME:
    case RET_SMALL:
	bitmap = BITMAP_BITS(info->i.layout.bitmap);
	size   = BITMAP_SIZE(info->i.layout.bitmap);
	// NOTE: the payload starts immediately after the info-ptr, we
	// don't have an StgHeader in the same sense as a heap closure.
	p++;
	p = scavenge_small_bitmap(p, size, bitmap);

    follow_srt:
	if (major_gc) 
	    scavenge_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap);
	continue;

    case RET_BCO: {
	StgBCO *bco;
	nat size;

	p++;
	*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	bco = (StgBCO *)*p;
	p++;
	size = BCO_BITMAP_SIZE(bco);
	scavenge_large_bitmap(p, BCO_BITMAP(bco), size);
	p += size;
	continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine) 
    case RET_BIG:
    {
	nat size;

	size = GET_LARGE_BITMAP(&info->i)->size;
	p++;
	scavenge_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
	p += size;
	// and don't forget to follow the SRT 
	goto follow_srt;
    }

      // Dynamic bitmap: the mask is stored on the stack, and
      // there are a number of non-pointers followed by a number
      // of pointers above the bitmapped area.  (see StgMacros.h,
      // HEAP_CHK_GEN).
    case RET_DYN:
    {
	StgWord dyn;
	dyn = ((StgRetDyn *)p)->liveness;

	// traverse the bitmap first
	bitmap = RET_DYN_LIVENESS(dyn);
	p      = (P_)&((StgRetDyn *)p)->payload[0];
	size   = RET_DYN_BITMAP_SIZE;
	p = scavenge_small_bitmap(p, size, bitmap);

	// skip over the non-ptr words
	p += RET_DYN_NONPTRS(dyn) + RET_DYN_NONPTR_REGS_SIZE;
	
	// follow the ptr words
	for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    p++;
	}
	continue;
    }

    case RET_FUN:
    {
	StgRetFun *ret_fun = (StgRetFun *)p;
	StgFunInfoTable *fun_info;

	ret_fun->fun = evacuate(ret_fun->fun);
 	fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
	p = scavenge_arg_block(fun_info, ret_fun->payload);
	goto follow_srt;
    }

    default:
	barf("scavenge_stack: weird activation record found on stack: %d", (int)(info->i.type));
    }
  }		     
}

/*-----------------------------------------------------------------------------
  scavenge the large object list.

  evac_gen set by caller; similar games played with evac_gen as with
  scavenge() - see comment at the top of scavenge().  Most large
  objects are (repeatedly) mutable, so most of the time evac_gen will
  be zero.
  --------------------------------------------------------------------------- */

void
scavenge_large(step *stp)
{
  bdescr *bd;
  StgPtr p;

  bd = stp->new_large_objects;

  for (; bd != NULL; bd = stp->new_large_objects) {

    /* take this object *off* the large objects list and put it on
     * the scavenged large objects list.  This is so that we can
     * treat new_large_objects as a stack and push new objects on
     * the front when evacuating.
     */
    stp->new_large_objects = bd->link;
    dbl_link_onto(bd, &stp->scavenged_large_objects);

    // update the block count in this step.
    stp->n_scavenged_large_blocks += bd->blocks;

    p = bd->start;
    if (scavenge_one(p)) {
	if (stp->gen_no > 0) {
	    recordMutableGen((StgClosure *)p, stp->gen);
	}
    }
  }
}

