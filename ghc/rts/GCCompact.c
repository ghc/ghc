/* -----------------------------------------------------------------------------
 * $Id: GCCompact.c,v 1.10 2001/10/19 09:41:11 sewardj Exp $
 *
 * (c) The GHC Team 2001
 *
 * Compacting garbage collector
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "GCCompact.h"
#include "Schedule.h"
#include "StablePriv.h"

/* -----------------------------------------------------------------------------
   Threading / unthreading pointers.

   The basic idea here is to chain together all the fields pointing at
   a particular object, with the root of the chain in the object's
   info table field.  The original contents of the info pointer goes
   at the end of the chain.

   Adding a new field to the chain is a matter of swapping the
   contents of the field with the contents of the object's info table
   field.

   To unthread the chain, we walk down it updating all the fields on
   the chain with the new location of the object.  We stop when we
   reach the info pointer at the end.

   We use a trick to identify the info pointer, because the
   LOOKS_LIKE_GHC_INFO() macro involves a function call and can be
   expensive.  The trick is that when swapping pointers for threading,
   we set the low bit of the original pointer, with the result that
   all the pointers in the chain have their low bits set except for
   the info pointer.
   -------------------------------------------------------------------------- */

static inline void
thread( StgPtr p )
{
    StgPtr q = (StgPtr)*p;
    bdescr *bd;

    ASSERT(!LOOKS_LIKE_GHC_INFO(q));
    if (HEAP_ALLOCED(q)) {
	bd = Bdescr(q); 
	// a handy way to discover whether the ptr is into the
	// compacted area of the old gen, is that the EVACUATED flag
	// is zero (it's non-zero for all the other areas of live
	// memory).
	if ((bd->flags & BF_EVACUATED) == 0) {
	    *p = (StgWord)*q;
	    *q = (StgWord)p + 1;	// set the low bit
	}
    }
}

static inline void
unthread( StgPtr p, StgPtr free )
{
    StgPtr q = (StgPtr)*p, r;
    
    while (((StgWord)q & 1) != 0) {
	(StgWord)q -= 1;	// unset the low bit again
	r = (StgPtr)*q;
	*q = (StgWord)free;
	q = r;
    }
    *p = (StgWord)q;
}

static inline StgInfoTable *
get_threaded_info( StgPtr p )
{
    StgPtr q = (P_)GET_INFO((StgClosure *)p);

    while (((StgWord)q & 1) != 0) {
	q = (P_)*((StgPtr)((StgWord)q-1));
    }
    return INFO_PTR_TO_STRUCT((StgInfoTable *)q);
}

// A word-aligned memmove will be faster for small objects than libc's or gcc's.
// Remember, the two regions *might* overlap, but: to <= from.
static inline void
move(StgPtr to, StgPtr from, nat size)
{
    for(; size > 0; --size) {
	*to++ = *from++;
    }
}

static inline nat
obj_sizeW( StgClosure *p, StgInfoTable *info )
{
    switch (info->type) {
    case FUN_0_1:
    case CONSTR_0_1:
    case FUN_1_0:
    case CONSTR_1_0:
	return sizeofW(StgHeader) + 1;
    case THUNK_0_1:
    case THUNK_0_2:
    case FUN_0_2:
    case CONSTR_0_2:
    case THUNK_1_0:
    case THUNK_1_1:
    case FUN_1_1:
    case CONSTR_1_1:
    case THUNK_2_0:
    case FUN_2_0:
    case CONSTR_2_0:
	return sizeofW(StgHeader) + 2; // MIN_UPD_SIZE
    case THUNK_SELECTOR:
	return THUNK_SELECTOR_sizeW();
    case AP_UPD:
    case PAP:
	return pap_sizeW((StgPAP *)p);
    case ARR_WORDS:
	return arr_words_sizeW((StgArrWords *)p);
    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
	return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
    case TSO:
	return tso_sizeW((StgTSO *)p);
    default:
	return sizeW_fromITBL(info);
    }
}

static void
thread_static( StgClosure* p )
{
  const StgInfoTable *info;

  // keep going until we've threaded all the objects on the linked
  // list... 
  while (p != END_OF_STATIC_LIST) {

    info = get_itbl(p);
    switch (info->type) {
      
    case IND_STATIC:
	thread((StgPtr)&((StgInd *)p)->indirectee);
	p = IND_STATIC_LINK(p);
	continue;
      
    case THUNK_STATIC:
	p = THUNK_STATIC_LINK(p);
	continue;
    case FUN_STATIC:
	p = FUN_STATIC_LINK(p);
	continue;
    case CONSTR_STATIC:
	p = STATIC_LINK(info,p);
	continue;
      
    default:
	barf("thread_static: strange closure %d", (int)(info->type));
    }

  }
}

static void
thread_stack(StgPtr p, StgPtr stack_end)
{
    StgPtr q;
    const StgInfoTable* info;
    StgWord bitmap;
    
    // highly similar to scavenge_stack, but we do pointer threading here.
    
    while (p < stack_end) {
	q = (StgPtr)*p;

	// If we've got a tag, skip over that many words on the stack 
	if ( IS_ARG_TAG((W_)q) ) {
	    p += ARG_SIZE(q);
	    p++; continue;
	}
	
	// Is q a pointer to a closure?
	if ( !LOOKS_LIKE_GHC_INFO(q) ) {
	    thread(p);
	    p++; 
	    continue;
	}
	
	// Otherwise, q must be the info pointer of an activation
	// record.  All activation records have 'bitmap' style layout
	// info.
	//
	info  = get_itbl((StgClosure *)p);
	
	switch (info->type) {
	    
	    // Dynamic bitmap: the mask is stored on the stack 
	case RET_DYN:
	    bitmap = ((StgRetDyn *)p)->liveness;
	    p      = (P_)&((StgRetDyn *)p)->payload[0];
	    goto small_bitmap;
	    
	    // probably a slow-entry point return address: 
	case FUN:
	case FUN_STATIC:
	    p++;
	    continue;
	    
	    // small bitmap (<= 32 entries, or 64 on a 64-bit machine) 
	case UPDATE_FRAME:
	case STOP_FRAME:
	case CATCH_FRAME:
	case SEQ_FRAME:
	case RET_BCO:
	case RET_SMALL:
	case RET_VEC_SMALL:
	    bitmap = info->layout.bitmap;
	    p++;
	    // this assumes that the payload starts immediately after the info-ptr 
	small_bitmap:
	    while (bitmap != 0) {
		if ((bitmap & 1) == 0) {
		    thread(p);
		}
		p++;
		bitmap = bitmap >> 1;
	    }
	    continue;

	    // large bitmap (> 32 entries, or 64 on a 64-bit machine) 
	case RET_BIG:
	case RET_VEC_BIG:
	{
	    StgPtr q;
	    StgLargeBitmap *large_bitmap;
	    nat i;

	    large_bitmap = info->layout.large_bitmap;
	    p++;

	    for (i=0; i<large_bitmap->size; i++) {
		bitmap = large_bitmap->bitmap[i];
		q = p + BITS_IN(W_);
		while (bitmap != 0) {
		    if ((bitmap & 1) == 0) {
			thread(p);
		    }
		    p++;
		    bitmap = bitmap >> 1;
		}
		if (i+1 < large_bitmap->size) {
		    while (p < q) {
			thread(p);
			p++;
		    }
		}
	    }
	    continue;
	}

	default:
	    barf("thread_stack: weird activation record found on stack: %d", 
		 (int)(info->type));
	}
    }
}

static void
update_fwd_large( bdescr *bd )
{
  StgPtr p;
  const StgInfoTable* info;

  for (; bd != NULL; bd = bd->link) {

    p = bd->start;
    info  = get_itbl((StgClosure *)p);

    switch (info->type) {

    case ARR_WORDS:
      // nothing to follow 
      continue;

    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
      // follow everything 
      {
	StgPtr next;

	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    thread(p);
	}
	continue;
      }

    case TSO:
    {
	StgTSO *tso = (StgTSO *)p;
	thread_stack(tso->sp, &(tso->stack[tso->stack_size]));
	thread((StgPtr)&tso->link);
	thread((StgPtr)&tso->global_link);
	continue;
    }

    case AP_UPD:
    case PAP:
      { 
	StgPAP* pap = (StgPAP *)p;
	thread((StgPtr)&pap->fun);
	thread_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
	continue;
      }

    default:
      barf("update_fwd_large: unknown/strange object  %d", (int)(info->type));
    }
  }
}

static void
update_fwd( bdescr *blocks )
{
    StgPtr p;
    bdescr *bd;
    StgInfoTable *info;

    bd = blocks;

#if defined(PAR)
    barf("update_fwd: ToDo");
#endif

    // cycle through all the blocks in the step
    for (; bd != NULL; bd = bd->link) {
	p = bd->start;

	// linearly scan the objects in this block
	while (p < bd->free) {

	    info = get_itbl((StgClosure *)p);

	    ASSERT(p && (LOOKS_LIKE_GHC_INFO(info)
			 || IS_HUGS_CONSTR_INFO(info)));

	    switch (info->type) {
	    case FUN_0_1:
	    case CONSTR_0_1:
		p += sizeofW(StgHeader) + 1;
		break;

	    case FUN_1_0:
	    case CONSTR_1_0:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		p += sizeofW(StgHeader) + 1;
		break;

	    case THUNK_1_0:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		p += sizeofW(StgHeader) + 2; // MIN_UPD_SIZE
		break;

	    case THUNK_0_1: // MIN_UPD_SIZE
	    case THUNK_0_2:
	    case FUN_0_2:
	    case CONSTR_0_2:
		p += sizeofW(StgHeader) + 2;
		break;

	    case THUNK_1_1:
	    case FUN_1_1:
	    case CONSTR_1_1:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		p += sizeofW(StgHeader) + 2;
		break;

	    case THUNK_2_0:
	    case FUN_2_0:
	    case CONSTR_2_0:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		thread((StgPtr)&((StgClosure *)p)->payload[1]);
		p += sizeofW(StgHeader) + 2;
		break;

	    case FUN:
	    case THUNK:
	    case CONSTR:
	    case FOREIGN:
	    case STABLE_NAME:
	    case BCO:
	    case IND_PERM:
	    case MUT_VAR:
	    case MUT_CONS:
	    case CAF_BLACKHOLE:
	    case SE_CAF_BLACKHOLE:
	    case SE_BLACKHOLE:
	    case BLACKHOLE:
	    case BLACKHOLE_BQ:
	    {
		StgPtr end;
		
		end = (P_)((StgClosure *)p)->payload + 
		    info->layout.payload.ptrs;
		for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
		    thread(p);
		}
		p += info->layout.payload.nptrs;
		break;
	    }

	    // the info table for a weak ptr lies about the number of ptrs
	    // (because we have special GC routines for them, but we
	    // want to use the standard evacuate code).  So we have to
	    // special case here.
	    case WEAK:
	    {
		StgWeak *w = (StgWeak *)p;
		thread((StgPtr)&w->key);
		thread((StgPtr)&w->value);
		thread((StgPtr)&w->finalizer);
		if (w->link != NULL) {
		    thread((StgPtr)&w->link);
		}
		p += sizeofW(StgWeak);
		break;
	    }

	    // again, the info table for MVar isn't suitable here (it includes
	    // the mut_link field as a pointer, and we don't want to
	    // thread it).
	    case MVAR:
	    { 
		StgMVar *mvar = (StgMVar *)p;
		thread((StgPtr)&mvar->head);
		thread((StgPtr)&mvar->tail);
		thread((StgPtr)&mvar->value);
		p += sizeofW(StgMVar);
		break;
	    }

	    case IND_OLDGEN:
	    case IND_OLDGEN_PERM:
		thread((StgPtr)&((StgIndOldGen *)p)->indirectee);
		p += sizeofW(StgIndOldGen);
		break;

	    case THUNK_SELECTOR:
	    { 
		StgSelector *s = (StgSelector *)p;
		thread((StgPtr)&s->selectee);
		p += THUNK_SELECTOR_sizeW();
		break;
	    }

	    case AP_UPD: // same as PAPs 
	    case PAP:
	    { 
		StgPAP* pap = (StgPAP *)p;
		
		thread((P_)&pap->fun);
		thread_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
		p += pap_sizeW(pap);
		break;
	    }
      
	    case ARR_WORDS:
		p += arr_words_sizeW((StgArrWords *)p);
		break;

	    case MUT_ARR_PTRS:
	    case MUT_ARR_PTRS_FROZEN:
		// follow everything 
	    {
		StgPtr next;
		
		next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
		for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		    thread(p);
		}
		break;
	    }

	    case TSO:
	    { 
		StgTSO *tso = (StgTSO *)p;
		thread_stack(tso->sp, &(tso->stack[tso->stack_size]));
		thread((StgPtr)&tso->link);
		thread((StgPtr)&tso->global_link);
		p += tso_sizeW(tso);
		break;
	    }

	    default:
		barf("update_fwd: unknown/strange object  %d", (int)(info->type));
	    }
	}
    }
} 

static void
update_fwd_compact( bdescr *blocks )
{
    StgPtr p, q, free;
    StgWord m;
    bdescr *bd, *free_bd;
    StgInfoTable *info;
    nat size;

    bd = blocks;
    free_bd = blocks;
    free = free_bd->start;

#if defined(PAR)
    barf("update_fwd: ToDo");
#endif

    // cycle through all the blocks in the step
    for (; bd != NULL; bd = bd->link) {
	p = bd->start;

	while (p < bd->free ) {

	    while ( p < bd->free && !is_marked(p,bd) ) {
		p++;
	    }
	    if (p >= bd->free) {
		break;
	    }

#if 0
    next:
	m = * ((StgPtr)bd->u.bitmap + ((p - bd->start) / (BITS_IN(StgWord))));
	m >>= ((p - bd->start) & (BITS_IN(StgWord) - 1));

	while ( p < bd->free ) {

	    if ((m & 1) == 0) {
		m >>= 1;
		p++;
		if (((StgWord)p & (sizeof(W_) * BITS_IN(StgWord))) == 0) {
		    goto next;
		} else {
		    continue;
		}
	    }
#endif

	    // Problem: we need to know the destination for this cell
	    // in order to unthread its info pointer.  But we can't
	    // know the destination without the size, because we may
	    // spill into the next block.  So we have to run down the 
	    // threaded list and get the info ptr first.
	    info = get_threaded_info(p);

	    q = p;
	    ASSERT(p && (LOOKS_LIKE_GHC_INFO(info)
			 || IS_HUGS_CONSTR_INFO(info)));

	    switch (info->type) {
	    case FUN_0_1:
	    case CONSTR_0_1:
		p += sizeofW(StgHeader) + 1;
		break;

	    case FUN_1_0:
	    case CONSTR_1_0:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		p += sizeofW(StgHeader) + 1;
		break;

	    case THUNK_1_0:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		p += sizeofW(StgHeader) + 2; // MIN_UPD_SIZE
		break;

	    case THUNK_0_1: // MIN_UPD_SIZE
	    case THUNK_0_2:
	    case FUN_0_2:
	    case CONSTR_0_2:
		p += sizeofW(StgHeader) + 2;
		break;

	    case THUNK_1_1:
	    case FUN_1_1:
	    case CONSTR_1_1:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		p += sizeofW(StgHeader) + 2;
		break;

	    case THUNK_2_0:
	    case FUN_2_0:
	    case CONSTR_2_0:
		thread((StgPtr)&((StgClosure *)p)->payload[0]);
		thread((StgPtr)&((StgClosure *)p)->payload[1]);
		p += sizeofW(StgHeader) + 2;
		break;

	    case FUN:
	    case THUNK:
	    case CONSTR:
	    case FOREIGN:
	    case STABLE_NAME:
	    case BCO:
	    case IND_PERM:
	    case MUT_VAR:
	    case MUT_CONS:
	    case CAF_BLACKHOLE:
	    case SE_CAF_BLACKHOLE:
	    case SE_BLACKHOLE:
	    case BLACKHOLE:
	    case BLACKHOLE_BQ:
	    {
		StgPtr end;
		
		end = (P_)((StgClosure *)p)->payload + 
		    info->layout.payload.ptrs;
		for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
		    thread(p);
		}
		p += info->layout.payload.nptrs;
		break;
	    }

	    case WEAK:
	    {
		StgWeak *w = (StgWeak *)p;
		thread((StgPtr)&w->key);
		thread((StgPtr)&w->value);
		thread((StgPtr)&w->finalizer);
		if (w->link != NULL) {
		    thread((StgPtr)&w->link);
		}
		p += sizeofW(StgWeak);
		break;
	    }

	    case MVAR:
	    { 
		StgMVar *mvar = (StgMVar *)p;
		thread((StgPtr)&mvar->head);
		thread((StgPtr)&mvar->tail);
		thread((StgPtr)&mvar->value);
		p += sizeofW(StgMVar);
		break;
	    }

	    case IND_OLDGEN:
	    case IND_OLDGEN_PERM:
		thread((StgPtr)&((StgIndOldGen *)p)->indirectee);
		p += sizeofW(StgIndOldGen);
		break;

	    case THUNK_SELECTOR:
	    { 
		StgSelector *s = (StgSelector *)p;
		thread((StgPtr)&s->selectee);
		p += THUNK_SELECTOR_sizeW();
		break;
	    }

	    case AP_UPD: // same as PAPs 
	    case PAP:
	    { 
		StgPAP* pap = (StgPAP *)p;
		
		thread((P_)&pap->fun);
		thread_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
		p += pap_sizeW(pap);
		break;
	    }
      
	    case ARR_WORDS:
		p += arr_words_sizeW((StgArrWords *)p);
		break;

	    case MUT_ARR_PTRS:
	    case MUT_ARR_PTRS_FROZEN:
		// follow everything 
	    {
		StgPtr next;
		
		next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
		for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		    thread(p);
		}
		break;
	    }

	    case TSO:
	    { 
		StgTSO *tso = (StgTSO *)p;
		thread_stack(tso->sp, &(tso->stack[tso->stack_size]));
		thread((StgPtr)&tso->link);
		thread((StgPtr)&tso->global_link);
		p += tso_sizeW(tso);
		break;
	    }

	    default:
		barf("update_fwd: unknown/strange object  %d", (int)(info->type));
	    }

	    size = p - q;
	    if (free + size > free_bd->start + BLOCK_SIZE_W) {
		// unset the next bit in the bitmap to indicate that
		// this object needs to be pushed into the next
		// block.  This saves us having to run down the
		// threaded info pointer list twice during the next pass.
		unmark(q+1,bd);
		free_bd = free_bd->link;
		free = free_bd->start;
	    } else {
		ASSERT(is_marked(q+1,bd));
	    }

	    unthread(q,free);
	    free += size;
#if 0
	    goto next;
#endif
	}
    }
}

static nat
update_bkwd_compact( step *stp )
{
    StgPtr p, free;
    StgWord m;
    bdescr *bd, *free_bd;
    StgInfoTable *info;
    nat size, free_blocks;

    bd = free_bd = stp->blocks;
    free = free_bd->start;
    free_blocks = 1;

#if defined(PAR)
    barf("update_bkwd: ToDo");
#endif

    // cycle through all the blocks in the step
    for (; bd != NULL; bd = bd->link) {
	p = bd->start;

	while (p < bd->free ) {

	    while ( p < bd->free && !is_marked(p,bd) ) {
		p++;
	    }
	    if (p >= bd->free) {
		break;
	    }

#if 0
    next:
	m = * ((StgPtr)bd->u.bitmap + ((p - bd->start) / (BITS_IN(StgWord))));
	m >>= ((p - bd->start) & (BITS_IN(StgWord) - 1));

	while ( p < bd->free ) {

	    if ((m & 1) == 0) {
		m >>= 1;
		p++;
		if (((StgWord)p & (sizeof(W_) * BITS_IN(StgWord))) == 0) {
		    goto next;
		} else {
		    continue;
		}
	    }
#endif

	    if (!is_marked(p+1,bd)) {
		// don't forget to update the free ptr in the block desc.
		free_bd->free = free;
		free_bd = free_bd->link;
		free = free_bd->start;
		free_blocks++;
	    }

	    unthread(p,free);
	    info = get_itbl((StgClosure *)p);
	    size = obj_sizeW((StgClosure *)p,info);

	    ASSERT(p && (LOOKS_LIKE_GHC_INFO(info)
			 || IS_HUGS_CONSTR_INFO(info)));

	    if (free != p) {
		move(free,p,size);
	    }

	    // Rebuild the mutable list for the old generation.
	    if (ip_MUTABLE(info)) {
		recordMutable((StgMutClosure *)free);
	    }

	    // relocate TSOs
	    if (info->type == TSO) {
		move_TSO((StgTSO *)p, (StgTSO *)free);
	    }

	    free += size;
	    p += size;
#if 0
	    goto next;
#endif
	}
    }

    // free the remaining blocks and count what's left.
    free_bd->free = free;
    if (free_bd->link != NULL) {
	freeChain(free_bd->link);
	free_bd->link = NULL;
    }
    stp->n_blocks = free_blocks;

    return free_blocks;
}

static void
thread_mut_once_list( generation *g )
{
    StgMutClosure *p, *next;

    for (p = g->mut_once_list; p != END_MUT_LIST; p = next) {
	next = p->mut_link;
	thread((StgPtr)&p->mut_link);
    }
    
    thread((StgPtr)&g->mut_once_list);
}

void
compact( void (*get_roots)(evac_fn) )
{
    nat g, s, blocks;
    step *stp;
    extern StgWeak *old_weak_ptr_list; // tmp

    // 1. thread the roots
    get_roots((evac_fn)thread);

    // the weak pointer lists...
    if (weak_ptr_list != NULL) {
	thread((StgPtr)&weak_ptr_list);
    }
    if (old_weak_ptr_list != NULL) {
	thread((StgPtr)&old_weak_ptr_list); // tmp
    }

    // mutable lists
    for (g = 1; g < RtsFlags.GcFlags.generations; g++) {
	thread((StgPtr)&generations[g].mut_list);
	thread_mut_once_list(&generations[g]);
    }

    // the global thread list
    thread((StgPtr)&all_threads);

    // the static objects
    thread_static(scavenged_static_objects);

    // the stable pointer table
    threadStablePtrTable((evac_fn)thread);

    // the CAF list (used by GHCi)
    markCAFs((evac_fn)thread);

    // 2. update forward ptrs
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	for (s = 0; s < generations[g].n_steps; s++) {
	    stp = &generations[g].steps[s];
	    IF_DEBUG(gc, fprintf(stderr,"update_fwd:  %d.%d\n", stp->gen->no, stp->no););

	    update_fwd(stp->to_blocks);
	    update_fwd_large(stp->scavenged_large_objects);
	    if (g == RtsFlags.GcFlags.generations-1 && stp->blocks != NULL) {
		IF_DEBUG(gc, fprintf(stderr,"update_fwd:  %d.%d (compact)\n", stp->gen->no, stp->no););
		update_fwd_compact(stp->blocks);
	    }
	}
    }

    // 3. update backward ptrs
    stp = &oldest_gen->steps[0];
    if (stp->blocks != NULL) {
	blocks = update_bkwd_compact(stp);
	IF_DEBUG(gc, fprintf(stderr,"update_bkwd: %d.%d (compact, old: %d blocks, now %d blocks)\n", 
			     stp->gen->no, stp->no,
			     stp->n_blocks, blocks););
	stp->n_blocks = blocks;
    }
}
