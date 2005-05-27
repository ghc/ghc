/* -----------------------------------------------------------------------------
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
#include "OSThreads.h"
#include "Storage.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "GCCompact.h"
#include "Schedule.h"
#include "Apply.h"

// Turn off inlining when debugging - it obfuscates things
#ifdef DEBUG
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

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

   We use a trick to identify the info pointer: when swapping pointers
   for threading, we set the low bit of the original pointer, with the
   result that all the pointers in the chain have their low bits set
   except for the info pointer.
   -------------------------------------------------------------------------- */

STATIC_INLINE void
thread( StgPtr p )
{
    StgPtr q = (StgPtr)*p;
    bdescr *bd;

    // It doesn't look like a closure at the moment, because the info
    // ptr is possibly threaded:
    // ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

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

STATIC_INLINE void
unthread( StgPtr p, StgPtr free )
{
    StgWord q = *p, r;
    
    while ((q & 1) != 0) {
	q -= 1;	// unset the low bit again
	r = *((StgPtr)q);
	*((StgPtr)q) = (StgWord)free;
	q = r;
    }
    *p = q;
}

STATIC_INLINE StgInfoTable *
get_threaded_info( StgPtr p )
{
    StgPtr q = (P_)GET_INFO((StgClosure *)p);

    while (((StgWord)q & 1) != 0) {
	q = (P_)*((StgPtr)((StgWord)q-1));
    }

    ASSERT(LOOKS_LIKE_INFO_PTR(q));
    return INFO_PTR_TO_STRUCT((StgInfoTable *)q);
}

// A word-aligned memmove will be faster for small objects than libc's or gcc's.
// Remember, the two regions *might* overlap, but: to <= from.
STATIC_INLINE void
move(StgPtr to, StgPtr from, nat size)
{
    for(; size > 0; --size) {
	*to++ = *from++;
    }
}

STATIC_INLINE nat
obj_sizeW( StgClosure *p, StgInfoTable *info )
{
    switch (info->type) {
    case THUNK_0_1:
    case THUNK_1_0:
	return sizeofW(StgThunk) + 1;
    case FUN_0_1:
    case CONSTR_0_1:
    case FUN_1_0:
    case CONSTR_1_0:
	return sizeofW(StgHeader) + 1;
    case THUNK_0_2:
    case THUNK_1_1:
    case THUNK_2_0:
	return sizeofW(StgThunk) + 2;
    case FUN_0_2:
    case CONSTR_0_2:
    case FUN_1_1:
    case CONSTR_1_1:
    case FUN_2_0:
    case CONSTR_2_0:
	return sizeofW(StgHeader) + 2;
    case THUNK_SELECTOR:
	return THUNK_SELECTOR_sizeW();
    case AP_STACK:
	return ap_stack_sizeW((StgAP_STACK *)p);
    case AP:
    case PAP:
	return pap_sizeW((StgPAP *)p);
    case ARR_WORDS:
	return arr_words_sizeW((StgArrWords *)p);
    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
	return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
    case TSO:
	return tso_sizeW((StgTSO *)p);
    case BCO:
	return bco_sizeW((StgBCO *)p);
    case TVAR_WAIT_QUEUE:
        return sizeofW(StgTVarWaitQueue);
    case TVAR:
        return sizeofW(StgTVar);
    case TREC_CHUNK:
        return sizeofW(StgTRecChunk);
    case TREC_HEADER:
        return sizeofW(StgTRecHeader);
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
	p = *IND_STATIC_LINK(p);
	continue;
      
    case THUNK_STATIC:
	p = *THUNK_STATIC_LINK(p);
	continue;
    case FUN_STATIC:
	p = *FUN_STATIC_LINK(p);
	continue;
    case CONSTR_STATIC:
	p = *STATIC_LINK(info,p);
	continue;
      
    default:
	barf("thread_static: strange closure %d", (int)(info->type));
    }

  }
}

STATIC_INLINE void
thread_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, nat size )
{
    nat i, b;
    StgWord bitmap;

    b = 0;
    bitmap = large_bitmap->bitmap[b];
    for (i = 0; i < size; ) {
	if ((bitmap & 1) == 0) {
	    thread(p);
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
thread_arg_block (StgFunInfoTable *fun_info, StgClosure **args)
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
	thread_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
	size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		thread(p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

static void
thread_stack(StgPtr p, StgPtr stack_end)
{
    const StgRetInfoTable* info;
    StgWord bitmap;
    nat size;
    
    // highly similar to scavenge_stack, but we do pointer threading here.
    
    while (p < stack_end) {

	// *p must be the info pointer of an activation
	// record.  All activation records have 'bitmap' style layout
	// info.
	//
	info  = get_ret_itbl((StgClosure *)p);
	
	switch (info->i.type) {
	    
	    // Dynamic bitmap: the mask is stored on the stack 
	case RET_DYN:
	{
	    StgWord dyn;
	    dyn = ((StgRetDyn *)p)->liveness;

	    // traverse the bitmap first
	    bitmap = RET_DYN_LIVENESS(dyn);
	    p      = (P_)&((StgRetDyn *)p)->payload[0];
	    size   = RET_DYN_BITMAP_SIZE;
	    while (size > 0) {
		if ((bitmap & 1) == 0) {
		    thread(p);
		}
		p++;
		bitmap = bitmap >> 1;
		size--;
	    }
	    
	    // skip over the non-ptr words
	    p += RET_DYN_NONPTRS(dyn) + RET_DYN_NONPTR_REGS_SIZE;
	    
	    // follow the ptr words
	    for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
		thread(p);
		p++;
	    }
	    continue;
	}
	    
	    // small bitmap (<= 32 entries, or 64 on a 64-bit machine) 
        case CATCH_RETRY_FRAME:
        case CATCH_STM_FRAME:
        case ATOMICALLY_FRAME:
	case UPDATE_FRAME:
	case STOP_FRAME:
	case CATCH_FRAME:
	case RET_SMALL:
	case RET_VEC_SMALL:
	    bitmap = BITMAP_BITS(info->i.layout.bitmap);
	    size   = BITMAP_SIZE(info->i.layout.bitmap);
	    p++;
	    // NOTE: the payload starts immediately after the info-ptr, we
	    // don't have an StgHeader in the same sense as a heap closure.
	    while (size > 0) {
		if ((bitmap & 1) == 0) {
		    thread(p);
		}
		p++;
		bitmap = bitmap >> 1;
		size--;
	    }
	    continue;

	case RET_BCO: {
	    StgBCO *bco;
	    nat size;
	    
	    p++;
	    bco = (StgBCO *)*p;
	    thread(p);
	    p++;
	    size = BCO_BITMAP_SIZE(bco);
	    thread_large_bitmap(p, BCO_BITMAP(bco), size);
	    p += size;
	    continue;
	}

	    // large bitmap (> 32 entries, or 64 on a 64-bit machine) 
	case RET_BIG:
	case RET_VEC_BIG:
	    p++;
	    size = GET_LARGE_BITMAP(&info->i)->size;
	    thread_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
	    p += size;
	    continue;

	case RET_FUN:
	{
	    StgRetFun *ret_fun = (StgRetFun *)p;
	    StgFunInfoTable *fun_info;
	    
	    fun_info = itbl_to_fun_itbl(
		get_threaded_info((StgPtr)ret_fun->fun));
	         // *before* threading it!
	    thread((StgPtr)&ret_fun->fun);
	    p = thread_arg_block(fun_info, ret_fun->payload);
	    continue;
	}

	default:
	    barf("thread_stack: weird activation record found on stack: %d", 
		 (int)(info->i.type));
	}
    }
}

STATIC_INLINE StgPtr
thread_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size)
{
    StgPtr p;
    StgWord bitmap;
    StgFunInfoTable *fun_info;

    fun_info = itbl_to_fun_itbl(get_threaded_info((StgPtr)fun));
    ASSERT(fun_info->i.type != PAP);

    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
	goto small_bitmap;
    case ARG_GEN_BIG:
	thread_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    case ARG_BCO:
	thread_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		thread(p);
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
thread_PAP (StgPAP *pap)
{
    StgPtr p;
    p = thread_PAP_payload(pap->fun, pap->payload, pap->n_args);
    thread((StgPtr)&pap->fun);
    return p;
}
    
STATIC_INLINE StgPtr
thread_AP (StgAP *ap)
{
    StgPtr p;
    p = thread_PAP_payload(ap->fun, ap->payload, ap->n_args);
    thread((StgPtr)&ap->fun);
    return p;
}    

STATIC_INLINE StgPtr
thread_AP_STACK (StgAP_STACK *ap)
{
    thread((StgPtr)&ap->fun);
    thread_stack((P_)ap->payload, (P_)ap->payload + ap->size);
    return (P_)ap + sizeofW(StgAP_STACK) + ap->size;
}

static StgPtr
thread_TSO (StgTSO *tso)
{
    thread((StgPtr)&tso->link);
    thread((StgPtr)&tso->global_link);

    if (   tso->why_blocked == BlockedOnMVar
	|| tso->why_blocked == BlockedOnBlackHole
	|| tso->why_blocked == BlockedOnException
#if defined(PAR)
	|| tso->why_blocked == BlockedOnGA
	|| tso->why_blocked == BlockedOnGA_NoSend
#endif
	) {
	thread((StgPtr)&tso->block_info.closure);
    }
    if ( tso->blocked_exceptions != NULL ) {
	thread((StgPtr)&tso->blocked_exceptions);
    }
    
    thread((StgPtr)&tso->trec);

    thread_stack(tso->sp, &(tso->stack[tso->stack_size]));
    return (StgPtr)tso + tso_sizeW(tso);
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
    case MUT_ARR_PTRS_FROZEN0:
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
	thread_TSO((StgTSO *)p);
	continue;

    case AP_STACK:
	thread_AP_STACK((StgAP_STACK *)p);
	continue;

    case PAP:
	thread_PAP((StgPAP *)p);
	continue;

    default:
      barf("update_fwd_large: unknown/strange object  %d", (int)(info->type));
    }
  }
}

STATIC_INLINE StgPtr
thread_obj (StgInfoTable *info, StgPtr p)
{
    switch (info->type) {
    case THUNK_0_1:
	return p + sizeofW(StgThunk) + 1;

    case FUN_0_1:
    case CONSTR_0_1:
	return p + sizeofW(StgHeader) + 1;
	
    case FUN_1_0:
    case CONSTR_1_0:
	thread((StgPtr)&((StgClosure *)p)->payload[0]);
	return p + sizeofW(StgHeader) + 1;
	
    case THUNK_1_0:
	thread((StgPtr)&((StgThunk *)p)->payload[0]);
	return p + sizeofW(StgThunk) + 1;
	
    case THUNK_0_2:
	return p + sizeofW(StgThunk) + 2;

    case FUN_0_2:
    case CONSTR_0_2:
	return p + sizeofW(StgHeader) + 2;
	
    case THUNK_1_1:
	thread((StgPtr)&((StgThunk *)p)->payload[0]);
	return p + sizeofW(StgThunk) + 2;

    case FUN_1_1:
    case CONSTR_1_1:
	thread((StgPtr)&((StgClosure *)p)->payload[0]);
	return p + sizeofW(StgHeader) + 2;
	
    case THUNK_2_0:
	thread((StgPtr)&((StgThunk *)p)->payload[0]);
	thread((StgPtr)&((StgThunk *)p)->payload[1]);
	return p + sizeofW(StgThunk) + 2;

    case FUN_2_0:
    case CONSTR_2_0:
	thread((StgPtr)&((StgClosure *)p)->payload[0]);
	thread((StgPtr)&((StgClosure *)p)->payload[1]);
	return p + sizeofW(StgHeader) + 2;
	
    case BCO: {
	StgBCO *bco = (StgBCO *)p;
	thread((StgPtr)&bco->instrs);
	thread((StgPtr)&bco->literals);
	thread((StgPtr)&bco->ptrs);
	thread((StgPtr)&bco->itbls);
	return p + bco_sizeW(bco);
    }

    case THUNK:
    {
	StgPtr end;
	
	end = (P_)((StgThunk *)p)->payload + 
	    info->layout.payload.ptrs;
	for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
	    thread(p);
	}
	return p + info->layout.payload.nptrs;
    }

    case FUN:
    case CONSTR:
    case FOREIGN:
    case STABLE_NAME:
    case IND_PERM:
    case MUT_VAR:
    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
    case SE_BLACKHOLE:
    case BLACKHOLE:
    {
	StgPtr end;
	
	end = (P_)((StgClosure *)p)->payload + 
	    info->layout.payload.ptrs;
	for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
	    thread(p);
	}
	return p + info->layout.payload.nptrs;
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
	return p + sizeofW(StgWeak);
    }
    
    case MVAR:
    { 
	StgMVar *mvar = (StgMVar *)p;
	thread((StgPtr)&mvar->head);
	thread((StgPtr)&mvar->tail);
	thread((StgPtr)&mvar->value);
	return p + sizeofW(StgMVar);
    }
    
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
	thread((StgPtr)&((StgInd *)p)->indirectee);
	return p + sizeofW(StgInd);

    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	thread((StgPtr)&s->selectee);
	return p + THUNK_SELECTOR_sizeW();
    }
    
    case AP_STACK:
	return thread_AP_STACK((StgAP_STACK *)p);
	
    case PAP:
	return thread_PAP((StgPAP *)p);

    case AP:
	return thread_AP((StgAP *)p);
	
    case ARR_WORDS:
	return p + arr_words_sizeW((StgArrWords *)p);
	
    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
	// follow everything 
    {
	StgPtr next;
	
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    thread(p);
	}
	return p;
    }
    
    case TSO:
	return thread_TSO((StgTSO *)p);
    
    case TVAR_WAIT_QUEUE:
    {
        StgTVarWaitQueue *wq = (StgTVarWaitQueue *)p;
	thread((StgPtr)&wq->waiting_tso);
	thread((StgPtr)&wq->next_queue_entry);
	thread((StgPtr)&wq->prev_queue_entry);
	return p + sizeofW(StgTVarWaitQueue);
    }
    
    case TVAR:
    {
        StgTVar *tvar = (StgTVar *)p;
	thread((StgPtr)&tvar->current_value);
	thread((StgPtr)&tvar->first_wait_queue_entry);
	thread((StgPtr)&tvar->last_update_by);
	return p + sizeofW(StgTVar);
    }
    
    case TREC_HEADER:
    {
        StgTRecHeader *trec = (StgTRecHeader *)p;
	thread((StgPtr)&trec->enclosing_trec);
	thread((StgPtr)&trec->current_chunk);
	return p + sizeofW(StgTRecHeader);
    }

    case TREC_CHUNK:
    {
        StgWord i;
        StgTRecChunk *tc = (StgTRecChunk *)p;
	TRecEntry *e = &(tc -> entries[0]);
	thread((StgPtr)&tc->prev_chunk);
	for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	  thread((StgPtr)&e->tvar);
	  thread((StgPtr)&e->expected_value);
	  thread((StgPtr)&e->new_value);
	}
	return p + sizeofW(StgTRecChunk);
    }

    default:
	barf("update_fwd: unknown/strange object  %d", (int)(info->type));
	return NULL;
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
	    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
	    info = get_itbl((StgClosure *)p);
	    p = thread_obj(info, p);
	}
    }
} 

static void
update_fwd_compact( bdescr *blocks )
{
    StgPtr p, q, free;
#if 0
    StgWord m;
#endif
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

	    p = thread_obj(info, p);

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
#if 0
    StgWord m;
#endif
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
	    ASSERT(LOOKS_LIKE_INFO_PTR(((StgClosure *)p)->header.info));
	    info = get_itbl((StgClosure *)p);
	    size = obj_sizeW((StgClosure *)p,info);

	    if (free != p) {
		move(free,p,size);
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

void
compact( void (*get_roots)(evac_fn) )
{
    nat g, s, blocks;
    step *stp;

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
	bdescr *bd;
	StgPtr p;
	for (bd = generations[g].mut_list; bd != NULL; bd = bd->link) {
	    for (p = bd->start; p < bd->free; p++) {
		thread(p);
	    }
	}
    }

    // the global thread list
    thread((StgPtr)&all_threads);

    // any threads resurrected during this GC
    thread((StgPtr)&resurrected_threads);

    // the main threads list
    {
	StgMainThread *m;
	for (m = main_threads; m != NULL; m = m->link) {
	    thread((StgPtr)&m->tso);
	}
    }

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
	    IF_DEBUG(gc, debugBelch("update_fwd:  %d.%d\n", stp->gen->no, stp->no););

	    update_fwd(stp->to_blocks);
	    update_fwd_large(stp->scavenged_large_objects);
	    if (g == RtsFlags.GcFlags.generations-1 && stp->blocks != NULL) {
		IF_DEBUG(gc, debugBelch("update_fwd:  %d.%d (compact)\n", stp->gen->no, stp->no););
		update_fwd_compact(stp->blocks);
	    }
	}
    }

    // 3. update backward ptrs
    stp = &oldest_gen->steps[0];
    if (stp->blocks != NULL) {
	blocks = update_bkwd_compact(stp);
	IF_DEBUG(gc, debugBelch("update_bkwd: %d.%d (compact, old: %d blocks, now %d blocks)\n", 
			     stp->gen->no, stp->no,
			     stp->n_blocks, blocks););
	stp->n_blocks = blocks;
    }
}
