/* -----------------------------------------------------------------------------
 * $Id: Sanity.c,v 1.5 1999/01/18 15:18:06 simonm Exp $
 *
 * Sanity checking code for the heap and stack.
 *
 * Used when debugging: check that the stack looks reasonable.
 *
 *    - All things that are supposed to be pointers look like pointers.
 *
 *    - Objects in text space are marked as static closures, those
 *	in the heap are dynamic.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef DEBUG

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "Sanity.h"

#define LOOKS_LIKE_PTR(r) \
  (IS_DATA_PTR(r) || ((IS_USER_PTR(r) && Bdescr((P_)r)->free != (void *)-1)))

/* -----------------------------------------------------------------------------
   Check stack sanity
   -------------------------------------------------------------------------- */

StgOffset checkStackClosure( StgClosure* c );

StgOffset checkStackObject( StgPtr sp );

void      checkStackChunk( StgPtr sp, StgPtr stack_end );

static StgOffset checkSmallBitmap(  StgPtr payload, StgNat32 bitmap );

static StgOffset checkLargeBitmap( StgPtr payload, 
				   StgLargeBitmap* large_bitmap );

void checkClosureShallow( StgClosure* p );

static StgOffset 
checkSmallBitmap( StgPtr payload, StgNat32 bitmap )
{
    StgOffset i;

    i = 0;
    for(; bitmap != 0; ++i, bitmap >>= 1 ) {
	if ((bitmap & 1) == 0) {
	    checkClosure(stgCast(StgClosure*,payload[i]));
	}
    }
    return i;
}


static StgOffset 
checkLargeBitmap( StgPtr payload, StgLargeBitmap* large_bitmap )
{
    StgNat32 bmp;
    StgOffset i;

    i = 0;
    for (bmp=0; bmp<large_bitmap->size; bmp++) {
	StgNat32 bitmap = large_bitmap->bitmap[bmp];
	for(; bitmap != 0; ++i, bitmap >>= 1 ) {
	    if ((bitmap & 1) == 0) {
		checkClosure(stgCast(StgClosure*,payload[i]));
	    }
	}
    }
    return i;
}

StgOffset 
checkStackClosure( StgClosure* c )
{    
    const StgInfoTable* info = get_itbl(c);

    /* All activation records have 'bitmap' style layout info. */
    switch (info->type) {
    case RET_DYN: /* Dynamic bitmap: the mask is stored on the stack */
	{
	    StgRetDyn* r = stgCast(StgRetDyn*,c);
	    return sizeofW(StgRetDyn) + 
	           checkSmallBitmap(r->payload,r->liveness);
	}
    case RET_BCO: /* small bitmap (<= 32 entries) */
    case RET_SMALL:
    case RET_VEC_SMALL:
    case UPDATE_FRAME:
    case CATCH_FRAME:
    case STOP_FRAME:
    case SEQ_FRAME:
	    return sizeofW(StgClosure) + 
	           checkSmallBitmap((StgPtr)c->payload,info->layout.bitmap);
    case RET_BIG: /* large bitmap (> 32 entries) */
    case RET_VEC_BIG:
	    return sizeofW(StgClosure) + 
	           checkLargeBitmap((StgPtr)c->payload,
				    info->layout.large_bitmap);
    case FUN:
    case FUN_STATIC: /* probably a slow-entry point return address: */
	    return 1;
    default:
       	    /* if none of the above, maybe it's a closure which looks a
       	     * little like an infotable
       	     */
	    checkClosureShallow(*stgCast(StgClosure**,c));
	    return 1;
	    /* barf("checkStackClosure: weird activation record found on stack (%p).",c); */
    }
}

/*
 * check that it looks like a valid closure - without checking its payload
 * used to avoid recursion between checking PAPs and checking stack
 * chunks.
 */
 
void 
checkClosureShallow( StgClosure* p )
{
    ASSERT(LOOKS_LIKE_GHC_INFO(p->header.info));

    /* Is it a static closure (i.e. in the data segment)? */
    if (LOOKS_LIKE_STATIC(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
	ASSERT(LOOKS_LIKE_PTR(p));
    }
}

/* check an individual stack object */
StgOffset 
checkStackObject( StgPtr sp )
{
    if (IS_ARG_TAG(*sp)) {
        /* Tagged words might be "stubbed" pointers, so there's no
	 * point checking to see whether they look like pointers or
	 * not (some of them will).
	 */
	return ARG_SIZE(*sp) + 1;
    } else if (LOOKS_LIKE_GHC_INFO(*stgCast(StgPtr*,sp))) {
        return checkStackClosure(stgCast(StgClosure*,sp));
    } else { /* must be an untagged closure pointer in the stack */
	checkClosureShallow(*stgCast(StgClosure**,sp));
	return 1;
    }
}

/* check sections of stack between update frames */
void 
checkStackChunk( StgPtr sp, StgPtr stack_end )
{
    StgPtr p;

    p = sp;
    while (p < stack_end) {
	p += checkStackObject( p );
    }
    ASSERT( p == stack_end );
}

StgOffset 
checkClosure( StgClosure* p )
{
    const StgInfoTable *info;

#ifndef INTERPRETER    
    ASSERT(LOOKS_LIKE_GHC_INFO(p->header.info));
#endif

    /* Is it a static closure (i.e. in the data segment)? */
    if (LOOKS_LIKE_STATIC(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
	ASSERT(LOOKS_LIKE_PTR(p));
    }

    info = get_itbl(p);
    switch (info->type) {
    case BCO:
	{
	    StgBCO* bco = stgCast(StgBCO*,p);
	    nat i;
	    for(i=0; i < bco->n_ptrs; ++i) {
		ASSERT(LOOKS_LIKE_PTR(bcoConstPtr(bco,i)));
	    }
	    return bco_sizeW(bco);
	}

    case MVAR:
      { 
	StgMVar *mvar = (StgMVar *)p;
	ASSERT(LOOKS_LIKE_PTR(mvar->head));
	ASSERT(LOOKS_LIKE_PTR(mvar->tail));
	ASSERT(LOOKS_LIKE_PTR(mvar->value));
	return sizeofW(StgMVar);
      }

    case FUN:
    case THUNK:
    case CONSTR:
    case IND_PERM:
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
    case CAF_UNENTERED:
    case CAF_ENTERED:
    case CAF_BLACKHOLE:
    case BLACKHOLE:
    case BLACKHOLE_BQ:
    case FOREIGN:
    case MUT_VAR:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
    case THUNK_STATIC:
    case FUN_STATIC:
    case IND_STATIC:
	{
	    nat i;
	    for (i = 0; i < info->layout.payload.ptrs; i++) {
		ASSERT(LOOKS_LIKE_PTR(payloadPtr(p,i)));
	    }
	    return sizeW_fromITBL(info);
	}

    case WEAK:
      /* deal with these specially - the info table isn't
       * representative of the actual layout.
       */
      { StgWeak *w = (StgWeak *)p;
	ASSERT(LOOKS_LIKE_PTR(w->key));
	ASSERT(LOOKS_LIKE_PTR(w->value));
	ASSERT(LOOKS_LIKE_PTR(w->finaliser));
	if (w->link) {
	  ASSERT(LOOKS_LIKE_PTR(w->link));
	}
	return sizeW_fromITBL(info);
      }

    case THUNK_SELECTOR:
	    ASSERT(LOOKS_LIKE_PTR(stgCast(StgSelector*,p)->selectee));
	    return sizeofW(StgHeader) + MIN_UPD_SIZE;

    case IND:
	{ 
  	    /* we don't expect to see any of these after GC
	     * but they might appear during execution
	     */
	    P_ q;
	    StgInd *ind = stgCast(StgInd*,p);
	    ASSERT(LOOKS_LIKE_PTR(ind->indirectee));
	    q = (P_)p + sizeofW(StgInd);
	    while (!*q) { q++; }; /* skip padding words (see GC.c: evacuate())*/
	    return q - (P_)p;
	}

    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
    case RET_BIG:
    case RET_VEC_BIG:
    case RET_DYN:
    case UPDATE_FRAME:
    case STOP_FRAME:
    case CATCH_FRAME:
    case SEQ_FRAME:
	    barf("checkClosure: stack frame");

    case AP_UPD: /* we can treat this as being the same as a PAP */
    case PAP:
	{ 
	    StgPAP *pap = stgCast(StgPAP*,p);
	    ASSERT(LOOKS_LIKE_PTR(pap->fun));
	    checkStackChunk((StgPtr)pap->payload, 
			    (StgPtr)pap->payload + pap->n_args
			    );
	    return pap_sizeW(pap);
	}

    case ARR_WORDS:
    case MUT_ARR_WORDS:
	    return arr_words_sizeW(stgCast(StgArrWords*,p));

    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
	{
	    StgMutArrPtrs* a = stgCast(StgMutArrPtrs*,p);
	    nat i;
	    for (i = 0; i < a->ptrs; i++) {
		ASSERT(LOOKS_LIKE_PTR(a->payload[i]));
	    }
	    return mut_arr_ptrs_sizeW(a);
	}

    case TSO:
        checkTSO((StgTSO *)p);
        return tso_sizeW((StgTSO *)p);

    case BLOCKED_FETCH:
    case FETCH_ME:
    case EVACUATED:
	    barf("checkClosure: unimplemented/strange closure type");
    default:
	    barf("checkClosure");
    }
#undef LOOKS_LIKE_PTR
}

/* -----------------------------------------------------------------------------
   Check Heap Sanity

   After garbage collection, the live heap is in a state where we can
   run through and check that all the pointers point to the right
   place.  This function starts at a given position and sanity-checks
   all the objects in the remainder of the chain.
   -------------------------------------------------------------------------- */

extern void 
checkHeap(bdescr *bd, StgPtr start)
{
    StgPtr p;

    if (start == NULL) {
      p = bd->start;
    } else {
      p = start;
    }

    while (bd != NULL) {
      while (p < bd->free) {
        nat size = checkClosure(stgCast(StgClosure*,p));
        /* This is the smallest size of closure that can live in the heap. */
        ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
	p += size;
	while (p < bd->free &&
	       *p && !LOOKS_LIKE_GHC_INFO(*p)) { p++; } /* skip over slop */
      }
      bd = bd->link;
      if (bd != NULL) {
	p = bd->start;
      }
    }
}

extern void
checkChain(bdescr *bd)
{
  while (bd != NULL) {
    checkClosure((StgClosure *)bd->start);
    bd = bd->link;
  }
}

/* check stack - making sure that update frames are linked correctly */
void 
checkStack(StgPtr sp, StgPtr stack_end, StgUpdateFrame* su )
{
    /* check everything down to the first update frame */
    checkStackChunk( sp, stgCast(StgPtr,su) );
    while ( stgCast(StgPtr,su) < stack_end) {
	sp = stgCast(StgPtr,su);
	switch (get_itbl(su)->type) {
	case UPDATE_FRAME:
		su = su->link;
		break;
	case SEQ_FRAME:
		su = stgCast(StgSeqFrame*,su)->link;
		break;
	case CATCH_FRAME:
		su = stgCast(StgCatchFrame*,su)->link;
		break;
	case STOP_FRAME:
      	        /* not quite: ASSERT(stgCast(StgPtr,su) == stack_end); */
		return;
	default:
		barf("checkStack: weird record found on update frame list.");
	}
	checkStackChunk( sp, stgCast(StgPtr,su) );
    }
    ASSERT(stgCast(StgPtr,su) == stack_end);
}

extern void
checkTSO(StgTSO *tso)
{
    StgPtr sp = tso->sp;
    StgPtr stack = tso->stack;
    StgUpdateFrame* su = tso->su;
    StgOffset stack_size = tso->stack_size;
    StgPtr stack_end = stack + stack_size;

    if (tso->whatNext == ThreadComplete ||  tso->whatNext == ThreadKilled) {
      /* The garbage collector doesn't bother following any pointers
       * from dead threads, so don't check sanity here.  
       */
      return;
    }

    ASSERT(stack <= sp && sp < stack_end);
    ASSERT(sp <= stgCast(StgPtr,su));

    checkStack(sp, stack_end, su);
}

/* -----------------------------------------------------------------------------
   Check Blackhole Sanity

   Test whether an object is already on the update list.
   It isn't necessarily an rts error if it is - it might be a programming
   error.

   Future versions might be able to test for a blackhole without traversing
   the update frame list.

   -------------------------------------------------------------------------- */
rtsBool isBlackhole( StgTSO* tso, StgClosure* p )
{
  StgUpdateFrame* su = tso->su;
  do {
    switch (get_itbl(su)->type) {
    case UPDATE_FRAME:
      if (su->updatee == p) {
	return rtsTrue;
      } else {
	su = su->link;
      }
      break;
    case SEQ_FRAME:
      su = stgCast(StgSeqFrame*,su)->link;
      break;
    case CATCH_FRAME:
      su = stgCast(StgCatchFrame*,su)->link;
      break;
    case STOP_FRAME:
      return rtsFalse;
    default:
      barf("isBlackhole: weird record found on update frame list.");
    }
  } while (1);
}

#endif /* DEBUG */
