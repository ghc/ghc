/* -----------------------------------------------------------------------------
 * $Id: Sanity.c,v 1.29 2001/07/24 05:04:59 ken Exp $
 *
 * (c) The GHC Team, 1998-2001
 *
 * Sanity checking code for the heap and stack.
 *
 * Used when debugging: check that everything reasonable.
 *
 *    - All things that are supposed to be pointers look like pointers.
 *
 *    - Objects in text space are marked as static closures, those
 *	in the heap are dynamic.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef DEBUG                                                   /* whole file */

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "Sanity.h"
#include "MBlock.h"
#include "Storage.h"
#include "Schedule.h"
#include "StoragePriv.h"   // for END_OF_STATIC_LIST

/* -----------------------------------------------------------------------------
   A valid pointer is either:

     - a pointer to a static closure, or
     - a pointer into the heap, and
       - the block is not free
       - either: - the object is large, or 
                 - it is not after the free pointer in the block
       - the contents of the pointer is not 0xaaaaaaaa

   -------------------------------------------------------------------------- */

#define LOOKS_LIKE_PTR(r)			\
  ({ bdescr *bd = Bdescr((P_)r);		\
     LOOKS_LIKE_STATIC_CLOSURE(r) ||		\
	(HEAP_ALLOCED(r) 			\
	 && bd != (void *)-1			\
	 && ((StgWord)(*(StgPtr)r)!=0xaaaaaaaa)	\
	);					\
   })

// NOT always true, but can be useful for spotting bugs: (generally
// true after GC, but not for things just allocated using allocate(),
// for example):
//	    (bd->flags & BF_LARGE || bd->free > (P_)r) 

/* -----------------------------------------------------------------------------
   Forward decls.
   -------------------------------------------------------------------------- */

static StgOffset checkStackClosure   ( StgClosure* c );
static StgOffset checkStackObject    ( StgPtr sp );
static StgOffset checkSmallBitmap    ( StgPtr payload, StgWord bitmap );
static StgOffset checkLargeBitmap    ( StgPtr payload, StgLargeBitmap* );
static void      checkClosureShallow ( StgClosure* p );

/* -----------------------------------------------------------------------------
   Check stack sanity
   -------------------------------------------------------------------------- */

static StgOffset 
checkSmallBitmap( StgPtr payload, StgWord bitmap )
{
    StgOffset i;

    i = 0;
    for(; bitmap != 0; ++i, bitmap >>= 1 ) {
	if ((bitmap & 1) == 0) {
	    checkClosure((StgClosure *)payload[i]);
	}
    }
    return i;
}

static StgOffset 
checkLargeBitmap( StgPtr payload, StgLargeBitmap* large_bitmap )
{
    StgWord bmp;
    StgOffset i;

    i = 0;
    for (bmp=0; bmp<large_bitmap->size; bmp++) {
	StgWord bitmap = large_bitmap->bitmap[bmp];
	for(; bitmap != 0; ++i, bitmap >>= 1 ) {
	    if ((bitmap & 1) == 0) {
		checkClosure((StgClosure *)payload[i]);
	    }
	}
    }
    return i;
}

static StgOffset 
checkStackClosure( StgClosure* c )
{    
    const StgInfoTable* info = get_itbl(c);

    /* All activation records have 'bitmap' style layout info. */
    switch (info->type) {
    case RET_DYN: /* Dynamic bitmap: the mask is stored on the stack */
	{
	    StgRetDyn* r = (StgRetDyn *)c;
	    return sizeofW(StgRetDyn) + 
	           checkSmallBitmap(r->payload,r->liveness);
	}
    case RET_BCO: /* small bitmap (<= 32 entries) */
    case RET_SMALL:
    case RET_VEC_SMALL:
            return 1 + checkSmallBitmap((StgPtr)c + 1,info->layout.bitmap);
      
    case UPDATE_FRAME:
      ASSERT(LOOKS_LIKE_PTR(((StgUpdateFrame*)c)->updatee));
    case CATCH_FRAME:
    case SEQ_FRAME:
      /* check that the link field points to another stack frame */
      ASSERT(get_itbl(((StgFrame*)c)->link)->type == UPDATE_FRAME ||
	     get_itbl(((StgFrame*)c)->link)->type == CATCH_FRAME ||
	     get_itbl(((StgFrame*)c)->link)->type == STOP_FRAME ||
	     get_itbl(((StgFrame*)c)->link)->type == SEQ_FRAME);
      /* fall through */
    case STOP_FRAME:
#if defined(GRAN)
            return 2 +
#else
            return 1 +
#endif
	               checkSmallBitmap((StgPtr)c + 1,info->layout.bitmap);
    case RET_BIG: /* large bitmap (> 32 entries) */
    case RET_VEC_BIG:
	    return 1 + checkLargeBitmap((StgPtr)c + 1,info->layout.large_bitmap);
    case FUN:
    case FUN_STATIC: /* probably a slow-entry point return address: */
#if 0 && defined(GRAN)
            return 2;
#else
            return 1;
#endif
    default:
       	    /* if none of the above, maybe it's a closure which looks a
       	     * little like an infotable
       	     */
	    checkClosureShallow(*(StgClosure **)c);
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
    ASSERT(p);
    ASSERT(LOOKS_LIKE_GHC_INFO(GET_INFO(p))
           || IS_HUGS_CONSTR_INFO(GET_INFO(p)));

    /* Is it a static closure (i.e. in the data segment)? */
    if (LOOKS_LIKE_STATIC(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
	ASSERT(LOOKS_LIKE_PTR(p));
    }
}

// check an individual stack object
StgOffset 
checkStackObject( StgPtr sp )
{
    if (IS_ARG_TAG(*sp)) {
        // Tagged words might be "stubbed" pointers, so there's no
	// point checking to see whether they look like pointers or
	// not (some of them will).
	return ARG_SIZE(*sp) + 1;
    } else if (LOOKS_LIKE_GHC_INFO(*(StgPtr *)sp)) {
        return checkStackClosure((StgClosure *)sp);
    } else { // must be an untagged closure pointer in the stack
	checkClosureShallow(*(StgClosure **)sp);
	return 1;
    }
}

// check sections of stack between update frames
void 
checkStackChunk( StgPtr sp, StgPtr stack_end )
{
    StgPtr p;

    p = sp;
    while (p < stack_end) {
	p += checkStackObject( p );
    }
    // ASSERT( p == stack_end ); -- HWL
}

StgOffset 
checkClosure( StgClosure* p )
{
    const StgInfoTable *info;

    ASSERT(LOOKS_LIKE_GHC_INFO(p->header.info));

    /* Is it a static closure (i.e. in the data segment)? */
    if (LOOKS_LIKE_STATIC(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
	ASSERT(LOOKS_LIKE_PTR(p));
    }

    info = get_itbl(p);
    switch (info->type) {

    case MVAR:
      { 
	StgMVar *mvar = (StgMVar *)p;
	ASSERT(LOOKS_LIKE_PTR(mvar->head));
	ASSERT(LOOKS_LIKE_PTR(mvar->tail));
	ASSERT(LOOKS_LIKE_PTR(mvar->value));
#if 0
#if defined(PAR)
	checkBQ((StgBlockingQueueElement *)mvar->head, p);
#else
	checkBQ(mvar->head, p);
#endif
#endif
	return sizeofW(StgMVar);
      }

    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
      {
	nat i;
	for (i = 0; i < info->layout.payload.ptrs; i++) {
	  ASSERT(LOOKS_LIKE_PTR(p->payload[i]));
	}
	return stg_max(sizeW_fromITBL(info), sizeofW(StgHeader) + MIN_UPD_SIZE);
      }

    case BLACKHOLE_BQ:
      checkBQ(((StgBlockingQueue *)p)->blocking_queue, p);
      /* fall through to basic ptr check */
    case FUN:
    case FUN_1_0:
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
    case IND_PERM:
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
#ifdef TICKY_TICKY
    case SE_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
#endif
    case BLACKHOLE:
    case CAF_BLACKHOLE:
    case FOREIGN:
    case BCO:
    case STABLE_NAME:
    case MUT_VAR:
    case MUT_CONS:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
    case THUNK_STATIC:
    case FUN_STATIC:
	{
	    nat i;
	    for (i = 0; i < info->layout.payload.ptrs; i++) {
		ASSERT(LOOKS_LIKE_PTR(p->payload[i]));
	    }
	    return sizeW_fromITBL(info);
	}

    case IND_STATIC: /* (1, 0) closure */
      ASSERT(LOOKS_LIKE_PTR(((StgIndStatic*)p)->indirectee));
      return sizeW_fromITBL(info);

    case WEAK:
      /* deal with these specially - the info table isn't
       * representative of the actual layout.
       */
      { StgWeak *w = (StgWeak *)p;
	ASSERT(LOOKS_LIKE_PTR(w->key));
	ASSERT(LOOKS_LIKE_PTR(w->value));
	ASSERT(LOOKS_LIKE_PTR(w->finalizer));
	if (w->link) {
	  ASSERT(LOOKS_LIKE_PTR(w->link));
	}
	return sizeW_fromITBL(info);
      }

    case THUNK_SELECTOR:
	    ASSERT(LOOKS_LIKE_PTR(((StgSelector *)p)->selectee));
	    return sizeofW(StgHeader) + MIN_UPD_SIZE;

    case IND:
	{ 
  	    /* we don't expect to see any of these after GC
	     * but they might appear during execution
	     */
	    P_ q;
	    StgInd *ind = (StgInd *)p;
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
	    StgPAP *pap = (StgPAP *)p;
	    ASSERT(LOOKS_LIKE_PTR(pap->fun));
	    checkStackChunk((StgPtr)pap->payload, 
			    (StgPtr)pap->payload + pap->n_args
			    );
	    return pap_sizeW(pap);
	}

    case ARR_WORDS:
	    return arr_words_sizeW((StgArrWords *)p);

    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
	{
	    StgMutArrPtrs* a = (StgMutArrPtrs *)p;
	    nat i;
	    for (i = 0; i < a->ptrs; i++) {
		ASSERT(LOOKS_LIKE_PTR(a->payload[i]));
	    }
	    return mut_arr_ptrs_sizeW(a);
	}

    case TSO:
        checkTSO((StgTSO *)p);
        return tso_sizeW((StgTSO *)p);

#if defined(PAR)

    case BLOCKED_FETCH:
      ASSERT(LOOKS_LIKE_GA(&(((StgBlockedFetch *)p)->ga)));
      ASSERT(LOOKS_LIKE_PTR((((StgBlockedFetch *)p)->node)));
      return sizeofW(StgBlockedFetch);  // see size used in evacuate()

#ifdef DIST
    case REMOTE_REF:
      return sizeofW(StgFetchMe); 
#endif //DIST
      
    case FETCH_ME:
      ASSERT(LOOKS_LIKE_GA(((StgFetchMe *)p)->ga));
      return sizeofW(StgFetchMe);  // see size used in evacuate()

    case FETCH_ME_BQ:
      checkBQ(((StgFetchMeBlockingQueue *)p)->blocking_queue, (StgClosure *)p);
      return sizeofW(StgFetchMeBlockingQueue); // see size used in evacuate()

    case RBH:
      /* In an RBH the BQ may be empty (ie END_BQ_QUEUE) but not NULL */
      ASSERT(((StgRBH *)p)->blocking_queue!=NULL);
      if (((StgRBH *)p)->blocking_queue!=END_BQ_QUEUE)
	checkBQ(((StgRBH *)p)->blocking_queue, p);
      ASSERT(LOOKS_LIKE_GHC_INFO(REVERT_INFOPTR(get_itbl((StgClosure *)p))));
      return BLACKHOLE_sizeW();   // see size used in evacuate()
      // sizeW_fromITBL(REVERT_INFOPTR(get_itbl((StgClosure *)p)));

#endif
      
    case EVACUATED:
	    barf("checkClosure: found EVACUATED closure %d",
		 info->type);
    default:
	    barf("checkClosure (closure type %d)", info->type);
    }
}

#if defined(PAR)

#define PVM_PE_MASK    0xfffc0000
#define MAX_PVM_PES    MAX_PES
#define MAX_PVM_TIDS   MAX_PES
#define MAX_SLOTS      100000

rtsBool
looks_like_tid(StgInt tid)
{
  StgInt hi = (tid & PVM_PE_MASK) >> 18;
  StgInt lo = (tid & ~PVM_PE_MASK);
  rtsBool ok = (hi != 0) && (lo < MAX_PVM_TIDS) && (hi < MAX_PVM_TIDS);
  return ok;
}

rtsBool
looks_like_slot(StgInt slot)
{
  /* if tid is known better use looks_like_ga!! */
  rtsBool ok = slot<MAX_SLOTS;
  // This refers only to the no. of slots on the current PE
  // rtsBool ok = slot<=highest_slot();
  return ok; 
}

rtsBool
looks_like_ga(globalAddr *ga)
{
  rtsBool is_tid = looks_like_tid((ga)->payload.gc.gtid);
  rtsBool is_slot = ((ga)->payload.gc.gtid==mytid) ? 
                     (ga)->payload.gc.slot<=highest_slot() : 
                     (ga)->payload.gc.slot<MAX_SLOTS;
  rtsBool ok = is_tid && is_slot;
  return ok;
}

#endif


/* -----------------------------------------------------------------------------
   Check Heap Sanity

   After garbage collection, the live heap is in a state where we can
   run through and check that all the pointers point to the right
   place.  This function starts at a given position and sanity-checks
   all the objects in the remainder of the chain.
   -------------------------------------------------------------------------- */

void 
checkHeap(bdescr *bd)
{
    StgPtr p;

    for (; bd != NULL; bd = bd->link) {
	p = bd->start;
	while (p < bd->free) {
	    nat size = checkClosure((StgClosure *)p);
	    /* This is the smallest size of closure that can live in the heap */
	    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
	    p += size;
	    
	    /* skip over slop */
	    while (p < bd->free &&
		   (*p < 0x1000 || !LOOKS_LIKE_GHC_INFO((void*)*p))) { p++; } 
	}
    }
}

#if defined(PAR)
/* 
   Check heap between start and end. Used after unpacking graphs.
*/
void 
checkHeapChunk(StgPtr start, StgPtr end)
{
  extern globalAddr *LAGAlookup(StgClosure *addr);
  StgPtr p;
  nat size;

  for (p=start; p<end; p+=size) {
    ASSERT(LOOKS_LIKE_GHC_INFO((void*)*p));
    if (get_itbl((StgClosure*)p)->type == FETCH_ME &&
	*(p+1) == 0x0000eeee /* ie. unpack garbage (see SetGAandCommonUp) */) {
      /* if it's a FM created during unpack and commoned up, it's not global */
      ASSERT(LAGAlookup((StgClosure*)p)==NULL);
      size = sizeofW(StgFetchMe);
    } else if (get_itbl((StgClosure*)p)->type == IND) {
      *(p+2) = 0x0000ee11; /* mark slop in IND as garbage */
      size = MIN_UPD_SIZE;
    } else {
      size = checkClosure((StgClosure *)p);
      /* This is the smallest size of closure that can live in the heap. */
      ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    }
  }
}
#else /* !PAR */
void 
checkHeapChunk(StgPtr start, StgPtr end)
{
  StgPtr p;
  nat size;

  for (p=start; p<end; p+=size) {
    ASSERT(LOOKS_LIKE_GHC_INFO((void*)*p));
    size = checkClosure((StgClosure *)p);
    /* This is the smallest size of closure that can live in the heap. */
    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
  }
}
#endif

void
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
    checkStackChunk( sp, (StgPtr)su );
    while ( (StgPtr)su < stack_end) {
	sp = (StgPtr)su;
	switch (get_itbl(su)->type) {
	case UPDATE_FRAME:
		su = su->link;
		break;
	case SEQ_FRAME:
		su = ((StgSeqFrame *)su)->link;
		break;
	case CATCH_FRAME:
		su = ((StgCatchFrame *)su)->link;
		break;
	case STOP_FRAME:
      	        /* not quite: ASSERT((StgPtr)su == stack_end); */
		return;
	default:
		barf("checkStack: weird record found on update frame list.");
	}
	checkStackChunk( sp, (StgPtr)su );
    }
    ASSERT((StgPtr)su == stack_end);
}


void
checkTSO(StgTSO *tso)
{
    StgPtr sp = tso->sp;
    StgPtr stack = tso->stack;
    StgUpdateFrame* su = tso->su;
    StgOffset stack_size = tso->stack_size;
    StgPtr stack_end = stack + stack_size;

    if (tso->what_next == ThreadRelocated) {
      checkTSO(tso->link);
      return;
    }

    if (tso->what_next == ThreadComplete || tso->what_next == ThreadKilled) {
      /* The garbage collector doesn't bother following any pointers
       * from dead threads, so don't check sanity here.  
       */
      return;
    }

    ASSERT(stack <= sp && sp < stack_end);
    ASSERT(sp <= (StgPtr)su);

#if defined(PAR)
    ASSERT(tso->par.magic==TSO_MAGIC);

    switch (tso->why_blocked) {
    case BlockedOnGA: 
      checkClosureShallow(tso->block_info.closure);
      ASSERT(/* Can't be a FETCH_ME because *this* closure is on its BQ */
	     get_itbl(tso->block_info.closure)->type==FETCH_ME_BQ);
      break;
    case BlockedOnGA_NoSend: 
      checkClosureShallow(tso->block_info.closure);
      ASSERT(get_itbl(tso->block_info.closure)->type==FETCH_ME_BQ);
      break;
    case BlockedOnBlackHole: 
      checkClosureShallow(tso->block_info.closure);
      ASSERT(/* Can't be a BLACKHOLE because *this* closure is on its BQ */
	     get_itbl(tso->block_info.closure)->type==BLACKHOLE_BQ ||
             get_itbl(tso->block_info.closure)->type==RBH);
      break;
    case BlockedOnRead:
    case BlockedOnWrite:
    case BlockedOnDelay:
      /* isOnBQ(blocked_queue) */
      break;
    case BlockedOnException:
      /* isOnSomeBQ(tso) */
      ASSERT(get_itbl(tso->block_info.tso)->type==TSO);
      break;
    case BlockedOnMVar:
      ASSERT(get_itbl(tso->block_info.closure)->type==MVAR);
      break;
    default:
      /* 
	 Could check other values of why_blocked but I am more 
	 lazy than paranoid (bad combination) -- HWL 
      */
    }

    /* if the link field is non-nil it most point to one of these
       three closure types */
    ASSERT(tso->link == END_TSO_QUEUE ||
	   get_itbl(tso->link)->type == TSO ||
	   get_itbl(tso->link)->type == BLOCKED_FETCH ||
	   get_itbl(tso->link)->type == CONSTR);
#endif

    checkStack(sp, stack_end, su);
}

#if defined(GRAN)
void  
checkTSOsSanity(void) {
  nat i, tsos;
  StgTSO *tso;
  
  belch("Checking sanity of all runnable TSOs:");
  
  for (i=0, tsos=0; i<RtsFlags.GranFlags.proc; i++) {
    for (tso=run_queue_hds[i]; tso!=END_TSO_QUEUE; tso=tso->link) {
      fprintf(stderr, "TSO %p on PE %d ...", tso, i);
      checkTSO(tso); 
      fprintf(stderr, "OK, ");
      tsos++;
    }
  }
  
  belch(" checked %d TSOs on %d PEs; ok\n", tsos, RtsFlags.GranFlags.proc);
}


// still GRAN only

rtsBool
checkThreadQSanity (PEs proc, rtsBool check_TSO_too) 
{
  StgTSO *tso, *prev;

  /* the NIL value for TSOs is END_TSO_QUEUE; thus, finding NULL is an error */
  ASSERT(run_queue_hds[proc]!=NULL);
  ASSERT(run_queue_tls[proc]!=NULL);
  /* if either head or tail is NIL then the other one must be NIL, too */
  ASSERT(run_queue_hds[proc]!=END_TSO_QUEUE || run_queue_tls[proc]==END_TSO_QUEUE);
  ASSERT(run_queue_tls[proc]!=END_TSO_QUEUE || run_queue_hds[proc]==END_TSO_QUEUE);
  for (tso=run_queue_hds[proc], prev=END_TSO_QUEUE; 
       tso!=END_TSO_QUEUE;
       prev=tso, tso=tso->link) {
    ASSERT((prev!=END_TSO_QUEUE || tso==run_queue_hds[proc]) &&
	   (prev==END_TSO_QUEUE || prev->link==tso));
    if (check_TSO_too)
      checkTSO(tso);
  }
  ASSERT(prev==run_queue_tls[proc]);
}

rtsBool
checkThreadQsSanity (rtsBool check_TSO_too)
{
  PEs p;
  
  for (p=0; p<RtsFlags.GranFlags.proc; p++)
    checkThreadQSanity(p, check_TSO_too);
}
#endif /* GRAN */

/* 
   Check that all TSOs have been evacuated.
   Optionally also check the sanity of the TSOs.
*/
void
checkGlobalTSOList (rtsBool checkTSOs)
{
  extern  StgTSO *all_threads;
  StgTSO *tso;
  for (tso=all_threads; tso != END_TSO_QUEUE; tso = tso->global_link) {
      ASSERT(LOOKS_LIKE_PTR(tso));
      ASSERT(get_itbl(tso)->type == TSO);
      if (checkTSOs)
	  checkTSO(tso);
  }
}

/* -----------------------------------------------------------------------------
   Check mutable list sanity.
   -------------------------------------------------------------------------- */

void
checkMutableList( StgMutClosure *p, nat gen )
{
    bdescr *bd;

    for (; p != END_MUT_LIST; p = p->mut_link) {
	bd = Bdescr((P_)p);
	ASSERT(closure_MUTABLE(p));
	ASSERT(bd->gen_no == gen);
	ASSERT(LOOKS_LIKE_PTR(p->mut_link));
    }
}

void
checkMutOnceList( StgMutClosure *p, nat gen )
{
    bdescr *bd;
    StgInfoTable *info;

    for (; p != END_MUT_LIST; p = p->mut_link) {
	bd = Bdescr((P_)p);
	info = get_itbl(p);

	ASSERT(!closure_MUTABLE(p));
	ASSERT(ip_STATIC(info) || bd->gen_no == gen);
	ASSERT(LOOKS_LIKE_PTR(p->mut_link));

	switch (info->type) {
	case IND_STATIC:
	case IND_OLDGEN:
	case IND_OLDGEN_PERM:
	case MUT_CONS:
	    break;
	default:
	    barf("checkMutOnceList: strange closure %p (%s)", 
		 p, info_type((StgClosure *)p));
	}
    }
}

/* -----------------------------------------------------------------------------
   Check Blackhole Sanity

   Test whether an object is already on the update list.
   It isn't necessarily an rts error if it is - it might be a programming
   error.

   Future versions might be able to test for a blackhole without traversing
   the update frame list.

   -------------------------------------------------------------------------- */
rtsBool 
isBlackhole( StgTSO* tso, StgClosure* p )
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
      su = ((StgSeqFrame *)su)->link;
      break;
    case CATCH_FRAME:
      su = ((StgCatchFrame *)su)->link;
      break;
    case STOP_FRAME:
      return rtsFalse;
    default:
      barf("isBlackhole: weird record found on update frame list.");
    }
  } while (1);
}

/*
  Check the static objects list.
*/
void
checkStaticObjects ( StgClosure* static_objects )
{
  StgClosure *p = static_objects;
  StgInfoTable *info;

  while (p != END_OF_STATIC_LIST) {
    checkClosure(p);
    info = get_itbl(p);
    switch (info->type) {
    case IND_STATIC:
      { 
	StgClosure *indirectee = ((StgIndStatic *)p)->indirectee;

	ASSERT(LOOKS_LIKE_PTR(indirectee));
	ASSERT(LOOKS_LIKE_GHC_INFO(indirectee->header.info));
	p = IND_STATIC_LINK((StgClosure *)p);
	break;
      }

    case THUNK_STATIC:
      p = THUNK_STATIC_LINK((StgClosure *)p);
      break;

    case FUN_STATIC:
      p = FUN_STATIC_LINK((StgClosure *)p);
      break;

    case CONSTR_STATIC:
      p = STATIC_LINK(info,(StgClosure *)p);
      break;

    default:
      barf("checkStaticObjetcs: strange closure %p (%s)", 
	   p, info_type(p));
    }
  }
}

/* 
   Check the sanity of a blocking queue starting at bqe with closure being
   the closure holding the blocking queue.
   Note that in GUM we can have several different closure types in a 
   blocking queue 
*/
#if defined(PAR)
void
checkBQ (StgBlockingQueueElement *bqe, StgClosure *closure) 
{
  rtsBool end = rtsFalse;
  StgInfoTable *info = get_itbl(closure);

  ASSERT(info->type == BLACKHOLE_BQ || info->type == MVAR
	 || info->type == FETCH_ME_BQ || info->type == RBH);

  do {
    switch (get_itbl(bqe)->type) {
    case BLOCKED_FETCH:
    case TSO:
      checkClosure((StgClosure *)bqe);
      bqe = bqe->link;
      end = (bqe==END_BQ_QUEUE);
      break;
    
    case CONSTR:
      checkClosure((StgClosure *)bqe);
      end = rtsTrue;
      break;

    default:
      barf("checkBQ: strange closure %d in blocking queue for closure %p (%s)\n", 
	   get_itbl(bqe)->type, closure, info_type(closure));
    }
  } while (!end);
}
#elif defined(GRAN)
void
checkBQ (StgTSO *bqe, StgClosure *closure) 
{  
  rtsBool end = rtsFalse;
  StgInfoTable *info = get_itbl(closure);

  ASSERT(info->type == BLACKHOLE_BQ || info->type == MVAR);

  do {
    switch (get_itbl(bqe)->type) {
    case BLOCKED_FETCH:
    case TSO:
      checkClosure((StgClosure *)bqe);
      bqe = bqe->link;
      end = (bqe==END_BQ_QUEUE);
      break;
    
    default:
      barf("checkBQ: strange closure %d in blocking queue for closure %p (%s)\n", 
	   get_itbl(bqe)->type, closure, info_type(closure));
    }
  } while (!end);
}
#else
void
checkBQ (StgTSO *bqe, StgClosure *closure) 
{  
  rtsBool end = rtsFalse;
  StgInfoTable *info = get_itbl(closure);

  ASSERT(info->type == BLACKHOLE_BQ || info->type == MVAR);

  do {
    switch (get_itbl(bqe)->type) {
    case TSO:
      checkClosure((StgClosure *)bqe);
      bqe = bqe->link;
      end = (bqe==END_TSO_QUEUE);
      break;

    default:
      barf("checkBQ: strange closure %d in blocking queue for closure %p\n", 
	   get_itbl(bqe)->type, closure, info->type);
    }
  } while (!end);
}
    
#endif
    


/*
  This routine checks the sanity of the LAGA and GALA tables. They are 
  implemented as lists through one hash table, LAtoGALAtable, because entries 
  in both tables have the same structure:
   - the LAGA table maps local addresses to global addresses; it starts
     with liveIndirections
   - the GALA table maps global addresses to local addresses; it starts 
     with liveRemoteGAs
*/

#if defined(PAR)
#include "Hash.h"

/* hidden in parallel/Global.c; only accessed for testing here */
extern GALA *liveIndirections;
extern GALA *liveRemoteGAs;
extern HashTable *LAtoGALAtable;

void
checkLAGAtable(rtsBool check_closures)
{
  GALA *gala, *gala0;
  nat n=0, m=0; // debugging

  for (gala = liveIndirections; gala != NULL; gala = gala->next) {
    n++;
    gala0 = lookupHashTable(LAtoGALAtable, (StgWord) gala->la);
    ASSERT(!gala->preferred || gala == gala0);
    ASSERT(LOOKS_LIKE_GHC_INFO(((StgClosure *)gala->la)->header.info));
    ASSERT(gala->next!=gala); // detect direct loops
    if ( check_closures ) {
      checkClosure((StgClosure *)gala->la);
    }
  }

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    m++;
    gala0 = lookupHashTable(LAtoGALAtable, (StgWord) gala->la);
    ASSERT(!gala->preferred || gala == gala0);
    ASSERT(LOOKS_LIKE_GHC_INFO(((StgClosure *)gala->la)->header.info));
    ASSERT(gala->next!=gala); // detect direct loops
    /*
    if ( check_closures ) {
      checkClosure((StgClosure *)gala->la);
    }
    */
  }
}
#endif

#endif /* DEBUG */
