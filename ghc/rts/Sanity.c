/* -----------------------------------------------------------------------------
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

#include "PosixSource.h"
#include "Rts.h"

#ifdef DEBUG                                                   /* whole file */

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "Sanity.h"
#include "MBlock.h"
#include "Storage.h"
#include "Schedule.h"
#include "Apply.h"

/* -----------------------------------------------------------------------------
   Forward decls.
   -------------------------------------------------------------------------- */

static void      checkSmallBitmap    ( StgPtr payload, StgWord bitmap, nat );
static void      checkLargeBitmap    ( StgPtr payload, StgLargeBitmap*, nat );
static void      checkClosureShallow ( StgClosure * );

/* -----------------------------------------------------------------------------
   Check stack sanity
   -------------------------------------------------------------------------- */

static void
checkSmallBitmap( StgPtr payload, StgWord bitmap, nat size )
{
    StgPtr p;
    nat i;

    p = payload;
    for(i = 0; i < size; i++, bitmap >>= 1 ) {
	if ((bitmap & 1) == 0) {
	    checkClosureShallow((StgClosure *)payload[i]);
	}
    }
}

static void
checkLargeBitmap( StgPtr payload, StgLargeBitmap* large_bitmap, nat size )
{
    StgWord bmp;
    nat i, j;

    i = 0;
    for (bmp=0; i < size; bmp++) {
	StgWord bitmap = large_bitmap->bitmap[bmp];
	j = 0;
	for(; i < size && j < BITS_IN(W_); j++, i++, bitmap >>= 1 ) {
	    if ((bitmap & 1) == 0) {
		checkClosureShallow((StgClosure *)payload[i]);
	    }
	}
    }
}

/*
 * check that it looks like a valid closure - without checking its payload
 * used to avoid recursion between checking PAPs and checking stack
 * chunks.
 */
 
static void 
checkClosureShallow( StgClosure* p )
{
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

    /* Is it a static closure? */
    if (!HEAP_ALLOCED(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
    }
}

// check an individual stack object
StgOffset 
checkStackFrame( StgPtr c )
{
    nat size;
    const StgRetInfoTable* info;

    info = get_ret_itbl((StgClosure *)c);

    /* All activation records have 'bitmap' style layout info. */
    switch (info->i.type) {
    case RET_DYN: /* Dynamic bitmap: the mask is stored on the stack */
    {
	StgWord dyn;
	StgPtr p;
	StgRetDyn* r;
	
	r = (StgRetDyn *)c;
	dyn = r->liveness;
	
	p = (P_)(r->payload);
	checkSmallBitmap(p,RET_DYN_LIVENESS(r->liveness),RET_DYN_BITMAP_SIZE);
	p += RET_DYN_BITMAP_SIZE + RET_DYN_NONPTR_REGS_SIZE;

	// skip over the non-pointers
	p += RET_DYN_NONPTRS(dyn);
	
	// follow the ptr words
	for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
	    checkClosureShallow((StgClosure *)*p);
	    p++;
	}
	
	return sizeofW(StgRetDyn) + RET_DYN_BITMAP_SIZE +
	    RET_DYN_NONPTR_REGS_SIZE +
	    RET_DYN_NONPTRS(dyn) + RET_DYN_PTRS(dyn);
    }

    case UPDATE_FRAME:
      ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgUpdateFrame*)c)->updatee));
    case ATOMICALLY_FRAME:
    case CATCH_RETRY_FRAME:
    case CATCH_STM_FRAME:
    case CATCH_FRAME:
      // small bitmap cases (<= 32 entries)
    case STOP_FRAME:
    case RET_SMALL:
    case RET_VEC_SMALL:
	size = BITMAP_SIZE(info->i.layout.bitmap);
	checkSmallBitmap((StgPtr)c + 1, 
			 BITMAP_BITS(info->i.layout.bitmap), size);
	return 1 + size;

    case RET_BCO: {
	StgBCO *bco;
	nat size;
	bco = (StgBCO *)*(c+1);
	size = BCO_BITMAP_SIZE(bco);
	checkLargeBitmap((StgPtr)c + 2, BCO_BITMAP(bco), size);
	return 2 + size;
    }

    case RET_BIG: // large bitmap (> 32 entries)
    case RET_VEC_BIG:
	size = GET_LARGE_BITMAP(&info->i)->size;
	checkLargeBitmap((StgPtr)c + 1, GET_LARGE_BITMAP(&info->i), size);
	return 1 + size;

    case RET_FUN:
    {
	StgFunInfoTable *fun_info;
	StgRetFun *ret_fun;

	ret_fun = (StgRetFun *)c;
	fun_info = get_fun_itbl(ret_fun->fun);
	size = ret_fun->size;
	switch (fun_info->f.fun_type) {
	case ARG_GEN:
	    checkSmallBitmap((StgPtr)ret_fun->payload, 
			     BITMAP_BITS(fun_info->f.b.bitmap), size);
	    break;
	case ARG_GEN_BIG:
	    checkLargeBitmap((StgPtr)ret_fun->payload,
			     GET_FUN_LARGE_BITMAP(fun_info), size);
	    break;
	default:
	    checkSmallBitmap((StgPtr)ret_fun->payload,
			     BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
			     size);
	    break;
	}
	return sizeofW(StgRetFun) + size;
    }

    default:
	barf("checkStackFrame: weird activation record found on stack (%p %d).",c,info->i.type);
    }
}

// check sections of stack between update frames
void 
checkStackChunk( StgPtr sp, StgPtr stack_end )
{
    StgPtr p;

    p = sp;
    while (p < stack_end) {
	p += checkStackFrame( p );
    }
    // ASSERT( p == stack_end ); -- HWL
}

static void
checkPAP (StgClosure *fun, StgClosure** payload, StgWord n_args)
{ 
    StgClosure *p;
    StgFunInfoTable *fun_info;
    
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(fun));
    fun_info = get_fun_itbl(fun);
    
    p = (StgClosure *)payload;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	checkSmallBitmap( (StgPtr)payload, 
			  BITMAP_BITS(fun_info->f.b.bitmap), n_args );
	break;
    case ARG_GEN_BIG:
	checkLargeBitmap( (StgPtr)payload, 
			  GET_FUN_LARGE_BITMAP(fun_info), 
			  n_args );
	break;
    case ARG_BCO:
	checkLargeBitmap( (StgPtr)payload, 
			  BCO_BITMAP(fun), 
			  n_args );
	break;
    default:
	checkSmallBitmap( (StgPtr)payload, 
			  BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
			  n_args );
	break;
    }
}


StgOffset 
checkClosure( StgClosure* p )
{
    const StgInfoTable *info;

    ASSERT(LOOKS_LIKE_INFO_PTR(p->header.info));

    /* Is it a static closure (i.e. in the data segment)? */
    if (!HEAP_ALLOCED(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
    }

    info = get_itbl(p);
    switch (info->type) {

    case MVAR:
      { 
	StgMVar *mvar = (StgMVar *)p;
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(mvar->head));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(mvar->tail));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(mvar->value));
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
	  ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgThunk *)p)->payload[i]));
	}
	return stg_max(thunk_sizeW_fromITBL(info), sizeofW(StgHeader)+MIN_UPD_SIZE);
      }

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
    case STABLE_NAME:
    case MUT_VAR:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
    case THUNK_STATIC:
    case FUN_STATIC:
	{
	    nat i;
	    for (i = 0; i < info->layout.payload.ptrs; i++) {
		ASSERT(LOOKS_LIKE_CLOSURE_PTR(p->payload[i]));
	    }
	    return sizeW_fromITBL(info);
	}

    case BCO: {
	StgBCO *bco = (StgBCO *)p;
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->instrs));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->literals));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->ptrs));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->itbls));
	return bco_sizeW(bco);
    }

    case IND_STATIC: /* (1, 0) closure */
      ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgIndStatic*)p)->indirectee));
      return sizeW_fromITBL(info);

    case WEAK:
      /* deal with these specially - the info table isn't
       * representative of the actual layout.
       */
      { StgWeak *w = (StgWeak *)p;
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(w->key));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(w->value));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(w->finalizer));
	if (w->link) {
	  ASSERT(LOOKS_LIKE_CLOSURE_PTR(w->link));
	}
	return sizeW_fromITBL(info);
      }

    case THUNK_SELECTOR:
	    ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgSelector *)p)->selectee));
	    return THUNK_SELECTOR_sizeW();

    case IND:
	{ 
  	    /* we don't expect to see any of these after GC
	     * but they might appear during execution
	     */
	    StgInd *ind = (StgInd *)p;
	    ASSERT(LOOKS_LIKE_CLOSURE_PTR(ind->indirectee));
	    return sizeofW(StgHeader) + MIN_UPD_SIZE;
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
    case ATOMICALLY_FRAME:
    case CATCH_RETRY_FRAME:
    case CATCH_STM_FRAME:
	    barf("checkClosure: stack frame");

    case AP:
    {
	StgAP* ap = (StgAP *)p;
	checkPAP (ap->fun, ap->payload, ap->n_args);
	return ap_sizeW(ap);
    }

    case PAP:
    {
	StgPAP* pap = (StgPAP *)p;
	checkPAP (pap->fun, pap->payload, pap->n_args);
	return pap_sizeW(pap);
    }

    case AP_STACK:
    { 
	StgAP_STACK *ap = (StgAP_STACK *)p;
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(ap->fun));
	checkStackChunk((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	return ap_stack_sizeW(ap);
    }

    case ARR_WORDS:
	    return arr_words_sizeW((StgArrWords *)p);

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
	{
	    StgMutArrPtrs* a = (StgMutArrPtrs *)p;
	    nat i;
	    for (i = 0; i < a->ptrs; i++) {
		ASSERT(LOOKS_LIKE_CLOSURE_PTR(a->payload[i]));
	    }
	    return mut_arr_ptrs_sizeW(a);
	}

    case TSO:
        checkTSO((StgTSO *)p);
        return tso_sizeW((StgTSO *)p);

#if defined(PAR)

    case BLOCKED_FETCH:
      ASSERT(LOOKS_LIKE_GA(&(((StgBlockedFetch *)p)->ga)));
      ASSERT(LOOKS_LIKE_CLOSURE_PTR((((StgBlockedFetch *)p)->node)));
      return sizeofW(StgBlockedFetch);  // see size used in evacuate()

#ifdef DIST
    case REMOTE_REF:
      return sizeofW(StgFetchMe); 
#endif /*DIST */
      
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
      ASSERT(LOOKS_LIKE_INFO_PTR(REVERT_INFOPTR(get_itbl((StgClosure *)p))));
      return BLACKHOLE_sizeW();   // see size used in evacuate()
      // sizeW_fromITBL(REVERT_INFOPTR(get_itbl((StgClosure *)p)));

#endif

    case TVAR_WAIT_QUEUE:
      {
        StgTVarWaitQueue *wq = (StgTVarWaitQueue *)p;
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(wq->next_queue_entry));
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(wq->prev_queue_entry));
        return sizeofW(StgTVarWaitQueue);
      }

    case TVAR:
      {
        StgTVar *tv = (StgTVar *)p;
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(tv->current_value));
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(tv->first_wait_queue_entry));
        return sizeofW(StgTVar);
      }

    case TREC_CHUNK:
      {
        nat i;
        StgTRecChunk *tc = (StgTRecChunk *)p;
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(tc->prev_chunk));
        for (i = 0; i < tc -> next_entry_idx; i ++) {
          ASSERT(LOOKS_LIKE_CLOSURE_PTR(tc->entries[i].tvar));
          ASSERT(LOOKS_LIKE_CLOSURE_PTR(tc->entries[i].expected_value));
          ASSERT(LOOKS_LIKE_CLOSURE_PTR(tc->entries[i].new_value));
        }
        return sizeofW(StgTRecChunk);
      }

    case TREC_HEADER:
      {
        StgTRecHeader *trec = (StgTRecHeader *)p;
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(trec -> enclosing_trec));
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(trec -> current_chunk));
        return sizeofW(StgTRecHeader);
      }
      
      
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

#if defined(SMP)
    // heap sanity checking doesn't work with SMP, because we can't
    // zero the slop (see Updates.h).
    return;
#endif

    for (; bd != NULL; bd = bd->link) {
	p = bd->start;
	while (p < bd->free) {
	    nat size = checkClosure((StgClosure *)p);
	    /* This is the smallest size of closure that can live in the heap */
	    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
	    p += size;
	    
	    /* skip over slop */
	    while (p < bd->free &&
		   (*p < 0x1000 || !LOOKS_LIKE_INFO_PTR((void*)*p))) { p++; } 
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
    ASSERT(LOOKS_LIKE_INFO_PTR((void*)*p));
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
    ASSERT(LOOKS_LIKE_INFO_PTR((void*)*p));
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

void
checkTSO(StgTSO *tso)
{
    StgPtr sp = tso->sp;
    StgPtr stack = tso->stack;
    StgOffset stack_size = tso->stack_size;
    StgPtr stack_end = stack + stack_size;

    if (tso->what_next == ThreadRelocated) {
      checkTSO(tso->link);
      return;
    }

    if (tso->what_next == ThreadKilled) {
      /* The garbage collector doesn't bother following any pointers
       * from dead threads, so don't check sanity here.  
       */
      return;
    }

    ASSERT(stack <= sp && sp < stack_end);

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
      ASSERT(get_itbl(tso->block_info.closure)->type==BLACKHOLE ||
             get_itbl(tso->block_info.closure)->type==RBH);
      break;
    case BlockedOnRead:
    case BlockedOnWrite:
    case BlockedOnDelay:
#if defined(mingw32_HOST_OS)
    case BlockedOnDoProc:
#endif
      /* isOnBQ(blocked_queue) */
      break;
    case BlockedOnException:
      /* isOnSomeBQ(tso) */
      ASSERT(get_itbl(tso->block_info.tso)->type==TSO);
      break;
    case BlockedOnMVar:
      ASSERT(get_itbl(tso->block_info.closure)->type==MVAR);
      break;
    case BlockedOnSTM:
      ASSERT(tso->block_info.closure == END_TSO_QUEUE);
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

    checkStackChunk(sp, stack_end);
}

#if defined(GRAN)
void  
checkTSOsSanity(void) {
  nat i, tsos;
  StgTSO *tso;
  
  debugBelch("Checking sanity of all runnable TSOs:");
  
  for (i=0, tsos=0; i<RtsFlags.GranFlags.proc; i++) {
    for (tso=run_queue_hds[i]; tso!=END_TSO_QUEUE; tso=tso->link) {
      debugBelch("TSO %p on PE %d ...", tso, i);
      checkTSO(tso); 
      debugBelch("OK, ");
      tsos++;
    }
  }
  
  debugBelch(" checked %d TSOs on %d PEs; ok\n", tsos, RtsFlags.GranFlags.proc);
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
      ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso));
      ASSERT(get_itbl(tso)->type == TSO);
      if (checkTSOs)
	  checkTSO(tso);
  }
}

/* -----------------------------------------------------------------------------
   Check mutable list sanity.
   -------------------------------------------------------------------------- */

void
checkMutableList( bdescr *mut_bd, nat gen )
{
    bdescr *bd;
    StgPtr q;
    StgClosure *p;

    for (bd = mut_bd; bd != NULL; bd = bd->link) {
	for (q = bd->start; q < bd->free; q++) {
	    p = (StgClosure *)*q;
	    ASSERT(!HEAP_ALLOCED(p) || Bdescr((P_)p)->gen_no == gen);
	}
    }
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

	ASSERT(LOOKS_LIKE_CLOSURE_PTR(indirectee));
	ASSERT(LOOKS_LIKE_INFO_PTR(indirectee->header.info));
	p = *IND_STATIC_LINK((StgClosure *)p);
	break;
      }

    case THUNK_STATIC:
      p = *THUNK_STATIC_LINK((StgClosure *)p);
      break;

    case FUN_STATIC:
      p = *FUN_STATIC_LINK((StgClosure *)p);
      break;

    case CONSTR_STATIC:
      p = *STATIC_LINK(info,(StgClosure *)p);
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

  ASSERT(info->type == MVAR || info->type == FETCH_ME_BQ || info->type == RBH);

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

  ASSERT(info->type == MVAR);

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
    ASSERT(LOOKS_LIKE_INFO_PTR(((StgClosure *)gala->la)->header.info));
    ASSERT(gala->next!=gala); // detect direct loops
    if ( check_closures ) {
      checkClosure((StgClosure *)gala->la);
    }
  }

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    m++;
    gala0 = lookupHashTable(LAtoGALAtable, (StgWord) gala->la);
    ASSERT(!gala->preferred || gala == gala0);
    ASSERT(LOOKS_LIKE_INFO_PTR(((StgClosure *)gala->la)->header.info));
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
