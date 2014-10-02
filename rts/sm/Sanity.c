/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
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

#include "RtsUtils.h"
#include "sm/Storage.h"
#include "sm/BlockAlloc.h"
#include "GCThread.h"
#include "Sanity.h"
#include "Schedule.h"
#include "Apply.h"
#include "Printer.h"
#include "Arena.h"
#include "RetainerProfile.h"

/* -----------------------------------------------------------------------------
   Forward decls.
   -------------------------------------------------------------------------- */

static void      checkSmallBitmap    ( StgPtr payload, StgWord bitmap, nat );
static void      checkLargeBitmap    ( StgPtr payload, StgLargeBitmap*, nat );
static void      checkClosureShallow ( StgClosure * );
static void      checkSTACK          (StgStack *stack);

/* -----------------------------------------------------------------------------
   Check stack sanity
   -------------------------------------------------------------------------- */

static void
checkSmallBitmap( StgPtr payload, StgWord bitmap, nat size )
{
    nat i;

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
    StgClosure *q;

    q = UNTAG_CLOSURE(p);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

    /* Is it a static closure? */
    if (!HEAP_ALLOCED(q)) {
	ASSERT(closure_STATIC(q));
    } else {
	ASSERT(!closure_STATIC(q));
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

    case UPDATE_FRAME:
      ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgUpdateFrame*)c)->updatee));
    case ATOMICALLY_FRAME:
    case CATCH_RETRY_FRAME:
    case CATCH_STM_FRAME:
    case CATCH_FRAME:
      // small bitmap cases (<= 32 entries)
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case RET_SMALL:
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
	size = GET_LARGE_BITMAP(&info->i)->size;
	checkLargeBitmap((StgPtr)c + 1, GET_LARGE_BITMAP(&info->i), size);
	return 1 + size;

    case RET_FUN:
    {
	StgFunInfoTable *fun_info;
	StgRetFun *ret_fun;

	ret_fun = (StgRetFun *)c;
	fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
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
checkPAP (StgClosure *tagged_fun, StgClosure** payload, StgWord n_args)
{ 
    StgClosure *fun;
    StgFunInfoTable *fun_info;
    
    fun = UNTAG_CLOSURE(tagged_fun);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(fun));
    fun_info = get_fun_itbl(fun);
    
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

    ASSERT(fun_info->f.arity > TAG_MASK ? GET_CLOSURE_TAG(tagged_fun) == 0
           : GET_CLOSURE_TAG(tagged_fun) == fun_info->f.arity);
}


StgOffset 
checkClosure( StgClosure* p )
{
    const StgInfoTable *info;

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

    p = UNTAG_CLOSURE(p);
    /* Is it a static closure (i.e. in the data segment)? */
    if (!HEAP_ALLOCED(p)) {
	ASSERT(closure_STATIC(p));
    } else {
	ASSERT(!closure_STATIC(p));
    }

    info = p->header.info;

    if (IS_FORWARDING_PTR(info)) {
        barf("checkClosure: found EVACUATED closure %d", info->type);
    }
    info = INFO_PTR_TO_STRUCT(info);

    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
      { 
	StgMVar *mvar = (StgMVar *)p;
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(mvar->head));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(mvar->tail));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(mvar->value));
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
	return thunk_sizeW_fromITBL(info);
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
    case BLACKHOLE:
    case PRIM:
    case MUT_PRIM:
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    case TVAR:
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

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        // NO: the BH might have been updated now
        // ASSERT(get_itbl(bq->bh)->type == BLACKHOLE);
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(bq->bh));

        ASSERT(get_itbl((StgClosure *)(bq->owner))->type == TSO);
        ASSERT(bq->queue == (MessageBlackHole*)END_TSO_QUEUE 
               || bq->queue->header.info == &stg_MSG_BLACKHOLE_info);
        ASSERT(bq->link == (StgBlockingQueue*)END_TSO_QUEUE || 
               get_itbl((StgClosure *)(bq->link))->type == IND ||
               get_itbl((StgClosure *)(bq->link))->type == BLOCKING_QUEUE);

        return sizeofW(StgBlockingQueue);
    }

    case BCO: {
	StgBCO *bco = (StgBCO *)p;
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->instrs));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->literals));
	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->ptrs));
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
	    return sizeofW(StgInd);
	}

    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case UPDATE_FRAME:
    case UNDERFLOW_FRAME:
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
        return sizeofW(StgTSO);

    case STACK:
        checkSTACK((StgStack*)p);
        return stack_sizeW((StgStack*)p);

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
      
    default:
	    barf("checkClosure (closure type %d)", info->type);
    }
}


/* -----------------------------------------------------------------------------
   Check Heap Sanity

   After garbage collection, the live heap is in a state where we can
   run through and check that all the pointers point to the right
   place.  This function starts at a given position and sanity-checks
   all the objects in the remainder of the chain.
   -------------------------------------------------------------------------- */

void checkHeapChain (bdescr *bd)
{
    StgPtr p;

    for (; bd != NULL; bd = bd->link) {
        if(!(bd->flags & BF_SWEPT)) {
            p = bd->start;
            while (p < bd->free) {
                nat size = checkClosure((StgClosure *)p);
                /* This is the smallest size of closure that can live in the heap */
                ASSERT( size >= MIN_PAYLOAD_SIZE + sizeofW(StgHeader) );
                p += size;
	    
                /* skip over slop */
                while (p < bd->free &&
                       (*p < 0x1000 || !LOOKS_LIKE_INFO_PTR(*p))) { p++; }
            }
	}
    }
}

void
checkHeapChunk(StgPtr start, StgPtr end)
{
  StgPtr p;
  nat size;

  for (p=start; p<end; p+=size) {
    ASSERT(LOOKS_LIKE_INFO_PTR(*p));
    size = checkClosure((StgClosure *)p);
    /* This is the smallest size of closure that can live in the heap. */
    ASSERT( size >= MIN_PAYLOAD_SIZE + sizeofW(StgHeader) );
  }
}

void
checkLargeObjects(bdescr *bd)
{
  while (bd != NULL) {
    if (!(bd->flags & BF_PINNED)) {
      checkClosure((StgClosure *)bd->start);
    }
    bd = bd->link;
  }
}

static void
checkSTACK (StgStack *stack)
{
    StgPtr sp = stack->sp;
    StgOffset stack_size = stack->stack_size;
    StgPtr stack_end = stack->stack + stack_size;

    ASSERT(stack->stack <= sp && sp <= stack_end);

    checkStackChunk(sp, stack_end);
}

void
checkTSO(StgTSO *tso)
{
    StgTSO *next;
    const StgInfoTable *info;

    if (tso->what_next == ThreadKilled) {
      /* The garbage collector doesn't bother following any pointers
       * from dead threads, so don't check sanity here.  
       */
      return;
    }

    next = tso->_link;
    info = (const StgInfoTable*) tso->_link->header.info;

    ASSERT(next == END_TSO_QUEUE ||
           info == &stg_MVAR_TSO_QUEUE_info ||
           info == &stg_TSO_info ||
           info == &stg_WHITEHOLE_info); // happens due to STM doing lockTSO()

    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
	|| tso->why_blocked == BlockedOnBlackHole
	|| tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
	) {
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso->block_info.closure));
    }

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso->bq));
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso->blocked_exceptions));
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso->stackobj));

    // XXX are we checking the stack twice?
    checkSTACK(tso->stackobj);
}

/*
   Check that all TSOs have been evacuated.
   Optionally also check the sanity of the TSOs.
*/
void
checkGlobalTSOList (rtsBool checkTSOs)
{
  StgTSO *tso;
  nat g;

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (tso=generations[g].threads; tso != END_TSO_QUEUE; 
           tso = tso->global_link) {
          ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso));
          ASSERT(get_itbl((StgClosure *)tso)->type == TSO);
          if (checkTSOs)
              checkTSO(tso);

          // If this TSO is dirty and in an old generation, it better
          // be on the mutable list.
          if (tso->dirty) {
              ASSERT(Bdescr((P_)tso)->gen_no == 0 || (tso->flags & TSO_MARKED));
              tso->flags &= ~TSO_MARKED;
          }

          {
              StgStack *stack;
              StgUnderflowFrame *frame;

              stack = tso->stackobj;
              while (1) {
                  if (stack->dirty & 1) {
                      ASSERT(Bdescr((P_)stack)->gen_no == 0 || (stack->dirty & TSO_MARKED));
                      stack->dirty &= ~TSO_MARKED;
                  }
                  frame = (StgUnderflowFrame*) (stack->stack + stack->stack_size
                                                - sizeofW(StgUnderflowFrame));
                  if (frame->info != &stg_stack_underflow_frame_info
                      || frame->next_chunk == (StgStack*)END_TSO_QUEUE) break;
                  stack = frame->next_chunk;
              }
          }
      }
  }
}

/* -----------------------------------------------------------------------------
   Check mutable list sanity.
   -------------------------------------------------------------------------- */

static void
checkMutableList( bdescr *mut_bd, nat gen )
{
    bdescr *bd;
    StgPtr q;
    StgClosure *p;

    for (bd = mut_bd; bd != NULL; bd = bd->link) {
	for (q = bd->start; q < bd->free; q++) {
	    p = (StgClosure *)*q;
            ASSERT(!HEAP_ALLOCED(p) || Bdescr((P_)p)->gen_no == gen);
            checkClosure(p);

            switch (get_itbl(p)->type) {
            case TSO:
                ((StgTSO *)p)->flags |= TSO_MARKED;
                break;
            case STACK:
                ((StgStack *)p)->dirty |= TSO_MARKED;
                break;
            }
        }
    }
}

static void
checkLocalMutableLists (nat cap_no)
{
    nat g;
    for (g = 1; g < RtsFlags.GcFlags.generations; g++) {
        checkMutableList(capabilities[cap_no]->mut_lists[g], g);
    }
}

static void
checkMutableLists (void)
{
    nat i;
    for (i = 0; i < n_capabilities; i++) {
        checkLocalMutableLists(i);
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
        StgClosure *indirectee = UNTAG_CLOSURE(((StgIndStatic *)p)->indirectee);

	ASSERT(LOOKS_LIKE_CLOSURE_PTR(indirectee));
	ASSERT(LOOKS_LIKE_INFO_PTR((StgWord)indirectee->header.info));
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

/* Nursery sanity check */
void
checkNurserySanity (nursery *nursery)
{
    bdescr *bd, *prev;
    nat blocks = 0;

    prev = NULL;
    for (bd = nursery->blocks; bd != NULL; bd = bd->link) {
        ASSERT(bd->gen == g0);
        ASSERT(bd->u.back == prev);
	prev = bd;
	blocks += bd->blocks;
    }

    ASSERT(blocks == nursery->n_blocks);
}

static void checkGeneration (generation *gen, 
                             rtsBool after_major_gc USED_IF_THREADS)
{
    nat n;
    gen_workspace *ws;

    ASSERT(countBlocks(gen->blocks) == gen->n_blocks);
    ASSERT(countBlocks(gen->large_objects) == gen->n_large_blocks);

#if defined(THREADED_RTS)
    // heap sanity checking doesn't work with SMP, because we can't
    // zero the slop (see Updates.h).  However, we can sanity-check
    // the heap after a major gc, because there is no slop.
    if (!after_major_gc) return;
#endif

    checkHeapChain(gen->blocks);

    for (n = 0; n < n_capabilities; n++) {
        ws = &gc_threads[n]->gens[gen->no];
        checkHeapChain(ws->todo_bd);
        checkHeapChain(ws->part_list);
        checkHeapChain(ws->scavd_list);
    }

    checkLargeObjects(gen->large_objects);
}

/* Full heap sanity check. */
static void checkFullHeap (rtsBool after_major_gc)
{
    nat g, n;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        checkGeneration(&generations[g], after_major_gc);
    }
    for (n = 0; n < n_capabilities; n++) {
        checkNurserySanity(&nurseries[n]);
    }
}

void checkSanity (rtsBool after_gc, rtsBool major_gc)
{
    checkFullHeap(after_gc && major_gc);

    checkFreeListSanity();

    // always check the stacks in threaded mode, because checkHeap()
    // does nothing in this case.
    if (after_gc) {
        checkMutableLists();
        checkGlobalTSOList(rtsTrue);
    }
}

// If memInventory() calculates that we have a memory leak, this
// function will try to find the block(s) that are leaking by marking
// all the ones that we know about, and search through memory to find
// blocks that are not marked.  In the debugger this can help to give
// us a clue about what kind of block leaked.  In the future we might
// annotate blocks with their allocation site to give more helpful
// info.
static void
findMemoryLeak (void)
{
    nat g, i;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (i = 0; i < n_capabilities; i++) {
            markBlocks(capabilities[i]->mut_lists[g]);
            markBlocks(gc_threads[i]->gens[g].part_list);
            markBlocks(gc_threads[i]->gens[g].scavd_list);
            markBlocks(gc_threads[i]->gens[g].todo_bd);
        }
        markBlocks(generations[g].blocks);
        markBlocks(generations[g].large_objects);
    }

    for (i = 0; i < n_capabilities; i++) {
        markBlocks(nurseries[i].blocks);
        markBlocks(capabilities[i]->pinned_object_block);
    }

#ifdef PROFILING
  // TODO:
  // if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER) {
  //    markRetainerBlocks();
  // }
#endif

  // count the blocks allocated by the arena allocator
  // TODO:
  // markArenaBlocks();

  // count the blocks containing executable memory
  markBlocks(exec_block);

  reportUnmarkedBlocks();
}

void
checkRunQueue(Capability *cap)
{
    StgTSO *prev, *tso;
    prev = END_TSO_QUEUE;
    for (tso = cap->run_queue_hd; tso != END_TSO_QUEUE; 
         prev = tso, tso = tso->_link) {
        ASSERT(prev == END_TSO_QUEUE || prev->_link == tso);
        ASSERT(tso->block_info.prev == prev);
    }
    ASSERT(cap->run_queue_tl == prev);
}

/* -----------------------------------------------------------------------------
   Memory leak detection

   memInventory() checks for memory leaks by counting up all the
   blocks we know about and comparing that to the number of blocks
   allegedly floating around in the system.
   -------------------------------------------------------------------------- */

// Useful for finding partially full blocks in gdb
void findSlop(bdescr *bd);
void findSlop(bdescr *bd)
{
    W_ slop;

    for (; bd != NULL; bd = bd->link) {
        slop = (bd->blocks * BLOCK_SIZE_W) - (bd->free - bd->start);
        if (slop > (1024/sizeof(W_))) {
            debugBelch("block at %p (bdescr %p) has %" FMT_SizeT "KB slop\n",
                       bd->start, bd, slop / (1024/sizeof(W_)));
        }
    }
}

static W_
genBlocks (generation *gen)
{
    ASSERT(countBlocks(gen->blocks) == gen->n_blocks);
    ASSERT(countBlocks(gen->large_objects) == gen->n_large_blocks);
    return gen->n_blocks + gen->n_old_blocks + 
	    countAllocdBlocks(gen->large_objects);
}

void
memInventory (rtsBool show)
{
  nat g, i;
  W_ gen_blocks[RtsFlags.GcFlags.generations];
  W_ nursery_blocks, retainer_blocks,
       arena_blocks, exec_blocks;
  W_ live_blocks = 0, free_blocks = 0;
  rtsBool leak;

  // count the blocks we current have

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      gen_blocks[g] = 0;
      for (i = 0; i < n_capabilities; i++) {
          gen_blocks[g] += countBlocks(capabilities[i]->mut_lists[g]);
          gen_blocks[g] += countBlocks(gc_threads[i]->gens[g].part_list);
          gen_blocks[g] += countBlocks(gc_threads[i]->gens[g].scavd_list);
          gen_blocks[g] += countBlocks(gc_threads[i]->gens[g].todo_bd);
      }
      gen_blocks[g] += genBlocks(&generations[g]);
  }

  nursery_blocks = 0;
  for (i = 0; i < n_capabilities; i++) {
      ASSERT(countBlocks(nurseries[i].blocks) == nurseries[i].n_blocks);
      nursery_blocks += nurseries[i].n_blocks;
      if (capabilities[i]->pinned_object_block != NULL) {
          nursery_blocks += capabilities[i]->pinned_object_block->blocks;
      }
      nursery_blocks += countBlocks(capabilities[i]->pinned_object_blocks);
  }

  retainer_blocks = 0;
#ifdef PROFILING
  if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER) {
      retainer_blocks = retainerStackBlocks();
  }
#endif

  // count the blocks allocated by the arena allocator
  arena_blocks = arenaBlocks();

  // count the blocks containing executable memory
  exec_blocks = countAllocdBlocks(exec_block);

  /* count the blocks on the free list */
  free_blocks = countFreeList();

  live_blocks = 0;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      live_blocks += gen_blocks[g];
  }
  live_blocks += nursery_blocks + 
               + retainer_blocks + arena_blocks + exec_blocks;

#define MB(n) (((double)(n) * BLOCK_SIZE_W) / ((1024*1024)/sizeof(W_)))

  leak = live_blocks + free_blocks != mblocks_allocated * BLOCKS_PER_MBLOCK;

  if (show || leak)
  {
      if (leak) { 
          debugBelch("Memory leak detected:\n");
      } else {
          debugBelch("Memory inventory:\n");
      }
      for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
          debugBelch("  gen %d blocks : %5" FMT_Word " blocks (%6.1lf MB)\n", g,
                     gen_blocks[g], MB(gen_blocks[g]));
      }
      debugBelch("  nursery      : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 nursery_blocks, MB(nursery_blocks));
      debugBelch("  retainer     : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 retainer_blocks, MB(retainer_blocks));
      debugBelch("  arena blocks : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 arena_blocks, MB(arena_blocks));
      debugBelch("  exec         : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 exec_blocks, MB(exec_blocks));
      debugBelch("  free         : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 free_blocks, MB(free_blocks));
      debugBelch("  total        : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 live_blocks + free_blocks, MB(live_blocks+free_blocks));
      if (leak) {
          debugBelch("\n  in system    : %5" FMT_Word " blocks (%" FMT_Word " MB)\n", 
                     (W_)(mblocks_allocated * BLOCKS_PER_MBLOCK), mblocks_allocated);
      }
  }

  if (leak) {
      debugBelch("\n");
      findMemoryLeak();
  }
  ASSERT(n_alloc_blocks == live_blocks);
  ASSERT(!leak);
}


#endif /* DEBUG */
