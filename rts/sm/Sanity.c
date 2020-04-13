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
 *      in the heap are dynamic.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#if defined(DEBUG)                                                   /* whole file */

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
#include "CNF.h"
#include "sm/NonMoving.h"
#include "sm/NonMovingMark.h"
#include "Profiling.h" // prof_arena

/* -----------------------------------------------------------------------------
   Forward decls.
   -------------------------------------------------------------------------- */

static void  checkSmallBitmap    ( StgPtr payload, StgWord bitmap, uint32_t );
static void  checkLargeBitmap    ( StgPtr payload, StgLargeBitmap*, uint32_t );
static void  checkClosureShallow ( const StgClosure * );
static void  checkSTACK          (StgStack *stack);

static W_    countNonMovingSegments ( struct NonmovingSegment *segs );
static W_    countNonMovingHeap     ( struct NonmovingHeap *heap );

/* -----------------------------------------------------------------------------
   Check stack sanity
   -------------------------------------------------------------------------- */

static void
checkSmallBitmap( StgPtr payload, StgWord bitmap, uint32_t size )
{
    uint32_t i;

    for(i = 0; i < size; i++, bitmap >>= 1 ) {
        if ((bitmap & 1) == 0) {
            checkClosureShallow((StgClosure *)payload[i]);
        }
    }
}

static void
checkLargeBitmap( StgPtr payload, StgLargeBitmap* large_bitmap, uint32_t size )
{
    StgWord bmp;
    uint32_t i, j;

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
checkClosureShallow( const StgClosure* p )
{
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(UNTAG_CONST_CLOSURE(p)));
}

// check an individual stack object
StgOffset
checkStackFrame( StgPtr c )
{
    uint32_t size;
    const StgRetInfoTable* info;

    info = get_ret_itbl((StgClosure *)c);

    /* All activation records have 'bitmap' style layout info. */
    switch (info->i.type) {

    case UPDATE_FRAME:
      ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgUpdateFrame*)c)->updatee));
      FALLTHROUGH;
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
        uint32_t size;
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
        const StgFunInfoTable *fun_info;
        StgRetFun *ret_fun;

        ret_fun = (StgRetFun *)c;
        fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(ret_fun->fun));
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
    ASSERT( p == stack_end );
}

static void
checkPAP (StgClosure *tagged_fun, StgClosure** payload, StgWord n_args)
{
    const StgClosure *fun;
    const StgFunInfoTable *fun_info;

    fun = UNTAG_CONST_CLOSURE(tagged_fun);
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

#if defined(PROFILING)
static void
checkClosureProfSanity(const StgClosure *p)
{
    StgProfHeader prof_hdr = p->header.prof;
    CostCentreStack *ccs = prof_hdr.ccs;
    if (HEAP_ALLOCED_GC((void*)ccs)) {
        checkPtrInArena((StgPtr)ccs, prof_arena);
    }
}
#endif

// Returns closure size in words
StgOffset
checkClosure( const StgClosure* p )
{
    const StgInfoTable *info;

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

    p = UNTAG_CONST_CLOSURE(p);

    info = p->header.info;
    load_load_barrier();

    if (IS_FORWARDING_PTR(info)) {
        barf("checkClosure: found EVACUATED closure %d", info->type);
    }

#if defined(PROFILING)
    checkClosureProfSanity(p);
#endif

    info = INFO_PTR_TO_STRUCT(info);
    load_load_barrier();

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
        uint32_t i;
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
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case BLACKHOLE:
    case PRIM:
    case MUT_PRIM:
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    case TVAR:
    case THUNK_STATIC:
    case FUN_STATIC:
    case COMPACT_NFDATA:
        {
            uint32_t i;
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
        ASSERT(// A bq with no other blocked TSOs:
               bq->queue == (MessageBlackHole*)END_TSO_QUEUE ||
               // A bq with blocked TSOs in its queue:
               bq->queue->header.info == &stg_MSG_BLACKHOLE_info ||
               // A bq with a deleted (in throwToMsg()) MSG_BLACKHOLE:
               bq->queue->header.info == &stg_IND_info);
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
            return arr_words_sizeW((StgArrBytes *)p);

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        {
            StgMutArrPtrs* a = (StgMutArrPtrs *)p;
            uint32_t i;
            for (i = 0; i < a->ptrs; i++) {
                ASSERT(LOOKS_LIKE_CLOSURE_PTR(a->payload[i]));
            }
            return mut_arr_ptrs_sizeW(a);
        }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        {
            StgSmallMutArrPtrs *a = (StgSmallMutArrPtrs *)p;
            for (uint32_t i = 0; i < a->ptrs; i++) {
                ASSERT(LOOKS_LIKE_CLOSURE_PTR(a->payload[i]));
            }
            return small_mut_arr_ptrs_sizeW(a);
        }

    case TSO:
        checkTSO((StgTSO *)p);
        return sizeofW(StgTSO);

    case STACK:
        checkSTACK((StgStack*)p);
        return stack_sizeW((StgStack*)p);

    case TREC_CHUNK:
      {
        uint32_t i;
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
    for (; bd != NULL; bd = bd->link) {
        if(!(bd->flags & BF_SWEPT)) {
            StgPtr p = bd->start;
            while (p < bd->free) {
                uint32_t size = checkClosure((StgClosure *)p);
                /* This is the smallest size of closure that can live in the heap */
                ASSERT( size >= MIN_PAYLOAD_SIZE + sizeofW(StgHeader) );
                p += size;

                /* skip over slop, see Note [slop on the heap] */
                while (p < bd->free &&
                       (*p < 0x1000 || !LOOKS_LIKE_INFO_PTR(*p))) { p++; }
            }
        }
    }
}

/* -----------------------------------------------------------------------------
 * Check nonmoving heap sanity
 *
 * After a concurrent sweep the nonmoving heap can be checked for validity.
 * -------------------------------------------------------------------------- */

static void checkNonmovingSegments (struct NonmovingSegment *seg)
{
    while (seg != NULL) {
        const nonmoving_block_idx count = nonmovingSegmentBlockCount(seg);
        for (nonmoving_block_idx i=0; i < count; i++) {
            if (seg->bitmap[i] == nonmovingMarkEpoch) {
                StgPtr p = nonmovingSegmentGetBlock(seg, i);
                checkClosure((StgClosure *) p);
            } else if (i < nonmovingSegmentInfo(seg)->next_free_snap){
                seg->bitmap[i] = 0;
            }
        }
        seg = seg->link;
    }
}

void checkNonmovingHeap (const struct NonmovingHeap *heap)
{
    for (unsigned int i=0; i < NONMOVING_ALLOCA_CNT; i++) {
        const struct NonmovingAllocator *alloc = heap->allocators[i];
        checkNonmovingSegments(alloc->filled);
        checkNonmovingSegments(alloc->active);
        for (unsigned int cap=0; cap < n_capabilities; cap++) {
            checkNonmovingSegments(alloc->current[cap]);
        }
    }
}


void
checkHeapChunk(StgPtr start, StgPtr end)
{
  StgPtr p;
  uint32_t size;

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
checkCompactObjects(bdescr *bd)
{
    // Compact objects are similar to large objects, but they have a
    // StgCompactNFDataBlock at the beginning, before the actual closure

    for ( ; bd != NULL; bd = bd->link) {
        ASSERT(bd->flags & BF_COMPACT);

        StgCompactNFDataBlock *block = (StgCompactNFDataBlock*)bd->start;
        StgCompactNFData *str = block->owner;
        ASSERT((W_)str == (W_)block + sizeof(StgCompactNFDataBlock));

        StgWord totalW = 0;
        StgCompactNFDataBlock *last;
        for ( ; block ; block = block->next) {
            last = block;
            ASSERT(block->owner == str);

            totalW += Bdescr((P_)block)->blocks * BLOCK_SIZE_W;

            StgPtr start = Bdescr((P_)block)->start + sizeofW(StgCompactNFDataBlock);
            StgPtr free;
            if (Bdescr((P_)block)->start == (P_)str->nursery) {
                free = str->hp;
            } else {
                free = Bdescr((P_)block)->free;
            }
            StgPtr p = start;
            while (p < free)  {
                // We can't use checkClosure() here because in
                // compactAdd#/compactAddWithSharing# when we see a non-
                // compactable object (a function, mutable object, or pinned
                // object) we leave the location for the object in the payload
                // empty.
                StgClosure *c = (StgClosure*)p;
                checkClosureShallow(c);
                p += closure_sizeW(c);
            }
        }

        ASSERT(str->totalW == totalW);
        ASSERT(str->last == last);
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
    StgTSO *next = tso->_link;
    const StgInfoTable *info = (const StgInfoTable*) tso->_link->header.info;
    load_load_barrier();

    ASSERT(next == END_TSO_QUEUE ||
           info == &stg_MVAR_TSO_QUEUE_info ||
           info == &stg_TSO_info ||
           info == &stg_WHITEHOLE_info); // used to happen due to STM doing
                                         // lockTSO(), might not happen now

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
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso->global_link) &&
            (tso->global_link == END_TSO_QUEUE ||
             get_itbl((StgClosure*)tso->global_link)->type == TSO));
}

/*
   Check that all TSOs have been evacuated.
   Optionally also check the sanity of the TSOs.
*/
void
checkGlobalTSOList (bool checkTSOs)
{
  for (uint32_t g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (StgTSO *tso = generations[g].threads; tso != END_TSO_QUEUE;
           tso = tso->global_link) {
          ASSERT(LOOKS_LIKE_CLOSURE_PTR(tso));
          ASSERT(get_itbl((StgClosure *)tso)->type == TSO);
          if (checkTSOs) {
              checkTSO(tso);
          }

          // If this TSO is dirty and in an old generation, it better
          // be on the mutable list.
          if (tso->dirty) {
              ASSERT(Bdescr((P_)tso)->gen_no == 0 || (tso->flags & TSO_MARKED));
              tso->flags &= ~TSO_MARKED;
          }

          StgStack *stack = tso->stackobj;
          while (1) {
              if (stack->dirty & STACK_DIRTY) {
                  ASSERT(Bdescr((P_)stack)->gen_no == 0 || (stack->dirty & STACK_SANE));
                  stack->dirty &= ~STACK_SANE;
              }
              StgUnderflowFrame *frame =
                  (StgUnderflowFrame*) (stack->stack + stack->stack_size
                          - sizeofW(StgUnderflowFrame));
              if (frame->info != &stg_stack_underflow_frame_info
                      || frame->next_chunk == (StgStack*)END_TSO_QUEUE) {
                  break;
              }
              stack = frame->next_chunk;
          }
      }
  }
}

/* -----------------------------------------------------------------------------
   Check mutable list sanity.
   -------------------------------------------------------------------------- */

static void
checkMutableList( bdescr *mut_bd, uint32_t gen )
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
                ((StgStack *)p)->dirty |= STACK_SANE;
                break;
            }
        }
    }
}

static void
checkLocalMutableLists (uint32_t cap_no)
{
    uint32_t g;
    for (g = 1; g < RtsFlags.GcFlags.generations; g++) {
        checkMutableList(capabilities[cap_no]->mut_lists[g], g);
    }
}

static void
checkMutableLists (void)
{
    uint32_t i;
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
  const StgInfoTable *info;

  while (p != END_OF_STATIC_OBJECT_LIST) {
    p = UNTAG_STATIC_LIST_PTR(p);
    checkClosure(p);
    info = get_itbl(p);
    switch (info->type) {
    case IND_STATIC:
      {
        const StgClosure *indirectee;

        indirectee = UNTAG_CONST_CLOSURE(((StgIndStatic *)p)->indirectee);
        ASSERT(LOOKS_LIKE_CLOSURE_PTR(indirectee));
        ASSERT(LOOKS_LIKE_INFO_PTR((StgWord)indirectee->header.info));
        p = *IND_STATIC_LINK((StgClosure *)p);
        break;
      }

    case THUNK_STATIC:
      p = *THUNK_STATIC_LINK((StgClosure *)p);
      break;

    case FUN_STATIC:
      p = *STATIC_LINK(info,(StgClosure *)p);
      break;

    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_2_0:
    case CONSTR_1_1:
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
    uint32_t blocks = 0;

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
                             bool after_major_gc USED_IF_THREADS)
{
    uint32_t n;
    gen_workspace *ws;

    //ASSERT(countBlocks(gen->blocks) == gen->n_blocks);
    ASSERT(countBlocks(gen->large_objects) == gen->n_large_blocks);

#if defined(THREADED_RTS)
    // Note [heap sanity checking with SMP]
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // heap sanity checking doesn't work with SMP for two reasons:
    //
    //   * We can't zero the slop. However, we can sanity-check the heap after a
    //     major gc, because there is no slop. See also Updates.h and Note
    //     [zeroing slop when overwriting closures].
    //
    //   * The nonmoving collector may be mutating its large object lists,
    //     unless we were in fact called by the nonmoving collector.
    if (!after_major_gc) return;
#endif

    if (RtsFlags.GcFlags.useNonmoving && gen == oldest_gen) {
        ASSERT(countNonMovingSegments(nonmovingHeap.free) == (W_) nonmovingHeap.n_free * NONMOVING_SEGMENT_BLOCKS);
        ASSERT(countBlocks(nonmoving_large_objects) == n_nonmoving_large_blocks);
        ASSERT(countBlocks(nonmoving_marked_large_objects) == n_nonmoving_marked_large_blocks);

        // Compact regions
        // Accounting here is tricky due to the fact that the CNF allocation
        // code modifies generation->n_compact_blocks directly. However, most
        // objects being swept by the nonmoving GC are tracked in
        // nonmoving_*_compact_objects. Consequently we can only maintain a very loose
        // sanity invariant here.
        uint32_t counted_cnf_blocks = 0;
        counted_cnf_blocks += countCompactBlocks(nonmoving_marked_compact_objects);
        counted_cnf_blocks += countCompactBlocks(nonmoving_compact_objects);
        counted_cnf_blocks += countCompactBlocks(oldest_gen->compact_objects);

        uint32_t total_cnf_blocks = 0;
        total_cnf_blocks += n_nonmoving_compact_blocks + oldest_gen->n_compact_blocks;
        total_cnf_blocks += n_nonmoving_marked_compact_blocks;

        ASSERT(counted_cnf_blocks == total_cnf_blocks);
    }

    checkHeapChain(gen->blocks);

    for (n = 0; n < n_capabilities; n++) {
        ws = &gc_threads[n]->gens[gen->no];
        checkHeapChain(ws->todo_bd);
        checkHeapChain(ws->part_list);
        checkHeapChain(ws->scavd_list);
    }

    checkLargeObjects(gen->large_objects);
    checkCompactObjects(gen->compact_objects);
}

/* Full heap sanity check. */
static void checkFullHeap (bool after_major_gc)
{
    uint32_t g, n;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        checkGeneration(&generations[g], after_major_gc);
    }
    for (n = 0; n < n_capabilities; n++) {
        checkNurserySanity(&nurseries[n]);
    }
}

void checkSanity (bool after_gc, bool major_gc)
{
    checkFullHeap(after_gc && major_gc);

    checkFreeListSanity();

    // always check the stacks in threaded mode, because checkHeap()
    // does nothing in this case.
    if (after_gc) {
        checkMutableLists();
        checkGlobalTSOList(true);
    }
}

static void
markCompactBlocks(bdescr *bd)
{
    for (; bd != NULL; bd = bd->link) {
        compactMarkKnown(((StgCompactNFDataBlock*)bd->start)->owner);
    }
}

static void
markNonMovingSegments(struct NonmovingSegment *seg)
{
    while (seg) {
        markBlocks(Bdescr((P_)seg));
        seg = seg->link;
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
    uint32_t g, i, j;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (i = 0; i < n_capabilities; i++) {
            markBlocks(capabilities[i]->mut_lists[g]);
            markBlocks(gc_threads[i]->gens[g].part_list);
            markBlocks(gc_threads[i]->gens[g].scavd_list);
            markBlocks(gc_threads[i]->gens[g].todo_bd);
        }
        markBlocks(generations[g].blocks);
        markBlocks(generations[g].large_objects);
        markCompactBlocks(generations[g].compact_objects);
    }

    for (i = 0; i < n_nurseries; i++) {
        markBlocks(nurseries[i].blocks);
    }

    for (i = 0; i < n_capabilities; i++) {
        markBlocks(gc_threads[i]->free_blocks);
        markBlocks(capabilities[i]->pinned_object_block);
        markBlocks(capabilities[i]->upd_rem_set.queue.blocks);
    }

    if (RtsFlags.GcFlags.useNonmoving) {
        markBlocks(upd_rem_set_block_list);
        markBlocks(nonmoving_large_objects);
        markBlocks(nonmoving_marked_large_objects);
        markBlocks(nonmoving_compact_objects);
        markBlocks(nonmoving_marked_compact_objects);
        for (i = 0; i < NONMOVING_ALLOCA_CNT; i++) {
            struct NonmovingAllocator *alloc = nonmovingHeap.allocators[i];
            markNonMovingSegments(alloc->filled);
            markNonMovingSegments(alloc->active);
            for (j = 0; j < n_capabilities; j++) {
                markNonMovingSegments(alloc->current[j]);
            }
        }
        markNonMovingSegments(nonmovingHeap.sweep_list);
        markNonMovingSegments(nonmovingHeap.free);
        if (current_mark_queue)
            markBlocks(current_mark_queue->blocks);
    }

#if defined(PROFILING)
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
    uint32_t n;
    for (n = 0, tso = cap->run_queue_hd; tso != END_TSO_QUEUE;
         prev = tso, tso = tso->_link, n++) {
        ASSERT(prev == END_TSO_QUEUE || prev->_link == tso);
        ASSERT(tso->block_info.prev == prev);
    }
    ASSERT(cap->run_queue_tl == prev);
    ASSERT(cap->n_run_queue == n);
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
            debugBelch("block at %p (bdescr %p) has %" FMT_Word "KB slop\n",
                       bd->start, bd, slop / (1024/(W_)sizeof(W_)));
        }
    }
}

static W_
genBlocks (generation *gen)
{
    W_ ret = 0;
    if (RtsFlags.GcFlags.useNonmoving && gen == oldest_gen) {
        // See Note [Live data accounting in nonmoving collector].
        ASSERT(countNonMovingHeap(&nonmovingHeap) == gen->n_blocks);
        ret += countAllocdBlocks(nonmoving_large_objects);
        ret += countAllocdBlocks(nonmoving_marked_large_objects);
        ret += countAllocdCompactBlocks(nonmoving_compact_objects);
        ret += countAllocdCompactBlocks(nonmoving_marked_compact_objects);
        ret += countNonMovingHeap(&nonmovingHeap);
        if (current_mark_queue)
            ret += countBlocks(current_mark_queue->blocks);
    } else {
        ASSERT(countBlocks(gen->blocks) == gen->n_blocks);
        ASSERT(countCompactBlocks(gen->compact_objects) == gen->n_compact_blocks);
        ASSERT(countCompactBlocks(gen->compact_blocks_in_import) == gen->n_compact_blocks_in_import);
        ret += gen->n_blocks;
    }

    ASSERT(countBlocks(gen->large_objects) == gen->n_large_blocks);

    ret += gen->n_old_blocks +
        countAllocdBlocks(gen->large_objects) +
        countAllocdCompactBlocks(gen->compact_objects) +
        countAllocdCompactBlocks(gen->compact_blocks_in_import);
    return ret;
}

static W_
countNonMovingSegments(struct NonmovingSegment *segs)
{
    W_ ret = 0;
    while (segs) {
        ret += countBlocks(Bdescr((P_)segs));
        segs = segs->link;
    }
    return ret;
}

static W_
countNonMovingAllocator(struct NonmovingAllocator *alloc)
{
    W_ ret = countNonMovingSegments(alloc->filled)
           + countNonMovingSegments(alloc->active);
    for (uint32_t i = 0; i < n_capabilities; ++i) {
        ret += countNonMovingSegments(alloc->current[i]);
    }
    return ret;
}

static W_
countNonMovingHeap(struct NonmovingHeap *heap)
{
    W_ ret = 0;
    for (int alloc_idx = 0; alloc_idx < NONMOVING_ALLOCA_CNT; alloc_idx++) {
        ret += countNonMovingAllocator(heap->allocators[alloc_idx]);
    }
    ret += countNonMovingSegments(heap->sweep_list);
    ret += countNonMovingSegments(heap->free);
    return ret;
}

void
memInventory (bool show)
{
  uint32_t g, i;
  W_ gen_blocks[RtsFlags.GcFlags.generations];
  W_ nursery_blocks = 0, retainer_blocks = 0,
      arena_blocks = 0, exec_blocks = 0, gc_free_blocks = 0,
      upd_rem_set_blocks = 0;
  W_ live_blocks = 0, free_blocks = 0;
  bool leak;

#if defined(THREADED_RTS)
  // Can't easily do a memory inventory: We might race with the nonmoving
  // collector. In principle we could try to take nonmoving_collection_mutex
  // and do an inventory if we have it but we don't currently implement this.
  if (RtsFlags.GcFlags.useNonmoving)
    return;
#endif

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

  for (i = 0; i < n_nurseries; i++) {
      ASSERT(countBlocks(nurseries[i].blocks) == nurseries[i].n_blocks);
      nursery_blocks += nurseries[i].n_blocks;
  }
  for (i = 0; i < n_capabilities; i++) {
      W_ n = countBlocks(gc_threads[i]->free_blocks);
      gc_free_blocks += n;
      if (capabilities[i]->pinned_object_block != NULL) {
          nursery_blocks += capabilities[i]->pinned_object_block->blocks;
      }
      nursery_blocks += countBlocks(capabilities[i]->pinned_object_blocks);
  }

#if defined(PROFILING)
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

  // count UpdRemSet blocks
  for (i = 0; i < n_capabilities; ++i) {
      upd_rem_set_blocks += countBlocks(capabilities[i]->upd_rem_set.queue.blocks);
  }
  upd_rem_set_blocks += countBlocks(upd_rem_set_block_list);

  live_blocks = 0;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      live_blocks += gen_blocks[g];
  }
  live_blocks += nursery_blocks +
               + retainer_blocks + arena_blocks + exec_blocks + gc_free_blocks
               + upd_rem_set_blocks;

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
      debugBelch("  GC free pool : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 gc_free_blocks, MB(gc_free_blocks));
      debugBelch("  free         : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 free_blocks, MB(free_blocks));
      debugBelch("  UpdRemSet    : %5" FMT_Word " blocks (%6.1lf MB)\n",
                 upd_rem_set_blocks, MB(upd_rem_set_blocks));
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
