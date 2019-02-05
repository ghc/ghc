/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Mark phase
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
// We call evacuate, which expects the thread-local gc_thread to be valid;
// This is sometimes declared as a register variable therefore it is necessary
// to include the declaration so that the compiler doesn't clobber the register.
#include "NonMovingMark.h"
#include "NonMoving.h"
#include "BlockAlloc.h"  /* for countBlocks */
#include "HeapAlloc.h"
#include "Task.h"
#include "Trace.h"
#include "HeapUtils.h"
#include "Printer.h"
#include "Schedule.h"
#include "Weak.h"
#include "STM.h"
#include "MarkWeak.h"
#include "sm/Storage.h"

static void mark_closure (MarkQueue *queue, StgClosure *p, StgClosure **origin);
static void mark_tso (MarkQueue *queue, StgTSO *tso);
static void mark_stack (MarkQueue *queue, StgStack *stack);
static void mark_PAP_payload (MarkQueue *queue,
                              StgClosure *fun,
                              StgClosure **payload,
                              StgWord size);

// How many Array# entries to add to the mark queue at once?
#define MARK_ARRAY_CHUNK_LENGTH 128

/* Note [Large objects in the non-moving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The nonmoving collector keeps a separate list of its large objects, apart from
 * oldest_gen->large_objects. There are two reasons for this:
 *
 *  1. oldest_gen is mutated by minor collections, which happen concurrently with
 *     marking
 *  2. the non-moving collector needs a consistent picture
 *
 * At the beginning of a major collection, nonmovingCollect takes the objects in
 * oldest_gen->large_objects (which includes all large objects evacuated by the
 * moving collector) and adds them to nonmoving_large_objects. This is the set
 * of large objects that will being collected in the current major GC cycle.
 *
 * As the concurrent mark phase proceeds, the large objects in
 * nonmoving_large_objects that are found to be live are moved to
 * nonmoving_marked_large_objects. During sweep we discard all objects that remain
 * in nonmoving_large_objects and move everything in nonmoving_marked_larged_objects
 * back to nonmoving_large_objects.
 *
 * During minor collections large objects will accumulate on
 * oldest_gen->large_objects, where they will be picked up by the nonmoving
 * collector and moved to nonmoving_large_objects during the next major GC.
 * When this happens the block gets its BF_NONMOVING_SWEEPING flag set to
 * indicate that it is part of the snapshot and consequently should be marked by
 * the nonmoving mark phase..
 */

bdescr *nonmoving_large_objects = NULL;
bdescr *nonmoving_marked_large_objects = NULL;
memcount n_nonmoving_large_blocks = 0;
memcount n_nonmoving_marked_large_blocks = 0;

/*
 * Where we keep our threads during collection since we must have a snapshot of
 * the threads that lived in the nonmoving heap at the time that the snapshot
 * was taken to safely resurrect.
 */
StgTSO *nonmoving_old_threads = END_TSO_QUEUE;
/* Same for weak pointers */
StgWeak *nonmoving_old_weak_ptr_list = NULL;
/* Because we can "tidy" thread and weak lists concurrently with a minor GC we
 * need to move marked threads and weaks to these lists until we pause for sync.
 * Then we move them to oldest_gen lists. */
StgTSO *nonmoving_threads = END_TSO_QUEUE;
StgWeak *nonmoving_weak_ptr_list = NULL;

#if defined(DEBUG)
// TODO (osa): Document
StgIndStatic *debug_caf_list_snapshot = (StgIndStatic*)END_OF_CAF_LIST;
#endif

/* Used to provide the current mark queue to the young generation
 * collector for scavenging.
 */
MarkQueue *current_mark_queue = NULL;

/*********************************************************
 * Pushing to either the mark queue or remembered set
 *********************************************************/

STATIC_INLINE void
push (MarkQueue *q, const MarkQueueEnt *ent)
{
    // Are we at the end of the block?
    if (q->top->head == MARK_QUEUE_BLOCK_ENTRIES) {
        // Yes, this block is full.
        // allocate a fresh block.
        ACQUIRE_SM_LOCK;
        bdescr *bd = allocGroup(1);
        bd->link = q->blocks;
        q->blocks = bd;
        q->top = (MarkQueueBlock *) bd->start;
        q->top->head = 0;
        RELEASE_SM_LOCK;
    }

    q->top->entries[q->top->head] = *ent;
    q->top->head++;
}

static inline
void push_closure (MarkQueue *q,
                   StgClosure *p,
                   StgClosure **origin)
{
    // TODO: Push this into callers where they already have the Bdescr
    if (HEAP_ALLOCED_GC(p) && (Bdescr((StgPtr) p)->gen != oldest_gen))
        return;

#if defined(DEBUG)
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    // Commenting out: too slow
    // if (RtsFlags.DebugFlags.sanity) {
    //     assert_in_nonmoving_heap((P_)p);
    //     if (origin)
    //         assert_in_nonmoving_heap((P_)origin);
    // }
#endif

    MarkQueueEnt ent = {
        .type = MARK_CLOSURE,
        .mark_closure = {
            .p = UNTAG_CLOSURE(p),
            .origin = origin,
        }
    };
    push(q, &ent);
}

static
void push_array (MarkQueue *q,
                 const StgMutArrPtrs *array,
                 StgWord start_index)
{
    // TODO: Push this into callers where they already have the Bdescr
    if (HEAP_ALLOCED_GC(array) && (Bdescr((StgPtr) array)->gen != oldest_gen))
        return;

    MarkQueueEnt ent = {
        .type = MARK_ARRAY,
        .mark_array = {
            .array = array,
            .start_index = start_index
        }
    };
    push(q, &ent);
}

static
void push_thunk_srt (MarkQueue *q, const StgInfoTable *info)
{
    const StgThunkInfoTable *thunk_info = itbl_to_thunk_itbl(info);
    if (thunk_info->i.srt) {
        push_closure(q, (StgClosure*)GET_SRT(thunk_info), NULL);
    }
}

static
void push_fun_srt (MarkQueue *q, const StgInfoTable *info)
{
    const StgFunInfoTable *fun_info = itbl_to_fun_itbl(info);
    if (fun_info->i.srt) {
        push_closure(q, (StgClosure*)GET_FUN_SRT(fun_info), NULL);
    }
}

/*********************************************************
 * Pushing to the mark queue
 *********************************************************/

void markQueuePush (MarkQueue *q, const MarkQueueEnt *ent)
{
    push(q, ent);
}

void markQueuePushClosure (MarkQueue *q,
                              StgClosure *p,
                              StgClosure **origin)
{
    push_closure(q, p, origin);
}

/* TODO: Do we really never want to specify the origin here? */
void markQueueAddRoot (MarkQueue* q, StgClosure** root)
{
    markQueuePushClosure(q, *root, NULL);
}

/* Push a closure to the mark queue without origin information */
void markQueuePushClosure_ (MarkQueue *q, StgClosure *p)
{
    markQueuePushClosure(q, p, NULL);
}

void markQueuePushFunSrt (MarkQueue *q, const StgInfoTable *info)
{
    push_fun_srt(q, info);
}

void markQueuePushThunkSrt (MarkQueue *q, const StgInfoTable *info)
{
    push_thunk_srt(q, info);
}

void markQueuePushArray (MarkQueue *q,
                            const StgMutArrPtrs *array,
                            StgWord start_index)
{
    push_array(q, array, start_index);
}

/*********************************************************
 * Popping from the mark queue
 *********************************************************/

// Returns invalid MarkQueueEnt if queue is empty.
static MarkQueueEnt markQueuePop (MarkQueue *q)
{
    MarkQueueBlock *top;

again:
    top = q->top;

    // Are we at the beginning of the block?
    if (top->head == 0) {
        // Is this the first block of the queue?
        if (q->blocks->link == NULL) {
            // Yes, therefore queue is empty...
            MarkQueueEnt none = { .type = NULL_ENTRY };
            return none;
        } else {
            // No, unwind to the previous block and try popping again...
            bdescr *old_block = q->blocks;
            q->blocks = old_block->link;
            q->top = (MarkQueueBlock*)q->blocks->start;
            ACQUIRE_SM_LOCK;
            freeGroup(old_block); // TODO: hold on to a block to avoid repeated allocation/deallocation?
            RELEASE_SM_LOCK;
            goto again;
        }
    }

    top->head--;
    MarkQueueEnt ent = top->entries[top->head];
    return ent;
}

/*********************************************************
 * Creating and destroying MarkQueues
 *********************************************************/

/* Must hold sm_mutex. */
static void init_mark_queue_ (MarkQueue *queue)
{
    bdescr *bd = allocGroup(1);
    queue->blocks = bd;
    queue->top = (MarkQueueBlock *) bd->start;
    queue->top->head = 0;
}

/* Must hold sm_mutex. */
void initMarkQueue (MarkQueue *queue)
{
    init_mark_queue_(queue);
    queue->marked_objects = allocHashTable();
}

void freeMarkQueue (MarkQueue *queue)
{
    bdescr* b = queue->blocks;
    ACQUIRE_SM_LOCK;
    while (b)
    {
        bdescr* b_ = b->link;
        freeGroup(b);
        b = b_;
    }
    RELEASE_SM_LOCK;
    freeHashTable(queue->marked_objects, NULL);
}


/*********************************************************
 * Marking
 *********************************************************/

/*
 * N.B. Mutation of TRecHeaders is completely unprotected by any write
 * barrier. Consequently it's quite important that we deeply mark
 * any outstanding transactions.
 */
static void mark_trec_header (MarkQueue *queue, StgTRecHeader *trec)
{
    while (trec != NO_TREC) {
        StgTRecChunk *chunk = trec->current_chunk;
        markQueuePushClosure_(queue, (StgClosure *) trec);
        markQueuePushClosure_(queue, (StgClosure *) chunk);
        while (chunk != END_STM_CHUNK_LIST) {
            for (StgWord i=0; i < chunk->next_entry_idx; i++) {
                TRecEntry *ent = &chunk->entries[i];
                markQueuePushClosure_(queue, (StgClosure *) ent->tvar);
                markQueuePushClosure_(queue, ent->expected_value);
                markQueuePushClosure_(queue, ent->new_value);
            }
            chunk = chunk->prev_chunk;
        }
        trec = trec->enclosing_trec;
    }
}

static void mark_tso (MarkQueue *queue, StgTSO *tso)
{
    // TODO: Clear dirty if contains only old gen objects

    if (tso->bound != NULL) {
        markQueuePushClosure_(queue, (StgClosure *) tso->bound->tso);
    }

    markQueuePushClosure_(queue, (StgClosure *) tso->blocked_exceptions);
    markQueuePushClosure_(queue, (StgClosure *) tso->bq);
    mark_trec_header(queue, tso->trec);
    markQueuePushClosure_(queue, (StgClosure *) tso->stackobj);
    markQueuePushClosure_(queue, (StgClosure *) tso->_link);
    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
        || tso->why_blocked == BlockedOnBlackHole
        || tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
        ) {
        markQueuePushClosure_(queue, tso->block_info.closure);
    }
}

static void
do_push_closure (StgClosure **p, void *user)
{
    MarkQueue *queue = (MarkQueue *) user;
    // TODO: Origin? need reference to containing closure
    markQueuePushClosure_(queue, *p);
}

static void
mark_large_bitmap (MarkQueue *queue,
                   StgClosure **p,
                   StgLargeBitmap *large_bitmap,
                   StgWord size)
{
    walk_large_bitmap(do_push_closure, p, large_bitmap, size, queue);
}

static void
mark_small_bitmap (MarkQueue *queue, StgClosure **p, StgWord size, StgWord bitmap)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            // TODO: Origin?
            markQueuePushClosure(queue, *p, NULL);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
}

static GNUC_ATTR_HOT
void mark_PAP_payload (MarkQueue *queue,
                       StgClosure *fun,
                       StgClosure **payload,
                       StgWord size)
{
    const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    StgPtr p = (StgPtr) payload;

    StgWord bitmap;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        mark_large_bitmap(queue, payload, GET_FUN_LARGE_BITMAP(fun_info), size);
        break;
    case ARG_BCO:
        mark_large_bitmap(queue, payload, BCO_BITMAP(fun), size);
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        mark_small_bitmap(queue, (StgClosure **) p, size, bitmap);
        break;
    }
}

/* Helper for mark_stack; returns next stack frame. */
static StgPtr
mark_arg_block (MarkQueue *queue, const StgFunInfoTable *fun_info, StgClosure **args)
{
    StgWord bitmap, size;

    StgPtr p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        mark_large_bitmap(queue, (StgClosure**)p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        mark_small_bitmap(queue, (StgClosure**)p, size, bitmap);
        p += size;
        break;
    }
    return p;
}

static GNUC_ATTR_HOT void
mark_stack_ (MarkQueue *queue, StgPtr sp, StgPtr spBottom)
{
    ASSERT(sp <= spBottom);

    while (sp < spBottom) {
        const StgRetInfoTable *info = get_ret_itbl((StgClosure *)sp);
        switch (info->i.type) {
        case UPDATE_FRAME:
        {
            // See Note [upd-black-hole] in rts/Scav.c
            StgUpdateFrame *frame = (StgUpdateFrame *) sp;
            markQueuePushClosure_(queue, frame->updatee);
            sp += sizeofW(StgUpdateFrame);
            continue;
        }

            // small bitmap (< 32 entries, or 64 on a 64-bit machine)
        case CATCH_STM_FRAME:
        case CATCH_RETRY_FRAME:
        case ATOMICALLY_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case CATCH_FRAME:
        case RET_SMALL:
        {
            StgWord bitmap = BITMAP_BITS(info->i.layout.bitmap);
            StgWord size   = BITMAP_SIZE(info->i.layout.bitmap);
            // NOTE: the payload starts immediately after the info-ptr, we
            // don't have an StgHeader in the same sense as a heap closure.
            sp++;
            mark_small_bitmap(queue, (StgClosure **) sp, size, bitmap);
            sp += size;
        }
        follow_srt:
            if (info->i.srt) {
                markQueuePushClosure_(queue, (StgClosure*)GET_SRT(info));
            }
            continue;

        case RET_BCO: {
            sp++;
            markQueuePushClosure_(queue, *(StgClosure**)sp);
            StgBCO *bco = (StgBCO *)*sp;
            sp++;
            StgWord size = BCO_BITMAP_SIZE(bco);
            mark_large_bitmap(queue, (StgClosure **) sp, BCO_BITMAP(bco), size);
            sp += size;
            continue;
        }

          // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
        case RET_BIG:
        {
            StgWord size;

            size = GET_LARGE_BITMAP(&info->i)->size;
            sp++;
            mark_large_bitmap(queue, (StgClosure **) sp, GET_LARGE_BITMAP(&info->i), size);
            sp += size;
            // and don't forget to follow the SRT
            goto follow_srt;
        }

        case RET_FUN:
        {
            StgRetFun *ret_fun = (StgRetFun *)sp;
            const StgFunInfoTable *fun_info;

            markQueuePushClosure_(queue, ret_fun->fun);
            fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
            sp = mark_arg_block(queue, fun_info, ret_fun->payload);
            goto follow_srt;
        }

        default:
            barf("mark_stack: weird activation record found on stack: %d", (int)(info->i.type));
        }
    }
}

static GNUC_ATTR_HOT void
mark_stack (MarkQueue *queue, StgStack *stack)
{
    // TODO: Clear dirty if contains only old gen objects

    mark_stack_(queue, stack->sp, stack->stack + stack->stack_size);
}

static GNUC_ATTR_HOT void
mark_closure (MarkQueue *queue, StgClosure *p, StgClosure **origin)
{
    (void)origin; // TODO: should be used for selector/thunk optimisations

 try_again:
    p = UNTAG_CLOSURE(p);

#   define PUSH_FIELD(obj, field)                                \
        markQueuePushClosure(queue,                           \
                                (StgClosure *) (obj)->field,     \
                                (StgClosure **) &(obj)->field)

    if (!HEAP_ALLOCED_GC(p)) {
        const StgInfoTable *info = get_itbl(p);
        StgHalfWord type = info->type;

        if (type == CONSTR_0_1 || type == CONSTR_0_2 || type == CONSTR_NOCAF) {
            // no need to put these on the static linked list, they don't need
            // to be marked.
            return;
        }

        if (lookupHashTable(queue->marked_objects, (W_)p)) {
            // already marked
            return;
        }

        insertHashTable(queue->marked_objects, (W_)p, (P_)1);

        switch (type) {

        case THUNK_STATIC:
            if (info->srt != 0) {
                markQueuePushThunkSrt(queue, info); // TODO this function repeats the check above
            }
            return;

        case FUN_STATIC:
            if (info->srt != 0 || info->layout.payload.ptrs != 0) {
                markQueuePushFunSrt(queue, info); // TODO this function repeats the check above

                // a FUN_STATIC can also be an SRT, so it may have pointer
                // fields.  See Note [SRTs] in CmmBuildInfoTables, specifically
                // the [FUN] optimisation.
                // TODO (osa) I don't understand this comment
                for (StgHalfWord i = 0; i < info->layout.payload.ptrs; ++i) {
                    PUSH_FIELD(p, payload[i]);
                }
            }
            return;

        case IND_STATIC:
            PUSH_FIELD((StgInd *) p, indirectee);
            return;

        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_2_0:
        case CONSTR_1_1:
            for (StgHalfWord i = 0; i < info->layout.payload.ptrs; ++i) {
                PUSH_FIELD(p, payload[i]);
            }
            return;

        case WHITEHOLE:
            while (get_itbl(p)->type == WHITEHOLE);
                // busy_wait_nop(); // FIXME
            goto try_again;

        default:
            barf("mark_closure(static): strange closure type %d", (int)(info->type));
        }
    }

    bdescr *bd = Bdescr((StgPtr) p);

    if (bd->gen != oldest_gen) {
        // Here we have an object living outside of the non-moving heap. Since
        // we moved everything to the non-moving heap before starting the major
        // collection, we know that we don't need to trace it: it was allocated
        // after we took our snapshot.

        // This should never happen in the non-concurrent case
        barf("Closure outside of non-moving heap: %p", p);
    }

    ASSERTM(LOOKS_LIKE_CLOSURE_PTR(p), "invalid closure, info=%p", p->header.info);

    ASSERT(!IS_FORWARDING_PTR(p->header.info));

    if (bd->flags & BF_NONMOVING) {

        if (bd->flags & BF_LARGE) {
            if (! (bd->flags & BF_NONMOVING_SWEEPING)) {
                // Not in the snapshot
                return;
            }
            if (bd->flags & BF_MARKED) {
                return;
            }

            // Mark contents
            p = (StgClosure*)bd->start;
        } else {
            struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
            nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) p);

            /* We don't mark blocks that,
             *  - were not live at the time that the snapshot was taken, or
             *  - we have already marked this cycle
             */
            uint8_t mark = nonmovingGetMark(seg, block_idx);
            /* Don't mark things we've already marked (since we may loop) */
            if (mark == nonmovingMarkEpoch)
                return;

            StgClosure *snapshot_loc =
              (StgClosure *) nonmovingSegmentGetBlock(seg, seg->next_free_snap);
            if (p >= snapshot_loc && mark == 0) {
                /*
                 * In this case we are looking at a block that wasn't allocated
                 * at the time that the snapshot was taken. We mustn't trace
                 * things above the allocation pointer that aren't marked since
                 * they may not be valid objects.
                 */
                return;
            }
        }
    }

    // A pinned object that is still attached to a capability (because it's not
    // filled yet). No need to trace it pinned objects can't contain poiners.
    else if (bd->flags & BF_PINNED) {
#if defined(DEBUG)
        bool found_it = false;
        for (uint32_t i = 0; i < n_capabilities; ++i) {
            if (capabilities[i]->pinned_object_block == bd) {
                found_it = true;
                break;
            }
        }
        ASSERT(found_it);
#endif
        return;
    }

    else {
        barf("Strange closure in nonmoving mark: %p", p);
    }

    /////////////////////////////////////////////////////
    // Trace pointers
    /////////////////////////////////////////////////////

    const StgInfoTable *info = get_itbl(p);
    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY: {
        StgMVar *mvar = (StgMVar *) p;
        PUSH_FIELD(mvar, head);
        PUSH_FIELD(mvar, tail);
        PUSH_FIELD(mvar, value);
        break;
    }

    case TVAR: {
        StgTVar *tvar = ((StgTVar *)p);
        PUSH_FIELD(tvar, current_value);
        PUSH_FIELD(tvar, first_watch_queue_entry);
        break;
    }

    case FUN_2_0:
        markQueuePushFunSrt(queue, info);
        PUSH_FIELD(p, payload[1]);
        PUSH_FIELD(p, payload[0]);
        break;

    case THUNK_2_0: {
        StgThunk *thunk = (StgThunk *) p;
        markQueuePushThunkSrt(queue, info);
        PUSH_FIELD(thunk, payload[1]);
        PUSH_FIELD(thunk, payload[0]);
        break;
    }

    case CONSTR_2_0:
        PUSH_FIELD(p, payload[1]);
        PUSH_FIELD(p, payload[0]);
        break;

    case THUNK_1_0:
        markQueuePushThunkSrt(queue, info);
        PUSH_FIELD((StgThunk *) p, payload[0]);
        break;

    case FUN_1_0:
        markQueuePushFunSrt(queue, info);
        PUSH_FIELD(p, payload[0]);
        break;

    case CONSTR_1_0:
        PUSH_FIELD(p, payload[0]);
        break;

    case THUNK_0_1:
        markQueuePushThunkSrt(queue, info);
        break;

    case FUN_0_1:
        markQueuePushFunSrt(queue, info);
        break;

    case CONSTR_0_1:
    case CONSTR_0_2:
        break;

    case THUNK_0_2:
        markQueuePushThunkSrt(queue, info);
        break;

    case FUN_0_2:
        markQueuePushFunSrt(queue, info);
        break;

    case THUNK_1_1:
        markQueuePushThunkSrt(queue, info);
        PUSH_FIELD((StgThunk *) p, payload[0]);
        break;

    case FUN_1_1:
        markQueuePushFunSrt(queue, info);
        PUSH_FIELD(p, payload[0]);
        break;

    case CONSTR_1_1:
        PUSH_FIELD(p, payload[0]);
        break;

    case FUN:
        markQueuePushFunSrt(queue, info);
        goto gen_obj;

    case THUNK: {
        markQueuePushThunkSrt(queue, info);
        for (StgWord i = 0; i < info->layout.payload.ptrs; i++) {
            StgClosure **field = &((StgThunk *) p)->payload[i];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    gen_obj:
    case CONSTR:
    case CONSTR_NOCAF:
    case WEAK:
    case PRIM:
    {
        for (StgWord i = 0; i < info->layout.payload.ptrs; i++) {
            StgClosure **field = &((StgClosure *) p)->payload[i];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        PUSH_FIELD(bco, instrs);
        PUSH_FIELD(bco, literals);
        PUSH_FIELD(bco, ptrs);
        break;
    }


    case IND:
    case BLACKHOLE:
        PUSH_FIELD((StgInd *) p, indirectee);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        PUSH_FIELD((StgMutVar *)p, var);
        break;

    case BLOCKING_QUEUE: {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;
        PUSH_FIELD(bq, bh);
        PUSH_FIELD(bq, owner);
        PUSH_FIELD(bq, queue);
        PUSH_FIELD(bq, link);
        break;
    }

    case THUNK_SELECTOR:
        PUSH_FIELD((StgSelector *) p, selectee);
        // TODO: selector optimization
        break;

    case AP_STACK: {
        StgAP_STACK *ap = (StgAP_STACK *)p;
        PUSH_FIELD(ap, fun);
        mark_stack_(queue, (StgPtr) ap->payload, (StgPtr) ap->payload + ap->size);
        break;
    }

    case PAP: {
        StgPAP *pap = (StgPAP *) p;
        PUSH_FIELD(pap, fun);
        mark_PAP_payload(queue, pap->fun, pap->payload, pap->n_args);
        break;
    }

    case AP: {
        StgAP *ap = (StgAP *) p;
        PUSH_FIELD(ap, fun);
        mark_PAP_payload(queue, ap->fun, ap->payload, ap->n_args);
        break;
    }

    case ARR_WORDS:
        // nothing to follow
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        // TODO: Check this against Scav.c
        markQueuePushArray(queue, (StgMutArrPtrs *) p, 0);
        break;

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY: {
        StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs *) p;
        for (StgWord i = 0; i < arr->ptrs; i++) {
            StgClosure **field = &arr->payload[i];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    case TSO:
        mark_tso(queue, (StgTSO *) p);
        break;

    case STACK: {
        // See Note [StgStack dirtiness flags and concurrent marking]
        StgStack *stack = (StgStack *) p;
        mark_stack(queue, stack);
        break;
    }

    case MUT_PRIM: {
        for (StgHalfWord p_idx = 0; p_idx < info->layout.payload.ptrs; ++p_idx) {
            StgClosure **field = &p->payload[p_idx];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    case TREC_CHUNK: {
        // TODO: Should we abort here? This should have already been marked
        // when we dirtied the TSO
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        PUSH_FIELD(tc, prev_chunk);
        TRecEntry *end = &tc->entries[tc->next_entry_idx];
        for (TRecEntry *e = &tc->entries[0]; e < end; e++) {
            markQueuePushClosure_(queue, (StgClosure *) e->tvar);
            markQueuePushClosure_(queue, (StgClosure *) e->expected_value);
            markQueuePushClosure_(queue, (StgClosure *) e->new_value);
        }
        break;
    }

    case WHITEHOLE:
        while (get_itbl(p)->type == WHITEHOLE);
            // busy_wait_nop(); // FIXME
        goto try_again;

    default:
        barf("mark_closure: unimplemented/strange closure type %d @ %p",
             info->type, p);
    }

#   undef PUSH_FIELD

    /* Set the mark bit: it's important that we do this only after we actually push
     * the object's pointers since in the case of marking stacks there may be a
     * mutator waiting for us to finish so it can start execution.
     */
    if (bd->flags & BF_LARGE) {
        if (! (bd->flags & BF_MARKED)) {
            // Remove the object from nonmoving_large_objects and link it to
            // nonmoving_marked_large_objects
            dbl_link_remove(bd, &nonmoving_large_objects);
            dbl_link_onto(bd, &nonmoving_marked_large_objects);
            n_nonmoving_large_blocks -= bd->blocks;
            n_nonmoving_marked_large_blocks += bd->blocks;
            bd->flags |= BF_MARKED;
        }
    } else {
        // TODO: Kill repetition
        struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
        nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) p);
        nonmovingSetMark(seg, block_idx);
        nonmoving_live_words += nonmovingSegmentBlockSize(seg) / sizeof(W_);
    }
}

/* This is the main mark loop.
 * Invariants:
 *
 *  a. nonmovingPrepareMark has been called.
 *  b. the nursery has been fully evacuated into the non-moving generation.
 *  c. the mark queue has been seeded with a set of roots.
 *
 */
GNUC_ATTR_HOT void nonmovingMark (MarkQueue *queue)
{
    debugTrace(DEBUG_nonmoving_gc, "Starting mark pass");
    unsigned int count = 0;
    while (true) {
        count++;
        MarkQueueEnt ent = markQueuePop(queue);

        switch (ent.type) {
        case MARK_CLOSURE:
            mark_closure(queue, ent.mark_closure.p, ent.mark_closure.origin);
            break;
        case MARK_ARRAY: {
            const StgMutArrPtrs *arr = ent.mark_array.array;
            StgWord start = ent.mark_array.start_index;
            StgWord end = start + MARK_ARRAY_CHUNK_LENGTH;
            if (end < arr->ptrs) {
                markQueuePushArray(queue, ent.mark_array.array, end);
            } else {
                end = arr->ptrs;
            }
            for (StgWord i = start; i < end; i++) {
                markQueuePushClosure_(queue, arr->payload[i]);
            }
            break;
        }
        case NULL_ENTRY:
            // Nothing more to do
            debugTrace(DEBUG_nonmoving_gc, "Finished mark pass: %d", count);
            return;
        }
    }
}

// A variant of `isAlive` that works for non-moving heap. Used for:
//
// - Collecting weak pointers; checking key of a weak pointer.
// - Resurrecting threads; checking if a thread is dead.
// - Sweeping object lists: large_objects, mut_list, stable_name_table.
//
// This may only be used after a full mark but before nonmovingSweep as it
// relies on the correctness of the next_free_snap and mark bitmaps.
bool nonmovingIsAlive (StgClosure *p)
{
    // Ignore static closures. See comments in `isAlive`.
    if (!HEAP_ALLOCED_GC(p)) {
        return true;
    }

    bdescr *bd = Bdescr((P_)p);

    // All non-static objects in the non-moving heap should be marked as
    // BF_NONMOVING
    ASSERT(bd->flags & BF_NONMOVING);

    if (bd->flags & BF_LARGE) {
        return (bd->flags & BF_NONMOVING_SWEEPING) == 0
                   // the large object wasn't in the snapshot and therefore wasn't marked
            || (bd->flags & BF_MARKED) != 0;
                   // The object was marked
    } else {
        struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
        nonmoving_block_idx i = nonmovingGetBlockIdx((StgPtr) p);
        uint8_t mark =  nonmovingGetMark(seg, i);
        if (i >= seg->next_free_snap) {
            // If the object is allocated after next_free_snap then one of the
            // following must be true:
            //
            // * if its mark is 0 then the block was not allocated last time
            //   the segment was swept; however, it may have been allocated since
            //   then and therefore we must conclude that the block is alive.
            //
            // * if its mark is equal to nonmovingMarkEpoch then we found that
            //   the object was alive in the snapshot of the current GC (recall
            //   that this function may only be used after a mark).
            //   Consequently we must conclude that the object is still alive.
            //
            // * if its mark is not equal to nonmovingMarkEpoch then we found
            //   that the object was not reachable in the last snapshot.
            //   Assuming that the mark is complete we can conclude that the
            //   object is dead since the snapshot invariant guarantees that
            //   all objects alive in the snapshot would be marked.
            //
            return mark == nonmovingMarkEpoch || mark == 0;
        } else {
            // If the object is below next_free_snap then the snapshot
            // invariant guarantees that it is marked if reachable.
            return mark == nonmovingMarkEpoch;
        }
    }
}

// Check whether a snapshotted object is alive. That is for an object that we
// know to be in the snapshot, is its mark bit set. It is imperative that the
// object is in the snapshot (e.g. was in the nonmoving heap at the time that
// the snapshot was taken) since we assume that its mark bit reflects its
// reachability.
//
// This is used when
//
// - Collecting weak pointers; checking key of a weak pointer.
// - Resurrecting threads; checking if a thread is dead.
// - Sweeping object lists: large_objects, mut_list, stable_name_table.
//
static bool nonmovingIsNowAlive (StgClosure *p)
{
    // Ignore static closures. See comments in `isAlive`.
    if (!HEAP_ALLOCED_GC(p)) {
        return true;
    }

    bdescr *bd = Bdescr((P_)p);

    // All non-static objects in the non-moving heap should be marked as
    // BF_NONMOVING
    ASSERT(bd->flags & BF_NONMOVING);

    if (bd->flags & BF_LARGE) {
        return (bd->flags & BF_NONMOVING_SWEEPING) == 0
                   // the large object wasn't in the snapshot and therefore wasn't marked
            || (bd->flags & BF_MARKED) != 0;
                   // The object was marked
    } else {
        return nonmovingClosureMarkedThisCycle((P_)p);
    }
}

// Non-moving heap variant of `tidyWeakList`
bool nonmovingTidyWeaks (struct MarkQueue_ *queue)
{
    bool did_work = false;

    StgWeak **last_w = &nonmoving_old_weak_ptr_list;
    StgWeak *next_w;
    for (StgWeak *w = nonmoving_old_weak_ptr_list; w != NULL; w = next_w) {
        if (w->header.info == &stg_DEAD_WEAK_info) {
            // finalizeWeak# was called on the weak
            next_w = w->link;
            *last_w = next_w;
            continue;
        }

        // Otherwise it's a live weak
        ASSERT(w->header.info == &stg_WEAK_info);

        if (nonmovingIsNowAlive(w->key)) {
            nonmovingMarkLiveWeak(queue, w);
            did_work = true;

            // remove this weak ptr from old_weak_ptr list
            *last_w = w->link;
            next_w = w->link;

            // and put it on the weak ptr list
            w->link = nonmoving_weak_ptr_list;
            nonmoving_weak_ptr_list = w;
        } else {
            last_w = &(w->link);
            next_w = w->link;
        }
    }

    return did_work;
}

void nonmovingMarkDeadWeak (struct MarkQueue_ *queue, StgWeak *w)
{
    if (w->cfinalizers != &stg_NO_FINALIZER_closure) {
        markQueuePushClosure_(queue, w->value);
    }
    markQueuePushClosure_(queue, w->finalizer);
}

void nonmovingMarkLiveWeak (struct MarkQueue_ *queue, StgWeak *w)
{
    ASSERT(nonmovingClosureMarkedThisCycle((P_)w));
    markQueuePushClosure_(queue, w->value);
    markQueuePushClosure_(queue, w->finalizer);
    markQueuePushClosure_(queue, w->cfinalizers);
}

// When we're done with marking, any weak pointers with non-marked keys will be
// considered "dead". We mark values and finalizers of such weaks, and then
// schedule them for finalization in `scheduleFinalizers` (which we run during
// synchronization).
void nonmovingMarkDeadWeaks (struct MarkQueue_ *queue, StgWeak **dead_weaks)
{
    StgWeak *next_w;
    for (StgWeak *w = nonmoving_old_weak_ptr_list; w; w = next_w) {
        ASSERT(!nonmovingClosureMarkedThisCycle((P_)(w->key)));
        nonmovingMarkDeadWeak(queue, w);
        next_w = w ->link;
        w->link = *dead_weaks;
        *dead_weaks = w;
    }
}

// Non-moving heap variant of of `tidyThreadList`
void nonmovingTidyThreads ()
{
    StgTSO *next;
    StgTSO **prev = &nonmoving_old_threads;
    for (StgTSO *t = nonmoving_old_threads; t != END_TSO_QUEUE; t = next) {

        next = t->global_link;

        // N.B. This thread is in old_threads, consequently we *know* it is in
        // the snapshot and it is therefore safe to rely on the bitmap to
        // determine its reachability.
        if (nonmovingIsNowAlive((StgClosure*)t)) {
            // alive
            *prev = next;

            // move this thread onto threads list
            t->global_link = nonmoving_threads;
            nonmoving_threads = t;
        } else {
            // not alive (yet): leave this thread on the old_threads list
            prev = &(t->global_link);
        }
    }
}

void nonmovingResurrectThreads (struct MarkQueue_ *queue, StgTSO **resurrected_threads)
{
    StgTSO *next;
    for (StgTSO *t = nonmoving_old_threads; t != END_TSO_QUEUE; t = next) {
        next = t->global_link;

        switch (t->what_next) {
        case ThreadKilled:
        case ThreadComplete:
            continue;
        default:
            markQueuePushClosure_(queue, (StgClosure*)t);
            t->global_link = *resurrected_threads;
            *resurrected_threads = t;
        }
    }
}

#if defined(DEBUG)

void printMarkQueueEntry (MarkQueueEnt *ent)
{
    if (ent->type == MARK_CLOSURE) {
        debugBelch("Closure: ");
        printClosure(ent->mark_closure.p);
    } else if (ent->type == MARK_ARRAY) {
        debugBelch("Array\n");
    } else {
        debugBelch("End of mark\n");
    }
}

void printMarkQueue (MarkQueue *q)
{
    debugBelch("======== MARK QUEUE ========\n");
    for (bdescr *block = q->blocks; block; block = block->link) {
        MarkQueueBlock *queue = (MarkQueueBlock*)block->start;
        for (uint32_t i = 0; i < queue->head; ++i) {
            printMarkQueueEntry(&queue->entries[i]);
        }
    }
    debugBelch("===== END OF MARK QUEUE ====\n");
}

#endif
