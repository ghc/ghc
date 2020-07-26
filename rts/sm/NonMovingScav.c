#include "Rts.h"
#include "RtsUtils.h"
#include "NonMoving.h"
#include "NonMovingScav.h"
#include "Capability.h"
#include "Scav.h"
#include "Evac.h"
#include "GCThread.h" // for GCUtils.h
#include "GCUtils.h"
#include "Printer.h"
#include "MarkWeak.h" // scavengeLiveWeak

/*
 * Note [Scavenging the non-moving heap]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The moving GC promotes objects from the moving heap into the non-moving heap
 * via evacuation and subsequent scavenging. This of course raises the question
 * of how to keep track of which objects still require scavenging. The story
 * here is as follows:
 *
 *  - each GC thread maintains a list, `todo_seg`, of "todo" segments which
 *    still have outstanding scavenging work
 *  - similar to the moving collector, we use the scan pointer of a segment's
 *    block descriptor to track the first yet-to-be-scavenged object.
 *  - the first time we evacuate into an segment during a GC, we push the
 *    segment onto the `todo_seg` list
 *  - scavenge_find_work checks `todo_seg` as a source of scavenging work
 *
 * The scan pointer requires very careful treatment here. Specifically, we must
 * only scavenge objects which we evacuated in the current GC to avoid issues
 * like #21885. objects evacuated to the non-moving generation. In particular,
 * objects can be allocated into the non-moving generation by two ways:
 *
 *  a. evacuation out of from-space by the garbage collector
 *  b. direct allocation by the mutator
 *
 * Like all evacuation, objects moved by (a) must be scavenged, since they
 * may contain references to other objects located in from-space..
 * However, we must not neglect to consider objects allocated by path (b).
 * In short, the problem is that objects directly allocated by the mutator
 * may become unreachable (but not swept, since the containing segment is
 * not yet full), at which point they may contain references to swept objects.
 * Specifically, we observed this in #21885 in the following way:
 *
 * 1. the mutator (specifically in #21885, a `lockCAF`) allocates an object
 *    (specifically a blackhole, which here we will call `blkh`; see Note
 *    [Static objects under the nonmoving collector] for the reason why) on
 *    the non-moving heap. The bitmap of the allocated block remains 0
 *    (since allocation doesn't affect the bitmap) and the containing
 *    segment's (which we will call `blkh_seg`) `next_free` is advanced.
 * 2. We enter the blackhole, evaluating the blackhole to produce a result
 *    (specificaly a cons cell) in the nursery
 * 3. The blackhole gets updated into an indirection pointing to the cons
 *    cell; it is pushed to the generational remembered set
 * 4. we perform a GC, the cons cell is evacuated into the nonmoving heap
 *    (into segment `cons_seg`)
 * 5. the cons cell is marked
 * 6. the GC concludes
 * 7. the CAF and blackhole become unreachable
 * 8. `cons_seg` is filled
 * 9. we start another GC; the cons cell is swept
 * 10. we start a new GC
 * 11. something is evacuated into `blkh_seg`, adding it to the "todo" list
 * 12. we attempt to scavenge `blkh_seg` (namely, all unmarked blocks
 *     between `scan` and `next_free`, which includes `blkh`). We attempt to
 *     evacuate `blkh`'s indirectee, which is the previously-swept cons cell.
 *     This is unsafe, since the indirectee is no longer a valid heap
 *     object.
 *
 * The problem here was that the scavenging logic previously assumed that (a)
 * was the only source of allocations into the non-moving heap and therefore
 * *all* unmarked blocks between `scan` and `next_free` were evacuated.
 * However, due to (b) this is not true, since the scan pointer was only
 * updated (1) when the segment was initialized (to point to block 0),
 * and (2) when an object is scavenged (by advancing it to the next block).
 * Consequently, at the beginning of a GC `scan` may point a block which was
 * allocated by the mutator since the last GC.
 *
 * The solution is to ensure that that the scanned region only encompasses
 * the region of objects allocated for evacuation during the present GC. We do
 * this by updating `scan` as we push the segment to the todo-segment list to
 * point to the block which was evacuated into.
 *
 */

void
nonmovingScavengeOne (StgClosure *q0)
{
    StgClosure *q = q0;

    // N.B. There may be a gap before the first word of the closure in the case
    // of an aligned ByteArray# as allocated by allocatePinned().
    // See Note [Allocating pinned objects into the non-moving heap].
    while (*(StgPtr*) q == NULL)
        q = (StgClosure *) ((StgPtr*) q + 1);

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

    StgPtr p = (StgPtr)q;
    const StgInfoTable *info = get_itbl(q);
    const bool saved_eager_promotion = gct->eager_promotion;

    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = ((StgMVar *)p);
        gct->eager_promotion = false;
        evacuate((StgClosure **)&mvar->head);
        evacuate((StgClosure **)&mvar->tail);
        evacuate((StgClosure **)&mvar->value);
        gct->eager_promotion = saved_eager_promotion;
        if (gct->failed_to_evac) {
            mvar->header.info = &stg_MVAR_DIRTY_info;

            // See Note [Dirty flags in the non-moving collector] in NonMoving.c
            markQueuePushClosureGC(&gct->cap->upd_rem_set.queue, (StgClosure *) mvar->head);
            markQueuePushClosureGC(&gct->cap->upd_rem_set.queue, (StgClosure *) mvar->tail);
            markQueuePushClosureGC(&gct->cap->upd_rem_set.queue, (StgClosure *) mvar->value);
        } else {
            mvar->header.info = &stg_MVAR_CLEAN_info;
        }
        break;
    }

    case TVAR:
    {
        StgTVar *tvar = ((StgTVar *)p);
        gct->eager_promotion = false;
        evacuate((StgClosure **)&tvar->current_value);
        evacuate((StgClosure **)&tvar->first_watch_queue_entry);
        gct->eager_promotion = saved_eager_promotion;
        if (gct->failed_to_evac) {
            tvar->header.info = &stg_TVAR_DIRTY_info;

            // See Note [Dirty flags in the non-moving collector] in NonMoving.c
            markQueuePushClosureGC(&gct->cap->upd_rem_set.queue, (StgClosure *) tvar->current_value);
            markQueuePushClosureGC(&gct->cap->upd_rem_set.queue, (StgClosure *) tvar->first_watch_queue_entry);
        } else {
            tvar->header.info = &stg_TVAR_CLEAN_info;
        }
        break;
    }

    case FUN_2_0:
        scavenge_fun_srt(info);
        evacuate(&((StgClosure *)p)->payload[1]);
        evacuate(&((StgClosure *)p)->payload[0]);
        break;

    case THUNK_2_0:
        scavenge_thunk_srt(info);
        evacuate(&((StgThunk *)p)->payload[1]);
        evacuate(&((StgThunk *)p)->payload[0]);
        break;

    case CONSTR_2_0:
        evacuate(&((StgClosure *)p)->payload[1]);
        evacuate(&((StgClosure *)p)->payload[0]);
        break;

    case THUNK_1_0:
        scavenge_thunk_srt(info);
        evacuate(&((StgThunk *)p)->payload[0]);
        break;

    case FUN_1_0:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_1_0:
        evacuate(&((StgClosure *)p)->payload[0]);
        break;

    case THUNK_0_1:
        scavenge_thunk_srt(info);
        break;

    case FUN_0_1:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_0_1:
        break;

    case THUNK_0_2:
        scavenge_thunk_srt(info);
        break;

    case FUN_0_2:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_0_2:
        break;

    case THUNK_1_1:
        scavenge_thunk_srt(info);
        evacuate(&((StgThunk *)p)->payload[0]);
        break;

    case FUN_1_1:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_1_1:
        evacuate(&q->payload[0]);
        break;

    case FUN:
        scavenge_fun_srt(info);
        goto gen_obj;

    case THUNK:
    {
        scavenge_thunk_srt(info);
        StgPtr end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
            evacuate((StgClosure **)p);
        }
        break;
    }

    case WEAK:
    {
        // We must evacuate the key since it may refer to an object in the
        // moving heap which may be long gone by the time we call
        // nonmovingTidyWeaks.
        StgWeak *weak = (StgWeak *) p;
        gct->eager_promotion = true;
        evacuate(&weak->key);
        gct->eager_promotion = saved_eager_promotion;
        goto gen_obj;
    }

    gen_obj:
    case CONSTR:
    case CONSTR_NOCAF:
    case PRIM:
    {
        StgPtr end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            evacuate((StgClosure **)p);
        }
        break;
    }

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        evacuate((StgClosure **)&bco->instrs);
        evacuate((StgClosure **)&bco->literals);
        evacuate((StgClosure **)&bco->ptrs);
        break;
    }

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
        StgMutVar *mv = (StgMutVar *) p;
        gct->eager_promotion = false;
        evacuate(&mv->var);
        gct->eager_promotion = saved_eager_promotion;
        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;

            // See Note [Dirty flags in the non-moving collector] in NonMoving.c
            markQueuePushClosureGC(&gct->cap->upd_rem_set.queue, (StgClosure *) mv->var);
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
        }
        break;
    }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        gct->eager_promotion = false;
        evacuate(&bq->bh);
        evacuate((StgClosure**)&bq->owner);
        evacuate((StgClosure**)&bq->queue);
        evacuate((StgClosure**)&bq->link);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
        } else {
            bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
        }
        break;
    }

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        evacuate(&s->selectee);
        break;
    }

    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)p;

        evacuate(&ap->fun);
        scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
        break;
    }

    case PAP:
        p = scavenge_PAP((StgPAP *)p);
        break;

    case AP:
        scavenge_AP((StgAP *)p);
        break;

    case ARR_WORDS:
        // nothing to follow
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
        gct->eager_promotion = false;
        scavenge_mut_arr_ptrs((StgMutArrPtrs*)p);
        gct->eager_promotion = saved_eager_promotion;
        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
        }
        gct->failed_to_evac = true; // always put it on the mutable list.
        break;
    }

    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        // follow everything
    {
        scavenge_mut_arr_ptrs((StgMutArrPtrs*)p);

        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info;
        }
        break;
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
        // follow everything
    {
        StgPtr next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        gct->eager_promotion = false;
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_CLEAN_info;
        }
        gct->failed_to_evac = true; // always put it on the mutable list.
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        // follow everything
    {
        StgPtr next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }

        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info;
        }
        break;
    }

    case TSO:
    {
        scavengeTSO((StgTSO *)p);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        gct->eager_promotion = false;
        scavenge_stack(stack->sp, stack->stack + stack->stack_size);
        gct->eager_promotion = saved_eager_promotion;
        stack->dirty = gct->failed_to_evac;
        break;
    }

    case MUT_PRIM:
    {
        StgPtr end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        gct->eager_promotion = false;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            evacuate((StgClosure **)p);
        }
        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true; // mutable
        break;
    }

    case TREC_CHUNK:
      {
        StgWord i;
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        TRecEntry *e = &(tc -> entries[0]);
        gct->eager_promotion = false;
        evacuate((StgClosure **)&tc->prev_chunk);
        for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          evacuate((StgClosure **)&e->tvar);
          evacuate((StgClosure **)&e->expected_value);
          evacuate((StgClosure **)&e->new_value);
        }
        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true; // mutable
        break;
      }

    case IND:
    case BLACKHOLE:
    case IND_STATIC:
        evacuate(&((StgInd *)p)->indirectee);
        break;

    case COMPACT_NFDATA:
        scavenge_compact((StgCompactNFData*)p);
        break;

    case CONTINUATION:
        scavenge_continuation((StgContinuation *)p);
        break;

    default:
        barf("nonmoving scavenge: unimplemented/strange closure type %d @ %p",
             info->type, p);
    }

    if (gct->failed_to_evac) {
        // Mutable object or points to a younger object, add to the mut_list
        gct->failed_to_evac = false;
        if (oldest_gen->no > 0) {
            recordMutableGen_GC(q, oldest_gen->no);
        }
    }
}

/* Scavenge objects evacuated into a nonmoving segment by a minor GC */
void
scavengeNonmovingSegment (struct NonmovingSegment *seg)
{
    const StgWord blk_size = nonmovingSegmentBlockSize(seg);
    gct->evac_gen_no = oldest_gen->no;
    gct->failed_to_evac = false;

    // scavenge objects between scan and free_ptr whose bitmap bits are 0
    bdescr *seg_block = Bdescr((P_)seg);

    ASSERT(seg_block->u.scan >= (P_)nonmovingSegmentGetBlock(seg, 0));
    ASSERT(seg_block->u.scan <= (P_)nonmovingSegmentGetBlock(seg, seg->next_free));

    StgPtr scan = seg_block->u.scan;
    StgPtr scan_end = (P_)nonmovingSegmentGetBlock(seg, seg->next_free);
    if (scan == scan_end)
        return;

    // At this point the segment is not on the todo_seg list. Consequently, we
    // may need to re-add it during scavenging (as we may evacuate a new object
    // to this segment); this has the effect of updating the scan pointer.
    // We must therefore take care to move the scan pointer to the end of the
    // scanned region *before* doing any scavenging.
    seg_block->u.scan = scan_end;

    nonmoving_block_idx p_idx = nonmovingGetBlockIdx(scan);
    while (scan < scan_end) {
        StgClosure *p = (StgClosure*)scan;

        // bit set = was allocated in a previous GC, no need to scavenge
        // bit not set = new allocation, so scavenge
        if (nonmovingGetMark(seg, p_idx) == 0) {
            nonmovingScavengeOne(p);
        }

        scan = (StgPtr) ((uint8_t*) scan + blk_size);
        p_idx++;
    }
}
