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

void
nonmovingScavengeOne (StgClosure *q)
{
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

            // Note [Dirty flags in the non-moving collector] in NonMoving.c
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

            // Note [Dirty flags in the non-moving collector] in NonMoving.c
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

            // Note [Dirty flags in the non-moving collector] in NonMoving.c
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

    StgPtr scan_end = (P_)nonmovingSegmentGetBlock(seg, seg->next_free);
    if (seg_block->u.scan == scan_end)
        return;

    nonmoving_block_idx p_idx = nonmovingGetBlockIdx(seg_block->u.scan);
    while (seg_block->u.scan < scan_end) {
        StgClosure *p = (StgClosure*)seg_block->u.scan;

        // bit set = was allocated in a previous GC, no need to scavenge
        // bit not set = new allocation, so scavenge
        if (nonmovingGetMark(seg, p_idx) == 0) {
            nonmovingScavengeOne(p);
        }

        p_idx++;
        seg_block->u.scan = (P_)(((uint8_t*)seg_block->u.scan) + blk_size);
    }
}
