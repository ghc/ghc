/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2017
 *
 * Introspection into GHC's heap representation
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"

#include "Capability.h"
#include "Printer.h"
#include "AllocArray.h"

StgWord heap_view_closureSize(StgClosure *closure) {
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));
    return closure_sizeW(closure);
}

static void
heap_view_closure_ptrs_in_large_bitmap(StgClosure *ptrs[], StgWord *nptrs
                        , StgClosure **p, StgLargeBitmap *large_bitmap
                        , uint32_t size )
{
    uint32_t i, j, b;
    StgWord bitmap;

    b = 0;

    for (i = 0; i < size; b++) {
        bitmap = large_bitmap->bitmap[b];
        j = stg_min(size-i, BITS_IN(W_));
        i += j;
        for (; j > 0; j--, p++) {
            if ((bitmap & 1) == 0) {
                ptrs[(*nptrs)++] = *p;
            }
            bitmap = bitmap >> 1;
        }
    }
}

void heap_view_closure_ptrs_in_pap_payload(StgClosure *ptrs[], StgWord *nptrs
                      , StgClosure *fun, StgClosure **payload, StgWord size) {
    StgWord bitmap;
    const StgFunInfoTable *fun_info;

    fun_info = get_fun_itbl(UNTAG_CLOSURE(fun));
    // ASSERT(fun_info->i.type != PAP);
    StgClosure **p = payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        heap_view_closure_ptrs_in_large_bitmap(ptrs, nptrs, payload,
                                         GET_FUN_LARGE_BITMAP(fun_info), size);
        break;
    case ARG_BCO:
        heap_view_closure_ptrs_in_large_bitmap(ptrs, nptrs, payload,
                                               BCO_BITMAP(fun), size);
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        while (size > 0) {
            if ((bitmap & 1) == 0) {
                ptrs[(*nptrs)++] = *p;
            }
            bitmap = bitmap >> 1;
            p++;
            size--;
        }
        break;
    }
}

// See Heap.h
StgWord collect_pointers(StgClosure *closure, StgClosure *ptrs[]) {
    StgClosure **end;
    const StgInfoTable *info = get_itbl(closure);
    StgWord nptrs = 0;
    StgWord i;

    switch (info->type) {
        case INVALID_OBJECT:
            barf("Invalid Object");
            break;

        // No pointers
        case ARR_WORDS:
        case STACK:
            break;

        // Default layout
        case CONSTR_1_0:
        case CONSTR_0_1:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_0_2:
        case CONSTR:
        case CONSTR_NOCAF:


        case PRIM:

        case FUN:
        case FUN_1_0:
        case FUN_0_1:
        case FUN_1_1:
        case FUN_2_0:
        case FUN_0_2:
        case FUN_STATIC:
            end = closure->payload + info->layout.payload.ptrs;
            for (StgClosure **ptr = closure->payload; ptr < end; ptr++) {
                ptrs[nptrs++] = *ptr;
            }
            break;

        case THUNK:
        case THUNK_1_0:
        case THUNK_0_1:
        case THUNK_1_1:
        case THUNK_2_0:
        case THUNK_0_2:
        case THUNK_STATIC:
            end = ((StgThunk *)closure)->payload + info->layout.payload.ptrs;
            for (StgClosure **ptr = ((StgThunk *)closure)->payload; ptr < end; ptr++) {
                ptrs[nptrs++] = *ptr;
            }
            break;

        case THUNK_SELECTOR:
            ptrs[nptrs++] = ((StgSelector *)closure)->selectee;
            break;

        case AP:
            ptrs[nptrs++] = ((StgAP *)closure)->fun;
            heap_view_closure_ptrs_in_pap_payload(ptrs, &nptrs,
                ((StgAP *)closure)->fun,
                ((StgAP *)closure)->payload,
                ((StgAP *)closure)->n_args);
            break;

        case PAP:
            ptrs[nptrs++] = ((StgPAP *)closure)->fun;
            heap_view_closure_ptrs_in_pap_payload(ptrs, &nptrs,
                ((StgPAP *)closure)->fun,
                ((StgPAP *)closure)->payload,
                ((StgPAP *)closure)->n_args);
            break;

        case AP_STACK:
            ptrs[nptrs++] = ((StgAP_STACK *)closure)->fun;
            /*
              The payload is a stack, which consists of a mixture of pointers
              and non-pointers.  We can't simply pretend it's all pointers,
              because that will cause crashes in the GC later.  We could
              traverse the stack and extract pointers and non-pointers, but that
              would be complicated, so let's just ignore the payload for now.
              See #15375.
            */
            break;

        case BCO:
            ptrs[nptrs++] = (StgClosure *)((StgBCO *)closure)->instrs;
            ptrs[nptrs++] = (StgClosure *)((StgBCO *)closure)->literals;
            ptrs[nptrs++] = (StgClosure *)((StgBCO *)closure)->ptrs;
            break;

        case IND:
        case IND_STATIC:
        case BLACKHOLE:
            ptrs[nptrs++] = (StgClosure *) ACQUIRE_LOAD(&((StgInd *)closure)->indirectee);
            break;

        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
        case MUT_ARR_PTRS_FROZEN_CLEAN:
        case MUT_ARR_PTRS_FROZEN_DIRTY:
            for (i = 0; i < ((StgMutArrPtrs *)closure)->ptrs; ++i) {
                ptrs[nptrs++] = ((StgMutArrPtrs *)closure)->payload[i];
            }
            break;

        case SMALL_MUT_ARR_PTRS_CLEAN:
        case SMALL_MUT_ARR_PTRS_DIRTY:
        case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
        case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
            for (i = 0; i < ((StgSmallMutArrPtrs *)closure)->ptrs; ++i) {
                ptrs[nptrs++] = ((StgSmallMutArrPtrs *)closure)->payload[i];
            }
            break;

        case MUT_VAR_CLEAN:
        case MUT_VAR_DIRTY:
            ptrs[nptrs++] = ((StgMutVar *)closure)->var;
            break;
        case MVAR_DIRTY:
        case MVAR_CLEAN:
            ptrs[nptrs++] = (StgClosure *)((StgMVar *)closure)->head;
            ptrs[nptrs++] = (StgClosure *)((StgMVar *)closure)->tail;
            ptrs[nptrs++] = ((StgMVar *)closure)->value;
            break;
        case TSO:
            ASSERT((StgClosure *)((StgTSO *)closure)->_link != NULL);
            ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->_link;

            ASSERT((StgClosure *)((StgTSO *)closure)->global_link != NULL);
            ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->global_link;

            ASSERT((StgClosure *)((StgTSO *)closure)->stackobj != NULL);
            ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->stackobj;

            ASSERT((StgClosure *)((StgTSO *)closure)->trec != NULL);
            ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->trec;

            ASSERT((StgClosure *)((StgTSO *)closure)->blocked_exceptions != NULL);
            ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->blocked_exceptions;

            ASSERT((StgClosure *)((StgTSO *)closure)->bq != NULL);
            ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->bq;

            if ((StgClosure *)((StgTSO *)closure)->label != NULL) {
                ptrs[nptrs++] = (StgClosure *)((StgTSO *)closure)->label;
            }

            break;
        case WEAK: {
            StgWeak *w = (StgWeak *)closure;
            ptrs[nptrs++] = (StgClosure *) w->cfinalizers;
            ptrs[nptrs++] = (StgClosure *) w->key;
            ptrs[nptrs++] = (StgClosure *) w->value;
            ptrs[nptrs++] = (StgClosure *) w->finalizer;
            // link may be NULL which is not a valid GC pointer
            if (w->link) {
                ptrs[nptrs++] = (StgClosure *) w->link;
            }
            break;
        }

        case CONTINUATION:
            // See the note in AP_STACK about the stack chunk.
            break;

        case BLOCKING_QUEUE:
        {
            StgBlockingQueue *bq = (StgBlockingQueue *) closure;
            ptrs[nptrs++] = (StgClosure *) bq->link;
            ptrs[nptrs++] = bq->bh;
            ptrs[nptrs++] = (StgClosure *) bq->owner;
            ptrs[nptrs++] = (StgClosure *) bq->queue;
            break;
        }

        default:
            fprintf(stderr,"closurePtrs: Cannot handle type %s yet\n",
                           closure_type_names[info->type]);
            break;
    }

    return nptrs;
}

StgMutArrPtrs *heap_view_closurePtrs(Capability *cap, StgClosure *closure) {
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));

    StgWord size = heap_view_closureSize(closure);

    // First collect all pointers here, with the comfortable memory bound
    // of the whole closure. Afterwards we know how many pointers are in
    // the closure and then we can allocate space on the heap and copy them
    // there. Note that we cannot allocate this on the C stack as the closure
    // may be, e.g., a large array.
    StgClosure **ptrs = (StgClosure **) stgMallocBytes(sizeof(StgClosure *) * size, "heap_view_closurePtrs");
    StgWord nptrs = collect_pointers(closure, ptrs);

    StgMutArrPtrs *arr = allocateMutArrPtrs(cap, nptrs, cap->r.rCCCS);
    if (RTS_UNLIKELY(arr == NULL)) goto end;
    SET_INFO((StgClosure *) arr, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);

    for (StgWord i = 0; i<nptrs; i++) {
        arr->payload[i] = ptrs[i];
    }
end:
    stgFree(ptrs);
    return arr;
}
