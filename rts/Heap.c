/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2017
 *
 * Introspection into GHC's heap representation
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"

#include "Capability.h"
#include "Printer.h"

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

StgMutArrPtrs *heap_view_closurePtrs(Capability *cap, StgClosure *closure) {
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));

    StgWord size = heap_view_closureSize(closure);
    StgWord nptrs = 0;
    StgWord i;

    // First collect all pointers here, with the comfortable memory bound
    // of the whole closure. Afterwards we know how many pointers are in
    // the closure and then we can allocate space on the heap and copy them
    // there
    StgClosure *ptrs[size];

    StgClosure **end;
    StgClosure **ptr;

    const StgInfoTable *info = get_itbl(closure);

    switch (info->type) {
        case INVALID_OBJECT:
            barf("Invalid Object");
            break;

        // No pointers
        case ARR_WORDS:
            break;

        // Default layout
        case CONSTR_1_0:
        case CONSTR_0_1:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_0_2:
        case CONSTR:


        case PRIM:

        case FUN:
        case FUN_1_0:
        case FUN_0_1:
        case FUN_1_1:
        case FUN_2_0:
        case FUN_0_2:
        case FUN_STATIC:
            end = closure->payload + info->layout.payload.ptrs;
            for (ptr = closure->payload; ptr < end; ptr++) {
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
            for (ptr = ((StgThunk *)closure)->payload; ptr < end; ptr++) {
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
            for (i = 0; i < ((StgAP_STACK *)closure)->size; ++i) {
                ptrs[nptrs++] = ((StgAP_STACK *)closure)->payload[i];
            }
            break;

        case BCO:
            ptrs[nptrs++] = (StgClosure *)((StgBCO *)closure)->instrs;
            ptrs[nptrs++] = (StgClosure *)((StgBCO *)closure)->literals;
            ptrs[nptrs++] = (StgClosure *)((StgBCO *)closure)->ptrs;
            break;

        case IND:
        case IND_STATIC:
        case BLACKHOLE:
            ptrs[nptrs++] = (StgClosure *)(((StgInd *)closure)->indirectee);
            break;

        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            for (i = 0; i < ((StgMutArrPtrs *)closure)->ptrs; ++i) {
                ptrs[nptrs++] = ((StgMutArrPtrs *)closure)->payload[i];
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

        default:
            fprintf(stderr,"closurePtrs: Cannot handle type %s yet\n",
                           closure_type_names[info->type]);
            break;
    }

    size = nptrs + mutArrPtrsCardTableSize(nptrs);
    StgMutArrPtrs *arr =
        (StgMutArrPtrs *)allocate(cap, sizeofW(StgMutArrPtrs) + size);
    TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), nptrs, 0);
    SET_HDR(arr, &stg_MUT_ARR_PTRS_FROZEN_info, cap->r.rCCCS);
    arr->ptrs = nptrs;
    arr->size = size;

    for (i = 0; i<nptrs; i++) {
        arr->payload[i] = ptrs[i];
    }

    return arr;
}
