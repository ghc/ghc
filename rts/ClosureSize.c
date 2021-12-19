/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2021
 *
 * Implementation of closure_sizeW_
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

uint32_t
closure_sizeW_ (const StgClosure *p, const StgInfoTable *info)
{
    switch (info->type) {
    case THUNK_0_1:
    case THUNK_1_0:
        return sizeofW(StgThunk) + 1;
    case FUN_0_1:
    case CONSTR_0_1:
    case FUN_1_0:
    case CONSTR_1_0:
        return sizeofW(StgHeader) + 1;
    case THUNK_0_2:
    case THUNK_1_1:
    case THUNK_2_0:
        return sizeofW(StgThunk) + 2;
    case FUN_0_2:
    case CONSTR_0_2:
    case FUN_1_1:
    case CONSTR_1_1:
    case FUN_2_0:
    case CONSTR_2_0:
        return sizeofW(StgHeader) + 2;
    case THUNK:
        return thunk_sizeW_fromITBL(info);
    case THUNK_SELECTOR:
        return THUNK_SELECTOR_sizeW();
    case AP_STACK:
        return ap_stack_sizeW((StgAP_STACK *)p);
    case AP:
        return ap_sizeW((StgAP *)p);
    case PAP:
        return pap_sizeW((StgPAP *)p);
    case IND:
        return sizeofW(StgInd);
    case ARR_WORDS:
        return arr_words_sizeW((StgArrBytes *)p);
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        return small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
    case TSO:
        return sizeofW(StgTSO);
    case STACK:
        return stack_sizeW((StgStack*)p);
    case BCO:
        return bco_sizeW((StgBCO *)p);
    case TREC_CHUNK:
        return sizeofW(StgTRecChunk);
    default:
        return sizeW_fromITBL(info);
    }
}
