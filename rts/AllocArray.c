#include "rts/PosixSource.h"
#include "Rts.h"

#include "AllocArray.h"

StgMutArrPtrs *allocateMutArrPtrs (Capability *cap,
                                   StgWord nelements,
                                   CostCentreStack *ccs USED_IF_PROFILING)
{
    /* All sizes in words */

    /* The card table contains one byte for each 2^MUT_ARR_PTRS_CARD_BITS words
     * in the array, making sure we round up, and then rounding up to a whole
     * number of words. */
    StgWord cardsize = mutArrPtrsCardTableSize(nelements); /* card table */
    StgWord arrsize  = nelements + cardsize;               /* +array size */
    StgWord objsize  = sizeofW(StgMutArrPtrs) + arrsize;   /* +header size */
    StgMutArrPtrs *arr;
    arr = (StgMutArrPtrs *)allocateMightFail(cap, objsize);
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), arrsize, 0);

    /* No write barrier needed since this is a new allocation. */
    SET_HDR(arr, &stg_MUT_ARR_PTRS_DIRTY_info, ccs);
    arr->ptrs = nelements;
    arr->size = arrsize;

    /* Initialize the card array. Note that memset needs sizes in bytes. */
    memset(&(arr->payload[nelements]), 0, mutArrPtrsCards(nelements));

    return arr;
}

StgSmallMutArrPtrs *allocateSmallMutArrPtrs (Capability *cap,
                                             StgWord nelements,
                                             CostCentreStack *ccs
                                               USED_IF_PROFILING)
{
    /* All sizes in words */
    StgWord arrsize = nelements;                              /* array size */
    StgWord objsize = sizeofW(StgSmallMutArrPtrs) + arrsize;  /* +header size */
    StgSmallMutArrPtrs *arr;
    arr = (StgSmallMutArrPtrs *)allocateMightFail(cap, objsize);
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgSmallMutArrPtrs), arrsize, 0);

    /* No write barrier needed since this is a new allocation. */
    SET_HDR(arr, &stg_SMALL_MUT_ARR_PTRS_DIRTY_info, ccs);
    arr->ptrs = nelements;
    return arr;
}

StgArrBytes *allocateArrBytes (Capability *cap,
                               StgWord arrbytes,
                               CostCentreStack *ccs USED_IF_PROFILING)
{
    /* All sizes in words */
    StgWord arrwords = ROUNDUP_BYTES_TO_WDS(arrbytes);
    StgWord objsize  = sizeofW(StgArrBytes) + arrwords;
    StgArrBytes *arr;
    arr = (StgArrBytes *)allocateMightFail(cap, objsize);
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgArrBytes), arrwords, 0);
    /* No write barrier needed since this is a new allocation. */
    SET_HDR(arr, &stg_ARR_WORDS_info, ccs);
    arr->bytes = arrbytes;
    return arr;
}

StgArrBytes *allocateArrBytesPinned (Capability *cap,
                                     StgWord arrbytes,
                                     StgWord alignment,
                                     CostCentreStack *ccs USED_IF_PROFILING)
{
    /* we always supply at least word-aligned memory, so there's no
       need to allow extra space for alignment if the requirement is less
       than a word.  This also prevents mischief with alignment == 0. */
    if (alignment <= sizeof(StgWord)) { alignment = sizeof(StgWord); }

    /* All sizes in words */
    StgWord arrwords = ROUNDUP_BYTES_TO_WDS(arrbytes);
    StgWord objsize  = sizeofW(StgArrBytes) + arrwords;
    StgWord alignoff = sizeof(StgArrBytes); // it's the payload to be aligned
    StgArrBytes *arr;
    arr = (StgArrBytes *)allocatePinned(cap, objsize, alignment, alignoff);
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgArrBytes), arrwords, 0);
    /* No write barrier needed since this is a new allocation. */
    SET_HDR(arr, &stg_ARR_WORDS_info, ccs);
    arr->bytes = arrbytes;
    return arr;
}
