#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h" // needed by SET_HDR macro

#include "ClosureTable.h"
#include "sm/NonMovingMark.h"
#include "RtsUtils.h"

#include <string.h>


void initClosureTable(ClosureTable *t, enum ClosureTableMode mode)
{
    t->arr      = (StgMutArrPtrs *) &stg_CLOSURE_TABLE_NULL_closure;
    t->free     = (StgArrBytes   *) &stg_CLOSURE_TABLE_NULL_closure;
    t->free_hd  = -1;
    t->capacity = 0;
    t->elems    = 0;
    t->compact  = mode;
}


void markClosureTable(evac_fn evac, void *user, ClosureTable *t)
{
    evac(user, (StgClosure **)&t->arr);
    evac(user, (StgClosure **)&t->free);
}


int enlargeClosureTable(Capability *cap, ClosureTable *t, int newcapacity)
{
    /* Compact tables are only allowed to be enlarged when they are full.
     * Otherwise the free list would not be maintained in the right order.
     */
    ASSERT(t->compact == ClosureTableCompact ? isFullClosureTable(t) : true);

    /* Stash the old info before we overwrite things */
    StgMutArrPtrs *oldarr      = t->arr;
    int            oldcapacity = t->capacity;
    StgArrBytes   *oldfree     = t->free;

    /* We cannot shrink. The table need not be full however. */
    ASSERT(newcapacity >= oldcapacity);

    /* Allocate a new StgMutArrPtrs */
    StgMutArrPtrs *newarr;
    int cardsize = mutArrPtrsCardTableSize(newcapacity); /* card table */
    int arrsize  = newcapacity + cardsize;               /* +array size */
    {
        int objsize = sizeofW(StgMutArrPtrs) + arrsize;  /* +header size */
        newarr = (StgMutArrPtrs *)allocateMightFail(cap, objsize);
        if (RTS_UNLIKELY(newarr == NULL)) return objsize;
        TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), arrsize, 0);
        SET_HDR(newarr, &stg_MUT_ARR_PTRS_DIRTY_info, CCS_SYSTEM);
        newarr->ptrs = newcapacity;
        newarr->size = arrsize;
    }

    /* Allocate a new StgArrBytes */
    StgArrBytes *newfree;
    {
        /* We treat the StgArrBytes payload as an array of C int */
        int arrbytes = newcapacity * sizeof(int);
        int objsize  = sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(arrbytes);
        newfree = (StgArrBytes *)allocateMightFail(cap, objsize);
        if (RTS_UNLIKELY(newfree == NULL)) return objsize;
        TICK_ALLOC_PRIM(sizeofW(StgArrBytes), arrbytes, 0);
        SET_HDR(newfree, &stg_ARR_WORDS_info, CCS_SYSTEM);
        newfree->bytes = arrbytes;
    }

    /* With allocation done nothing can fail now, so ok to overwrite things */

    t->capacity = newcapacity;

    /* We build the new array by copying the elements over from the old array
     * and the card table from the old array. The extra elements (if any) are
     * initialised to a null value, and the extra card table entries (if any)
     * are cleared.
     */
    /* Copy the old elements */
    for (int i = 0; i < oldcapacity; i++) {
        newarr->payload[i] = oldarr->payload[i];
    }
    /* Set the new elements to a null value. */
    for (int i = oldcapacity; i < newcapacity; i++) {
        newarr->payload[i] = &stg_CLOSURE_TABLE_NULL_closure;
    }
    /* Copy the the mutable array's card table. And init extra elements to 0 */
    int oldcardsize = mutArrPtrsCardTableSize(oldcapacity);
    for (int i = 0; i < oldcardsize; i++) {
        newarr->payload[newcapacity+i] = oldarr->payload[oldcapacity+i];
    }
    for (int i = oldcardsize; i < cardsize; i++) {
        newarr->payload[newcapacity+i] = 0;
    }
    /* We conservatively assume the old array was dirty, so we marked the new
       one as dirty too, and now we put the array on the mutated list. */
    recordClosureMutated(cap, (StgClosure*)newarr);
    t->arr = newarr;

    /* Copy the free list. */
    int oldfreesizeW = ROUNDUP_BYTES_TO_WDS(oldcapacity * sizeof(int));
    for (int i = 0; i < oldfreesizeW; i++) {
        newfree->payload[i] = oldfree->payload[i];
    }
    /* Initialise the new free list entries.
     * They all get consed on the front of the existing free list:
     * The last new entry points to the old head entry. The new head is the
     * first of the new entries. Otherwise, point each entry to the next one.
     */
    for (int i = oldcapacity; i < newcapacity-1; i++) {
        StgArrBytesAsCInts(newfree)[i] = i+1;
    }
    if (newcapacity > oldcapacity) {
        StgArrBytesAsCInts(newfree)[newcapacity-1] = t->free_hd;
        t->free_hd = oldcapacity;
    }
    t->free = newfree;

    return 0;
}

int insertClosureTable(Capability *cap STG_UNUSED,
                       ClosureTable *t, void *v)
{
    int ix = t->free_hd;                   /* the next free entry index */

    ASSERT(ix >= 0 && ix < t->capacity);   /* and so specifically not -1 */
    ASSERT(t->arr->payload[ix] == &stg_CLOSURE_TABLE_NULL_closure);

    /* Note that unlike writeArray# we do not need any memory barrier here
     * because the ClosureTable is not shared between capabilities.
     *
     * Also unlike writeArray#, we don't need to use updateRemembSetPushClosure
     * for the old array entry we are overwriting, because we know that old
     * entry is stg_CLOSURE_TABLE_NULL_closure, which is static.
     */

    /* Fill in the entry with the supplied closure. */
    t->arr->payload[ix] = v;

    /* Mark the corresponding card table entry. */
    *mutArrPtrsCard(t->arr, ix >> MUT_ARR_PTRS_CARD_BITS) = (StgWord8)1;

    /* Mark the array as dirty. It is already on the mutated list. */
    SET_INFO((StgClosure *)t->arr, &stg_MUT_ARR_PTRS_DIRTY_info);

    /* Pop the free list */
    t->free_hd = StgArrBytesAsCInts(t->free)[ix];

    t->elems++;

    /* in debug, mark as used */
    DEBUG_ONLY(StgArrBytesAsCInts(t->free)[ix] = -2);

    return ix;
}


void removeClosureTable(Capability *cap, ClosureTable *t, int ix)
{
    /* removeClosureTable can only be used for non-compact tables */
    ASSERT(t->compact == ClosureTableNonCompact);

    ASSERT(ix >= 0 && ix < t->capacity);

    /* in debug, was marked as used */
    ASSERT(StgArrBytesAsCInts(t->free)[ix] == -2);

    /* Normally when modifying an array element we would need to mark the card
     * table entry, if the whole array wasn't already marked as dirty then we
     * would need to mark it as so and push it onto the mutated list (as we do
     * in insertClosureTable above).
     *
     * We normally need to do those things to maintain the generational
     * invariant because we (potentially, and indeed quite likely) have an
     * object in the old GC generation (the array) pointing to an object in a
     * newer GC generation (the array element).
     *
     * Here we are overwriting with stg_CLOSURE_TABLE_NULL_closure which is
     * statically allocated and has no outgoing pointers, so cannot point into
     * a newer GC generation. So due to that, we can skip some of the normal
     * steps.
     *
     * We do however still need to do the normal work to maintain the nonmoving
     * collector's snapshot invariant. Since we're potentially about to forget
     * the old element, we need to add it to the remembered set.
     */
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushClosure(cap, t->arr->payload[ix]);
    }
    t->arr->payload[ix] = &stg_CLOSURE_TABLE_NULL_closure;

    /* push ix onto the free list */
    StgArrBytesAsCInts(t->free)[ix] = t->free_hd;
    t->free_hd = ix;

    t->elems--;
}


#if defined(__clang__)
/* Don't error on unfixable clang warnings about 'unneeded' functions used only
 * in assertsions. They may be 'unneeded' but they are still used and cannot be
 * removed (or defined conditionally). */
#pragma clang diagnostic warning "-Wunneeded-internal-declaration"
#endif

static bool isCompactClosureTable(ClosureTable *t);


void removeCompactClosureTable(Capability *cap, ClosureTable *t,
                               int ix, int *ix_from, int *ix_to)
{
    /* removeCompactClosureTable can only be used for compact tables */
    ASSERT(t->compact == ClosureTableCompact);

    ASSERT(ix >= 0 && ix < t->capacity);

    /* in debug, was marked as used */
    ASSERT(StgArrBytesAsCInts(t->free)[ix] == -2);

    /* this action can only be used for compact tables */
    ASSERT(isCompactClosureTable(t));

    int gap_ix = ix;
    int end_ix = t->elems - 1;

    *ix_from = end_ix;
    *ix_to   = gap_ix;

    /* Note that all of the steps below still need to make sense when
     * gap_ix == end_ix, though it's ok to do some unnecessary steps.
     */

    /* We're deleting the element at gap_ix */
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushClosure(cap, t->arr->payload[gap_ix]);
    }

    /* move the end entry over the gap and mark the end as empty */
    t->arr->payload[gap_ix] = t->arr->payload[end_ix];
    t->arr->payload[end_ix] = &stg_CLOSURE_TABLE_NULL_closure;

    /* Mark the card table entry of the gap_ix that got overwritten.
     * We don't need to mark the array as dirty overall, it's dirty
     * status is unchanged either way, since the set of objects it
     * points to does not change.
     *
     * See also the notes in removeClosureTable about why we can skip
     * other steps.
     */
    *mutArrPtrsCard(t->arr, gap_ix >> MUT_ARR_PTRS_CARD_BITS) = 1;

    /* push end_ix onto the free list */
    StgArrBytesAsCInts(t->free)[end_ix] = t->free_hd;
    t->free_hd = end_ix;

    t->elems--;

    ASSERT(isCompactClosureTable(t));
    ASSERT(t->arr->payload[*ix_from] == &stg_CLOSURE_TABLE_NULL_closure);
    ASSERT(*ix_from != *ix_to
         ? t->arr->payload[*ix_to] != &stg_CLOSURE_TABLE_NULL_closure
         : true);
}


/* Check compactness: all the elements should be in the positions [0..elems-1].
 * The remaining indexes [elems..capacity-1] should be empty with the free-list
 * in order.
 */
static bool isCompactClosureTable(ClosureTable *t)
{
    bool isCompact = true;
    for (int n = 0; n < t->elems; n++) {
        isCompact &= (t->arr->payload[n] != &stg_CLOSURE_TABLE_NULL_closure);
        isCompact &= (StgArrBytesAsCInts(t->free)[n] == -2);
    }
    for (int n = t->elems; n < t->capacity; n++) {
        isCompact &= (t->arr->payload[n] == &stg_CLOSURE_TABLE_NULL_closure);
        /* the last element will be -1, otherwise it points to the next */
        isCompact &= (StgArrBytesAsCInts(t->free)[n] ==
                      (n == t->capacity - 1 ? -1 : n+1));
    }
    return isCompact;
}

