
#include "rts/PosixSource.h"
#include "Rts.h"
#include "ClosureTable.h"
#include "RtsUtils.h"
#include "StablePtr.h"


/* We need all these C wrappers because we cannot pass the ClosureTable *
   directly, since it's not a known GC ptr type on the Haskell side.

   Also in some cases we need to pass the capability, or we need to peek at
   hidden struct members.
 */

/* Cheeky C trick: malloc a bigger struct and stash extra info */
struct ClosureTableExtras {
    ClosureTable t;
    StgStablePtr arr;
    StgStablePtr free;
};

ClosureTable * newClosureTable(int mode)
{
    struct ClosureTableExtras *te =
        stgMallocBytes(sizeof(struct ClosureTableExtras), "newClosureTable");
    ClosureTable *t  = &(te->t);
    initClosureTable(t, (enum ClosureTableMode)mode);
    /* make these two GC roots */
    te->arr  = getStablePtr((StgPtr) t->arr);
    te->free = getStablePtr((StgPtr) t->free);
    return t;
}

void freeClosureTable(ClosureTable *t)
{
     /* we know this since we malloc'd it */
    struct ClosureTableExtras *te = (struct ClosureTableExtras *)t;
    freeStablePtr(te->arr);
    freeStablePtr(te->free);
    stgFree(te);
}

int sizeClosureTable_wrapper(ClosureTable *t)
{
    return sizeClosureTable(t); /* needed due to inline header def */
}

bool isFullClosureTable_wrapper(ClosureTable *t)
{
    return isFullClosureTable(t); /* needed due to inline header def */
}

bool isEmptyClosureTable_wrapper(ClosureTable *t)
{
    return isEmptyClosureTable(t); /* needed due to inline header def */
}

int capacityClosureTable_wrapper(ClosureTable *t)
{
    return capacityClosureTable(t); /* needed due to inline header def */
}

int insertClosureTable_wrapper(ClosureTable *t, HsStablePtr p)
{
    StgClosure *v = (StgClosure *) deRefStablePtr(p);
    int ix = insertClosureTable(&MainCapability, t, v);
    return ix;
}

void removeClosureTable_wrapper(ClosureTable *t, int ix)
{
    removeClosureTable(&MainCapability, t, ix);
}

void removeCompactClosureTable_wrapper(ClosureTable *t, int ix,
                                       int *ix_from, int *ix_to)
{
    removeCompactClosureTable(&MainCapability, t, ix, ix_from, ix_to);
}

HsStablePtr indexClosureTable_wrapper(ClosureTable *t, int ix)
{
    StgClosure *v = indexClosureTable(t, ix);
    return getStablePtr((StgPtr) v);
}

bool indexIsNullClosureTable(ClosureTable *t, int ix)
{
    bool isNull = t->arr->payload[ix] == &stg_CLOSURE_TABLE_NULL_closure;
    return isNull;
}

int indexFreeListClosureTable(ClosureTable *t, int ix)
{
    return StgArrBytesAsCInts(t->free)[ix];
}

int getFreeListHeadClosureTable(ClosureTable *t)
{
    return t->free_hd;
}

bool enlargeClosureTable_wrapper(ClosureTable *t, int capacity)
{
    return enlargeClosureTable(&MainCapability, t, capacity);
}

void printClosureTable(ClosureTable *t)
{
    printf("----------------\n");
    printf("t->capacity        = %i\n", t->capacity);
    printf("t->elems           = %i\n", t->elems);

    if (t->arr == (StgMutArrPtrs *) &stg_CLOSURE_TABLE_NULL_closure)
      printf("t->arr             = &stg_CLOSURE_TABLE_NULL_closure\n");
    else {
      printf("t->arr             = %p\n", t->arr);
      if (t->arr) {
        if (t->arr->header.info == &stg_MUT_ARR_PTRS_DIRTY_info) {
          printf("t->arr->info       = &stg_MUT_ARR_PTRS_DIRTY_info\n");
        } else if (t->arr->header.info == &stg_MUT_ARR_PTRS_CLEAN_info) {
          printf("t->arr->info       = &stg_MUT_ARR_PTRS_CLEAN_info\n");
        } else {
          printf("t->arr->info       = %p\n", t->arr->header.info);
        }
        printf("t->arr->ptrs       = %lu\n", t->arr->ptrs);
        printf("t->arr->size       = %lu\n", t->arr->size);
      }
    }

    if (t->arr && t->arr != (StgMutArrPtrs *) &stg_CLOSURE_TABLE_NULL_closure) {
      printf("  array elements:\n");
      for (unsigned i = 0; i < t->arr->ptrs; i++) {
        if (t->arr->payload[i] == &stg_CLOSURE_TABLE_NULL_closure)
          printf("t->arr->payload[%i] = &stg_CLOSURE_TABLE_NULL_closure\n", i);
        else
          printf("t->arr->payload[%i] = %p\n", i, t->arr->payload[i]);
      }
      printf("  card table:\n");
      for (unsigned i = t->arr->ptrs; i < t->arr->size; i++) {
        printf("t->arr->payload[%i] = %llx\n", i, (unsigned long long)t->arr->payload[i]);
      }
    }
    if (t->free == (StgArrBytes *) &stg_CLOSURE_TABLE_NULL_closure) {
        printf("t->free            = stg_CLOSURE_TABLE_NULL_closure\n");
    } else {
        printf("t->free            = %p\n", t->free);

        if (t->free->header.info == &stg_ARR_WORDS_info) {
          printf("t->free->info      = &stg_ARR_WORDS_info\n");
        } else {
          printf("t->free->info      = %p\n", t->free->header.info);
        }
        printf("t->free->bytes     = %i\n", t->free->bytes);
    }
    for (int i = 0; i < t->capacity; i++) {
      printf("t->free.payload[%i] = %i\n", i, StgArrBytesAsCInts(t->free)[i]);
    }
    printf("t->free_hd         = %i\n", t->free_hd);
    printf("\n\n");
}

