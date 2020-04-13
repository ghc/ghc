/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020
 *
 * Helpers to set-up synthetic heap closures good enough for tests
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <Rts.h>
#include <rts/storage/Closures.h>

typedef struct synthHeap_ {
    bdescr *heap;
    bdescr *descr;
} synthHeap;

static inline synthHeap allocSynthHeap(void)
{
    synthHeap sh = {
        .heap = allocGroup(1),
        .descr = allocGroup(1),
    };
    sh.heap->link = NULL;
    sh.descr->link = NULL;
    return sh;
}

static inline void freeSynthHeap(synthHeap sh)
{
    freeChain(sh.heap);
    freeChain(sh.descr);
}

static inline StgWord synthClosureId(synthHeap *sh, StgClosure *c)
{
    return sh->descr->start[ROUNDUP_BYTES_TO_WDS((StgWord)c & BLOCK_MASK)];
}

static StgInfoTable info_weak     = { .type = WEAK,
                                      .layout.payload = {3, 1} };
static StgInfoTable info_selector = { .type = THUNK_SELECTOR,
                                      .layout.payload = {1, 0} };
static StgInfoTable info_arrwords = { .type = ARR_WORDS,
                                      .layout.payload = {0, 1} };

// See INFO_PTR_TO_STRUCT in ClosureMacros.h
#if defined(TABLES_NEXT_TO_CODE)
#define INFO(ptr) ((StgInfoTable *)ptr + 1)
#else
#define INFO(ptr) ((StgInfoTable *)ptr)
#endif

static inline StgClosure*
node3(synthHeap *sh, W_ id, StgClosure *a, StgClosure *b, StgClosure *c)
{
    StgWeak *n = (StgWeak*)sh->heap->free;
    sh->heap->free += sizeofW(*n);
    sh->descr->start[ROUNDUP_BYTES_TO_WDS((StgWord)n & BLOCK_MASK)] = id;
    *n = (StgWeak) {
        .header = { .info = INFO(&info_weak), .prof = { .ccs = CCS_SYSTEM } },
        .key = a,
        .value = b,
        .finalizer = c,
    };
    return (StgClosure*)n;
}

static inline StgClosure*
node1(synthHeap *sh, W_ id, StgClosure *a)
{
    StgSelector *n = (StgSelector*)sh->heap->free;
    sh->heap->free += sizeofW(*n);
    sh->descr->start[ROUNDUP_BYTES_TO_WDS((StgWord)n & BLOCK_MASK)] = id;
    *n = (StgSelector) {
        .header = { .info = INFO(&info_selector),
                    .prof = { .ccs = CCS_SYSTEM } },
        .selectee = a,
    };
    return (StgClosure*)n;
}

static inline StgClosure*
node0(synthHeap *sh, W_ id)
{
    StgArrBytes *n = (StgArrBytes*)sh->heap->free;
    sh->heap->free += sizeofW(*n);
    sh->descr->start[ROUNDUP_BYTES_TO_WDS((StgWord)n & BLOCK_MASK)] = id;
    *n = (StgArrBytes) {
        .header = { .info = INFO(&info_arrwords),
                    .prof = { .ccs = CCS_SYSTEM } }
    };
    return (StgClosure*)n;
}
