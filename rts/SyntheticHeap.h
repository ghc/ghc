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

#include "BeginPrivate.h"

typedef struct synthHeap_ {
    bdescr *heap;
    bdescr *descr;
} synthHeap;

static inline synthHeap allocSynthHeap(void)
{
    return (synthHeap) {
        .heap = allocGroup(1),
        .descr = allocGroup(1),
    };
}

static inline void freeSynthHeap(synthHeap sh)
{
    freeGroup(sh.heap);
    freeGroup(sh.descr);
}

static inline StgWord synthClosureId(synthHeap *sh, StgClosure *c)
{
    return sh->descr->start[ROUNDUP_BYTES_TO_WDS((StgWord)c & (BLOCK_SIZE-1))];
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

#define node3(id, a,b,c)                                \
    StgClosure *n##id = (StgClosure*)p;                 \
    d[ROUNDUP_BYTES_TO_WDS((StgWord)p & (BLOCK_SIZE-1))] = id; \
    *(StgWeak*)n##id = (StgWeak){                       \
        .header = { .info = INFO(&info_weak) },         \
        .key = n##a,                                    \
        .value = n##b,                                  \
        .finalizer = n##c,                              \
    };                                                  \
    p += sizeofW(StgWeak);

#define node1(id, a)                                    \
    StgClosure *n##id = (StgClosure*)p;                 \
    d[ROUNDUP_BYTES_TO_WDS((StgWord)p & (BLOCK_SIZE-1))] = id; \
    *(StgSelector*)n##id = (StgSelector){               \
        .header = { .info = INFO(&info_selector) },     \
        .selectee = n##a,                               \
    };                                                  \
    p += sizeofW(StgSelector);

#define node0(id)                                       \
    StgClosure *n##id = (StgClosure*)p;                 \
    d[ROUNDUP_BYTES_TO_WDS((StgWord)p & (BLOCK_SIZE-1))] = id; \
    *(StgArrBytes*)n##id = (StgArrBytes){               \
        .header = { .info = INFO(&info_arrwords) },     \
    };                                                  \
    p += sizeofW(StgArrBytes);
