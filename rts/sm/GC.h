/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

#include "HeapAlloc.h"

void GarbageCollect (uint32_t force_major_gc,
                     bool do_heap_census,
                     uint32_t gc_type, Capability *cap, bool idle_cap[]);

typedef void (*evac_fn)(void *user, StgClosure **root);

StgClosure * isAlive      ( StgClosure *p );
void         markCAFs     ( evac_fn evac, void *user );

bool doIdleGCWork(Capability *cap, bool all);

extern uint32_t N;
extern bool major_gc;

extern bdescr *mark_stack_bd;
extern bdescr *mark_stack_top_bd;
extern StgPtr mark_sp;

extern bool work_stealing;

#if defined(DEBUG)
extern uint32_t mutlist_MUTVARS, mutlist_MUTARRS, mutlist_MVARS, mutlist_OTHERS,
    mutlist_TVAR,
    mutlist_TVAR_WATCH_QUEUE,
    mutlist_TREC_CHUNK,
    mutlist_TREC_HEADER;
#endif

#if defined(PROF_SPIN) && defined(THREADED_RTS)
extern volatile StgWord64 whitehole_gc_spin;
extern volatile StgWord64 waitForGcThreads_spin;
extern volatile StgWord64 waitForGcThreads_yield;
#endif

void gcWorkerThread (Capability *cap);
void initGcThreads (uint32_t from, uint32_t to);
void freeGcThreads (void);

#if defined(THREADED_RTS)
void waitForGcThreads (Capability *cap, bool idle_cap[]);
void releaseGCThreads (Capability *cap, bool idle_cap[]);
#endif

#define WORK_UNIT_WORDS 128

#include "EndPrivate.h"
