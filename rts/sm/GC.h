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

#include "HeapAlloc.h"

#include "BeginPrivate.h"

struct GcConfig {
    // which generation are we requesting be collected?
    uint32_t collect_gen;
    // is a heap census requested?
    bool do_heap_census;
    // is this GC triggered by a heap overflow?
    bool overflow_gc;
    // is this GC triggered by a deadlock?
    bool deadlock_detect;
    // should we force non-concurrent collection if the non-moving collector is
    // being used?
    bool nonconcurrent;
    // should we use parallel scavenging?
    bool parallel;
};

void GarbageCollect (struct GcConfig config,
                     Capability *cap,
                     bool idle_cap[]);

typedef void (*evac_fn)(void *user, StgClosure **root);

StgClosure * isAlive      ( StgClosure *p );
void         markCAFs     ( evac_fn evac, void *user );

bool doIdleGCWork(Capability *cap, bool all);

extern uint32_t N;
extern bool major_gc;
/* See Note [Deadlock detection under the nonmoving collector]. */
extern bool deadlock_detect_gc;
extern bool unload_mark_needed;

extern bdescr *mark_stack_bd;
extern bdescr *mark_stack_top_bd;
extern StgPtr mark_sp;

extern bool work_stealing;

#if defined(PROF_SPIN) && defined(THREADED_RTS)
extern volatile StgWord64 whitehole_gc_spin;
extern volatile StgWord64 waitForGcThreads_spin;
extern volatile StgWord64 waitForGcThreads_yield;
#endif

// mutable list scavenging statistics
#if defined(DEBUG)
typedef struct {
    StgWord n_MUTVAR;
    StgWord n_MUTARR;
    StgWord n_MVAR;
    StgWord n_TVAR;
    StgWord n_TREC_CHUNK;
    StgWord n_TVAR_WATCH_QUEUE;
    StgWord n_TREC_HEADER;
    StgWord n_OTHERS;
} MutListScavStats;

extern MutListScavStats mutlist_scav_stats;

void zeroMutListScavStats(MutListScavStats *src);
void addMutListScavStats(const MutListScavStats *src,
                         MutListScavStats *dest);
#endif /* DEBUG */

void gcWorkerThread (Capability *cap);
void initGcThreads (uint32_t from, uint32_t to);
void freeGcThreads (void);

void resizeGenerations (void);

#if defined(THREADED_RTS)
void notifyTodoBlock (void);
void waitForGcThreads (Capability *cap, bool idle_cap[]);
void releaseGCThreads (Capability *cap, bool idle_cap[]);
#endif

#define WORK_UNIT_WORDS 128

#include "EndPrivate.h"
