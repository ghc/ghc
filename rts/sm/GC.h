/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_GC_H
#define SM_GC_H

#include "BeginPrivate.h"

void GarbageCollect(rtsBool force_major_gc, nat gc_type, Capability *cap);

typedef void (*evac_fn)(void *user, StgClosure **root);

StgClosure * isAlive      ( StgClosure *p );
void         markCAFs     ( evac_fn evac, void *user );

StgPtr allocateInGen (Capability *cap, nat gen_ix, nat size);

extern rtsBool major_gc;       // collecting the oldest gen?
extern rtsBool work_stealing;  // work stealing is enabled?
extern nat     next_gc_gen;    // generation to collect next time

#ifdef DEBUG
extern nat mutlist_MUTVARS, mutlist_MUTARRS, mutlist_MVARS, mutlist_OTHERS;
#endif

#if defined(PROF_SPIN) && defined(THREADED_RTS)
extern StgWord64 whitehole_spin;
#endif

void gcWorkerThread (Capability *cap);
void initGcThreads (void);
void freeGcThreads (void);

#if defined(THREADED_RTS)
void waitForGcThreads (Capability *cap);
void releaseGCThreads (Capability *cap);
#endif

// used in globalise()
void prepare_gen_workspace (nat g);

#define WORK_UNIT_WORDS 128

#include "EndPrivate.h"

#endif /* SM_GC_H */
