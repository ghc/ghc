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

BEGIN_RTS_PRIVATE

void GarbageCollect(rtsBool force_major_gc, nat gc_type, Capability *cap);

typedef void (*evac_fn)(void *user, StgClosure **root);

StgClosure * isAlive      ( StgClosure *p );
void         markCAFs     ( evac_fn evac, void *user );

extern nat N;
extern rtsBool major_gc;

extern bdescr *mark_stack_bdescr;
extern StgPtr *mark_stack;
extern StgPtr *mark_sp;
extern StgPtr *mark_splim;

extern rtsBool mark_stack_overflowed;
extern bdescr *oldgen_scan_bd;
extern StgPtr  oldgen_scan;

extern long copied;

extern rtsBool work_stealing;

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

#define WORK_UNIT_WORDS 128

END_RTS_PRIVATE

#endif /* SM_GC_H */
