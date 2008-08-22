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

#ifndef GC_H
#define GC_H

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

#ifdef DEBUG
extern nat mutlist_MUTVARS, mutlist_MUTARRS, mutlist_MVARS, mutlist_OTHERS;
#endif

extern void markSomeCapabilities (evac_fn evac, void *user, nat i0, nat delta);

#ifdef THREADED_RTS
extern SpinLock gc_alloc_block_sync;
#endif

#if defined(PROF_SPIN) && defined(THREADED_RTS)
extern StgWord64 whitehole_spin;
#endif

#define WORK_UNIT_WORDS 128

#endif /* GC_H */
