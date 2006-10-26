/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: evacuation functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

// Use a register argument for evacuate, if available.
#if __GNUC__ >= 2
#define REGPARM1 __attribute__((regparm(1)))
#else
#define REGPARM1
#endif

REGPARM1 StgClosure * evacuate (StgClosure *q);

extern lnat thunk_selector_depth;
