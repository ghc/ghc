/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Weak pointers and weak-like things in the GC
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

extern StgWeak *old_weak_ptr_list;
extern StgTSO *resurrected_threads;

void    initWeakForGC          ( void );
rtsBool traverseWeakPtrList    ( void );
void    markWeakPtrList        ( void );
rtsBool traverseBlackholeQueue ( void );
