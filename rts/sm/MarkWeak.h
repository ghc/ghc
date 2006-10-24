/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Weak pointers and weak-like things in the GC
 *
 * ---------------------------------------------------------------------------*/

extern StgWeak *old_weak_ptr_list;
extern StgTSO *resurrected_threads;

void    initWeakForGC          ( void );
rtsBool traverseWeakPtrList    ( void );
void    markWeakPtrList        ( void );
rtsBool traverseBlackholeQueue ( void );
