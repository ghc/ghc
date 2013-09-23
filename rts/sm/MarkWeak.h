/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Weak pointers and weak-like things in the GC
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_MARKWEAK_H
#define SM_MARKWEAK_H

#include "BeginPrivate.h"

extern StgWeak *old_weak_ptr_list;
extern StgTSO *resurrected_threads;
extern StgTSO *exception_threads;

void    initWeakForGC          ( void );
rtsBool traverseWeakPtrList    ( void );
void    markWeakPtrList        ( void );

#include "EndPrivate.h"

#endif /* SM_MARKWEAK_H */
