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

#pragma once

#include "BeginPrivate.h"

extern StgTSO *resurrected_threads;

void    collectFreshWeakPtrs   ( void );
void    initWeakForGC          ( void );
bool    traverseWeakPtrList    ( void );
void    markWeakPtrList        ( void );
void    scavengeLiveWeak       ( StgWeak * );

#include "EndPrivate.h"
