/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Weak pointers and weak-like things in the GC
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

extern StgTSO *resurrected_threads;

void    collectFreshWeakPtrs   ( void );
void    initWeakForGC          ( void );
bool    traverseWeakPtrList    ( StgWeak **dead_weak_ptr_list, StgTSO **resurrected_threads );
void    markWeakPtrList        ( void );
void    scavengeLiveWeak       ( StgWeak * );

#include "EndPrivate.h"
