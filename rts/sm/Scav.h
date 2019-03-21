/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void    scavenge_loop (void);
void    scavenge_capability_mut_lists (Capability *cap);

#if defined(THREADED_RTS)
void    scavenge_loop1 (void);
void    scavenge_capability_mut_Lists1 (Capability *cap);
#endif

#include "EndPrivate.h"
