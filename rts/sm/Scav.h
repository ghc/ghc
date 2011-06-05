/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_SCAV_H
#define SM_SCAV_H

#include "BeginPrivate.h"

void    scavenge_loop (void);
void    scavenge_capability_mut_lists (Capability *cap);

#ifdef THREADED_RTS

void    scavenge_loop_par (void);
void    scavenge_capability_mut_lists_par (Capability *cap);

void    scavenge_loop_local (void);
void    scavenge_capability_mut_lists_local (Capability *cap);

#else

#define scavenge_loop1 scavenge_loop
#define scavenge_capability_mut_lists1 scavenge_capability_mut_lists

#endif

#include "EndPrivate.h"

#endif /* SM_SCAV_H */

