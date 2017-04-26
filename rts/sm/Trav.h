/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_TRAV_H
#define SM_TRAV_H

#include "BeginPrivate.h"

void dump_closure(const StgClosure *p);
void    traverse_loop (void);
void    traverse_capability_mut_lists (Capability *cap);

#ifdef THREADED_RTS
void    traverse_loop1 (void);
void    traverse_capability_mut_Lists1 (Capability *cap);
#endif

#include "EndPrivate.h"

#endif /* SM_TRAV_H */

