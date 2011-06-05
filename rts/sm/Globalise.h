/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2010
 *
 * Globalise data from the local heap to the global heap.
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_GLOBALISE_H
#define SM_GLOBALISE_H

#include "BeginPrivate.h"

StgClosure * publish       (Capability *cap, StgClosure *p);
StgClosure * publish_gen   (Capability *cap, StgClosure *p, generation *gen);

void         globalise     (Capability *cap, StgClosure **root);
StgClosure * globalise_    (Capability *cap, StgClosure *root);
StgClosure * globaliseFull_(Capability *cap, StgClosure *root);
void         globaliseWRT  (Capability *cap, StgClosure *parent,
                            StgClosure **root);
void         globaliseFullWRT (Capability *cap USED_IF_THREADS,
                               StgClosure *parent, StgClosure **root);

void         globalise_capability_mut_lists (Capability *cap);

// for use when migrating a TSO
StgTSO *     globalise_TSO (Capability *cap, StgTSO *tso);

#include "EndPrivate.h"

#endif /* SM_GLOBALISE_H */
