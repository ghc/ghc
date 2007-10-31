/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: utilities
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * --------------------------------------------------------------------------*/

#include "SMP.h"

#ifdef THREADED_RTS
extern SpinLock gc_alloc_block_sync;
#endif

bdescr *allocBlock_sync(void);

void    push_scan_block      (bdescr *bd, step_workspace *ws);
bdescr *grab_todo_block      (step_workspace *ws);
bdescr *gc_alloc_todo_block  (step_workspace *ws);
bdescr *gc_alloc_scavd_block (step_workspace *ws);

// Returns true if a block is 3/4 full.  This predicate is used to try
// to re-use partial blocks wherever possible, and to reduce wastage.
// We might need to tweak the actual value.
INLINE_HEADER rtsBool
isPartiallyFull(bdescr *bd)
{
    return (bd->free + BLOCK_SIZE_W/4 < bd->start + BLOCK_SIZE_W);
}


#if DEBUG
void printMutableList (generation *gen);
#endif
