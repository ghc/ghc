/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
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

bdescr *allocBlock_sync(void);
void    freeChain_sync(bdescr *bd);

void    push_scanned_block   (bdescr *bd, step_workspace *ws);
bdescr *grab_todo_block      (step_workspace *ws);
StgPtr  todo_block_full      (nat size, step_workspace *ws);
StgPtr  alloc_todo_block     (step_workspace *ws, nat size);

// Returns true if a block is partially full.  This predicate is used to try
// to re-use partial blocks wherever possible, and to reduce wastage.
// We might need to tweak the actual value.
INLINE_HEADER rtsBool
isPartiallyFull(bdescr *bd)
{
    return (bd->free + WORK_UNIT_WORDS < bd->start + BLOCK_SIZE_W);
}


#if DEBUG
void printMutableList (generation *gen);
#endif
