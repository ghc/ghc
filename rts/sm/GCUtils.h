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

#ifndef SM_GCUTILS_H
#define SM_GCUTILS_H

#include "BeginPrivate.h"

#include "GCTDecl.h"

bdescr *allocBlock_sync (void);
bdescr *allocGroup_sync (nat n);
void    freeChain_sync  (bdescr *bd);
void    freeGroup_sync  (bdescr *bd);

void    push_scanned_block   (bdescr *bd, gen_workspace *ws);
StgPtr  todo_block_full      (nat size, gen_workspace *ws);
StgPtr  alloc_todo_block     (gen_workspace *ws, nat size);

bdescr *grab_local_todo_block  (gen_workspace *ws);
#if defined(THREADED_RTS)
bdescr *steal_todo_block       (nat s);
#endif

// Returns true if a block is partially full.  This predicate is used to try
// to re-use partial blocks wherever possible, and to reduce wastage.
// We might need to tweak the actual value.
INLINE_HEADER rtsBool
isPartiallyFull(bdescr *bd)
{
    return (bd->free + WORK_UNIT_WORDS < bd->start + BLOCK_SIZE_W);
}


#if DEBUG
void printMutableList (bdescr *bd);
#endif

// returns True if the given generation index belongs to
// another GC thread.
INLINE_HEADER rtsBool isNonLocalGen (generation *gen)
{
    return (gen->is_local && gen->cap != gct->index);
}

INLINE_HEADER rtsBool isNonLocalGenIx (nat ix)
{
    return isNonLocalGen(&all_generations[ix]);
}

// Version of recordMutableGen for use during GC.  This uses the
// mutable lists attached to the current gc_thread structure, which
// are the same as the mutable lists on the Capability.
INLINE_HEADER void
recordMutableGen_GC (StgClosure *p, nat gen_no)
{
    bdescr *bd;

    bd = gct->mut_lists[gen_no];
    if (bd->free >= bd->start + BLOCK_SIZE_W) {
	bdescr *new_bd;
        new_bd = allocBlock_sync();
	new_bd->link = bd;
	bd = new_bd;
	gct->mut_lists[gen_no] = bd;
    }
    *bd->free++ = (StgWord)p;
}

#include "EndPrivate.h"

#endif /* SM_GCUTILS_H */
