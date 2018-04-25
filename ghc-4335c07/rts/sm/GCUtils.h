/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: utilities
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

#include "GCTDecl.h"

bdescr* allocGroup_sync(uint32_t n);
bdescr* allocGroupOnNode_sync(uint32_t node, uint32_t n);

INLINE_HEADER bdescr *allocBlock_sync(void)
{
    return allocGroup_sync(1);
}

INLINE_HEADER bdescr *allocBlockOnNode_sync(uint32_t node)
{
    return allocGroupOnNode_sync(node,1);
}

void    freeChain_sync(bdescr *bd);

void    push_scanned_block   (bdescr *bd, gen_workspace *ws);
StgPtr  todo_block_full      (uint32_t size, gen_workspace *ws);
StgPtr  alloc_todo_block     (gen_workspace *ws, uint32_t size);

bdescr *grab_local_todo_block  (gen_workspace *ws);
#if defined(THREADED_RTS)
bdescr *steal_todo_block       (uint32_t s);
#endif

// Returns true if a block is partially full.  This predicate is used to try
// to re-use partial blocks wherever possible, and to reduce wastage.
// We might need to tweak the actual value.
INLINE_HEADER bool
isPartiallyFull(bdescr *bd)
{
    return (bd->free + WORK_UNIT_WORDS < bd->start + BLOCK_SIZE_W);
}


#if defined(DEBUG)
void printMutableList (bdescr *bd);
#endif

// Version of recordMutableGen for use during GC.  This uses the
// mutable lists attached to the current gc_thread structure, which
// are the same as the mutable lists on the Capability.
INLINE_HEADER void
recordMutableGen_GC (StgClosure *p, uint32_t gen_no)
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
