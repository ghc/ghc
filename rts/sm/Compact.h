/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Compacting garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* -----------------------------------------------------------------------------
   Note [Mark bits in mark-compact collector]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   In mark-compact collector each closure has two mark bits:

   - Liveness bit: not marked == unreachable (dead)

   - "Too large" bit: when this is set it means that the closure won't fit in
     the current heap block and we need to move it to the next chain in the
     heap.

   Why do we need the second bit? We only know a closure's size *before*
   threading it, because after threading the info table pointer will be end of
   the chain. So by the time we do the second pass to move the closures and
   unthread chains we'd have to do two passes, one for to get the info table
   pointer at the end of the chain to compute the closure size and update the
   free pointer if it's too large to fit in the current block, and then another
   pass to actually unthread.

   To avoid this we update the second bit when we first visit an object (in the
   "forward" pass) and realize that it won't fit in the current block, and check
   that bit in the second pass (where we actually move the object and update all
   references). If the bit is set we move the object to the free location in the
   next block in heap chain, otherwise we use the free pointer in the current
   block.
   -------------------------------------------------------------------------- */

#define MARK_COMPACT_LIVE      0
#define MARK_COMPACT_TOO_LARGE 1

INLINE_HEADER void
mark(StgPtr p, bdescr *bd, int offset)
{
    ASSERT(offset == MARK_COMPACT_LIVE || offset == MARK_COMPACT_TOO_LARGE);
    uint32_t offset_within_block = p - bd->start; // in words
    uint32_t offset_in_bitmap = offset_within_block * 2 + offset; // 2 per object
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + (offset_in_bitmap / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_in_bitmap & (BITS_IN(W_) - 1));
    *bitmap_word |= bit_mask;
}

INLINE_HEADER StgWord
is_marked(StgPtr p, bdescr *bd, int offset)
{
    // offset 0: liveness bit
    // offset 1: "too large" bit
    ASSERT(offset == MARK_COMPACT_LIVE || offset == MARK_COMPACT_TOO_LARGE);
    uint32_t offset_within_block = p - bd->start; // in words
    uint32_t offset_in_bitmap = offset_within_block * 2 + offset; // 2 per object
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + (offset_in_bitmap / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_in_bitmap & (BITS_IN(W_)- 1));
    return (*bitmap_word & bit_mask);
}

INLINE_HEADER void
mark_live(StgPtr p, bdescr *bd)
{
    mark(p, bd, MARK_COMPACT_LIVE);
}

INLINE_HEADER StgWord
is_marked_live(StgPtr p, bdescr *bd)
{
    return is_marked(p, bd, MARK_COMPACT_LIVE);
}

INLINE_HEADER void
mark_too_large(StgPtr p, bdescr *bd)
{
    mark(p, bd, MARK_COMPACT_TOO_LARGE);
}

INLINE_HEADER StgWord
is_marked_too_large(StgPtr p, bdescr *bd)
{
    return is_marked(p, bd, MARK_COMPACT_TOO_LARGE);
}

void compact (StgClosure *static_objects,
              StgWeak **dead_weak_ptr_list,
              StgTSO **resurrected_threads);

#include "EndPrivate.h"
