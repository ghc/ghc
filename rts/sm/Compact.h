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
     the current heap block and we need to move it to the next block in the
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

   Instead of allocating two bits per object, we use the fact that most heap
   closures will be at least two words (as one-word closures will mostly be
   static objects), and allocate one bit per word in the heap. In the rare cases
   where we allocate single-word heap objects (e.g. a non-top-level FUN with
   empty payload) we add one non-pointer field to the payload so that the object
   will have two words. The minimum amount of words in the payload is defined in
   rts/include/rts/Constants.h as MIN_PAYLOAD_SIZE.

   (See also !1701 where we discussed lifting this restriction and allocating
   two bits per object)
   -------------------------------------------------------------------------- */

INLINE_HEADER void
mark(StgPtr p, bdescr *bd)
{
    uint32_t offset_within_block = p - bdescr_start(bd); // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap +
        (offset_within_block / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (BITS_IN(W_) - 1));
    *bitmap_word |= bit_mask;
}

INLINE_HEADER StgWord
is_marked(StgPtr p, bdescr *bd)
{
    uint32_t offset_within_block = p - bdescr_start(bd); // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap +
        (offset_within_block / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (BITS_IN(W_)- 1));
    return (*bitmap_word & bit_mask);
}

void compact (StgClosure *static_objects,
              StgWeak **dead_weak_ptr_list,
              StgTSO **resurrected_threads);

#include "EndPrivate.h"
