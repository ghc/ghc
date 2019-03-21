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

INLINE_HEADER void
mark(StgPtr p, bdescr *bd)
{
    uint32_t offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap +
        (offset_within_block / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (BITS_IN(W_) - 1));
    *bitmap_word |= bit_mask;
}

INLINE_HEADER void
unmark(StgPtr p, bdescr *bd)
{
    uint32_t offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap +
        (offset_within_block / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (BITS_IN(W_) - 1));
    *bitmap_word &= ~bit_mask;
}

INLINE_HEADER StgWord
is_marked(StgPtr p, bdescr *bd)
{
    uint32_t offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap +
        (offset_within_block / BITS_IN(W_));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (BITS_IN(W_)- 1));
    return (*bitmap_word & bit_mask);
}

void compact (StgClosure *static_objects,
              StgWeak **dead_weak_ptr_list,
              StgTSO **resurrected_threads);

#include "EndPrivate.h"
