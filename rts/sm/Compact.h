/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Compacting garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_COMPACT_H
#define SM_COMPACT_H

#include "BeginPrivate.h"

INLINE_HEADER void 
mark(StgPtr p, bdescr *bd)
{
    nat offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    *bitmap_word |= bit_mask;
}

INLINE_HEADER void 
unmark(StgPtr p, bdescr *bd)
{
    nat offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    *bitmap_word &= ~bit_mask;
}

INLINE_HEADER StgWord
is_marked(StgPtr p, bdescr *bd)
{
    nat offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    return (*bitmap_word & bit_mask);
}

void compact (StgClosure *static_objects);

#include "EndPrivate.h"

#endif /* SM_COMPACT_H */
