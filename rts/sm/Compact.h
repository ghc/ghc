/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Compacting garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_COMPACT_H
#define SM_COMPACT_H

#include "BeginPrivate.h"
#include "GCThread.h"

INLINE_HEADER void 
mark(StgPtr p, bdescr *bd)
{
    StgWord offset_within_block = ((W_)p & BLOCK_MASK) / sizeof(W_); // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    ASSERT(((W_)bitmap_word & MBLOCK_MASK) >= FIRST_BLOCK_OFF);
    *bitmap_word |= bit_mask;
}

INLINE_HEADER void 
unmark(StgPtr p, bdescr *bd)
{
    StgWord offset_within_block = ((W_)p & BLOCK_MASK) / sizeof(W_); // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    *bitmap_word &= ~bit_mask;
}

INLINE_HEADER StgWord
is_marked(StgPtr p, bdescr *bd)
{
    StgWord offset_within_block = ((W_)p & BLOCK_MASK) / sizeof(W_); // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    return (*bitmap_word & bit_mask);
}

void compact (gc_thread *);

#include "EndPrivate.h"

#endif /* SM_COMPACT_H */
