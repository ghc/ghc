/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Compacting garbage collector
 *
 * ---------------------------------------------------------------------------*/

#ifndef GCCOMPACT_H
#define GCCOMPACT_H

STATIC_INLINE void 
mark(StgPtr p, bdescr *bd)
{
    nat offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    *bitmap_word |= bit_mask;
}

STATIC_INLINE void 
unmark(StgPtr p, bdescr *bd)
{
    nat offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    *bitmap_word &= ~bit_mask;
}

STATIC_INLINE StgWord
is_marked(StgPtr p, bdescr *bd)
{
    nat offset_within_block = p - bd->start; // in words
    StgPtr bitmap_word = (StgPtr)bd->u.bitmap + 
	(offset_within_block / (sizeof(W_)*BITS_PER_BYTE));
    StgWord bit_mask = (StgWord)1 << (offset_within_block & (sizeof(W_)*BITS_PER_BYTE - 1));
    return (*bitmap_word & bit_mask);
}

void compact( void (*get_roots)(evac_fn) );

#endif /* GCCOMPACT_H */
