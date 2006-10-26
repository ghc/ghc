/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Compacting garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef GCCOMPACT_H
#define GCCOMPACT_H

INLINE_HEADER rtsBool
mark_stack_empty(void)
{
    return mark_sp == mark_stack;
}

INLINE_HEADER rtsBool
mark_stack_full(void)
{
    return mark_sp >= mark_splim;
}

INLINE_HEADER void
reset_mark_stack(void)
{
    mark_sp = mark_stack;
}

INLINE_HEADER void
push_mark_stack(StgPtr p)
{
    *mark_sp++ = p;
}

INLINE_HEADER StgPtr
pop_mark_stack(void)
{
    return *--mark_sp;
}

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

void compact(void);

#endif /* GCCOMPACT_H */
