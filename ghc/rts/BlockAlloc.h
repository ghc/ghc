/* -----------------------------------------------------------------------------
 * $Id: BlockAlloc.h,v 1.3 1999/01/13 17:25:38 simonm Exp $
 *
 * Block Allocator Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef BLOCK_ALLOC_H
#define BLOCK_ALLOC_H

/* Initialisation ---------------------------------------------------------- */

extern void initBlockAllocator(void);

/* Allocation -------------------------------------------------------------- */

extern bdescr *allocGroup(nat n);
extern bdescr *allocBlock(void);

/* De-Allocation ----------------------------------------------------------- */

extern void freeGroup(bdescr *p);
extern void freeChain(bdescr *p);

/* Finding the block descriptor for a given block -------------------------- */

static inline bdescr *Bdescr(StgPtr p)
{
  return (bdescr *)
    ((((W_)p &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) 
     | ((W_)p & ~MBLOCK_MASK)
     );
}

/* Debugging  -------------------------------------------------------------- */

#ifdef DEBUG
extern void checkFreeListSanity(void);
nat         countFreeList(void);
#endif

#endif BLOCK_ALLOC_H
