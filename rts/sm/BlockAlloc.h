/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block Allocator Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef BLOCK_ALLOC_H
#define BLOCK_ALLOC_H

/* Debugging  -------------------------------------------------------------- */

#ifdef DEBUG
extern void checkFreeListSanity(void);
nat         countFreeList(void);
void        markBlocks (bdescr *bd);
void        reportUnmarkedBlocks (void);
#endif

extern lnat n_alloc_blocks;   // currently allocated blocks
extern lnat hw_alloc_blocks;  // high-water allocated blocks

#endif /* BLOCK_ALLOC_H */
