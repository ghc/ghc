/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block Allocator Interface
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

bdescr *allocLargeChunk (W_ min, W_ max);
bdescr *allocLargeChunkOnNode (uint32_t node, W_ min, W_ max);

/* Debugging  -------------------------------------------------------------- */

extern W_ countBlocks       (bdescr *bd);
extern W_ countAllocdBlocks (bdescr *bd);
extern void returnMemoryToOS(uint32_t n);

#if defined(DEBUG)
void checkFreeListSanity(void);
W_   countFreeList(void);
void markBlocks (bdescr *bd);
void reportUnmarkedBlocks (void);
#endif

extern W_ n_alloc_blocks;   // currently allocated blocks
extern W_ hw_alloc_blocks;  // high-water allocated blocks

#include "EndPrivate.h"
