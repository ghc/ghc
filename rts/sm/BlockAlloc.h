/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block Allocator Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef BLOCK_ALLOC_H
#define BLOCK_ALLOC_H

#include "BeginPrivate.h"

bdescr *allocLargeChunk (W_ min, W_ max);

/* Debugging  -------------------------------------------------------------- */

extern W_ countBlocks       (bdescr *bd);
extern W_ countAllocdBlocks (bdescr *bd);
extern void returnMemoryToOS(nat n);

#ifdef DEBUG
void checkFreeListSanity(void);
W_   countFreeList(void);
void markBlocks (bdescr *bd);
void reportUnmarkedBlocks (void);
#endif

extern W_ n_alloc_blocks;   // currently allocated blocks
extern W_ hw_alloc_blocks;  // high-water allocated blocks

#include "EndPrivate.h"

#endif /* BLOCK_ALLOC_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
