/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * MegaBlock Allocator interface.
 *
 * See wiki commentary at
 *  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/heap-alloced
 *
 * ---------------------------------------------------------------------------*/

#pragma once

extern RTS_PUBLIC W_ peak_mblocks_allocated;
extern RTS_PUBLIC W_ mblocks_allocated;

extern RTS_PUBLIC void initMBlocks(void);
extern RTS_PUBLIC void * getMBlock(void);
extern RTS_PUBLIC void * getMBlocks(uint32_t n);
extern RTS_PUBLIC void * getMBlockOnNode(uint32_t node);
extern RTS_PUBLIC void * getMBlocksOnNode(uint32_t node, uint32_t n);
extern RTS_PUBLIC void freeMBlocks(void *addr, uint32_t n);
extern RTS_PUBLIC void releaseFreeMemory(void);
extern RTS_PUBLIC void freeAllMBlocks(void);

extern RTS_PUBLIC void *getFirstMBlock(void **state);
extern RTS_PUBLIC void *getNextMBlock(void **state, void *mblock);
