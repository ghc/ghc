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

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

extern RTS_EXPORT W_ peak_mblocks_allocated;
extern RTS_EXPORT W_ mblocks_allocated;

extern RTS_EXPORT void initMBlocks(void);
extern RTS_EXPORT void * getMBlock(void);
extern RTS_EXPORT void * getMBlocks(uint32_t n);
extern RTS_EXPORT void * getMBlockOnNode(uint32_t node);
extern RTS_EXPORT void * getMBlocksOnNode(uint32_t node, uint32_t n);
extern RTS_EXPORT void freeMBlocks(void *addr, uint32_t n);
extern RTS_EXPORT void releaseFreeMemory(void);
extern RTS_EXPORT void freeAllMBlocks(void);

extern RTS_EXPORT void *getFirstMBlock(void **state);
extern RTS_EXPORT void *getNextMBlock(void **state, void *mblock);
