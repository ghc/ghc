/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * MegaBlock Allocator interface.
 *
 * See wiki commentary at
 *  http://ghc.haskell.org/trac/ghc/wiki/Commentary/HeapAlloced
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STORAGE_MBLOCK_H
#define RTS_STORAGE_MBLOCK_H

extern W_ peak_mblocks_allocated;
extern W_ mblocks_allocated;

extern void initMBlocks(void);
extern void * getMBlock(void);
extern void * getMBlocks(nat n);
extern void freeMBlocks(void *addr, nat n);
extern void releaseFreeMemory(void);
extern void freeAllMBlocks(void);

extern void *getFirstMBlock(void **state);
extern void *getNextMBlock(void **state, void *mblock);

#ifdef THREADED_RTS
// needed for HEAP_ALLOCED below
extern SpinLock gc_alloc_block_sync;
#endif

#endif /* RTS_STORAGE_MBLOCK_H */
