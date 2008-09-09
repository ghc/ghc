/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * MegaBlock Allocator interface.
 *
 * ---------------------------------------------------------------------------*/

#ifndef MBLOCK_H
#define MBLOCK_H

extern lnat RTS_VAR(mblocks_allocated);

extern void initMBlocks(void);
extern void * getMBlock(void);
extern void * getMBlocks(nat n);
extern void freeAllMBlocks(void);

#ifdef DEBUG
extern void *getFirstMBlock(void);
extern void *getNextMBlock(void *mblock);
#endif

/* -----------------------------------------------------------------------------
   The HEAP_ALLOCED() test.

   HEAP_ALLOCED is called FOR EVERY SINGLE CLOSURE during GC.
   It needs to be FAST.

   Implementation of HEAP_ALLOCED
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Since heap is allocated in chunks of megablocks (MBLOCK_SIZE), we
   can just use a table to record which megablocks in the address
   space belong to the heap.  On a 32-bit machine, with 1Mb
   megablocks, using 8 bits for each entry in the table, the table
   requires 4k.  Lookups during GC will be fast, because the table
   will be quickly cached (indeed, performance measurements showed no
   measurable difference between doing the table lookup and using a
   constant comparison).

   On 64-bit machines, we cache one 12-bit block map that describes
   4096 megablocks or 4GB of memory. If HEAP_ALLOCED is called for
   an address that is not in the cache, it calls slowIsHeapAlloced
   (see MBlock.c) which will find the block map for the 4GB block in
   question.
   -------------------------------------------------------------------------- */

#if SIZEOF_VOID_P == 4
extern StgWord8 mblock_map[];

/* On a 32-bit machine a 4KB table is always sufficient */
# define MBLOCK_MAP_SIZE	4096
# define MBLOCK_MAP_ENTRY(p)	((StgWord)(p) >> MBLOCK_SHIFT)
# define HEAP_ALLOCED(p)	mblock_map[MBLOCK_MAP_ENTRY(p)]

#elif SIZEOF_VOID_P == 8

# define MBLOCK_MAP_SIZE	4096
# define MBLOCK_MAP_ENTRY(p)	(((StgWord)(p) & 0xffffffff) >> MBLOCK_SHIFT)

typedef struct {
    StgWord32	addrHigh32;
    StgWord8	mblocks[MBLOCK_MAP_SIZE];
} MBlockMap;

extern MBlockMap *mblock_cache;

StgBool slowIsHeapAlloced(void *p);

# define HEAP_ALLOCED(p)  					\
	( ((((StgWord)(p)) >> 32) == mblock_cache->addrHigh32)	\
	? mblock_cache->mblocks[MBLOCK_MAP_ENTRY(p)]		\
	: slowIsHeapAlloced(p) )

#else
# error HEAP_ALLOCED not defined
#endif

#endif /* MBLOCK_H */
