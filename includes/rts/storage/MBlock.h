/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * MegaBlock Allocator interface.
 *
 * See wiki commentary at
 *  http://hackage.haskell.org/trac/ghc/wiki/Commentary/HeapAlloced
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
extern void freeAllMBlocks(void);

extern void *getFirstMBlock(void);
extern void *getNextMBlock(void *mblock);

#ifdef THREADED_RTS
// needed for HEAP_ALLOCED below
extern SpinLock gc_alloc_block_sync;
#endif

/* -----------------------------------------------------------------------------
   The HEAP_ALLOCED() test.

   HEAP_ALLOCED is called FOR EVERY SINGLE CLOSURE during GC.
   It needs to be FAST.

   See wiki commentary at
     http://hackage.haskell.org/trac/ghc/wiki/Commentary/HeapAlloced

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
# define HEAP_ALLOCED_GC(p)     HEAP_ALLOCED(p)

/* -----------------------------------------------------------------------------
   HEAP_ALLOCED for 64-bit machines.

 Here are some cache layout options:

 [1]
 16KB cache of 16-bit entries, 1MB lines (capacity 8GB)
  mblock size =          20 bits
  entries   =     8192   13 bits
  line size =             0 bits (1 bit of value)
  tag size  =            15 bits
                       = 48 bits

 [2]
 32KB cache of 16-bit entries, 4MB lines (capacity 32GB)
  mblock size =          20 bits
  entries   =    16384   14 bits
  line size =             2 bits (4 bits of value)
  tag size  =            12 bits
                       = 48 bits

 [3]
 16KB cache of 16-bit entries, 2MB lines (capacity 16GB)
  mblock size =          20 bits
  entries   =    8192    13 bits
  line size =             1 bits (2 bits of value)
  tag size  =            14 bits
                       = 48 bits

 [4]
 4KB cache of 32-bit entries, 16MB lines (capacity 16GB)
  mblock size =          20 bits
  entries   =     1024   10 bits
  line size =             4 bits (16 bits of value)
  tag size  =            14 bits
                       = 48 bits

 [5]
 4KB cache of 64-bit entries, 32MB lines (capacity 16GB)
  mblock size =          20 bits
  entries   =     512     9 bits
  line size =             5 bits (32 bits of value)
  tag size  =            14 bits
                       = 48 bits

 We actually use none of the above.  After much experimentation it was
 found that optimising the lookup is the most important factor,
 followed by reducing the number of misses.  To that end, we use a
 variant of [1] in which each cache entry is ((mblock << 1) + value)
 where value is 0 for non-heap and 1 for heap.  The cache entries can
 be 32 bits, since the mblock number is 48-20 = 28 bits, and we need
 1 bit for the value.  The cache can be as big as we like, but
 currently we use 8k entries, giving us 8GB capacity.

 ---------------------------------------------------------------------------- */

#elif SIZEOF_VOID_P == 8

#define MBC_LINE_BITS 0
#define MBC_TAG_BITS 15

#if x86_64_HOST_ARCH
// 32bits are enough for 'entry' as modern amd64 boxes have
// only 48bit sized virtual addres.
typedef StgWord32 MbcCacheLine;
#else
// 32bits is not enough here as some arches (like ia64) use
// upper address bits to distinct memory areas.
typedef StgWord64 MbcCacheLine;
#endif

typedef StgWord8  MBlockMapLine;

#define MBLOCK_MAP_LINE(p)  (((StgWord)p & 0xffffffff) >> (MBLOCK_SHIFT + MBC_LINE_BITS))

#define MBC_LINE_SIZE  (1<<MBC_LINE_BITS)
#define MBC_SHIFT      (48 - MBLOCK_SHIFT - MBC_LINE_BITS - MBC_TAG_BITS)
#define MBC_ENTRIES    (1<<MBC_SHIFT)

extern MbcCacheLine mblock_cache[];

#define MBC_LINE(p) ((StgWord)p >> (MBLOCK_SHIFT + MBC_LINE_BITS))

#define MBLOCK_MAP_ENTRIES  (1 << (32 - MBLOCK_SHIFT - MBC_LINE_BITS))

typedef struct {
    StgWord32    addrHigh32;
    MBlockMapLine lines[MBLOCK_MAP_ENTRIES];
} MBlockMap;

extern W_ mpc_misses;

StgBool HEAP_ALLOCED_miss(StgWord mblock, void *p);

INLINE_HEADER
StgBool HEAP_ALLOCED(void *p)
{
    StgWord mblock;
    nat entry_no;
    MbcCacheLine entry, value;

    mblock   = (StgWord)p >> MBLOCK_SHIFT;
    entry_no = mblock & (MBC_ENTRIES-1);
    entry    = mblock_cache[entry_no];
    value    = entry ^ (mblock << 1);
    // this formulation coaxes gcc into prioritising the value==1
    // case, which we expect to be the most common.
    // __builtin_expect() didn't have any useful effect (gcc-4.3.0).
    if (value == 1) {
        return 1;
    } else if (value == 0) {
        return 0;
    } else {
        // putting the rest out of line turned out to be a slight
        // performance improvement:
        return HEAP_ALLOCED_miss(mblock,p);
    }
}

// In the parallel GC, the cache itself is safe to *read*, and can be
// updated atomically, but we need to place a lock around operations
// that touch the MBlock map.
INLINE_HEADER
StgBool HEAP_ALLOCED_GC(void *p)
{
    StgWord mblock;
    nat entry_no;
    MbcCacheLine entry, value;
    StgBool b;

    mblock   = (StgWord)p >> MBLOCK_SHIFT;
    entry_no = mblock & (MBC_ENTRIES-1);
    entry    = mblock_cache[entry_no];
    value    = entry ^ (mblock << 1);
    if (value == 1) {
        return 1;
    } else if (value == 0) {
        return 0;
    } else {
        // putting the rest out of line turned out to be a slight
        // performance improvement:
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        b = HEAP_ALLOCED_miss(mblock,p);
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
        return b;
    }
}

#else
# error HEAP_ALLOCED not defined
#endif

#endif /* RTS_STORAGE_MBLOCK_H */
