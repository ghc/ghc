/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.16 2002/11/22 06:54:05 matthewc Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * MegaBlock Allocator interface.
 *
 * ---------------------------------------------------------------------------*/

#ifndef __MBLOCK_H__
#define __MBLOCK_H__
extern lnat mblocks_allocated;

extern void * getMBlock(void);
extern void * getMBlocks(nat n);

#if osf3_TARGET_OS
/* ToDo: Perhaps by adjusting this value we can make linking without
 * -static work (i.e., not generate a core-dumping executable)? */
#if SIZEOF_VOID_P == 8
#define HEAP_BASE 0x180000000L
#else
#error I have no idea where to begin the heap on a non-64-bit osf3 machine.
#endif

#else

// we're using the generic method
#define HEAP_BASE 0

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
   -------------------------------------------------------------------------- */

extern StgWord8 mblock_map[];

#if SIZEOF_VOID_P == 4
/* On a 32-bit machine a 4KB table is always sufficient */
# define MBLOCK_MAP_SIZE	4096
# define MBLOCK_MAP_ENTRY(p)	((StgWord)(p) >> MBLOCK_SHIFT)
# define HEAP_ALLOCED(p)	mblock_map[MBLOCK_MAP_ENTRY(p)]
# define MARK_HEAP_ALLOCED(p)	(mblock_map[MBLOCK_MAP_ENTRY(p)] = 1)

#elif defined(ia64_TARGET_ARCH)
/* Instead of trying to cover the whole 64-bit address space (which would
 * require a better data structure), we assume that mmap allocates mappings
 * from the bottom of region 1, and track some portion of address space from
 * there upwards (currently 4GB). */
# define MBLOCK_MAP_SIZE	4096
# define MBLOCK_MAP_ENTRY(p)	(((StgWord)(p) - (1UL << 61)) >> MBLOCK_SHIFT)
# define HEAP_ALLOCED(p)	((MBLOCK_MAP_ENTRY(p) < MBLOCK_MAP_SIZE) \
					&& mblock_map[MBLOCK_MAP_ENTRY(p)])
# define MARK_HEAP_ALLOCED(p)	((MBLOCK_MAP_ENTRY(p) < MBLOCK_MAP_SIZE) \
					&& (mblock_map[MBLOCK_MAP_ENTRY(p)] = 1))

#elif defined(TEXT_BEFORE_HEAP)
/* Fall back to old method - assume heap above HEAP_BASE */
# define HEAP_ALLOCED(p)	((StgPtr)(p) >= (StgPtr)(HEAP_BASE))
# define MARK_HEAP_ALLOCED(p)	do {} while(0)

#else
# error HEAP_ALLOCED not defined
#endif

#endif // __MBLOCK_H__
