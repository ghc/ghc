/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.15 2002/10/21 11:38:54 simonmar Exp $
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

#if SIZEOF_VOID_P == 4

// This is the table.  Each byte is non-zero if the appropriate MBlock
// in the address space contains heap.
extern StgWord8 mblock_map[];

#define HEAP_ALLOCED(p) \
  ((int)(mblock_map[((StgWord)(p) & ~MBLOCK_MASK) >> MBLOCK_SHIFT]))

#else // SIZEOF_VOID_P != 4

// on a 64-bit machine, we need to extend the above scheme to use a
// 2-level mapping.  (ToDo)

#ifdef TEXT_BEFORE_HEAP
# define HEAP_ALLOCED(x)  ((StgPtr)(x) >= (StgPtr)(HEAP_BASE))
#else
#error HEAP_ALLOCED not defined
#endif

#endif // SIZEOF_VOID_P != 4

#endif // __MBLOCK_H__
