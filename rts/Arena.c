/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 2001

   Arena allocation.  Arenas provide fast memory allocation at the
   expense of fine-grained recycling of storage: memory may be
   only be returned to the system by freeing the entire arena, it
   isn't possible to return individual objects within an arena.

   Do not assume that sequentially allocated objects will be adjacent
   in memory.
   
   Quirks: this allocator makes use of the RTS block allocator.  If
   the current block doesn't have enough room for the requested
   object, then a new block is allocated.  This means that allocating
   large objects will tend to result in wasted space at the end of
   each block.  In the worst case, half of the allocated space is
   wasted.  This allocator is therefore best suited to situations in
   which most allocations are small.
   -------------------------------------------------------------------------- */

#include "Rts.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "Arena.h"

#include <stdlib.h>

// Each arena struct is allocated using malloc().
struct _Arena {
    bdescr *current;
    StgWord *free;		// ptr to next free byte in current block
    StgWord *lim;		// limit (== last free byte + 1)
};

// We like to keep track of how many blocks we've allocated for 
// Storage.c:memInventory().
static long arena_blocks = 0;

// Begin a new arena
Arena *
newArena( void )
{
    Arena *arena;

    arena = stgMallocBytes(sizeof(Arena), "newArena");
    arena->current = allocBlock();
    arena->current->link = NULL;
    arena->free = arena->current->start;
    arena->lim  = arena->current->start + BLOCK_SIZE_W;
    arena_blocks++;

    return arena;
}

// The minimum alignment of an allocated block.
#define MIN_ALIGN 8

/* 'n' is assumed to be a power of 2 */
#define ROUNDUP(x,n)  (((x)+((n)-1))&(~((n)-1)))
#define B_TO_W(x)     ((x) / sizeof(W_))

// Allocate some memory in an arena
void  *
arenaAlloc( Arena *arena, size_t size )
{
    void *p;
    nat size_w;
    nat req_blocks;
    bdescr *bd;

    // round up to nearest alignment chunk.
    size = ROUNDUP(size,MIN_ALIGN);

    // size of allocated block in words.
    size_w = B_TO_W(size);

    if ( arena->free + size_w < arena->lim ) {
	// enough room in the current block...
	p = arena->free;
	arena->free += size_w;
	return p;
    } else {
	// allocate a fresh block...
	req_blocks =  (lnat)BLOCK_ROUND_UP(size) / BLOCK_SIZE;
	bd = allocGroup(req_blocks);
	arena_blocks += req_blocks;

	bd->gen_no  = 0;
	bd->step    = NULL;
	bd->flags   = 0;
	bd->free    = bd->start;
	bd->link    = arena->current;
	arena->current = bd;
	arena->free = bd->free + size_w;
	arena->lim = bd->free + bd->blocks * BLOCK_SIZE_W;
	return bd->start;
    }
}

// Free an entire arena
void
arenaFree( Arena *arena )
{
    bdescr *bd, *next;

    for (bd = arena->current; bd != NULL; bd = next) {
	next = bd->link;
	arena_blocks -= bd->blocks;
	ASSERT(arena_blocks >= 0);
	freeGroup(bd);
    }
    stgFree(arena);
}

unsigned long
arenaBlocks( void )
{
    return arena_blocks;
}

