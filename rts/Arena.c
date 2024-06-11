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

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Arena.h"

// Each arena struct is allocated using malloc().
struct _Arena {
    bdescr *current;
    StgWord *free;              // ptr to next free byte in current block
    StgWord *lim;               // limit (== last free byte + 1)
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
    arena->current = allocBlock_lock();
    arena->current->link = NULL;
    arena->free = bdescr_start(arena->current);
    arena->lim  = bdescr_start(arena->current) + BLOCK_SIZE_W;
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
    uint32_t size_w;
    uint32_t req_blocks;
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
        req_blocks =  (W_)BLOCK_ROUND_UP(size) / BLOCK_SIZE;
        bd = allocGroup_lock(req_blocks);
        arena_blocks += bd->blocks;

        bd->gen_no  = 0;
        bd->dest_no = 0;
        bd->flags   = 0;
        bd->free    = bdescr_start(bd);
        bd->link    = arena->current;
        arena->current = bd;
        arena->free = bdescr_start(bd) + size_w;
        arena->lim = bdescr_start(bd) + bd->blocks * BLOCK_SIZE_W;
        return bdescr_start(bd);
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
        freeGroup_lock(bd);
    }
    stgFree(arena);
}

unsigned long
arenaBlocks( void )
{
    return arena_blocks;
}

#if defined(DEBUG)
void checkPtrInArena( StgPtr p, Arena *arena )
{
    // We don't update free pointers of arena blocks, so we have to check cached
    // free pointer for the first block.
    if (p >= bdescr_start(arena->current) && p < arena->free) {
        return;
    }

    // Rest of the blocks should be full (except there may be a little bit of
    // slop at the end). Again, free pointers are not updated so we can't use
    // those.
    for (bdescr *bd = arena->current->link; bd; bd = bd->link) {
        StgPtr start = bdescr_start(bd);
        if (p >= start && p < start + (bd->blocks*BLOCK_SIZE_W)) {
            return;
        }
    }

    barf("Location %p is not in arena %p", (void*)p, (void*)arena);
}
#endif
