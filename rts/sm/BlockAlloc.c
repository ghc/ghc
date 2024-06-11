/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * The block allocator and free list manager.
 *
 * This is the architecture independent part of the block allocator.
 * It requires only the following support from the operating system:
 *
 *    void *getMBlocks(uint32_t n);
 *
 * returns the address of an n*MBLOCK_SIZE region of memory, aligned on
 * an MBLOCK_SIZE boundary.  There are no other restrictions on the
 * addresses of memory returned by getMBlocks().
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "OSMem.h"

#include <string.h>

static void  initMBlock(void *mblock, uint32_t node);

/*
 * By default the DEBUG RTS is built with block allocator assertions
 * enabled. However, these are quite expensive and consequently it can
 * sometimes be useful to disable them if debugging an issue known to be
 * elsewhere
 */
#if defined(DEBUG)
#define BLOCK_ALLOC_DEBUG
#endif

/* -----------------------------------------------------------------------------

  Implementation notes
  ~~~~~~~~~~~~~~~~~~~~

  Terminology:
    - bdescr = block descriptor
    - bgroup = block group (1 or more adjacent blocks)
    - mblock = mega block
    - mgroup = mega group (1 or more adjacent mblocks)

   Invariants on block descriptors
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   bdescr_start(bd) always points to the start of the block.

   bd->free is either:
      - zero for a non-group-head; bd->link points to the head
      - (-1) for the head of a free block group
      - or it points within the block (group)

   bd->blocks is either:
      - zero for a non-group-head; bd->link points to the head
      - number of blocks in this group otherwise

   bd->link either points to a block descriptor or is NULL

   The following fields are not used by the allocator:
     bd->flags
     bd->gen_no
     bd->gen
     bd->dest

  Exceptions: we don't maintain invariants for all the blocks within a
  group on the free list, because it is expensive to modify every
  bdescr in a group when coalescing.  Just the head and last bdescrs
  will be correct for a group on the free list.


  Free lists
  ~~~~~~~~~~

  Preliminaries:
    - most allocations are for a small number of blocks
    - sometimes the OS gives us new memory backwards in the address
      space, sometimes forwards, so we should not be biased towards
      any particular layout in the address space
    - We want to avoid fragmentation
    - We want allocation and freeing to be O(1) or close.

  Coalescing trick: when a bgroup is freed (freeGroup()), we can check
  whether it can be coalesced with other free bgroups by checking the
  bdescrs for the blocks on either side of it.  This means that we can
  coalesce in O(1) time.  Every free bgroup must have its head and tail
  bdescrs initialised, the rest don't matter.

  We keep the free list in buckets, using a heap-sort strategy.
  Bucket N contains blocks with sizes 2^N - 2^(N+1)-1.  The list of
  blocks in each bucket is doubly-linked, so that if a block is
  coalesced we can easily remove it from its current free list.

  To allocate a new block of size S, grab a block from bucket
  log2ceiling(S) (i.e. log2() rounded up), in which all blocks are at
  least as big as S, and split it if necessary.  If there are no
  blocks in that bucket, look at bigger buckets until a block is found
  Allocation is therefore O(logN) time.

  To free a block:
    - coalesce it with neighbours.
    - remove coalesced neighbour(s) from free list(s)
    - add the new (coalesced) block to the front of the appropriate
      bucket, given by log2(S) where S is the size of the block.

  Free is O(1).

  Megablocks
  ~~~~~~~~~~

  Separately from the free list of block groups, which are smaller than
  an mblock, we maintain a free list of mblock groups.  This is the unit
  of memory the operating system gives us, and we may either split mblocks
  into blocks or allocate them directly (when very large contiguous regions
  of memory).  mblocks have a different set of invariants than blocks:

  bdescr_start(bd) points to the start of the block IF the block is in the first mblock
  bd->blocks and bd->link are only valid IF this block is the first block
    of the first mblock
  No other fields are used (in particular, free is not used, meaning that
    space that is not used by the (single) object is wasted.

  This has implications for the free list as well:
  We cannot play the coalescing trick with mblocks, because there is
  no requirement that the bdescrs in the second and subsequent mblock
  of an mgroup are initialised (the mgroup might be filled with a
  large array, overwriting the bdescrs for example).

  The separate free list for megablocks is thus sorted in *address*
  order, so that we can coalesce.  Allocation in this list is best-fit
  by traversing the whole list: we don't expect this list to be long,
  and allocation/freeing of large blocks is rare; avoiding
  fragmentation is more important than performance here.

  freeGroup() might end up moving a block from free_list to
  free_mblock_list, if after coalescing we end up with a full mblock.

  checkFreeListSanity() checks all the invariants on the free lists.

  --------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------
   WATCH OUT FOR OVERFLOW

   Be very careful with integer overflow here.  If you have an
   expression like (n_blocks * BLOCK_SIZE), and n_blocks is an int or
   a uint32_t, then it will very likely overflow on a 64-bit platform.
   Always cast to StgWord (or W_ for short) first: ((W_)n_blocks * BLOCK_SIZE).

  --------------------------------------------------------------------------- */

// free_list[i] contains blocks that are at least size 2^i, and at
// most size 2^(i+1) - 1.
//
// To find the free list in which to place a block, use log_2(size).
// To find a free block of the right size, use log_2_ceil(size).
//
// The largest free list (free_list[NUM_FREE_LISTS-1]) needs to contain sizes
// from half a megablock up to (but not including) a full megablock.

#define NUM_FREE_LISTS (MBLOCK_SHIFT-BLOCK_SHIFT)

// In THREADED_RTS mode, the free list is protected by sm_mutex.

static bdescr *free_list[MAX_NUMA_NODES][NUM_FREE_LISTS];
static bdescr *free_mblock_list[MAX_NUMA_NODES];

// For avoiding quadratic runtime performance when freeing a large number of
// mblocks during a single GC run, free will be deferred to a separate free list
// that foregoes sorting and coalecense. As the final step in a GC run we can
// then separately sort the deferred list, and merge it with the actual free
// list in one go.
static bool defer_mblock_frees;
static bdescr *deferred_free_mblock_list[MAX_NUMA_NODES];

W_ n_alloc_blocks;   // currently allocated blocks
W_ hw_alloc_blocks;  // high-water allocated blocks

W_ n_alloc_blocks_by_node[MAX_NUMA_NODES];


static bdescr* splitDeferredList(bdescr* head);
static void sortDeferredList(bdescr** head);

/* -----------------------------------------------------------------------------
   Initialisation
   -------------------------------------------------------------------------- */

void initBlockAllocator(void)
{
    uint32_t i, node;
    for (node = 0; node < MAX_NUMA_NODES; node++) {
        for (i=0; i < NUM_FREE_LISTS; i++) {
            free_list[node][i] = NULL;
        }
        free_mblock_list[node] = NULL;
        n_alloc_blocks_by_node[node] = 0;
    }
    n_alloc_blocks = 0;
    hw_alloc_blocks = 0;
}

/* -----------------------------------------------------------------------------
   Accounting
   -------------------------------------------------------------------------- */

STATIC_INLINE
void recordAllocatedBlocks(uint32_t node, uint32_t n)
{
    n_alloc_blocks += n;
    n_alloc_blocks_by_node[node] += n;
    if (n > 0 && n_alloc_blocks > hw_alloc_blocks) {
        hw_alloc_blocks = n_alloc_blocks;
    }
}

STATIC_INLINE
void recordFreedBlocks(uint32_t node, uint32_t n)
{
    ASSERT(n_alloc_blocks >= n);
    n_alloc_blocks -= n;
    n_alloc_blocks_by_node[node] -= n;
}

/* -----------------------------------------------------------------------------
   Allocation
   -------------------------------------------------------------------------- */

STATIC_INLINE bdescr *
tail_of (bdescr *bd)
{
    return bd + bd->blocks - 1;
}

STATIC_INLINE void
initGroup(bdescr *head)
{
  head->free   = bdescr_start(head);
  head->link   = NULL;

  // If this is a block group (but not a megablock group), we
  // make the last block of the group point to the head.  This is used
  // when coalescing blocks in freeGroup().  We don't do this for
  // megablock groups because blocks in the second and subsequent
  // mblocks don't have bdescrs; freeing these is handled in a
  // different way by free_mblock_group().
  if (head->blocks > 1 && head->blocks <= BLOCKS_PER_MBLOCK) {
      bdescr *last = tail_of(head);
      last->blocks = 0;
      last->link = head;
  }

#if defined(BLOCK_ALLOC_DEBUG)
  for (uint32_t i=0; i < head->blocks; i++) {
      head[i].flags = 0;
  }
#endif
}

#if SIZEOF_VOID_P == SIZEOF_LONG
#define CLZW(n) (__builtin_clzl(n))
#else
#define CLZW(n) (__builtin_clzll(n))
#endif

// log base 2 (floor), needs to support up to (2^NUM_FREE_LISTS)-1
STATIC_INLINE uint32_t
log_2(W_ n)
{
    ASSERT(n > 0 && n < (1<<NUM_FREE_LISTS));
#if defined(__GNUC__)
    return CLZW(n) ^ (sizeof(StgWord)*8 - 1);
    // generates good code on x86.  __builtin_clz() compiles to bsr+xor, but
    // we want just bsr, so the xor here cancels out gcc's xor.
#else
    W_ i, x;
    x = n;
    for (i=0; i < NUM_FREE_LISTS; i++) {
        x = x >> 1;
        if (x == 0) return i;
    }
    return NUM_FREE_LISTS;
#endif
}

// log base 2 (ceiling), needs to support up to (2^NUM_FREE_LISTS)-1
STATIC_INLINE uint32_t
log_2_ceil(W_ n)
{
    ASSERT(n > 0 && n < (1<<NUM_FREE_LISTS));
#if defined(__GNUC__)
    uint32_t r = log_2(n);
    return (n & (n-1)) ? r+1 : r;
#else
    W_ i, x;
    x = 1;
    for (i=0; i < MAX_FREE_LIST; i++) {
        if (x >= n) return i;
        x = x << 1;
    }
    return MAX_FREE_LIST;
#endif
}

STATIC_INLINE void
free_list_insert (uint32_t node, bdescr *bd)
{
    uint32_t ln;

    ASSERT(bd->blocks < BLOCKS_PER_MBLOCK);
    ln = log_2(bd->blocks);

    dbl_link_onto(bd, &free_list[node][ln]);
}

// After splitting a group, the last block of each group must have a
// tail that points to the head block, to keep our invariants for
// coalescing.
STATIC_INLINE void
setup_tail (bdescr *bd)
{
    bdescr *tail;
    tail = tail_of(bd);
    if (tail != bd) {
        tail->blocks = 0;
        tail->free = 0;
        tail->link = bd;
    }
}


// Take a free block group bd, and split off a group of size n from
// it.  Adjust the free list as necessary, and return the new group.
static bdescr *
split_free_block (bdescr *bd, uint32_t node, W_ n, uint32_t ln /* log_2_ceil(n) */)
{
    bdescr *fg; // free group

    ASSERT(bd->blocks > n);
    dbl_link_remove(bd, &free_list[node][ln]);
    fg = bd + bd->blocks - n; // take n blocks off the end
    fg->blocks = n;
    bd->blocks -= n;
    setup_tail(bd);
    ln = log_2(bd->blocks);
    dbl_link_onto(bd, &free_list[node][ln]);
    return fg;
}

// Take N blocks off the end, free the rest.
static bdescr *
split_block_high (bdescr *bd, W_ n)
{
    ASSERT(bd->blocks > n);

    bdescr* ret = bd + bd->blocks - n; // take n blocks off the end
    ret->blocks = n;
    ret->start = ret->free = bdescr_start(bd) + (bd->blocks - n)*BLOCK_SIZE_W;
    ret->link = NULL;

    bd->blocks -= n;

    setup_tail(ret);
    setup_tail(bd);
    freeGroup(bd);

    return ret;
}

// Like `split_block_high`, but takes n blocks off the beginning rather
// than the end.
static bdescr *
split_block_low (bdescr *bd, W_ n)
{
    ASSERT(bd->blocks > n);

    bdescr* bd_ = bd + n;
    bd_->blocks = bd->blocks - n;
    bd_->start = bd_->free = bdescr_start(bd) + (bd->blocks - n)*BLOCK_SIZE_W;

    bd->blocks = n;

    setup_tail(bd_);
    setup_tail(bd);
    freeGroup(bd_);

    return bd;
}


// A variant of `split_block_high` where we keep both blocks.
// The returned block has size `n`, which is split off `bd`.
static bdescr *
split_block_high_no_free (bdescr *bd, W_ n)
{
    ASSERT(bd->blocks > n);

    bdescr* ret = bd + bd->blocks - n; // take n blocks off the end
    ret->blocks = n;
    ret->start = ret->free = bdescr_start(bd) + (bd->blocks - n)*BLOCK_SIZE_W;
    ret->link = NULL;

    bd->blocks -= n;

    setup_tail(ret);
    setup_tail(bd);

    return ret;
}

// Allocate a MBlock worth of `n` block sized chunks aligned at `n`-block boundry.
// This returns a linked list of `bdescr` of length `BLOCKS_PER_MBLOCK / n`.
// We assume relevant locks are held.
bdescr *
allocMBlockAlignedGroupOnNode(uint32_t node, W_ n)
{
    bdescr *bd = allocGroupOnNode(node, BLOCKS_PER_MBLOCK);

    // Free unaligned blocks, as we can't use these.
    ASSERT(bd->blocks == BLOCKS_PER_MBLOCK);
    bd = split_block_high(bd, bd->blocks - bd->blocks % n);
    ASSERT(bd->blocks % n == 0);

    bdescr *last = NULL;
    bdescr *chunk = NULL;
    // Chain the aligned groups together into a linked-list
    while (bd->blocks > n) {
      chunk = split_block_high_no_free(bd, n);
      chunk->link = last;
      last = chunk;
    }
    bd->link = chunk;

    return bd;
}


/* Find a fitting block for the allocation request in the given free list.
   Returns:
     - not NULL: when an exact match was found in the free list.
     - NULL: when no exact match was found. In this case, the out parameter
       `best` can be inspected to get the best fitting block from the free list.
 */
static bdescr *
alloc_mega_group_from_free_list (bdescr** free_list_head, StgWord n, bdescr** best)
{
    bdescr *bd, *prev;
    *best = NULL;
    prev = NULL;
    for (bd = *free_list_head; bd != NULL; prev = bd, bd = bd->link)
    {
        if (bd->blocks == n)
        {
            if (prev) {
                prev->link = bd->link;
            } else {
                *free_list_head = bd->link;
            }
            return bd;
        }
        else if (bd->blocks > n)
        {
            if (!*best || bd->blocks < (*best)->blocks)
            {
                *best = bd;
            }
        }
    }
    return NULL;
}

/* Only initializes the start pointers on the first megablock and the
 * blocks field of the first bdescr; callers are responsible for calling
 * initGroup afterwards.
 */
static bdescr *
alloc_mega_group (uint32_t node, StgWord mblocks)
{
    bdescr *best, *bd;
    StgWord n;

    n = MBLOCK_GROUP_BLOCKS(mblocks);

    if(defer_mblock_frees)
    {
        // Freeing mega groups is currently deferred. Try to serve new requests
        // preferentially from our deferred free list.
        bd = alloc_mega_group_from_free_list(&deferred_free_mblock_list[node], n, &best);
        if(bd)
        {
            return bd;
        }
        else if(!best)
        {
            // If there was neither an exact nor a best match, try the regular free list.
            bd = alloc_mega_group_from_free_list(&free_mblock_list[node], n, &best);
        }
    }
    else
    {
        // Otherwise, just always use the regular free list
        bd = alloc_mega_group_from_free_list(&free_mblock_list[node], n, &best);
    }

    if (bd)
    {
        return bd;
    }
    else if (best)
    {
        // we take our chunk off the end here.
        StgWord best_mblocks  = BLOCKS_TO_MBLOCKS(best->blocks);
        bd = FIRST_BDESCR((StgWord8*)MBLOCK_ROUND_DOWN(best) +
                          (best_mblocks-mblocks)*MBLOCK_SIZE);

        best->blocks = MBLOCK_GROUP_BLOCKS(best_mblocks - mblocks);
        initMBlock(MBLOCK_ROUND_DOWN(bd), node);
    }
    else
    {
        void *mblock;
        if (RtsFlags.GcFlags.numa) {
            mblock = getMBlocksOnNode(node, mblocks);
        } else {
            mblock = getMBlocks(mblocks);
        }
        initMBlock(mblock, node); // only need to init the 1st one
        bd = FIRST_BDESCR(mblock);
    }
    bd->blocks = MBLOCK_GROUP_BLOCKS(mblocks);
    return bd;
}

bdescr *
allocGroupOnNode (uint32_t node, W_ n)
{
    bdescr *bd, *rem;
    StgWord ln;

    if (n == 0) barf("allocGroup: requested zero blocks");

    if (n >= BLOCKS_PER_MBLOCK)
    {
        StgWord mblocks;

        mblocks = BLOCKS_TO_MBLOCKS(n);

        // n_alloc_blocks doesn't count the extra blocks we get in a
        // megablock group.
        recordAllocatedBlocks(node, mblocks * BLOCKS_PER_MBLOCK);

        bd = alloc_mega_group(node, mblocks);
        // only the bdescrs of the first MB are required to be initialised
        initGroup(bd);
        goto finish;
    }

    recordAllocatedBlocks(node, n);

    ln = log_2_ceil(n);

    while (ln < NUM_FREE_LISTS && free_list[node][ln] == NULL) {
        ln++;
    }

    if (ln == NUM_FREE_LISTS) {
#if 0  /* useful for debugging fragmentation */
        if ((W_)mblocks_allocated * BLOCKS_PER_MBLOCK * BLOCK_SIZE_W
             - (W_)((n_alloc_blocks - n) * BLOCK_SIZE_W) > (2*1024*1024)/sizeof(W_)) {
            debugBelch("Fragmentation, wanted %d blocks, %ld MB free\n", n, ((mblocks_allocated * BLOCKS_PER_MBLOCK) - n_alloc_blocks) / BLOCKS_PER_MBLOCK);
            RtsFlags.DebugFlags.block_alloc = 1;
            checkFreeListSanity();
        }
#endif

        bd = alloc_mega_group(node,1);
        bd->blocks = n;
        initGroup(bd);                   // we know the group will fit
        rem = bd + n;
        rem->blocks = BLOCKS_PER_MBLOCK-n;
        initGroup(rem);                  // init the slop
        recordAllocatedBlocks(node,rem->blocks);
        freeGroup(rem);                  // add the slop on to the free list
        goto finish;
    }

    bd = free_list[node][ln];

    if (bd->blocks == n)                // exactly the right size!
    {
        dbl_link_remove(bd, &free_list[node][ln]);
        initGroup(bd);
    }
    else if (bd->blocks >  n)            // block too big...
    {
        bd = split_free_block(bd, node, n, ln);
        ASSERT(bd->blocks == n);
        initGroup(bd);
    }
    else
    {
        barf("allocGroup: free list corrupted");
    }

finish:
    IF_DEBUG(zero_on_gc, memset(bdescr_start(bd), 0xaa, bd->blocks * BLOCK_SIZE));
    IF_DEBUG(sanity, checkFreeListSanity());
    return bd;
}

// Allocate `n` blocks aligned to `n` blocks, e.g. when n = 8, the blocks will
// be aligned at `8 * BLOCK_SIZE`. For a group with `n` blocks this can be used
// for easily accessing the beginning of the group from a location p in the
// group with
//
//     p % (BLOCK_SIZE*n)
//
// Used by the non-moving collector for allocating segments.
//
// Because the storage manager does not support aligned allocations, we have to
// allocate `2*n - 1` blocks here to make sure we'll be able to find an aligned
// region in the allocated blocks. After finding the aligned area we want to
// free slop on the low and high sides, and block allocator doesn't support
// freeing only some portion of a megablock (we can only free whole megablocks).
// So we disallow allocating megablocks here, and allow allocating at most
// `BLOCKS_PER_MBLOCK / 2` blocks.
bdescr *
allocAlignedGroupOnNode (uint32_t node, W_ n)
{
    // allocate enough blocks to have enough space aligned at n-block boundary
    // free any slops on the low and high side of this space

    // number of blocks to allocate to make sure we have enough aligned space
    W_ num_blocks = 2*n - 1;

    if (num_blocks >= BLOCKS_PER_MBLOCK) {
        barf("allocAlignedGroupOnNode: allocating megablocks is not supported\n"
             "    requested blocks: %" FMT_Word "\n"
             "    required for alignment: %" FMT_Word "\n"
             "    megablock size (in blocks): %" FMT_Word,
             n, num_blocks, (W_) BLOCKS_PER_MBLOCK);
    }

    W_ group_size = n * BLOCK_SIZE;

    // To reduce splitting and fragmentation we use allocLargeChunkOnNode here.
    // Tweak the max allocation to avoid allocating megablocks. Splitting slop
    // below doesn't work with megablocks (freeGroup can't free only a portion
    // of a megablock so we can't allocate megablocks and free some parts of
    // them).
    W_ max_blocks = stg_min(num_blocks * 3, BLOCKS_PER_MBLOCK - 1);
    bdescr *bd = allocLargeChunkOnNode(node, num_blocks, max_blocks);
    // We may allocate more than num_blocks, so update it
    num_blocks = bd->blocks;

    // slop on the low side
    W_ slop_low = 0;
    if ((uintptr_t)bdescr_start(bd) % group_size != 0) {
        slop_low = group_size - ((uintptr_t)bdescr_start(bd) % group_size);
    }

    W_ slop_high = (num_blocks * BLOCK_SIZE) - group_size - slop_low;

    ASSERT((slop_low % BLOCK_SIZE) == 0);
    ASSERT((slop_high % BLOCK_SIZE) == 0);

    W_ slop_low_blocks = slop_low / BLOCK_SIZE;
    W_ slop_high_blocks = slop_high / BLOCK_SIZE;

    ASSERT(slop_low_blocks + slop_high_blocks + n == num_blocks);

#if defined(BLOCK_ALLOC_DEBUG)
    checkFreeListSanity();
    W_ free_before = countFreeList();
#endif

    if (slop_low_blocks != 0) {
        bd = split_block_high(bd, num_blocks - slop_low_blocks);
        ASSERT(countBlocks(bd) == num_blocks - slop_low_blocks);
    }

#if defined(BLOCK_ALLOC_DEBUG)
    ASSERT(countFreeList() == free_before + slop_low_blocks);
    checkFreeListSanity();
#endif

    // At this point the bd should be aligned, but we may have slop on the high side
    ASSERT((uintptr_t)bdescr_start(bd) % group_size == 0);

#if defined(BLOCK_ALLOC_DEBUG)
    free_before = countFreeList();
#endif

    if (slop_high_blocks != 0) {
        bd = split_block_low(bd, n);
        ASSERT(bd->blocks == n);
    }

#if defined(BLOCK_ALLOC_DEBUG)
    ASSERT(countFreeList() == free_before + slop_high_blocks);
    checkFreeListSanity();
#endif

    // Should still be aligned
    ASSERT((uintptr_t)bdescr_start(bd) % group_size == 0);

    return bd;
}

STATIC_INLINE
uint32_t nodeWithLeastBlocks (void)
{
    uint32_t node = 0, i;
    uint32_t min_blocks = n_alloc_blocks_by_node[0];
    for (i = 1; i < n_numa_nodes; i++) {
        if (n_alloc_blocks_by_node[i] < min_blocks) {
            min_blocks = n_alloc_blocks_by_node[i];
            node = i;
        }
    }
    return node;
}

bdescr* allocGroup (W_ n)
{
    return allocGroupOnNode(nodeWithLeastBlocks(),n);
}


//
// Allocate a chunk of blocks that is at least min and at most max
// blocks in size. This API is used by the nursery allocator that
// wants contiguous memory preferably, but doesn't require it.  When
// memory is fragmented we might have lots of chunks that are
// less than a full megablock, so allowing the nursery allocator to
// use these reduces fragmentation considerably.  e.g. on a GHC build
// with +RTS -H, I saw fragmentation go from 17MB down to 3MB on a
// single compile.
//
// Further to this: in #7257 there is a program that creates serious
// fragmentation such that the heap is full of tiny <4 block chains.
// The nursery allocator therefore has to use single blocks to avoid
// fragmentation, but we make sure that we allocate large blocks
// preferably if there are any.
//
bdescr* allocLargeChunkOnNode (uint32_t node, W_ min, W_ max)
{
    bdescr *bd;
    StgWord ln, lnmax;

    if (min >= BLOCKS_PER_MBLOCK) {
        return allocGroupOnNode(node,max);
    }

    ln = log_2_ceil(min);
    lnmax = log_2_ceil(max);

    while (ln < NUM_FREE_LISTS && ln < lnmax && free_list[node][ln] == NULL) {
        ln++;
    }
    if (ln == NUM_FREE_LISTS || ln == lnmax) {
        return allocGroupOnNode(node,max);
    }
    bd = free_list[node][ln];

    if (bd->blocks <= max)              // exactly the right size!
    {
        dbl_link_remove(bd, &free_list[node][ln]);
        initGroup(bd);
    }
    else   // block too big...
    {
        bd = split_free_block(bd, node, max, ln);
        ASSERT(bd->blocks == max);
        initGroup(bd);
    }

    recordAllocatedBlocks(node, bd->blocks);

    IF_DEBUG(zero_on_gc, memset(bdescr_start(bd), 0xaa, bd->blocks * BLOCK_SIZE));
    IF_DEBUG(sanity, checkFreeListSanity());
    return bd;
}

bdescr* allocLargeChunk (W_ min, W_ max)
{
    return allocLargeChunkOnNode(nodeWithLeastBlocks(), min, max);
}

bdescr *
allocGroup_lock(W_ n)
{
    bdescr *bd;
    ACQUIRE_SM_LOCK;
    bd = allocGroup(n);
    RELEASE_SM_LOCK;
    return bd;
}

bdescr *
allocBlock_lock(void)
{
    bdescr *bd;
    ACQUIRE_SM_LOCK;
    bd = allocBlock();
    RELEASE_SM_LOCK;
    return bd;
}

bdescr *
allocGroupOnNode_lock(uint32_t node, W_ n)
{
    bdescr *bd;
    ACQUIRE_SM_LOCK;
    bd = allocGroupOnNode(node,n);
    RELEASE_SM_LOCK;
    return bd;
}

bdescr *
allocBlockOnNode_lock(uint32_t node)
{
    bdescr *bd;
    ACQUIRE_SM_LOCK;
    bd = allocBlockOnNode(node);
    RELEASE_SM_LOCK;
    return bd;
}

/* -----------------------------------------------------------------------------
   De-Allocation
   -------------------------------------------------------------------------- */

STATIC_INLINE bdescr *
coalesce_mblocks (bdescr *p)
{
    bdescr *q;

    q = p->link;
    if (q != NULL &&
        MBLOCK_ROUND_DOWN(q) ==
        (StgWord8*)MBLOCK_ROUND_DOWN(p) +
        BLOCKS_TO_MBLOCKS(p->blocks) * MBLOCK_SIZE) {
        // can coalesce
        p->blocks  = MBLOCK_GROUP_BLOCKS(BLOCKS_TO_MBLOCKS(p->blocks) +
                                         BLOCKS_TO_MBLOCKS(q->blocks));
        p->link = q->link;
        return p;
    }
    return q;
}

static void
free_mega_group (bdescr *mg)
{
    bdescr *bd, *prev;
    uint32_t node;

    node = mg->node;

    if(defer_mblock_frees) {
        // Put the block on the deferred free list without coalescing.
        mg->link = deferred_free_mblock_list[node];
        deferred_free_mblock_list[node] = mg;
    } else {
        // Find the right place in the free list.  free_mblock_list is
        // sorted by *address*, not by size as the free_list is.
        prev = NULL;
        bd = free_mblock_list[node];
        while (bd && bdescr_start(bd) < bdescr_start(mg)) {
            prev = bd;
            bd = bd->link;
        }

        // coalesce backwards
        if (prev)
        {
            mg->link = prev->link;
            prev->link = mg;
            mg = coalesce_mblocks(prev);
        }
        else
        {
            mg->link = free_mblock_list[node];
            free_mblock_list[node] = mg;
        }
        // coalesce forwards
        coalesce_mblocks(mg);

        IF_DEBUG(sanity, checkFreeListSanity());
    }
}

static void
free_deferred_mega_groups (uint32_t node)
{
    bdescr *mg, *bd, *prev, *new_head;

    sortDeferredList(&deferred_free_mblock_list[node]);

    new_head = deferred_free_mblock_list[node];
    deferred_free_mblock_list[node] = NULL;

    // Keeping track of the location in the free list
    prev = NULL;
    bd = free_mblock_list[node];

    while(new_head != NULL) {
        // Obtain mblock to free
        mg = new_head;
        new_head = new_head->link;

        // Find the right place in the free list. This is similar to the process
        // in `free_mega_group`, but we can exploit that the deferred list is
        // sorted: the search starts out where the previous mblock was inserted.
        // This means we only need to traverse the free list once to free all
        // the mblocks, rather than once per mblock.
        while (bd && bdescr_start(bd) < bdescr_start(mg)) {
            prev = bd;
            bd = bd->link;
        }

        // coalesce backwards
        if (prev)
        {
            mg->link = prev->link;
            prev->link = mg;
            mg = coalesce_mblocks(prev);
        }
        else
        {
            mg->link = free_mblock_list[node];
            free_mblock_list[node] = mg;
        }

        // coalesce forwards
        coalesce_mblocks(mg);

        // initialize search for next round
        prev = mg;
        bd = prev->link;
    }

    IF_DEBUG(sanity, checkFreeListSanity());
}


/* Note [Data races in freeGroup]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * freeGroup commits a rather serious concurrency sin in its block coalescence
 * logic: When freeing a block it looks at bd->free of the previous/next block
 * to see whether it is allocated. However, the free'ing thread likely does not
 * own the previous/next block, nor do we make any attempt to synchronize with
 * the thread that *does* own it; this makes this access a data race.
 *
 * The original design argued that this was correct because `bd->free` will
 * only take a value of -1 when the block is free and thereby owned by the
 * storage manager. However, this is nevertheless unsafe under the C11 data
 * model, which guarantees no particular semantics for data races.
 *
 * We currently assume (and hope) we won't see torn values and consequently
 * we will never see `bd->free == -1` for an allocated block which we do not
 * own. However, this is all extremely dodgy.
 *
 * This is tracked as #18913.
 */

void
freeGroup(bdescr *p)
{
  StgWord ln;
  uint32_t node;

  // not true in multithreaded GC:
  // ASSERT_SM_LOCK();

  ASSERT(RELAXED_LOAD(&p->free) != (P_)-1);

#if defined(BLOCK_ALLOC_DEBUG)
  for (uint32_t i=0; i < p->blocks; i++) {
      p[i].flags = 0;
  }
#endif

  node = p->node;

  RELAXED_STORE(&p->free, (void *) -1);  /* indicates that this block is free */
  RELAXED_STORE(&p->gen, NULL);
  RELAXED_STORE(&p->gen_no, 0);
  /* fill the block group with garbage if sanity checking is on */
  IF_DEBUG(zero_on_gc, memset(bdescr_start(p), 0xaa, (W_)p->blocks * BLOCK_SIZE));

  if (p->blocks == 0) barf("freeGroup: block size is zero");

  if (p->blocks >= BLOCKS_PER_MBLOCK)
  {
      StgWord mblocks;

      mblocks = BLOCKS_TO_MBLOCKS(p->blocks);
      // If this is an mgroup, make sure it has the right number of blocks
      ASSERT(p->blocks == MBLOCK_GROUP_BLOCKS(mblocks));

      recordFreedBlocks(node, mblocks * BLOCKS_PER_MBLOCK);

      free_mega_group(p);
      return;
  }

  recordFreedBlocks(node, p->blocks);

  // coalesce forwards
  {
      bdescr *next;
      next = p + p->blocks;

      // See Note [Data races in freeGroup].
      TSAN_ANNOTATE_BENIGN_RACE(&next->free, "freeGroup");
      if (next <= LAST_BDESCR(MBLOCK_ROUND_DOWN(p))
          && RELAXED_LOAD(&next->free) == (P_)-1)
      {
          p->blocks += next->blocks;
          ln = log_2(next->blocks);
          dbl_link_remove(next, &free_list[node][ln]);
          if (p->blocks == BLOCKS_PER_MBLOCK)
          {
              free_mega_group(p);
              return;
          }
          setup_tail(p);
      }
  }

  // coalesce backwards
  if (p != FIRST_BDESCR(MBLOCK_ROUND_DOWN(p)))
  {
      bdescr *prev;
      prev = p - 1;
      if (prev->blocks == 0) prev = prev->link; // find the head

      // See Note [Data races in freeGroup].
      TSAN_ANNOTATE_BENIGN_RACE(&prev->free, "freeGroup");
      if (RELAXED_LOAD(&prev->free) == (P_)-1)
      {
          ln = log_2(prev->blocks);
          dbl_link_remove(prev, &free_list[node][ln]);
          prev->blocks += p->blocks;
          if (prev->blocks >= BLOCKS_PER_MBLOCK)
          {
              free_mega_group(prev);
              return;
          }
          p = prev;
      }
  }

  setup_tail(p);
  free_list_insert(node,p);

  IF_DEBUG(sanity, checkFreeListSanity());
}

void
freeGroup_lock(bdescr *p)
{
    ACQUIRE_SM_LOCK;
    freeGroup(p);
    RELEASE_SM_LOCK;
}

void
freeChain(bdescr *bd)
{
  bdescr *next_bd;
  while (bd != NULL) {
    next_bd = bd->link;
    freeGroup(bd);
    bd = next_bd;
  }
}

void
freeChain_lock(bdescr *bd)
{
    ACQUIRE_SM_LOCK;
    freeChain(bd);
    RELEASE_SM_LOCK;
}

static void
initMBlock(void *mblock, uint32_t node)
{
    bdescr *bd;
    StgWord8 *block;

    /* the first few Bdescr's in a block are unused, so we don't want to
     * put them all on the free list.
     */
    block = FIRST_BLOCK(mblock);
    bd    = FIRST_BDESCR(mblock);

    /* Initialise the start field of each block descriptor
     */
    for (; block <= (StgWord8*)LAST_BLOCK(mblock); bd += 1,
             block += BLOCK_SIZE) {
        bd->start = (void*)block;
        bd->node = node;
    }
}

// Find the midpoint of the linked list.
static bdescr* splitDeferredList(bdescr* head) {
    bdescr *fast, *slow, *second_half;

    slow = head;
    fast = slow->link;

    while(fast != NULL) {
        fast = fast->link;
        if(fast) {
            fast = fast->link;
            slow = slow->link;
        }
    }

    second_half = slow->link;
    // Cap first list at half
    slow->link = NULL;
    return second_half;
}

static void sortDeferredList(bdescr** head) {
    bdescr *first_half, *second_half, *cur;

    if(*head == NULL || (*head)->link == NULL) {
        // 0 or 1 elements, done
        return;
    }

    first_half = *head;
    second_half = splitDeferredList(*head);

    sortDeferredList(&first_half);
    sortDeferredList(&second_half);

    // Sort by address
    if(bdescr_start(first_half) < bdescr_start(second_half)) {
        *head = first_half;
        first_half = first_half->link;
    } else {
        *head = second_half;
        second_half = second_half->link;
    }
    cur = *head;

    while(first_half != NULL && second_half != NULL) {
        if(bdescr_start(first_half) < bdescr_start(second_half)) {
            cur->link = first_half;
            first_half = first_half->link;
        } else {
            cur->link = second_half;
            second_half = second_half->link;
        }
        cur = cur->link;
    }

    // Now one of the two is exhausted, so order doesn't matter
    while(first_half != NULL) {
        cur->link = first_half;
        first_half = first_half->link;
        cur = cur->link;
    }
    while(second_half != NULL) {
        cur->link = second_half;
        second_half = second_half->link;
        cur = cur->link;
    }
}

void deferMBlockFreeing(void) {
    if(defer_mblock_frees) {
        barf("MBlock freeing is already deferred");
    }
    defer_mblock_frees = true;
}

void commitMBlockFreeing(void) {
    if(! defer_mblock_frees) {
        barf("MBlock freeing was never deferred");
    }
    defer_mblock_frees = false;

    for(uint32_t node = 0; node < n_numa_nodes; node++) {
        free_deferred_mega_groups(node);
    }
}

/* -----------------------------------------------------------------------------
   Stats / metrics
   -------------------------------------------------------------------------- */

W_
countBlocks(bdescr *bd)
{
    W_ n;
    for (n=0; bd != NULL; bd=bd->link) {
        n += bd->blocks;
    }
    return n;
}

// (*1) Just like countBlocks, except that we adjust the count for a
// megablock group so that it doesn't include the extra few blocks
// that would be taken up by block descriptors in the second and
// subsequent megablock.  This is so we can tally the count with the
// number of blocks allocated in the system, for memInventory().
W_
countAllocdBlocks(bdescr *bd)
{
    W_ n;
    for (n=0; bd != NULL; bd=bd->link) {
        n += bd->blocks;

        // hack for megablock groups: see (*1) above
        if (bd->blocks > BLOCKS_PER_MBLOCK) {
            n -= (MBLOCK_SIZE / BLOCK_SIZE - BLOCKS_PER_MBLOCK)
                * (bd->blocks/(MBLOCK_SIZE/BLOCK_SIZE));
        }
    }
    return n;
}

// Returns the number of blocks which were able to be freed
uint32_t returnMemoryToOS(uint32_t n /* megablocks */)
{
#if defined(wasm32_HOST_ARCH)
    // See Note [Megablock allocator on wasm].
    return 0;
#else
    bdescr *bd;
    uint32_t node;
    StgWord size;
    uint32_t init_n;
    init_n = n;

    // TODO: This is inefficient because this loop will essentially result in
    // quadratic runtime behavior: for each call to `freeMBlocks`, the
    // USE_LARGE_ADDRESS_SPACE implementation of it will walk the internal free
    // list to insert it at the right position, and thus traverse all previously
    // inserted values to get to it. We can do better though: both the internal
    // free list and the `free_mblock_list` here are sorted, so one walk should
    // be enough.

    // ToDo: not fair, we free all the memory starting with node 0.
    for (node = 0; n > 0 && node < n_numa_nodes; node++) {
        bd = free_mblock_list[node];
        while ((n > 0) && (bd != NULL)) {
            size = BLOCKS_TO_MBLOCKS(bd->blocks);
            if (size > n) {
                StgWord newSize = size - n;
                char *freeAddr = MBLOCK_ROUND_DOWN(bdescr_start(bd));
                freeAddr += newSize * MBLOCK_SIZE;
                bd->blocks = MBLOCK_GROUP_BLOCKS(newSize);
                freeMBlocks(freeAddr, n);
                n = 0;
            }
            else {
                char *freeAddr = MBLOCK_ROUND_DOWN(bdescr_start(bd));
                n -= size;
                bd = bd->link;
                freeMBlocks(freeAddr, size);
            }
        }
        free_mblock_list[node] = bd;
    }

    // Ask the OS to release any address space portion
    // that was associated with the just released MBlocks
    //
    // Historically, we used to ask the OS directly (via
    // osReleaseFreeMemory()) - now the MBlock layer might
    // have a reason to preserve the address space range,
    // so we keep it
    releaseFreeMemory();

    IF_DEBUG(gc,
        if (n != 0) {
            debugBelch("Wanted to free %d more MBlocks than are freeable\n",
                       n);
        }
    );
    return (init_n - n);
#endif
}

/* -----------------------------------------------------------------------------
   Debugging
   -------------------------------------------------------------------------- */

#if defined(DEBUG)
static void
check_tail (bdescr *bd)
{
    bdescr *tail = tail_of(bd);

    if (tail != bd)
    {
        ASSERT(tail->blocks == 0);
        ASSERT(tail->free == 0);
        ASSERT(tail->link == bd);
    }
}

void
checkFreeListSanity(void)
{
    bdescr *bd, *prev;
    StgWord ln, min;
    uint32_t node;

    for (node = 0; node < n_numa_nodes; node++) {
        min = 1;
        for (ln = 0; ln < NUM_FREE_LISTS; ln++) {
            IF_DEBUG(block_alloc,
                     debugBelch("free block list [%" FMT_Word "]:\n", ln));

            prev = NULL;
            for (bd = free_list[node][ln]; bd != NULL; prev = bd, bd = bd->link)
            {
                IF_DEBUG(block_alloc,
                         debugBelch("group at %p, length %ld blocks\n",
                                    bdescr_start(bd), (long)bd->blocks));
                ASSERT(bd->free == (P_)-1);
                ASSERT(bd->blocks > 0 && bd->blocks < BLOCKS_PER_MBLOCK);
                ASSERT(bd->blocks >= min && bd->blocks <= (min*2 - 1));
                ASSERT(bd->link != bd); // catch easy loops
                ASSERT(bd->node == node);

                check_tail(bd);

                if (prev)
                    ASSERT(bd->u.back == prev);
                else
                    ASSERT(bd->u.back == NULL);

                {
                    bdescr *next;
                    next = bd + bd->blocks;
                    if (next <= LAST_BDESCR(MBLOCK_ROUND_DOWN(bd)))
                    {
                        ASSERT(next->free != (P_)-1);
                    }
                }
            }
            min = min << 1;
        }

        prev = NULL;
        for (bd = free_mblock_list[node]; bd != NULL; prev = bd, bd = bd->link)
        {
            IF_DEBUG(block_alloc,
                     debugBelch("mega group at %p, length %ld blocks\n",
                                bdescr_start(bd), (long)bd->blocks));

            ASSERT(bd->link != bd); // catch easy loops

            if (bd->link != NULL)
            {
                // make sure the list is sorted
                ASSERT(bdescr_start(bd) < bdescr_start(bd->link));
            }

            ASSERT(bd->blocks >= BLOCKS_PER_MBLOCK);
            ASSERT(MBLOCK_GROUP_BLOCKS(BLOCKS_TO_MBLOCKS(bd->blocks))
                   == bd->blocks);

            // make sure we're fully coalesced
            if (bd->link != NULL)
            {
                ASSERT(MBLOCK_ROUND_DOWN(bd->link) !=
                       (StgWord8*)MBLOCK_ROUND_DOWN(bd) +
                       BLOCKS_TO_MBLOCKS(bd->blocks) * MBLOCK_SIZE);
            }
        }
    }
}

W_ /* BLOCKS */
countFreeList(void)
{
  bdescr *bd;
  W_ total_blocks = 0;
  StgWord ln;
  uint32_t node;

  for (node = 0; node < n_numa_nodes; node++) {
      for (ln=0; ln < NUM_FREE_LISTS; ln++) {
          for (bd = free_list[node][ln]; bd != NULL; bd = bd->link) {
              total_blocks += bd->blocks;
          }
      }
      for (bd = free_mblock_list[node]; bd != NULL; bd = bd->link) {
          total_blocks += BLOCKS_PER_MBLOCK * BLOCKS_TO_MBLOCKS(bd->blocks);
          // The caller of this function, memInventory(), expects to match
          // the total number of blocks in the system against mblocks *
          // BLOCKS_PER_MBLOCK, so we must subtract the space for the
          // block descriptors from *every* mblock.
      }
  }
  return total_blocks;
}

void
markBlocks (bdescr *bd)
{
    for (; bd != NULL; bd = bd->link) {
        bd->flags |= BF_KNOWN;
    }
}

void
reportUnmarkedBlocks (void)
{
    void *mblock;
    void *state;
    bdescr *bd;

    debugBelch("Unreachable blocks:\n");
    for (mblock = getFirstMBlock(&state); mblock != NULL;
         mblock = getNextMBlock(&state, mblock)) {
        for (bd = FIRST_BDESCR(mblock); bd <= LAST_BDESCR(mblock); ) {
            if (!(bd->flags & BF_KNOWN) && bd->free != (P_)-1) {
                debugBelch("  %p\n",bd);
            }
            if (bd->blocks >= BLOCKS_PER_MBLOCK) {
                mblock = (StgWord8*)mblock +
                    (BLOCKS_TO_MBLOCKS(bd->blocks) - 1) * MBLOCK_SIZE;
                break;
            } else {
                bd += bd->blocks;
            }
        }
    }
}

#endif

void clear_free_list(void) {
    for (uint32_t node = 0; node < n_numa_nodes; ++node) {
        for (bdescr *bd = free_mblock_list[node]; bd != NULL; bd = bd->link) {
            clear_blocks(bd);
        }

        for (int ln = 0; ln < NUM_FREE_LISTS; ++ln) {
            for (bdescr *bd = free_list[node][ln]; bd != NULL; bd = bd->link) {
                clear_blocks(bd);
            }
        }
    }
}
