/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 * 
 * The block allocator and free list manager.
 *
 * This is the architecture independent part of the block allocator.
 * It requires only the following support from the operating system: 
 *
 *    void *getMBlock(nat n);
 *
 * returns the address of an n*MBLOCK_SIZE region of memory, aligned on
 * an MBLOCK_SIZE boundary.  There are no other restrictions on the
 * addresses of memory returned by getMBlock().
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "OSMem.h"

#include <string.h>

static void  initMBlock(void *mblock);

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
   bd->start always points to the start of the block.

   bd->free is either:
      - zero for a non-group-head; bd->link points to the head
      - (-1) for the head of a free block group
      - or it points within the block

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

  We cannot play this coalescing trick with mblocks, because there is
  no requirement that the bdescrs in the second and subsequent mblock
  of an mgroup are initialised (the mgroup might be filled with a
  large array, overwriting the bdescrs for example).

  So there is a separate free list for megablocks, sorted in *address*
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
   a nat, then it will very likely overflow on a 64-bit platform.
   Always cast to StgWord (or W_ for short) first: ((W_)n_blocks * BLOCK_SIZE).

  --------------------------------------------------------------------------- */

#define MAX_FREE_LIST 9

// In THREADED_RTS mode, the free list is protected by sm_mutex.

static bdescr *free_list[MAX_FREE_LIST];
static bdescr *free_mblock_list;

// free_list[i] contains blocks that are at least size 2^i, and at
// most size 2^(i+1) - 1.  
// 
// To find the free list in which to place a block, use log_2(size).
// To find a free block of the right size, use log_2_ceil(size).

W_ n_alloc_blocks;   // currently allocated blocks
W_ hw_alloc_blocks;  // high-water allocated blocks

/* -----------------------------------------------------------------------------
   Initialisation
   -------------------------------------------------------------------------- */

void initBlockAllocator(void)
{
    nat i;
    for (i=0; i < MAX_FREE_LIST; i++) {
        free_list[i] = NULL;
    }
    free_mblock_list = NULL;
    n_alloc_blocks = 0;
    hw_alloc_blocks = 0;
}

/* -----------------------------------------------------------------------------
   Allocation
   -------------------------------------------------------------------------- */

STATIC_INLINE void
initGroup(bdescr *head)
{
  bdescr *bd;
  W_ i, n;

  n = head->blocks;
  head->free   = head->start;
  head->link   = NULL;
  for (i=1, bd = head+1; i < n; i++, bd++) {
      bd->free = 0;
      bd->blocks = 0;
      bd->link = head;
  }
}

// There are quicker non-loopy ways to do log_2, but we expect n to be
// usually small, and MAX_FREE_LIST is also small, so the loop version
// might well be the best choice here.
STATIC_INLINE nat
log_2_ceil(W_ n)
{
    W_ i, x;
    x = 1;
    for (i=0; i < MAX_FREE_LIST; i++) {
        if (x >= n) return i;
        x = x << 1;
    }
    return MAX_FREE_LIST;
}

STATIC_INLINE nat
log_2(W_ n)
{
    W_ i, x;
    x = n;
    for (i=0; i < MAX_FREE_LIST; i++) {
        x = x >> 1;
        if (x == 0) return i;
    }
    return MAX_FREE_LIST;
}

STATIC_INLINE void
free_list_insert (bdescr *bd)
{
    nat ln;

    ASSERT(bd->blocks < BLOCKS_PER_MBLOCK);
    ln = log_2(bd->blocks);
    
    dbl_link_onto(bd, &free_list[ln]);
}


STATIC_INLINE bdescr *
tail_of (bdescr *bd)
{
    return bd + bd->blocks - 1;
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
split_free_block (bdescr *bd, W_ n, nat ln)
{
    bdescr *fg; // free group

    ASSERT(bd->blocks > n);
    dbl_link_remove(bd, &free_list[ln]);
    fg = bd + bd->blocks - n; // take n blocks off the end
    fg->blocks = n;
    bd->blocks -= n;
    setup_tail(bd);
    ln = log_2(bd->blocks);
    dbl_link_onto(bd, &free_list[ln]);
    return fg;
}

static bdescr *
alloc_mega_group (nat mblocks)
{
    bdescr *best, *bd, *prev;
    nat n;

    n = MBLOCK_GROUP_BLOCKS(mblocks);

    best = NULL;
    prev = NULL;
    for (bd = free_mblock_list; bd != NULL; prev = bd, bd = bd->link)
    {
        if (bd->blocks == n) 
        {
            if (prev) {
                prev->link = bd->link;
            } else {
                free_mblock_list = bd->link;
            }
            initGroup(bd);
            return bd;
        }
        else if (bd->blocks > n)
        {
            if (!best || bd->blocks < best->blocks)
            {
                best = bd;
            }
        }
    }

    if (best)
    {
        // we take our chunk off the end here.
        StgWord best_mblocks  = BLOCKS_TO_MBLOCKS(best->blocks);
        bd = FIRST_BDESCR((StgWord8*)MBLOCK_ROUND_DOWN(best) + 
                          (best_mblocks-mblocks)*MBLOCK_SIZE);

        best->blocks = MBLOCK_GROUP_BLOCKS(best_mblocks - mblocks);
        initMBlock(MBLOCK_ROUND_DOWN(bd));
    }
    else
    {
        void *mblock = getMBlocks(mblocks);
        initMBlock(mblock);		// only need to init the 1st one
        bd = FIRST_BDESCR(mblock);
    }
    bd->blocks = MBLOCK_GROUP_BLOCKS(mblocks);
    return bd;
}

bdescr *
allocGroup (W_ n)
{
    bdescr *bd, *rem;
    nat ln;

    if (n == 0) barf("allocGroup: requested zero blocks");
    
    if (n >= BLOCKS_PER_MBLOCK)
    {
        nat mblocks;

        mblocks = BLOCKS_TO_MBLOCKS(n);

        // n_alloc_blocks doesn't count the extra blocks we get in a
        // megablock group.
        n_alloc_blocks += mblocks * BLOCKS_PER_MBLOCK;
        if (n_alloc_blocks > hw_alloc_blocks) hw_alloc_blocks = n_alloc_blocks;

        bd = alloc_mega_group(mblocks);
        // only the bdescrs of the first MB are required to be initialised
        initGroup(bd);
        goto finish;
    }
    
    n_alloc_blocks += n;
    if (n_alloc_blocks > hw_alloc_blocks) hw_alloc_blocks = n_alloc_blocks;

    ln = log_2_ceil(n);

    while (ln < MAX_FREE_LIST && free_list[ln] == NULL) {
        ln++;
    }

    if (ln == MAX_FREE_LIST) {
#if 0  /* useful for debugging fragmentation */
        if ((W_)mblocks_allocated * BLOCKS_PER_MBLOCK * BLOCK_SIZE_W
             - (W_)((n_alloc_blocks - n) * BLOCK_SIZE_W) > (2*1024*1024)/sizeof(W_)) {
            debugBelch("Fragmentation, wanted %d blocks, %ld MB free\n", n, ((mblocks_allocated * BLOCKS_PER_MBLOCK) - n_alloc_blocks) / BLOCKS_PER_MBLOCK);
            RtsFlags.DebugFlags.block_alloc = 1;
            checkFreeListSanity();
        }
#endif

        bd = alloc_mega_group(1);
        bd->blocks = n;
        initGroup(bd);		         // we know the group will fit
        rem = bd + n;
        rem->blocks = BLOCKS_PER_MBLOCK-n;
        initGroup(rem); // init the slop
        n_alloc_blocks += rem->blocks;
        freeGroup(rem);      	         // add the slop on to the free list
        goto finish;
    }

    bd = free_list[ln];

    if (bd->blocks == n)	        // exactly the right size!
    {
        dbl_link_remove(bd, &free_list[ln]);
        initGroup(bd);
    }
    else if (bd->blocks >  n)            // block too big...
    {                              
        bd = split_free_block(bd, n, ln);
        ASSERT(bd->blocks == n);
        initGroup(bd);
    }
    else
    {
        barf("allocGroup: free list corrupted");
    }

finish:
    IF_DEBUG(sanity, memset(bd->start, 0xaa, bd->blocks * BLOCK_SIZE));
    IF_DEBUG(sanity, checkFreeListSanity());
    return bd;
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
bdescr *
allocLargeChunk (W_ min, W_ max)
{
    bdescr *bd;
    nat ln, lnmax;

    if (min >= BLOCKS_PER_MBLOCK) {
        return allocGroup(max);
    }

    ln = log_2_ceil(min);
    lnmax = log_2_ceil(max); // tops out at MAX_FREE_LIST

    while (ln < lnmax && free_list[ln] == NULL) {
        ln++;
    }
    if (ln == lnmax) {
        return allocGroup(max);
    }
    bd = free_list[ln];

    if (bd->blocks <= max)              // exactly the right size!
    {
        dbl_link_remove(bd, &free_list[ln]);
        initGroup(bd);
    }
    else   // block too big...
    {                              
        bd = split_free_block(bd, max, ln);
        ASSERT(bd->blocks == max);
        initGroup(bd);
    }

    n_alloc_blocks += bd->blocks;
    if (n_alloc_blocks > hw_alloc_blocks) hw_alloc_blocks = n_alloc_blocks;

    IF_DEBUG(sanity, memset(bd->start, 0xaa, bd->blocks * BLOCK_SIZE));
    IF_DEBUG(sanity, checkFreeListSanity());
    return bd;
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
allocBlock(void)
{
    return allocGroup(1);
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

    // Find the right place in the free list.  free_mblock_list is
    // sorted by *address*, not by size as the free_list is.
    prev = NULL;
    bd = free_mblock_list;
    while (bd && bd->start < mg->start) {
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
        mg->link = free_mblock_list;
        free_mblock_list = mg;
    }
    // coalesce forwards
    coalesce_mblocks(mg);

    IF_DEBUG(sanity, checkFreeListSanity());
}    


void
freeGroup(bdescr *p)
{
  nat ln;

  // Todo: not true in multithreaded GC
  // ASSERT_SM_LOCK();

  ASSERT(p->free != (P_)-1);

  p->free = (void *)-1;  /* indicates that this block is free */
  p->gen = NULL;
  p->gen_no = 0;
  /* fill the block group with garbage if sanity checking is on */
  IF_DEBUG(sanity,memset(p->start, 0xaa, (W_)p->blocks * BLOCK_SIZE));

  if (p->blocks == 0) barf("freeGroup: block size is zero");

  if (p->blocks >= BLOCKS_PER_MBLOCK)
  {
      nat mblocks;

      mblocks = BLOCKS_TO_MBLOCKS(p->blocks);
      // If this is an mgroup, make sure it has the right number of blocks
      ASSERT(p->blocks == MBLOCK_GROUP_BLOCKS(mblocks));

      n_alloc_blocks -= mblocks * BLOCKS_PER_MBLOCK;

      free_mega_group(p);
      return;
  }

  ASSERT(n_alloc_blocks >= p->blocks);
  n_alloc_blocks -= p->blocks;

  // coalesce forwards
  {
      bdescr *next;
      next = p + p->blocks;
      if (next <= LAST_BDESCR(MBLOCK_ROUND_DOWN(p)) && next->free == (P_)-1)
      {
          p->blocks += next->blocks;
          ln = log_2(next->blocks);
          dbl_link_remove(next, &free_list[ln]);
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

      if (prev->free == (P_)-1)
      {
          ln = log_2(prev->blocks);
          dbl_link_remove(prev, &free_list[ln]);
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
  free_list_insert(p);

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
initMBlock(void *mblock)
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

void returnMemoryToOS(nat n /* megablocks */)
{
    static bdescr *bd;
    nat size;

    bd = free_mblock_list;
    while ((n > 0) && (bd != NULL)) {
        size = BLOCKS_TO_MBLOCKS(bd->blocks);
        if (size > n) {
            nat newSize = size - n;
            char *freeAddr = MBLOCK_ROUND_DOWN(bd->start);
            freeAddr += newSize * MBLOCK_SIZE;
            bd->blocks = MBLOCK_GROUP_BLOCKS(newSize);
            freeMBlocks(freeAddr, n);
            n = 0;
        }
        else {
            char *freeAddr = MBLOCK_ROUND_DOWN(bd->start);
            n -= size;
            bd = bd->link;
            freeMBlocks(freeAddr, size);
        }
    }
    free_mblock_list = bd;

    osReleaseFreeMemory();

    IF_DEBUG(gc,
        if (n != 0) {
            debugBelch("Wanted to free %d more MBlocks than are freeable\n",
                       n);
        }
    );
}

/* -----------------------------------------------------------------------------
   Debugging
   -------------------------------------------------------------------------- */

#ifdef DEBUG
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
    nat ln, min;


    min = 1;
    for (ln = 0; ln < MAX_FREE_LIST; ln++) {
        IF_DEBUG(block_alloc, debugBelch("free block list [%d]:\n", ln));

        prev = NULL;
        for (bd = free_list[ln]; bd != NULL; prev = bd, bd = bd->link)
        {
            IF_DEBUG(block_alloc,
                     debugBelch("group at %p, length %ld blocks\n", 
                                bd->start, (long)bd->blocks));
            ASSERT(bd->free == (P_)-1);
            ASSERT(bd->blocks > 0 && bd->blocks < BLOCKS_PER_MBLOCK);
            ASSERT(bd->blocks >= min && bd->blocks <= (min*2 - 1));
            ASSERT(bd->link != bd); // catch easy loops

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
    for (bd = free_mblock_list; bd != NULL; prev = bd, bd = bd->link)
    {
        IF_DEBUG(block_alloc,
                 debugBelch("mega group at %p, length %ld blocks\n", 
                            bd->start, (long)bd->blocks));

        ASSERT(bd->link != bd); // catch easy loops

        if (bd->link != NULL)
        {
            // make sure the list is sorted
            ASSERT(bd->start < bd->link->start);
        }

        ASSERT(bd->blocks >= BLOCKS_PER_MBLOCK);
        ASSERT(MBLOCK_GROUP_BLOCKS(BLOCKS_TO_MBLOCKS(bd->blocks))
               == bd->blocks);

        // make sure we're fully coalesced
        if (bd->link != NULL)
        {
            ASSERT (MBLOCK_ROUND_DOWN(bd->link) != 
                    (StgWord8*)MBLOCK_ROUND_DOWN(bd) + 
                    BLOCKS_TO_MBLOCKS(bd->blocks) * MBLOCK_SIZE);
        }
    }
}

W_ /* BLOCKS */
countFreeList(void)
{
  bdescr *bd;
  W_ total_blocks = 0;
  nat ln;

  for (ln=0; ln < MAX_FREE_LIST; ln++) {
      for (bd = free_list[ln]; bd != NULL; bd = bd->link) {
          total_blocks += bd->blocks;
      }
  }
  for (bd = free_mblock_list; bd != NULL; bd = bd->link) {
      total_blocks += BLOCKS_PER_MBLOCK * BLOCKS_TO_MBLOCKS(bd->blocks);
      // The caller of this function, memInventory(), expects to match
      // the total number of blocks in the system against mblocks *
      // BLOCKS_PER_MBLOCK, so we must subtract the space for the
      // block descriptors from *every* mblock.
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
    bdescr *bd;

    debugBelch("Unreachable blocks:\n");
    for (mblock = getFirstMBlock(); mblock != NULL;
         mblock = getNextMBlock(mblock)) {
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
