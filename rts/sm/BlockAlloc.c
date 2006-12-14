/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
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
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "Storage.h"

#include <string.h>

static void  initMBlock(void *mblock);

// The free_list is kept sorted by size, smallest first.
// In THREADED_RTS mode, the free list is protected by sm_mutex.

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
     bd->step

  Exceptions: we don't maintain invariants for all the blocks within a
  group on the free list, because it is expensive to modify every
  bdescr in a group when coalescing.  Just the head and last bdescrs
  will be correct for a group on the free list.


  Free lists
  ~~~~~~~~~~
  Preliminaries:
    - most allocations are for single blocks
    - we cannot be dependent on address-space ordering; sometimes the
      OS gives us new memory backwards in the address space, sometimes
      forwards
    - We want to avoid fragmentation in the free list
    
  Coalescing trick: when a bgroup is freed (freeGroup()), we can check
  whether it can be coalesced with othre free bgroups by checking the
  bdescrs for the blocks on either side of it.  This means that:

    - freeGroup is O(1) if we coalesce with an existing free block
      group.  Otherwise we have to insert in the free list, but since
      most blocks are small and the free list is sorted by size, this
      is usually quick.
    - the free list must be double-linked, so we can insert into the
      middle.
    - every free group in the free list must have its head and tail
      bdescrs initialised, the rest don't matter.
    - we cannot play this trick with mblocks, because there is no
      requirement that the bdescrs in the second and subsequent mblock
      of an mgroup are initialised (the mgroup might be filled with a
      large array, overwriting the bdescrs for example).

  So there are two free lists:

    - free_list contains bgroups smaller than an mblock.
       - it is doubly-linked
       - sorted in *size* order: allocation is best-fit
       - free bgroups are always fully coalesced
       - we do the coalescing trick in freeGroup()

    - free_mblock_list contains mgroups only
       - it is singly-linked (no need to double-link)
       - sorted in *address* order, so we can coalesce using the list
       - allocation is best-fit by traversing the whole list: we don't
         expect this list to be long, avoiding fragmentation is more
         important. 

  freeGroup() might end up moving a block from free_list to
  free_mblock_list, if after coalescing we end up with a full mblock.

  checkFreeListSanity() checks all the invariants on the free lists.

  --------------------------------------------------------------------------- */

static bdescr *free_list;
static bdescr *free_mblock_list;


/* -----------------------------------------------------------------------------
   Initialisation
   -------------------------------------------------------------------------- */

void initBlockAllocator(void)
{
    free_list = NULL;
    free_mblock_list = NULL;
}

/* -----------------------------------------------------------------------------
   Allocation
   -------------------------------------------------------------------------- */

STATIC_INLINE void
initGroup(nat n, bdescr *head)
{
  bdescr *bd;
  nat i;

  if (n != 0) {
    head->free   = head->start;
    head->link   = NULL;
    for (i=1, bd = head+1; i < n; i++, bd++) {
      bd->free = 0;
      bd->blocks = 0;
      bd->link = head;
    }
  }
}

// when a block has been shortened by allocGroup(), we need to push
// the remaining chunk backwards in the free list in order to keep the
// list sorted by size.
static void
free_list_push_backwards (bdescr *bd)
{
    bdescr *p;

    p = bd->u.back;
    while (p != NULL && p->blocks > bd->blocks) {
        p = p->u.back;
    }
    if (p != bd->u.back) {
        dbl_link_remove(bd, &free_list);
        if (p != NULL)
            dbl_link_insert_after(bd, p);
        else
            dbl_link_onto(bd, &free_list);
    }
}

// when a block has been coalesced by freeGroup(), we need to push the
// remaining chunk forwards in the free list in order to keep the list
// sorted by size.
static void
free_list_push_forwards (bdescr *bd)
{
    bdescr *p;

    p = bd;
    while (p->link != NULL && p->link->blocks < bd->blocks) {
        p = p->link;
    }
    if (p != bd) {
        dbl_link_remove(bd, &free_list);
        dbl_link_insert_after(bd, p);
    }
}

static void
free_list_insert (bdescr *bd)
{
    bdescr *p, *prev;

    if (!free_list) {
        dbl_link_onto(bd, &free_list);
        return;
    }

    prev = NULL;
    p = free_list;
    while (p != NULL && p->blocks < bd->blocks) {
        prev = p;
        p = p->link;
    }
    if (prev == NULL)
    {
        dbl_link_onto(bd, &free_list);
    }
    else 
    {
        dbl_link_insert_after(bd, prev);
    }
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
split_free_block (bdescr *bd, nat n)
{
    bdescr *fg; // free group

    ASSERT(bd->blocks > n);
    fg = bd + bd->blocks - n; // take n blocks off the end
    fg->blocks = n;
    bd->blocks -= n;
    setup_tail(bd);
    free_list_push_backwards(bd);
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
            initGroup(n, bd);
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
        nat best_mblocks  = BLOCKS_TO_MBLOCKS(best->blocks);
        bd = FIRST_BDESCR(MBLOCK_ROUND_DOWN(best) + 
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
allocGroup (nat n)
{
    bdescr *bd, *rem;

    ASSERT_SM_LOCK();

    if (n == 0) barf("allocGroup: requested zero blocks");
    
    if (n >= BLOCKS_PER_MBLOCK)
    {
        bd = alloc_mega_group(BLOCKS_TO_MBLOCKS(n));
        // only the bdescrs of the first MB are required to be initialised
        initGroup(BLOCKS_PER_MBLOCK, bd);
        IF_DEBUG(sanity, checkFreeListSanity());
        return bd;
    }
    
    // The free list is sorted by size, so we get best fit.
    for (bd = free_list; bd != NULL; bd = bd->link)
    {
        if (bd->blocks == n)	        // exactly the right size!
        {
            dbl_link_remove(bd, &free_list);
            initGroup(n, bd);		// initialise it
            IF_DEBUG(sanity, checkFreeListSanity());
            return bd;
        }
        if (bd->blocks >  n)            // block too big...
        {                              
            bd = split_free_block(bd, n);
            initGroup(n, bd);		// initialise the new chunk
            IF_DEBUG(sanity, checkFreeListSanity());
            return bd;
        }
    }

    bd = alloc_mega_group(1);
    bd->blocks = n;
    initGroup(n,bd);		         // we know the group will fit
    rem = bd + n;
    rem->blocks = BLOCKS_PER_MBLOCK-n;
    initGroup(BLOCKS_PER_MBLOCK-n, rem); // init the slop
    freeGroup(rem);      	         // add the slop on to the free list
    IF_DEBUG(sanity, checkFreeListSanity());
    return bd;
}

bdescr *
allocGroup_lock(nat n)
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
        MBLOCK_ROUND_DOWN(p) + BLOCKS_TO_MBLOCKS(p->blocks) * MBLOCK_SIZE) {
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
  nat p_on_free_list = 0;

  ASSERT_SM_LOCK();

  ASSERT(p->free != (P_)-1);

  p->free = (void *)-1;  /* indicates that this block is free */
  p->step = NULL;
  p->gen_no = 0;
  /* fill the block group with garbage if sanity checking is on */
  IF_DEBUG(sanity,memset(p->start, 0xaa, p->blocks * BLOCK_SIZE));

  if (p->blocks == 0) barf("freeGroup: block size is zero");

  if (p->blocks >= BLOCKS_PER_MBLOCK)
  {
      // If this is an mgroup, make sure it has the right number of blocks
      ASSERT(p->blocks == MBLOCK_GROUP_BLOCKS(BLOCKS_TO_MBLOCKS(p->blocks)));
      free_mega_group(p);
      return;
  }

  // coalesce forwards
  {
      bdescr *next;
      next = p + p->blocks;
      if (next <= LAST_BDESCR(MBLOCK_ROUND_DOWN(p)) && next->free == (P_)-1)
      {
          p->blocks += next->blocks;
          if (p->blocks == BLOCKS_PER_MBLOCK)
          {
              dbl_link_remove(next, &free_list);
              free_mega_group(p);
              return;
          }
          dbl_link_replace(p, next, &free_list);
          setup_tail(p);
          free_list_push_forwards(p);
          p_on_free_list = 1;
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
          prev->blocks += p->blocks;
          if (prev->blocks >= BLOCKS_PER_MBLOCK)
          {
              if (p_on_free_list)
              {
                  dbl_link_remove(p, &free_list);
              }
              dbl_link_remove(prev, &free_list);
              free_mega_group(prev);
              return;
          }
          else if (p_on_free_list)
          {
              // p was already coalesced forwards
              dbl_link_remove(p, &free_list);
          }
          setup_tail(prev);
          free_list_push_forwards(prev);
          p = prev;
          p_on_free_list = 1;
      }
  }
      
  if (!p_on_free_list)
  {
      setup_tail(p);
      free_list_insert(p);
  }

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
  void *block;

  /* the first few Bdescr's in a block are unused, so we don't want to
   * put them all on the free list.
   */
  block = FIRST_BLOCK(mblock);
  bd    = FIRST_BDESCR(mblock);

  /* Initialise the start field of each block descriptor
   */
  for (; block <= LAST_BLOCK(mblock); bd += 1, block += BLOCK_SIZE) {
    bd->start = block;
  }
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

    IF_DEBUG(block_alloc, debugBelch("free block list:\n"));

    prev = NULL;
    for (bd = free_list; bd != NULL; prev = bd, bd = bd->link)
    {
        IF_DEBUG(block_alloc,
                 debugBelch("group at %p, length %ld blocks\n", 
                            bd->start, (long)bd->blocks));
        ASSERT(bd->blocks > 0 && bd->blocks < BLOCKS_PER_MBLOCK);
        ASSERT(bd->link != bd); // catch easy loops

        check_tail(bd);

        if (prev)
            ASSERT(bd->u.back == prev);
        else 
            ASSERT(bd->u.back == NULL);

        if (bd->link != NULL)
        {
            // make sure the list is sorted
            ASSERT(bd->blocks <= bd->link->blocks);
        }

        {
            bdescr *next;
            next = bd + bd->blocks;
            if (next <= LAST_BDESCR(MBLOCK_ROUND_DOWN(bd)))
            {
                ASSERT(next->free != (P_)-1);
            }
        }
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
                    MBLOCK_ROUND_DOWN(bd) + 
                    BLOCKS_TO_MBLOCKS(bd->blocks) * MBLOCK_SIZE);
        }
    }
}

nat /* BLOCKS */
countFreeList(void)
{
  bdescr *bd;
  lnat total_blocks = 0;

  for (bd = free_list; bd != NULL; bd = bd->link) {
      total_blocks += bd->blocks;
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
#endif
