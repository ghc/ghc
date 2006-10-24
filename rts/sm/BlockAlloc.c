/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 * 
 * The block allocator and free list manager.
 *
 * This is the architecture independent part of the block allocator.
 * It requires only the following support from the operating system: 
 *
 *    void *getMBlock();
 *
 * returns the address of an MBLOCK_SIZE region of memory, aligned on
 * an MBLOCK_SIZE boundary.  There is no requirement for successive
 * calls to getMBlock to return strictly increasing addresses.
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

static void    initMBlock(void *mblock);
static bdescr *allocMegaGroup(nat mblocks);
static void    freeMegaGroup(bdescr *bd);

// In THREADED_RTS mode, the free list is protected by sm_mutex.
static bdescr *free_list = NULL;

/* -----------------------------------------------------------------------------
   Initialisation
   -------------------------------------------------------------------------- */

void initBlockAllocator(void)
{
    // The free list starts off NULL
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
    head->blocks = n;
    head->free   = head->start;
    head->link   = NULL;
    for (i=1, bd = head+1; i < n; i++, bd++) {
      bd->free = 0;
      bd->blocks = 0;
      bd->link = head;
    }
  }
}

bdescr *
allocGroup(nat n)
{
  void *mblock;
  bdescr *bd, **last;

  ASSERT_SM_LOCK();
  ASSERT(n != 0);

  if (n > BLOCKS_PER_MBLOCK) {
    return allocMegaGroup(BLOCKS_TO_MBLOCKS(n));
  }

  last = &free_list;
  for (bd = free_list; bd != NULL; bd = bd->link) {
    if (bd->blocks == n) {	/* exactly the right size! */
      *last = bd->link;
      /* no initialisation necessary - this is already a
       * self-contained block group. */
      bd->free = bd->start;	/* block isn't free now */
      bd->link = NULL;
      return bd;
    }
    if (bd->blocks >  n) {	/* block too big... */
      bd->blocks -= n;		/* take a chunk off the *end* */
      bd += bd->blocks;
      initGroup(n, bd);		/* initialise it */
      return bd;
    }
    last = &bd->link;
  }
  
  mblock = getMBlock();		/* get a new megablock */
  initMBlock(mblock);		/* initialise the start fields */
  bd = FIRST_BDESCR(mblock);
  initGroup(n,bd);		/* we know the group will fit */
  if (n < BLOCKS_PER_MBLOCK) {
    initGroup(BLOCKS_PER_MBLOCK-n, bd+n);
    freeGroup(bd+n);      	/* add the rest on to the free list */
  }
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
   Any request larger than BLOCKS_PER_MBLOCK needs a megablock group.
   First, search the free list for enough contiguous megablocks to
   fulfill the request - if we don't have enough, we need to
   allocate some new ones.

   A megablock group looks just like a normal block group, except that
   the blocks field in the head will be larger than BLOCKS_PER_MBLOCK.

   Note that any objects placed in this group must start in the first
   megablock, since the other blocks don't have block descriptors.
   -------------------------------------------------------------------------- */
   
static bdescr *
allocMegaGroup(nat n)
{
  nat mbs_found;
  bdescr *bd, *last, *grp_start, *grp_prev;

  mbs_found = 0;
  grp_start = NULL;
  grp_prev  = NULL;
  last      = NULL;
  for (bd = free_list; bd != NULL; bd = bd->link) {

    if (bd->blocks == BLOCKS_PER_MBLOCK) {	/* whole megablock found */

      /* is it the first one we've found or a non-contiguous megablock? */
      if (grp_start == NULL ||
          bd->start != last->start + MBLOCK_SIZE/sizeof(W_)) {
	grp_start = bd;
	grp_prev  = last;
	mbs_found = 1;
      } else {
	mbs_found++;
      }

      if (mbs_found == n) {	/* found enough contig megablocks? */
	break;
      }
    } 

    else {			/* only a partial megablock, start again */
      grp_start = NULL;
    }

    last = bd;
  }

  /* found all the megablocks we need on the free list
   */
  if (mbs_found == n) {
    /* remove the megablocks from the free list */
    if (grp_prev == NULL) {	/* bd now points to the last mblock */
      free_list = bd->link;
    } else {
      grp_prev->link = bd->link;
    }
  }

  /* the free list wasn't sufficient, allocate all new mblocks.
   */
  else {
    void *mblock = getMBlocks(n);
    initMBlock(mblock);		/* only need to init the 1st one */
    grp_start = FIRST_BDESCR(mblock);
  }

  /* set up the megablock group */
  initGroup(BLOCKS_PER_MBLOCK, grp_start);
  grp_start->blocks = MBLOCK_GROUP_BLOCKS(n);
  return grp_start;
}

/* -----------------------------------------------------------------------------
   De-Allocation
   -------------------------------------------------------------------------- */

/* coalesce the group p with p->link if possible.
 *
 * Returns p->link if no coalescing was done, otherwise returns a
 * pointer to the newly enlarged group p.
 */

STATIC_INLINE bdescr *
coalesce(bdescr *p)
{
  bdescr *bd, *q;
  nat i, blocks;

  q = p->link;
  if (q != NULL && p->start + p->blocks * BLOCK_SIZE_W == q->start) {
    /* can coalesce */
    p->blocks += q->blocks;
    p->link    = q->link;
    blocks = q->blocks;
    for (i = 0, bd = q; i < blocks; bd++, i++) {
	bd->free = 0;
	bd->blocks = 0;
	bd->link = p;
    }
    return p;
  }
  return q;
}

void
freeGroup(bdescr *p)
{
  bdescr *bd, *last;
  
  ASSERT_SM_LOCK();

  /* are we dealing with a megablock group? */
  if (p->blocks > BLOCKS_PER_MBLOCK) {
    freeMegaGroup(p);
    return;
  }


  p->free = (void *)-1;  /* indicates that this block is free */
  p->step = NULL;
  p->gen_no = 0;
  /* fill the block group with garbage if sanity checking is on */
  IF_DEBUG(sanity,memset(p->start, 0xaa, p->blocks * BLOCK_SIZE));

  /* find correct place in free list to place new group */
  last = NULL;
  for (bd = free_list; bd != NULL && bd->start < p->start; 
       bd = bd->link) {
    last = bd;
  }

  /* now, last = previous group (or NULL) */
  if (last == NULL) {
    p->link = free_list;
    free_list = p;
  } else {
    /* coalesce with previous group if possible */
    p->link = last->link;
    last->link = p;
    p = coalesce(last);
  }

  /* coalesce with next group if possible */
  coalesce(p);
  IF_DEBUG(sanity, checkFreeListSanity());
}

void
freeGroup_lock(bdescr *p)
{
    ACQUIRE_SM_LOCK;
    freeGroup(p);
    RELEASE_SM_LOCK;
}

static void
freeMegaGroup(bdescr *p)
{
  nat n;
  void *q = p;

  n = ((bdescr *)q)->blocks * BLOCK_SIZE / MBLOCK_SIZE + 1;
  for (; n > 0; q += MBLOCK_SIZE, n--) {
    initMBlock(MBLOCK_ROUND_DOWN(q));
    initGroup(BLOCKS_PER_MBLOCK, (bdescr *)q);
    freeGroup((bdescr *)q);
  }
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
checkWellFormedGroup( bdescr *bd )
{
    nat i;

    for (i = 1; i < bd->blocks; i++) {
	ASSERT(bd[i].blocks == 0);
	ASSERT(bd[i].free   == 0);
	ASSERT(bd[i].link   == bd);
    }
}

void
checkFreeListSanity(void)
{
  bdescr *bd;

  for (bd = free_list; bd != NULL; bd = bd->link) {
    IF_DEBUG(block_alloc,
	     debugBelch("group at 0x%p, length %ld blocks\n", 
			bd->start, (long)bd->blocks));
    ASSERT(bd->blocks > 0);
    checkWellFormedGroup(bd);
    if (bd->link != NULL) {
      /* make sure we're fully coalesced */
      ASSERT(bd->start + bd->blocks * BLOCK_SIZE_W != bd->link->start);
      ASSERT(bd->start < bd->link->start);
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
  return total_blocks;
}
#endif
