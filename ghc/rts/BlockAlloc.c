/* -----------------------------------------------------------------------------
 * $Id: BlockAlloc.c,v 1.6 1999/07/01 13:48:22 panne Exp $
 *
 * (c) The GHC Team 1998-1999
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

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "MBlock.h"

static void    initMBlock(void *mblock);
static bdescr *allocMegaGroup(nat mblocks);
static void    freeMegaGroup(bdescr *bd);

static bdescr *free_list;

/* -----------------------------------------------------------------------------
   Initialisation
   -------------------------------------------------------------------------- */

void initBlockAllocator(void)
{
  free_list = NULL;
}

/* -----------------------------------------------------------------------------
   Allocation
   -------------------------------------------------------------------------- */

static inline void
initGroup(nat n, bdescr *head)
{
  bdescr *bd;
  nat i;

  if (n != 0) {
    head->blocks = n;
    head->free = head->start;
    for (i=1, bd = head+1; i < n; i++, bd++) {
      bd->free = 0;
      bd->link = head;
    }
  }
}

bdescr *
allocGroup(nat n)
{
  void *mblock;
  bdescr *bd, **last;

  if (n > BLOCKS_PER_MBLOCK) {
    return allocMegaGroup(BLOCKS_TO_MBLOCKS(n));
  }

  last = &free_list;
  for (bd = free_list; bd != NULL; bd = bd->link) {
    if (bd->blocks == n) {	/* exactly the right size! */
      *last = bd->link;
      /* no initialisation necessary - this is already a
       * self-contained block group. */
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
allocBlock(void)
{
  return allocGroup(1);
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

static inline bdescr *
coalesce(bdescr *p)
{
  bdescr *bd, *q;
  nat i;

  q = p->link;
  if (q != NULL && p->start + p->blocks * BLOCK_SIZE_W == q->start) {
    /* can coalesce */
    p->blocks += q->blocks;
    p->link    = q->link;
    for (i = 0, bd = q; i < q->blocks; bd++, i++) {
	bd->free = 0;
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
  
  /* are we dealing with a megablock group? */
  if (p->blocks > BLOCKS_PER_MBLOCK) {
    freeMegaGroup(p);
    return;
  }

#ifdef DEBUG
  p->free = (void *)-1;  /* indicates that this block is free */
  p->step = NULL;
  p->gen  = NULL;
  /* fill the block group with garbage if sanity checking is on */
  IF_DEBUG(sanity,memset(p->start, 0xaa, p->blocks * BLOCK_SIZE));
#endif

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

static void
freeMegaGroup(bdescr *p)
{
  nat n;

  n = p->blocks * BLOCK_SIZE / MBLOCK_SIZE + 1;
  for (; n > 0; (W_)p += MBLOCK_SIZE, n--) {
    initMBlock((void *)((W_)p & ~MBLOCK_MASK));
    initGroup(BLOCKS_PER_MBLOCK, p);
    freeGroup(p);
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
  for (; block <= LAST_BLOCK(mblock); bd += 1, (lnat)block += BLOCK_SIZE) {
    bd->start = block;
  }
}

/* -----------------------------------------------------------------------------
   Debugging
   -------------------------------------------------------------------------- */

#ifdef DEBUG
void
checkFreeListSanity(void)
{
  bdescr *bd;

  for (bd = free_list; bd != NULL; bd = bd->link) {
    IF_DEBUG(block_alloc,
	     fprintf(stderr,"group at 0x%x, length %d blocks\n", 
		     (nat)bd->start, bd->blocks));
    ASSERT(bd->blocks > 0);
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
