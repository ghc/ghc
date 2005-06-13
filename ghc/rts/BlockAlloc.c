/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
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

#include <string.h>

static void    initMBlock(void *mblock);
static bdescr *allocMegaGroup(nat mblocks);
static void    freeMegaGroup(bdescr *bd);

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
initGroupTail(nat n, bdescr *head, bdescr *tail)
{
    bdescr *bd;
    nat i;

    for (i=0, bd = tail; i < n; i++, bd++) {
      bd->flags  = 0;
      bd->free   = 0;
      bd->blocks = 0;
      bd->link   = head;
    }
}

STATIC_INLINE void
initGroup(nat n, bdescr *head)
{
  if (n != 0) {
    head->blocks = n;
    head->free   = head->start;
    head->link   = NULL;
    head->flags  = 0;
    initGroupTail( n-1, head, head+1 );
  }
}

bdescr *
allocGroup(nat n)
{
  void *mblock;
  bdescr *bd, **last;

  ASSERT(n != 0);

  if (n > BLOCKS_PER_MBLOCK) {
    return allocMegaGroup(BLOCKS_TO_MBLOCKS(n));
  }

  last = &free_list;
  for (bd = free_list; bd != NULL; bd = bd->link) {
    if (bd->blocks == n) {	/* exactly the right size! */
      if (bd->link) {
        bd->link->u.back = bd->u.back;
      }
      *last = bd->link;
      /* no initialisation necessary - this is already a
       * self-contained block group. */
      bd->flags = 0;
      bd->free  = bd->start;
      bd->link  = NULL;
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

  /* found all the megablocks we need on the free list */
  if (mbs_found == n) {
    /* remove the megablocks from the free list */
    if (grp_prev == NULL) {	/* bd now points to the last mblock */
      free_list = bd->link;
      if (free_list) {
        free_list->u.back = NULL;
      }
    } else {
      grp_prev->link = bd->link;
      if (bd->link) {
        bd->link->u.back = grp_prev;
      }
    }
  }

  /* the free list wasn't sufficient, allocate all new mblocks. */
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

/* coalesce the group p with its predecessor and successor groups, if possible
 *
 * Returns NULL if no coalescing was done, otherwise returns a
 * pointer to the newly enlarged group p.
 */

STATIC_INLINE bdescr *
coalesce(bdescr *p)
{
    bdescr *first, *q, *result = NULL;
    
    /* Get first megablock descriptor */
    first = FIRST_BDESCR(MBLOCK_ROUND_DOWN(p->start));
    
    /* Attempt to coalesce with predecessor if not the first block */
    if (p != first) {
	q = p - 1;
	if (!q->blocks) {   // not a block head?
	    q = q->link;    // find the head.
	}
	/* Predecessor is free? */
	if (q->flags & BF_FREE) {
	    q->blocks += p->blocks;
	    initGroupTail( p->blocks, q, p );
	    p = result = q;
	}
    }

    /* Attempt to coalesce with successor if not the last block */
    q = p + p->blocks;
    if (q != first + BLOCKS_PER_MBLOCK) {
	/* Successor is free */
	if (q->flags & BF_FREE) {
	    if (result) {
		/* p is on free_list, q is on free_list, unlink
		 * q completely and patch up list
		 */
		if (q->u.back) {
		    q->u.back->link = q->link;
		}
		if (q->link) {
		    q->link->u.back = q->u.back;
		}
		if (free_list == q) {
		    free_list = q->link;
		}
	    } else {
		/* p is not on free_list just assume q's links */
		p->u.back = q->u.back;
		if (p->u.back) {
		    p->u.back->link = p;
		}
		p->link = q->link;
		if (p->link) {
		    p->link->u.back = p;
		}
		if (q == free_list) {
		    free_list = p;
		    free_list->u.back = NULL;
		}
	    }
	    
	    p->blocks += q->blocks;
	    initGroupTail( q->blocks, p, q );
	    result = p;
	}
    }
    
    return result;
}

void
freeGroup(bdescr *p)
{
  /* are we dealing with a megablock group? */
  if (p->blocks > BLOCKS_PER_MBLOCK) {
    freeMegaGroup(p);
    return;
  }

  p->flags = BF_FREE;
  p->u.back = NULL;
  p->link = NULL;
  p->step = NULL;
  p->gen_no = 0;
  /* fill the block group with garbage if sanity checking is on */
  IF_DEBUG(sanity,memset(p->start, 0xaa, p->blocks * BLOCK_SIZE));

  if (!coalesce(p)) {
    dbl_link_onto(p, &free_list);
  }

  IF_DEBUG(sanity, checkFreeListSanity());
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
checkWellFormedGroup(bdescr *bd)
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
	     debugBelch("group at 0x%x, length %d blocks\n", 
			(nat)bd->start, bd->blocks));
    ASSERT(bd->blocks > 0);
    ASSERT(bd->link ? bd->link->u.back == bd : 1);
    ASSERT(bd->u.back ? bd->u.back->link == bd : 1);
    checkWellFormedGroup(bd);
    if (bd->link != NULL) {
      /* make sure we're fully coalesced */
      ASSERT(bd->start + bd->blocks * BLOCK_SIZE_W != bd->link->start);
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
