/* -----------------------------------------------------------------------------
 * $Id: Storage.c,v 1.5 1999/01/19 17:06:05 simonm Exp $
 *
 * Storage manager front end
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Stats.h"
#include "Hooks.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "gmp.h"
#include "Weak.h"

#include "Storage.h"
#include "StoragePriv.h"

bdescr *current_nursery;	/* next available nursery block, or NULL */
nat nursery_blocks;		/* number of blocks in the nursery */

StgClosure    *caf_list         = NULL;

bdescr *small_alloc_list;	/* allocate()d small objects */
bdescr *large_alloc_list;	/* allocate()d large objects */
nat alloc_blocks;		/* number of allocate()d blocks since GC */
nat alloc_blocks_lim;		/* approximate limit on alloc_blocks */

StgPtr alloc_Hp    = NULL;	/* next free byte in small_alloc_list */
StgPtr alloc_HpLim = NULL;	/* end of block at small_alloc_list   */

generation *generations;	/* all the generations */
generation *g0;			/* generation 0, for convenience */
generation *oldest_gen;		/* oldest generation, for convenience */
step *g0s0;			/* generation 0, step 0, for convenience */

/*
 * Forward references
 */
static void *stgAllocForGMP   (size_t size_in_bytes);
static void *stgReallocForGMP (void *ptr, size_t old_size, size_t new_size);
static void  stgDeallocForGMP (void *ptr, size_t size);

void
initStorage (void)
{
  nat g, s;
  step *step;
  generation *gen;

  initBlockAllocator();
  
  /* allocate generation info array */
  generations = (generation *)stgMallocBytes(RtsFlags.GcFlags.generations 
					     * sizeof(struct _generation),
					     "initStorage: gens");

  /* Initialise all generations */
  for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
    gen = &generations[g];
    gen->no = g;
    gen->mut_list = END_MUT_LIST;
    gen->collections = 0;
    gen->failed_promotions = 0;
    gen->max_blocks = RtsFlags.GcFlags.minOldGenSize;
  }

  /* A couple of convenience pointers */
  g0 = &generations[0];
  oldest_gen = &generations[RtsFlags.GcFlags.generations-1];

  /* Allocate step structures in each generation */
  if (RtsFlags.GcFlags.generations > 1) {
    /* Only for multiple-generations */

    /* Oldest generation: one step */
    oldest_gen->n_steps = 1;
    oldest_gen->steps = 
      stgMallocBytes(1 * sizeof(struct _step), "initStorage: last step");

    /* set up all except the oldest generation with 2 steps */
    for(g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
      generations[g].n_steps = 2;
      generations[g].steps  = stgMallocBytes (2 * sizeof(struct _step),
					      "initStorage: steps");
    }
    
  } else {
    /* single generation, i.e. a two-space collector */
    g0->n_steps = 1;
    g0->steps = stgMallocBytes (sizeof(struct _step), "initStorage: steps");
  }

  /* Initialise all steps */
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      step = &generations[g].steps[s];
      step->no = s;
      step->blocks = NULL;
      step->n_blocks = 0;
      step->gen = &generations[g];
      step->hp = NULL;
      step->hpLim = NULL;
      step->hp_bd = NULL;
      step->large_objects = NULL;
      step->new_large_objects = NULL;
      step->scavenged_large_objects = NULL;
    }
  }
  
  /* Set up the destination pointers in each younger gen. step */
  for (g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      step = &generations[g].steps[s];
      if ( s == 1 ) {
	step->to = &generations[g+1].steps[0];
      } else {
	step->to = &generations[g].steps[s+1];
      }
    }
  }
  
  /* The oldest generation has one step and its destination is the
   * same step. */
  oldest_gen->steps[0].to = &oldest_gen->steps[0];

  /* generation 0 is special: that's the nursery */
  generations[0].max_blocks = 0;

  /* G0S0: the allocation area */
  step = &generations[0].steps[0];
  g0s0 = step;
  step->blocks   = allocNursery(NULL, RtsFlags.GcFlags.minAllocAreaSize);
  step->n_blocks = RtsFlags.GcFlags.minAllocAreaSize;
  nursery_blocks = RtsFlags.GcFlags.minAllocAreaSize;
  current_nursery = step->blocks;
  /* hp, hpLim, hp_bd, to_space etc. aren't used in G0S0 */

  weak_ptr_list = NULL;
  caf_list = NULL;
   
  /* initialise the allocate() interface */
  small_alloc_list = NULL;
  large_alloc_list = NULL;
  alloc_blocks = 0;
  alloc_blocks_lim = RtsFlags.GcFlags.minAllocAreaSize;

#ifdef COMPILER
  /* Tell GNU multi-precision pkg about our custom alloc functions */
  mp_set_memory_functions(stgAllocForGMP, stgReallocForGMP, stgDeallocForGMP);
#endif

  IF_DEBUG(gc, stat_describe_gens());
}

extern bdescr *
allocNursery (bdescr *last_bd, nat blocks)
{
  bdescr *bd;
  nat i;

  /* Allocate a nursery */
  for (i=0; i < blocks; i++) {
    bd = allocBlock();
    bd->link = last_bd;
    bd->step = g0s0;
    bd->gen = g0;
    bd->evacuated = 0;
    bd->free = bd->start;
    last_bd = bd;
  }
  return last_bd;
}

void
exitStorage (void)
{
  lnat allocated;
  bdescr *bd;

  /* Return code ignored for now */
  /* ToDo: allocation figure is slightly wrong (see also GarbageCollect()) */
  allocated = (nursery_blocks * BLOCK_SIZE_W) + allocated_bytes();
  for ( bd = current_nursery->link; bd != NULL; bd = bd->link ) {
    allocated -= BLOCK_SIZE_W;
  }
  stat_exit(allocated);
}

void
recordMutable(StgMutClosure *p)
{
  bdescr *bd;

  ASSERT(closure_MUTABLE(p));

  bd = Bdescr((P_)p);

  /* no need to bother in generation 0 */
  if (bd->gen == g0) { 
    return; 
  } 

  if (p->mut_link == NULL) {
    p->mut_link = bd->gen->mut_list;
    bd->gen->mut_list = p;
  }
}

void
newCAF(StgClosure* caf)
{
  /* Put this CAF on the mutable list for the old generation.
   * This is a HACK - the IND_STATIC closure doesn't really have
   * a mut_link field, but we pretend it has - in fact we re-use
   * the STATIC_LINK field for the time being, because when we
   * come to do a major GC we won't need the mut_link field
   * any more and can use it as a STATIC_LINK.
   */
  ((StgMutClosure *)caf)->mut_link = oldest_gen->mut_list;
  oldest_gen->mut_list = (StgMutClosure *)caf;

#ifdef DEBUG
  { 
    const StgInfoTable *info;
    
    info = get_itbl(caf);
    ASSERT(info->type == IND_STATIC);
    STATIC_LINK2(info,caf) = caf_list;
    caf_list = caf;
  }
#endif
}

/* -----------------------------------------------------------------------------
   The allocate() interface

   allocate(n) always succeeds, and returns a chunk of memory n words
   long.  n can be larger than the size of a block if necessary, in
   which case a contiguous block group will be allocated.
   -------------------------------------------------------------------------- */

StgPtr
allocate(nat n)
{
  bdescr *bd;
  StgPtr p;

  TICK_ALLOC_PRIM(n,wibble,wibble,wibble)
  CCS_ALLOC(CCCS,n);

  /* big allocation (>LARGE_OBJECT_THRESHOLD) */
  /* ToDo: allocate directly into generation 1 */
  if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
    nat req_blocks =  (lnat)BLOCK_ROUND_UP(n*sizeof(W_)) / BLOCK_SIZE;
    bd = allocGroup(req_blocks);
    dbl_link_onto(bd, &g0s0->large_objects);
    bd->gen  = g0;
    bd->step = g0s0;
    bd->evacuated = 0;
    bd->free = bd->start;
    /* don't add these blocks to alloc_blocks, since we're assuming
     * that large objects are likely to remain live for quite a while
     * (eg. running threads), so garbage collecting early won't make
     * much difference.
     */
    return bd->start;

  /* small allocation (<LARGE_OBJECT_THRESHOLD) */
  } else if (small_alloc_list == NULL || alloc_Hp + n > alloc_HpLim) {
    if (small_alloc_list) {
      small_alloc_list->free = alloc_Hp;
    }
    bd = allocBlock();
    bd->link = small_alloc_list;
    small_alloc_list = bd;
    bd->gen = g0;
    bd->step = g0s0;
    bd->evacuated = 0;
    alloc_Hp = bd->start;
    alloc_HpLim = bd->start + BLOCK_SIZE_W;
    alloc_blocks++;
  }
  
  p = alloc_Hp;
  alloc_Hp += n;
  return p;
}

lnat allocated_bytes(void)
{
  return (alloc_blocks * BLOCK_SIZE_W - (alloc_HpLim - alloc_Hp));
}

/* -----------------------------------------------------------------------------
   Allocation functions for GMP.

   These all use the allocate() interface - we can't have any garbage
   collection going on during a gmp operation, so we use allocate()
   which always succeeds.  The gmp operations which might need to
   allocate will ask the storage manager (via doYouWantToGC()) whether
   a garbage collection is required, in case we get into a loop doing
   only allocate() style allocation.
   -------------------------------------------------------------------------- */

static void *
stgAllocForGMP (size_t size_in_bytes)
{
  StgArrWords* arr;
  nat data_size_in_words, total_size_in_words;
  
  /* should be a multiple of sizeof(StgWord) (whole no. of limbs) */
  ASSERT(size_in_bytes % sizeof(W_) == 0);
  
  data_size_in_words  = size_in_bytes / sizeof(W_);
  total_size_in_words = sizeofW(StgArrWords) + data_size_in_words;
  
  /* allocate and fill it in. */
  arr = (StgArrWords *)allocate(total_size_in_words);
  SET_ARR_HDR(arr, &ARR_WORDS_info, CCCS, data_size_in_words);
  
  /* and return a ptr to the goods inside the array */
  return(BYTE_ARR_CTS(arr));
}

static void *
stgReallocForGMP (void *ptr, size_t old_size, size_t new_size)
{
    void *new_stuff_ptr = stgAllocForGMP(new_size);
    nat i = 0;
    char *p = (char *) ptr;
    char *q = (char *) new_stuff_ptr;

    for (; i < old_size; i++, p++, q++) {
	*q = *p;
    }

    return(new_stuff_ptr);
}

static void
stgDeallocForGMP (void *ptr STG_UNUSED, 
		  size_t size STG_UNUSED)
{
    /* easy for us: the garbage collector does the dealloc'n */
}

/* -----------------------------------------------------------------------------
   Debugging

   memInventory() checks for memory leaks by counting up all the
   blocks we know about and comparing that to the number of blocks
   allegedly floating around in the system.
   -------------------------------------------------------------------------- */

#ifdef DEBUG

extern void
memInventory(void)
{
  nat g, s;
  step *step;
  bdescr *bd;
  lnat total_blocks = 0, free_blocks = 0;

  /* count the blocks we current have */

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      step = &generations[g].steps[s];
      total_blocks += step->n_blocks;
      if (RtsFlags.GcFlags.generations == 1) {
	/* two-space collector has a to-space too :-) */
	total_blocks += g0s0->to_blocks;
      }
      for (bd = step->large_objects; bd; bd = bd->link) {
	total_blocks += bd->blocks;
	/* hack for megablock groups: they have an extra block or two in
	   the second and subsequent megablocks where the block
	   descriptors would normally go.
	*/
	if (bd->blocks > BLOCKS_PER_MBLOCK) {
	  total_blocks -= (MBLOCK_SIZE / BLOCK_SIZE - BLOCKS_PER_MBLOCK)
	                  * bd->blocks/(MBLOCK_SIZE/BLOCK_SIZE);
	}
      }
    }
  }

  /* any blocks held by allocate() */
  for (bd = small_alloc_list; bd; bd = bd->link) {
    total_blocks += bd->blocks;
  }
  for (bd = large_alloc_list; bd; bd = bd->link) {
    total_blocks += bd->blocks;
  }
  
  /* count the blocks on the free list */
  free_blocks = countFreeList();

  ASSERT(total_blocks + free_blocks == mblocks_allocated * BLOCKS_PER_MBLOCK);

#if 0
  if (total_blocks + free_blocks != mblocks_allocated *
      BLOCKS_PER_MBLOCK) {
    fprintf(stderr, "Blocks: %ld live + %ld free  = %ld total (%ld around)\n",
	    total_blocks, free_blocks, total_blocks + free_blocks,
	    mblocks_allocated * BLOCKS_PER_MBLOCK);
  }
#endif
}

#endif
