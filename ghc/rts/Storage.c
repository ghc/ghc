/* -----------------------------------------------------------------------------
 * $Id: Storage.c,v 1.2 1998/12/02 13:28:57 simonm Exp $
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
#include "gmp.h"
#include "Weak.h"

#include "Storage.h"
#include "StoragePriv.h"

bdescr *nursery;		/* chained-blocks in the nursery */
bdescr *current_nursery;	/* next available nursery block, or NULL */
nat nursery_blocks;		/* number of blocks in the nursery */

StgClosure    *caf_list         = NULL;

bdescr *small_alloc_list;	/* allocate()d small objects */
bdescr *large_alloc_list;	/* allocate()d large objects */
nat alloc_blocks;		/* number of allocate()d blocks since GC */
nat alloc_blocks_lim;		/* approximate limit on alloc_blocks */

StgPtr alloc_Hp    = NULL;	/* next free byte in small_alloc_list */
StgPtr alloc_HpLim = NULL;	/* end of block at small_alloc_list   */

/*
 * Forward references
 */
static void *stgAllocForGMP   (size_t size_in_bytes);
static void *stgReallocForGMP (void *ptr, size_t old_size, size_t new_size);
static void  stgDeallocForGMP (void *ptr, size_t size);

void
initStorage (void)
{
  initBlockAllocator();
  
  nursery = allocNursery(NULL, RtsFlags.GcFlags.minAllocAreaSize);

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
}

bdescr *
allocNursery (bdescr *last_bd, nat blocks)
{
  bdescr *bd;
  nat i;

  /* Allocate a nursery */
  for (i=0; i < blocks; i++) {
    bd = allocBlock();
    bd->link = last_bd;
    bd->step = 0;
    bd->free = bd->start;
    last_bd = bd;
  }
  nursery_blocks = blocks;
  current_nursery = last_bd;
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
newCAF(StgClosure* caf)
{
  const StgInfoTable *info = get_itbl(caf);

  ASSERT(info->type == IND_STATIC);
  STATIC_LINK2(info,caf) = caf_list;
  caf_list = caf;
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
  if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
    nat req_blocks =  (lnat)BLOCK_ROUND_UP(n*sizeof(W_)) / BLOCK_SIZE;
    bd = allocGroup(req_blocks);
    bd->link = large_alloc_list; 
    bd->back = NULL;
    if (large_alloc_list) {
      large_alloc_list->back = bd; /* double-link the list */
    }
    large_alloc_list = bd;
    bd->step = 0;
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
    bd->step = 0;
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
