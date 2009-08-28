/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * Storage manager front end
 *
 * Documentation on the architecture of the Storage Manager can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "BlockAlloc.h"
#include "Weak.h"
#include "Sanity.h"
#include "Arena.h"
#include "Capability.h"
#include "Schedule.h"
#include "RetainerProfile.h"	// for counting memory blocks (memInventory)
#include "OSMem.h"
#include "Trace.h"
#include "GC.h"
#include "Evac.h"

#include <string.h>

#include "ffi.h"

/* 
 * All these globals require sm_mutex to access in THREADED_RTS mode.
 */
StgClosure    *caf_list         = NULL;
StgClosure    *revertible_caf_list = NULL;
rtsBool       keepCAFs;

bdescr *pinned_object_block;    /* allocate pinned objects into this block */
nat alloc_blocks;		/* number of allocate()d blocks since GC */
nat alloc_blocks_lim;		/* approximate limit on alloc_blocks */

static bdescr *exec_block;

generation *generations = NULL;	/* all the generations */
generation *g0		= NULL; /* generation 0, for convenience */
generation *oldest_gen  = NULL; /* oldest generation, for convenience */
step *g0s0 		= NULL; /* generation 0, step 0, for convenience */

nat total_steps         = 0;
step *all_steps         = NULL; /* single array of steps */

ullong total_allocated = 0;	/* total memory allocated during run */

nat n_nurseries         = 0;    /* == RtsFlags.ParFlags.nNodes, convenience */
step *nurseries         = NULL; /* array of nurseries, >1 only if THREADED_RTS */

#ifdef THREADED_RTS
/*
 * Storage manager mutex:  protects all the above state from
 * simultaneous access by two STG threads.
 */
Mutex sm_mutex;
#endif

static void allocNurseries ( void );

static void
initStep (step *stp, int g, int s)
{
    stp->no = s;
    stp->abs_no = RtsFlags.GcFlags.steps * g + s;
    stp->blocks = NULL;
    stp->n_blocks = 0;
    stp->n_words = 0;
    stp->live_estimate = 0;
    stp->old_blocks = NULL;
    stp->n_old_blocks = 0;
    stp->gen = &generations[g];
    stp->gen_no = g;
    stp->large_objects = NULL;
    stp->n_large_blocks = 0;
    stp->scavenged_large_objects = NULL;
    stp->n_scavenged_large_blocks = 0;
    stp->mark = 0;
    stp->compact = 0;
    stp->bitmap = NULL;
#ifdef THREADED_RTS
    initSpinLock(&stp->sync_large_objects);
#endif
    stp->threads = END_TSO_QUEUE;
    stp->old_threads = END_TSO_QUEUE;
}

void
initStorage( void )
{
  nat g, s;
  generation *gen;

  if (generations != NULL) {
      // multi-init protection
      return;
  }

  initMBlocks();

  /* Sanity check to make sure the LOOKS_LIKE_ macros appear to be
   * doing something reasonable.
   */
  /* We use the NOT_NULL variant or gcc warns that the test is always true */
  ASSERT(LOOKS_LIKE_INFO_PTR_NOT_NULL((StgWord)&stg_BLACKHOLE_info));
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(&stg_dummy_ret_closure));
  ASSERT(!HEAP_ALLOCED(&stg_dummy_ret_closure));
  
  if (RtsFlags.GcFlags.maxHeapSize != 0 &&
      RtsFlags.GcFlags.heapSizeSuggestion > 
      RtsFlags.GcFlags.maxHeapSize) {
    RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
  }

  if (RtsFlags.GcFlags.maxHeapSize != 0 &&
      RtsFlags.GcFlags.minAllocAreaSize > 
      RtsFlags.GcFlags.maxHeapSize) {
      errorBelch("maximum heap size (-M) is smaller than minimum alloc area size (-A)");
      RtsFlags.GcFlags.minAllocAreaSize = RtsFlags.GcFlags.maxHeapSize;
  }

  initBlockAllocator();
  
#if defined(THREADED_RTS)
  initMutex(&sm_mutex);
#endif

  ACQUIRE_SM_LOCK;

  /* allocate generation info array */
  generations = (generation *)stgMallocBytes(RtsFlags.GcFlags.generations 
					     * sizeof(struct generation_),
					     "initStorage: gens");

  /* allocate all the steps into an array.  It is important that we do
     it this way, because we need the invariant that two step pointers
     can be directly compared to see which is the oldest.
     Remember that the last generation has only one step. */
  total_steps = 1 + (RtsFlags.GcFlags.generations - 1) * RtsFlags.GcFlags.steps;
  all_steps   = stgMallocBytes(total_steps * sizeof(struct step_),
                               "initStorage: steps");

  /* Initialise all generations */
  for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
    gen = &generations[g];
    gen->no = g;
    gen->mut_list = allocBlock();
    gen->collections = 0;
    gen->par_collections = 0;
    gen->failed_promotions = 0;
    gen->max_blocks = 0;
  }

  /* A couple of convenience pointers */
  g0 = &generations[0];
  oldest_gen = &generations[RtsFlags.GcFlags.generations-1];

  /* Allocate step structures in each generation */
  if (RtsFlags.GcFlags.generations > 1) {
    /* Only for multiple-generations */

    /* Oldest generation: one step */
    oldest_gen->n_steps = 1;
    oldest_gen->steps   = all_steps + (RtsFlags.GcFlags.generations - 1)
	                              * RtsFlags.GcFlags.steps;

    /* set up all except the oldest generation with 2 steps */
    for(g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
      generations[g].n_steps = RtsFlags.GcFlags.steps;
      generations[g].steps   = all_steps + g * RtsFlags.GcFlags.steps;
    }
    
  } else {
    /* single generation, i.e. a two-space collector */
    g0->n_steps = 1;
    g0->steps   = all_steps;
  }

#ifdef THREADED_RTS
  n_nurseries = n_capabilities;
#else
  n_nurseries = 1;
#endif
  nurseries = stgMallocBytes (n_nurseries * sizeof(struct step_),
			      "initStorage: nurseries");

  /* Initialise all steps */
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
	initStep(&generations[g].steps[s], g, s);
    }
  }
  
  for (s = 0; s < n_nurseries; s++) {
      initStep(&nurseries[s], 0, s);
  }
  
  /* Set up the destination pointers in each younger gen. step */
  for (g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
    for (s = 0; s < generations[g].n_steps-1; s++) {
      generations[g].steps[s].to = &generations[g].steps[s+1];
    }
    generations[g].steps[s].to = &generations[g+1].steps[0];
  }
  oldest_gen->steps[0].to = &oldest_gen->steps[0];
  
  for (s = 0; s < n_nurseries; s++) {
      nurseries[s].to = generations[0].steps[0].to;
  }
  
  /* The oldest generation has one step. */
  if (RtsFlags.GcFlags.compact || RtsFlags.GcFlags.sweep) {
      if (RtsFlags.GcFlags.generations == 1) {
	  errorBelch("WARNING: compact/sweep is incompatible with -G1; disabled");
      } else {
	  oldest_gen->steps[0].mark = 1;
          if (RtsFlags.GcFlags.compact)
              oldest_gen->steps[0].compact = 1;
      }
  }

  generations[0].max_blocks = 0;
  g0s0 = &generations[0].steps[0];

  /* The allocation area.  Policy: keep the allocation area
   * small to begin with, even if we have a large suggested heap
   * size.  Reason: we're going to do a major collection first, and we
   * don't want it to be a big one.  This vague idea is borne out by 
   * rigorous experimental evidence.
   */
  allocNurseries();

  weak_ptr_list = NULL;
  caf_list = NULL;
  revertible_caf_list = NULL;
   
  /* initialise the allocate() interface */
  alloc_blocks = 0;
  alloc_blocks_lim = RtsFlags.GcFlags.minAllocAreaSize;

  exec_block = NULL;

#ifdef THREADED_RTS
  initSpinLock(&gc_alloc_block_sync);
  whitehole_spin = 0;
#endif

  N = 0;

  initGcThreads();

  IF_DEBUG(gc, statDescribeGens());

  RELEASE_SM_LOCK;
}

void
exitStorage (void)
{
    stat_exit(calcAllocated());
}

void
freeStorage (void)
{
    stgFree(g0s0); // frees all the steps
    stgFree(generations);
    freeAllMBlocks();
#if defined(THREADED_RTS)
    closeMutex(&sm_mutex);
#endif
    stgFree(nurseries);
    freeGcThreads();
}

/* -----------------------------------------------------------------------------
   CAF management.

   The entry code for every CAF does the following:
     
      - builds a CAF_BLACKHOLE in the heap
      - pushes an update frame pointing to the CAF_BLACKHOLE
      - invokes UPD_CAF(), which:
          - calls newCaf, below
	  - updates the CAF with a static indirection to the CAF_BLACKHOLE
      
   Why do we build a BLACKHOLE in the heap rather than just updating
   the thunk directly?  It's so that we only need one kind of update
   frame - otherwise we'd need a static version of the update frame too.

   newCaf() does the following:
       
      - it puts the CAF on the oldest generation's mut-once list.
        This is so that we can treat the CAF as a root when collecting
	younger generations.

   For GHCI, we have additional requirements when dealing with CAFs:

      - we must *retain* all dynamically-loaded CAFs ever entered,
        just in case we need them again.
      - we must be able to *revert* CAFs that have been evaluated, to
        their pre-evaluated form.

      To do this, we use an additional CAF list.  When newCaf() is
      called on a dynamically-loaded CAF, we add it to the CAF list
      instead of the old-generation mutable list, and save away its
      old info pointer (in caf->saved_info) for later reversion.

      To revert all the CAFs, we traverse the CAF list and reset the
      info pointer to caf->saved_info, then throw away the CAF list.
      (see GC.c:revertCAFs()).

      -- SDM 29/1/01

   -------------------------------------------------------------------------- */

void
newCAF(StgClosure* caf)
{
  ACQUIRE_SM_LOCK;

#ifdef DYNAMIC
  if(keepCAFs)
  {
    // HACK:
    // If we are in GHCi _and_ we are using dynamic libraries,
    // then we can't redirect newCAF calls to newDynCAF (see below),
    // so we make newCAF behave almost like newDynCAF.
    // The dynamic libraries might be used by both the interpreted
    // program and GHCi itself, so they must not be reverted.
    // This also means that in GHCi with dynamic libraries, CAFs are not
    // garbage collected. If this turns out to be a problem, we could
    // do another hack here and do an address range test on caf to figure
    // out whether it is from a dynamic library.
    ((StgIndStatic *)caf)->saved_info  = (StgInfoTable *)caf->header.info;
    ((StgIndStatic *)caf)->static_link = caf_list;
    caf_list = caf;
  }
  else
#endif
  {
    /* Put this CAF on the mutable list for the old generation.
    * This is a HACK - the IND_STATIC closure doesn't really have
    * a mut_link field, but we pretend it has - in fact we re-use
    * the STATIC_LINK field for the time being, because when we
    * come to do a major GC we won't need the mut_link field
    * any more and can use it as a STATIC_LINK.
    */
    ((StgIndStatic *)caf)->saved_info = NULL;
    recordMutableGen(caf, oldest_gen->no);
  }
  
  RELEASE_SM_LOCK;
}

// An alternate version of newCaf which is used for dynamically loaded
// object code in GHCi.  In this case we want to retain *all* CAFs in
// the object code, because they might be demanded at any time from an
// expression evaluated on the command line.
// Also, GHCi might want to revert CAFs, so we add these to the
// revertible_caf_list.
//
// The linker hackily arranges that references to newCaf from dynamic
// code end up pointing to newDynCAF.
void
newDynCAF(StgClosure *caf)
{
    ACQUIRE_SM_LOCK;

    ((StgIndStatic *)caf)->saved_info  = (StgInfoTable *)caf->header.info;
    ((StgIndStatic *)caf)->static_link = revertible_caf_list;
    revertible_caf_list = caf;

    RELEASE_SM_LOCK;
}

/* -----------------------------------------------------------------------------
   Nursery management.
   -------------------------------------------------------------------------- */

static bdescr *
allocNursery (step *stp, bdescr *tail, nat blocks)
{
    bdescr *bd;
    nat i;

    // Allocate a nursery: we allocate fresh blocks one at a time and
    // cons them on to the front of the list, not forgetting to update
    // the back pointer on the tail of the list to point to the new block.
    for (i=0; i < blocks; i++) {
	// @LDV profiling
	/*
	  processNursery() in LdvProfile.c assumes that every block group in
	  the nursery contains only a single block. So, if a block group is
	  given multiple blocks, change processNursery() accordingly.
	*/
	bd = allocBlock();
	bd->link = tail;
	// double-link the nursery: we might need to insert blocks
	if (tail != NULL) {
	    tail->u.back = bd;
	}
	bd->step = stp;
	bd->gen_no = 0;
	bd->flags = 0;
	bd->free = bd->start;
	tail = bd;
    }
    tail->u.back = NULL;
    return tail;
}

static void
assignNurseriesToCapabilities (void)
{
#ifdef THREADED_RTS
    nat i;

    for (i = 0; i < n_nurseries; i++) {
	capabilities[i].r.rNursery        = &nurseries[i];
	capabilities[i].r.rCurrentNursery = nurseries[i].blocks;
	capabilities[i].r.rCurrentAlloc   = NULL;
    }
#else /* THREADED_RTS */
    MainCapability.r.rNursery        = &nurseries[0];
    MainCapability.r.rCurrentNursery = nurseries[0].blocks;
    MainCapability.r.rCurrentAlloc   = NULL;
#endif
}

static void
allocNurseries( void )
{ 
    nat i;

    for (i = 0; i < n_nurseries; i++) {
	nurseries[i].blocks = 
	    allocNursery(&nurseries[i], NULL, 
			 RtsFlags.GcFlags.minAllocAreaSize);
	nurseries[i].n_blocks    = RtsFlags.GcFlags.minAllocAreaSize;
	nurseries[i].old_blocks   = NULL;
	nurseries[i].n_old_blocks = 0;
    }
    assignNurseriesToCapabilities();
}
      
void
resetNurseries( void )
{
    nat i;
    bdescr *bd;
    step *stp;

    for (i = 0; i < n_nurseries; i++) {
	stp = &nurseries[i];
	for (bd = stp->blocks; bd; bd = bd->link) {
	    bd->free = bd->start;
	    ASSERT(bd->gen_no == 0);
	    ASSERT(bd->step == stp);
	    IF_DEBUG(sanity,memset(bd->start, 0xaa, BLOCK_SIZE));
	}
    }
    assignNurseriesToCapabilities();
}

lnat
countNurseryBlocks (void)
{
    nat i;
    lnat blocks = 0;

    for (i = 0; i < n_nurseries; i++) {
	blocks += nurseries[i].n_blocks;
    }
    return blocks;
}

static void
resizeNursery ( step *stp, nat blocks )
{
  bdescr *bd;
  nat nursery_blocks;

  nursery_blocks = stp->n_blocks;
  if (nursery_blocks == blocks) return;

  if (nursery_blocks < blocks) {
      debugTrace(DEBUG_gc, "increasing size of nursery to %d blocks", 
		 blocks);
    stp->blocks = allocNursery(stp, stp->blocks, blocks-nursery_blocks);
  } 
  else {
    bdescr *next_bd;
    
    debugTrace(DEBUG_gc, "decreasing size of nursery to %d blocks", 
	       blocks);

    bd = stp->blocks;
    while (nursery_blocks > blocks) {
	next_bd = bd->link;
	next_bd->u.back = NULL;
	nursery_blocks -= bd->blocks; // might be a large block
	freeGroup(bd);
	bd = next_bd;
    }
    stp->blocks = bd;
    // might have gone just under, by freeing a large block, so make
    // up the difference.
    if (nursery_blocks < blocks) {
	stp->blocks = allocNursery(stp, stp->blocks, blocks-nursery_blocks);
    }
  }
  
  stp->n_blocks = blocks;
  ASSERT(countBlocks(stp->blocks) == stp->n_blocks);
}

// 
// Resize each of the nurseries to the specified size.
//
void
resizeNurseriesFixed (nat blocks)
{
    nat i;
    for (i = 0; i < n_nurseries; i++) {
	resizeNursery(&nurseries[i], blocks);
    }
}

// 
// Resize the nurseries to the total specified size.
//
void
resizeNurseries (nat blocks)
{
    // If there are multiple nurseries, then we just divide the number
    // of available blocks between them.
    resizeNurseriesFixed(blocks / n_nurseries);
}


/* -----------------------------------------------------------------------------
   move_TSO is called to update the TSO structure after it has been
   moved from one place to another.
   -------------------------------------------------------------------------- */

void
move_TSO (StgTSO *src, StgTSO *dest)
{
    ptrdiff_t diff;

    // relocate the stack pointer... 
    diff = (StgPtr)dest - (StgPtr)src; // In *words* 
    dest->sp = (StgPtr)dest->sp + diff;
}

/* -----------------------------------------------------------------------------
   The allocate() interface

   allocateInGen() function allocates memory directly into a specific
   generation.  It always succeeds, and returns a chunk of memory n
   words long.  n can be larger than the size of a block if necessary,
   in which case a contiguous block group will be allocated.

   allocate(n) is equivalent to allocateInGen(g0).
   -------------------------------------------------------------------------- */

StgPtr
allocateInGen (generation *g, lnat n)
{
    step *stp;
    bdescr *bd;
    StgPtr ret;

    ACQUIRE_SM_LOCK;
    
    TICK_ALLOC_HEAP_NOCTR(n);
    CCS_ALLOC(CCCS,n);

    stp = &g->steps[0];

    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_))
    {
	lnat req_blocks =  (lnat)BLOCK_ROUND_UP(n*sizeof(W_)) / BLOCK_SIZE;

        // Attempting to allocate an object larger than maxHeapSize
        // should definitely be disallowed.  (bug #1791)
        if (RtsFlags.GcFlags.maxHeapSize > 0 && 
            req_blocks >= RtsFlags.GcFlags.maxHeapSize) {
            heapOverflow();
            // heapOverflow() doesn't exit (see #2592), but we aren't
            // in a position to do a clean shutdown here: we
            // either have to allocate the memory or exit now.
            // Allocating the memory would be bad, because the user
            // has requested that we not exceed maxHeapSize, so we
            // just exit.
	    stg_exit(EXIT_HEAPOVERFLOW);
        }

	bd = allocGroup(req_blocks);
	dbl_link_onto(bd, &stp->large_objects);
	stp->n_large_blocks += bd->blocks; // might be larger than req_blocks
	alloc_blocks += bd->blocks;
	bd->gen_no  = g->no;
	bd->step = stp;
	bd->flags = BF_LARGE;
	bd->free = bd->start + n;
	ret = bd->start;
    }
    else
    {
        // small allocation (<LARGE_OBJECT_THRESHOLD) */
        bd = stp->blocks;
	if (bd == NULL || bd->free + n > bd->start + BLOCK_SIZE_W) {
            bd = allocBlock();
            bd->gen_no = g->no;
            bd->step = stp;
            bd->flags = 0;
            bd->link = stp->blocks;
            stp->blocks = bd;
            stp->n_blocks++;
            alloc_blocks++;
        }
        ret = bd->free;
        bd->free += n;
    }

    RELEASE_SM_LOCK;

    return ret;
}

StgPtr
allocate (lnat n)
{
    return allocateInGen(g0,n);
}

lnat
allocatedBytes( void )
{
    lnat allocated;

    allocated = alloc_blocks * BLOCK_SIZE_W;
    if (pinned_object_block != NULL) {
	allocated -= (pinned_object_block->start + BLOCK_SIZE_W) - 
	    pinned_object_block->free;
    }
	
    return allocated;
}

// split N blocks off the front of the given bdescr, returning the
// new block group.  We treat the remainder as if it
// had been freshly allocated in generation 0.
bdescr *
splitLargeBlock (bdescr *bd, nat blocks)
{
    bdescr *new_bd;

    // subtract the original number of blocks from the counter first
    bd->step->n_large_blocks -= bd->blocks;

    new_bd = splitBlockGroup (bd, blocks);

    dbl_link_onto(new_bd, &g0s0->large_objects);
    g0s0->n_large_blocks += new_bd->blocks;
    new_bd->gen_no  = g0s0->no;
    new_bd->step    = g0s0;
    new_bd->flags   = BF_LARGE;
    new_bd->free    = bd->free;
    ASSERT(new_bd->free <= new_bd->start + new_bd->blocks * BLOCK_SIZE_W);

    // add the new number of blocks to the counter.  Due to the gaps
    // for block descriptor, new_bd->blocks + bd->blocks might not be
    // equal to the original bd->blocks, which is why we do it this way.
    bd->step->n_large_blocks += bd->blocks;

    return new_bd;
}

/* -----------------------------------------------------------------------------
   allocateLocal()

   This allocates memory in the current thread - it is intended for
   use primarily from STG-land where we have a Capability.  It is
   better than allocate() because it doesn't require taking the
   sm_mutex lock in the common case.

   Memory is allocated directly from the nursery if possible (but not
   from the current nursery block, so as not to interfere with
   Hp/HpLim).
   -------------------------------------------------------------------------- */

StgPtr
allocateLocal (Capability *cap, lnat n)
{
    bdescr *bd;
    StgPtr p;

    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
        return allocateInGen(g0,n);
    }

    /* small allocation (<LARGE_OBJECT_THRESHOLD) */

    TICK_ALLOC_HEAP_NOCTR(n);
    CCS_ALLOC(CCCS,n);
    
    bd = cap->r.rCurrentAlloc;
    if (bd == NULL || bd->free + n > bd->start + BLOCK_SIZE_W) {
        
        // The CurrentAlloc block is full, we need to find another
        // one.  First, we try taking the next block from the
        // nursery:
        bd = cap->r.rCurrentNursery->link;
        
        if (bd == NULL || bd->free + n > bd->start + BLOCK_SIZE_W) {
            // The nursery is empty, or the next block is already
            // full: allocate a fresh block (we can't fail here).
            ACQUIRE_SM_LOCK;
            bd = allocBlock();
            cap->r.rNursery->n_blocks++;
            RELEASE_SM_LOCK;
            bd->gen_no = 0;
            bd->step = cap->r.rNursery;
            bd->flags = 0;
            // NO: alloc_blocks++;
            // calcAllocated() uses the size of the nursery, and we've
            // already bumpted nursery->n_blocks above.  We'll GC
            // pretty quickly now anyway, because MAYBE_GC() will
            // notice that CurrentNursery->link is NULL.
        } else {
            // we have a block in the nursery: take it and put
            // it at the *front* of the nursery list, and use it
            // to allocate() from.
            cap->r.rCurrentNursery->link = bd->link;
            if (bd->link != NULL) {
                bd->link->u.back = cap->r.rCurrentNursery;
            }
        }
        dbl_link_onto(bd, &cap->r.rNursery->blocks);
        cap->r.rCurrentAlloc = bd;
        IF_DEBUG(sanity, checkNurserySanity(cap->r.rNursery));
    }
    p = bd->free;
    bd->free += n;
    return p;
}

/* ---------------------------------------------------------------------------
   Allocate a fixed/pinned object.

   We allocate small pinned objects into a single block, allocating a
   new block when the current one overflows.  The block is chained
   onto the large_object_list of generation 0 step 0.

   NOTE: The GC can't in general handle pinned objects.  This
   interface is only safe to use for ByteArrays, which have no
   pointers and don't require scavenging.  It works because the
   block's descriptor has the BF_LARGE flag set, so the block is
   treated as a large object and chained onto various lists, rather
   than the individual objects being copied.  However, when it comes
   to scavenge the block, the GC will only scavenge the first object.
   The reason is that the GC can't linearly scan a block of pinned
   objects at the moment (doing so would require using the
   mostly-copying techniques).  But since we're restricting ourselves
   to pinned ByteArrays, not scavenging is ok.

   This function is called by newPinnedByteArray# which immediately
   fills the allocated memory with a MutableByteArray#.
   ------------------------------------------------------------------------- */

StgPtr
allocatePinned( lnat n )
{
    StgPtr p;
    bdescr *bd = pinned_object_block;

    // If the request is for a large object, then allocate()
    // will give us a pinned object anyway.
    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	p = allocate(n);
        Bdescr(p)->flags |= BF_PINNED;
        return p;
    }

    ACQUIRE_SM_LOCK;
    
    TICK_ALLOC_HEAP_NOCTR(n);
    CCS_ALLOC(CCCS,n);

    // If we don't have a block of pinned objects yet, or the current
    // one isn't large enough to hold the new object, allocate a new one.
    if (bd == NULL || (bd->free + n) > (bd->start + BLOCK_SIZE_W)) {
	pinned_object_block = bd = allocBlock();
	dbl_link_onto(bd, &g0s0->large_objects);
	g0s0->n_large_blocks++;
	bd->gen_no = 0;
	bd->step   = g0s0;
	bd->flags  = BF_PINNED | BF_LARGE;
	bd->free   = bd->start;
	alloc_blocks++;
    }

    p = bd->free;
    bd->free += n;
    RELEASE_SM_LOCK;
    return p;
}

/* -----------------------------------------------------------------------------
   Write Barriers
   -------------------------------------------------------------------------- */

/*
   This is the write barrier for MUT_VARs, a.k.a. IORefs.  A
   MUT_VAR_CLEAN object is not on the mutable list; a MUT_VAR_DIRTY
   is.  When written to, a MUT_VAR_CLEAN turns into a MUT_VAR_DIRTY
   and is put on the mutable list.
*/
void
dirty_MUT_VAR(StgRegTable *reg, StgClosure *p)
{
    Capability *cap = regTableToCapability(reg);
    bdescr *bd;
    if (p->header.info == &stg_MUT_VAR_CLEAN_info) {
	p->header.info = &stg_MUT_VAR_DIRTY_info;
	bd = Bdescr((StgPtr)p);
	if (bd->gen_no > 0) recordMutableCap(p,cap,bd->gen_no);
    }
}

// Setting a TSO's link field with a write barrier.
// It is *not* necessary to call this function when
//    * setting the link field to END_TSO_QUEUE
//    * putting a TSO on the blackhole_queue
//    * setting the link field of the currently running TSO, as it
//      will already be dirty.
void
setTSOLink (Capability *cap, StgTSO *tso, StgTSO *target)
{
    bdescr *bd;
    if (tso->dirty == 0 && (tso->flags & TSO_LINK_DIRTY) == 0) {
        tso->flags |= TSO_LINK_DIRTY;
	bd = Bdescr((StgPtr)tso);
	if (bd->gen_no > 0) recordMutableCap((StgClosure*)tso,cap,bd->gen_no);
    }
    tso->_link = target;
}

void
dirty_TSO (Capability *cap, StgTSO *tso)
{
    bdescr *bd;
    if (tso->dirty == 0 && (tso->flags & TSO_LINK_DIRTY) == 0) {
	bd = Bdescr((StgPtr)tso);
	if (bd->gen_no > 0) recordMutableCap((StgClosure*)tso,cap,bd->gen_no);
    }
    tso->dirty = 1;
}

/*
   This is the write barrier for MVARs.  An MVAR_CLEAN objects is not
   on the mutable list; a MVAR_DIRTY is.  When written to, a
   MVAR_CLEAN turns into a MVAR_DIRTY and is put on the mutable list.
   The check for MVAR_CLEAN is inlined at the call site for speed,
   this really does make a difference on concurrency-heavy benchmarks
   such as Chaneneos and cheap-concurrency.
*/
void
dirty_MVAR(StgRegTable *reg, StgClosure *p)
{
    Capability *cap = regTableToCapability(reg);
    bdescr *bd;
    bd = Bdescr((StgPtr)p);
    if (bd->gen_no > 0) recordMutableCap(p,cap,bd->gen_no);
}

/* -----------------------------------------------------------------------------
 * Stats and stuff
 * -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * calcAllocated()
 *
 * Approximate how much we've allocated: number of blocks in the
 * nursery + blocks allocated via allocate() - unused nusery blocks.
 * This leaves a little slop at the end of each block, and doesn't
 * take into account large objects (ToDo).
 * -------------------------------------------------------------------------- */

lnat
calcAllocated( void )
{
  nat allocated;
  bdescr *bd;

  allocated = allocatedBytes();
  allocated += countNurseryBlocks() * BLOCK_SIZE_W;
  
  {
#ifdef THREADED_RTS
  nat i;
  for (i = 0; i < n_nurseries; i++) {
      Capability *cap;
      for ( bd = capabilities[i].r.rCurrentNursery->link; 
	    bd != NULL; bd = bd->link ) {
	  allocated -= BLOCK_SIZE_W;
      }
      cap = &capabilities[i];
      if (cap->r.rCurrentNursery->free < 
	  cap->r.rCurrentNursery->start + BLOCK_SIZE_W) {
	  allocated -= (cap->r.rCurrentNursery->start + BLOCK_SIZE_W)
	      - cap->r.rCurrentNursery->free;
      }
  }
#else
  bdescr *current_nursery = MainCapability.r.rCurrentNursery;

  for ( bd = current_nursery->link; bd != NULL; bd = bd->link ) {
      allocated -= BLOCK_SIZE_W;
  }
  if (current_nursery->free < current_nursery->start + BLOCK_SIZE_W) {
      allocated -= (current_nursery->start + BLOCK_SIZE_W)
	  - current_nursery->free;
  }
#endif
  }

  total_allocated += allocated;
  return allocated;
}  

/* Approximate the amount of live data in the heap.  To be called just
 * after garbage collection (see GarbageCollect()).
 */
lnat 
calcLiveBlocks(void)
{
  nat g, s;
  lnat live = 0;
  step *stp;

  if (RtsFlags.GcFlags.generations == 1) {
      return g0s0->n_large_blocks + g0s0->n_blocks;
  }

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      /* approximate amount of live data (doesn't take into account slop
       * at end of each block).
       */
      if (g == 0 && s == 0) { 
	  continue; 
      }
      stp = &generations[g].steps[s];
      live += stp->n_large_blocks + stp->n_blocks;
    }
  }
  return live;
}

lnat
countOccupied(bdescr *bd)
{
    lnat words;

    words = 0;
    for (; bd != NULL; bd = bd->link) {
        ASSERT(bd->free <= bd->start + bd->blocks * BLOCK_SIZE_W);
        words += bd->free - bd->start;
    }
    return words;
}

// Return an accurate count of the live data in the heap, excluding
// generation 0.
lnat
calcLiveWords(void)
{
    nat g, s;
    lnat live;
    step *stp;
    
    if (RtsFlags.GcFlags.generations == 1) {
        return g0s0->n_words + countOccupied(g0s0->large_objects);
    }
    
    live = 0;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (s = 0; s < generations[g].n_steps; s++) {
            if (g == 0 && s == 0) continue; 
            stp = &generations[g].steps[s];
            live += stp->n_words + countOccupied(stp->large_objects);
        } 
    }
    return live;
}

/* Approximate the number of blocks that will be needed at the next
 * garbage collection.
 *
 * Assume: all data currently live will remain live.  Steps that will
 * be collected next time will therefore need twice as many blocks
 * since all the data will be copied.
 */
extern lnat 
calcNeeded(void)
{
    lnat needed = 0;
    nat g, s;
    step *stp;
    
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	for (s = 0; s < generations[g].n_steps; s++) {
	    if (g == 0 && s == 0) { continue; }
	    stp = &generations[g].steps[s];

            // we need at least this much space
            needed += stp->n_blocks + stp->n_large_blocks;

            // any additional space needed to collect this gen next time?
	    if (g == 0 || // always collect gen 0
                (generations[g].steps[0].n_blocks +
                 generations[g].steps[0].n_large_blocks 
                 > generations[g].max_blocks)) {
                // we will collect this gen next time
                if (stp->mark) {
                    //  bitmap:
                    needed += stp->n_blocks / BITS_IN(W_);
                    //  mark stack:
                    needed += stp->n_blocks / 100;
                }
                if (stp->compact) {
                    continue; // no additional space needed for compaction
                } else {
                    needed += stp->n_blocks;
                }
	    }
	}
    }
    return needed;
}

/* ----------------------------------------------------------------------------
   Executable memory

   Executable memory must be managed separately from non-executable
   memory.  Most OSs these days require you to jump through hoops to
   dynamically allocate executable memory, due to various security
   measures.

   Here we provide a small memory allocator for executable memory.
   Memory is managed with a page granularity; we allocate linearly
   in the page, and when the page is emptied (all objects on the page
   are free) we free the page again, not forgetting to make it
   non-executable.

   TODO: The inability to handle objects bigger than BLOCK_SIZE_W means that
         the linker cannot use allocateExec for loading object code files
         on Windows. Once allocateExec can handle larger objects, the linker
         should be modified to use allocateExec instead of VirtualAlloc.
   ------------------------------------------------------------------------- */

#if defined(linux_HOST_OS)

// On Linux we need to use libffi for allocating executable memory,
// because it knows how to work around the restrictions put in place
// by SELinux.

void *allocateExec (nat bytes, void **exec_ret)
{
    void **ret, **exec;
    ACQUIRE_SM_LOCK;
    ret = ffi_closure_alloc (sizeof(void *) + (size_t)bytes, (void**)&exec);
    RELEASE_SM_LOCK;
    if (ret == NULL) return ret;
    *ret = ret; // save the address of the writable mapping, for freeExec().
    *exec_ret = exec + 1;
    return (ret + 1);
}

// freeExec gets passed the executable address, not the writable address. 
void freeExec (void *addr)
{
    void *writable;
    writable = *((void**)addr - 1);
    ACQUIRE_SM_LOCK;
    ffi_closure_free (writable);
    RELEASE_SM_LOCK
}

#else

void *allocateExec (nat bytes, void **exec_ret)
{
    void *ret;
    nat n;

    ACQUIRE_SM_LOCK;

    // round up to words.
    n  = (bytes + sizeof(W_) + 1) / sizeof(W_);

    if (n+1 > BLOCK_SIZE_W) {
	barf("allocateExec: can't handle large objects");
    }

    if (exec_block == NULL || 
	exec_block->free + n + 1 > exec_block->start + BLOCK_SIZE_W) {
	bdescr *bd;
	lnat pagesize = getPageSize();
	bd = allocGroup(stg_max(1, pagesize / BLOCK_SIZE));
	debugTrace(DEBUG_gc, "allocate exec block %p", bd->start);
	bd->gen_no = 0;
	bd->flags = BF_EXEC;
	bd->link = exec_block;
	if (exec_block != NULL) {
	    exec_block->u.back = bd;
	}
	bd->u.back = NULL;
	setExecutable(bd->start, bd->blocks * BLOCK_SIZE, rtsTrue);
	exec_block = bd;
    }
    *(exec_block->free) = n;  // store the size of this chunk
    exec_block->gen_no += n;  // gen_no stores the number of words allocated
    ret = exec_block->free + 1;
    exec_block->free += n + 1;

    RELEASE_SM_LOCK
    *exec_ret = ret;
    return ret;
}

void freeExec (void *addr)
{
    StgPtr p = (StgPtr)addr - 1;
    bdescr *bd = Bdescr((StgPtr)p);

    if ((bd->flags & BF_EXEC) == 0) {
	barf("freeExec: not executable");
    }

    if (*(StgPtr)p == 0) {
	barf("freeExec: already free?");
    }

    ACQUIRE_SM_LOCK;

    bd->gen_no -= *(StgPtr)p;
    *(StgPtr)p = 0;

    if (bd->gen_no == 0) {
        // Free the block if it is empty, but not if it is the block at
        // the head of the queue.
        if (bd != exec_block) {
            debugTrace(DEBUG_gc, "free exec block %p", bd->start);
            dbl_link_remove(bd, &exec_block);
            setExecutable(bd->start, bd->blocks * BLOCK_SIZE, rtsFalse);
            freeGroup(bd);
        } else {
            bd->free = bd->start;
        }
    }

    RELEASE_SM_LOCK
}    

#endif /* mingw32_HOST_OS */

/* -----------------------------------------------------------------------------
   Debugging

   memInventory() checks for memory leaks by counting up all the
   blocks we know about and comparing that to the number of blocks
   allegedly floating around in the system.
   -------------------------------------------------------------------------- */

#ifdef DEBUG

// Useful for finding partially full blocks in gdb
void findSlop(bdescr *bd);
void findSlop(bdescr *bd)
{
    lnat slop;

    for (; bd != NULL; bd = bd->link) {
        slop = (bd->blocks * BLOCK_SIZE_W) - (bd->free - bd->start);
        if (slop > (1024/sizeof(W_))) {
            debugBelch("block at %p (bdescr %p) has %ldKB slop\n",
                       bd->start, bd, slop / (1024/sizeof(W_)));
        }
    }
}

nat
countBlocks(bdescr *bd)
{
    nat n;
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
static nat
countAllocdBlocks(bdescr *bd)
{
    nat n;
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

static lnat
stepBlocks (step *stp)
{
    ASSERT(countBlocks(stp->blocks) == stp->n_blocks);
    ASSERT(countBlocks(stp->large_objects) == stp->n_large_blocks);
    return stp->n_blocks + stp->n_old_blocks + 
	    countAllocdBlocks(stp->large_objects);
}

// If memInventory() calculates that we have a memory leak, this
// function will try to find the block(s) that are leaking by marking
// all the ones that we know about, and search through memory to find
// blocks that are not marked.  In the debugger this can help to give
// us a clue about what kind of block leaked.  In the future we might
// annotate blocks with their allocation site to give more helpful
// info.
static void
findMemoryLeak (void)
{
  nat g, s, i;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (i = 0; i < n_capabilities; i++) {
	  markBlocks(capabilities[i].mut_lists[g]);
      }
      markBlocks(generations[g].mut_list);
      for (s = 0; s < generations[g].n_steps; s++) {
	  markBlocks(generations[g].steps[s].blocks);
	  markBlocks(generations[g].steps[s].large_objects);
      }
  }

  for (i = 0; i < n_nurseries; i++) {
      markBlocks(nurseries[i].blocks);
      markBlocks(nurseries[i].large_objects);
  }

#ifdef PROFILING
  // TODO:
  // if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER) {
  //    markRetainerBlocks();
  // }
#endif

  // count the blocks allocated by the arena allocator
  // TODO:
  // markArenaBlocks();

  // count the blocks containing executable memory
  markBlocks(exec_block);

  reportUnmarkedBlocks();
}


void
memInventory (rtsBool show)
{
  nat g, s, i;
  step *stp;
  lnat gen_blocks[RtsFlags.GcFlags.generations];
  lnat nursery_blocks, retainer_blocks,
       arena_blocks, exec_blocks;
  lnat live_blocks = 0, free_blocks = 0;
  rtsBool leak;

  // count the blocks we current have

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      gen_blocks[g] = 0;
      for (i = 0; i < n_capabilities; i++) {
	  gen_blocks[g] += countBlocks(capabilities[i].mut_lists[g]);
      }	  
      gen_blocks[g] += countAllocdBlocks(generations[g].mut_list);
      for (s = 0; s < generations[g].n_steps; s++) {
	  stp = &generations[g].steps[s];
	  gen_blocks[g] += stepBlocks(stp);
      }
  }

  nursery_blocks = 0;
  for (i = 0; i < n_nurseries; i++) {
      nursery_blocks += stepBlocks(&nurseries[i]);
  }

  retainer_blocks = 0;
#ifdef PROFILING
  if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER) {
      retainer_blocks = retainerStackBlocks();
  }
#endif

  // count the blocks allocated by the arena allocator
  arena_blocks = arenaBlocks();

  // count the blocks containing executable memory
  exec_blocks = countAllocdBlocks(exec_block);

  /* count the blocks on the free list */
  free_blocks = countFreeList();

  live_blocks = 0;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      live_blocks += gen_blocks[g];
  }
  live_blocks += nursery_blocks + 
               + retainer_blocks + arena_blocks + exec_blocks;

#define MB(n) (((n) * BLOCK_SIZE_W) / ((1024*1024)/sizeof(W_)))

  leak = live_blocks + free_blocks != mblocks_allocated * BLOCKS_PER_MBLOCK;

  if (show || leak)
  {
      if (leak) { 
          debugBelch("Memory leak detected:\n");
      } else {
          debugBelch("Memory inventory:\n");
      }
      for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	  debugBelch("  gen %d blocks : %5lu blocks (%lu MB)\n", g, 
                     gen_blocks[g], MB(gen_blocks[g]));
      }
      debugBelch("  nursery      : %5lu blocks (%lu MB)\n", 
                 nursery_blocks, MB(nursery_blocks));
      debugBelch("  retainer     : %5lu blocks (%lu MB)\n", 
                 retainer_blocks, MB(retainer_blocks));
      debugBelch("  arena blocks : %5lu blocks (%lu MB)\n", 
                 arena_blocks, MB(arena_blocks));
      debugBelch("  exec         : %5lu blocks (%lu MB)\n", 
                 exec_blocks, MB(exec_blocks));
      debugBelch("  free         : %5lu blocks (%lu MB)\n", 
                 free_blocks, MB(free_blocks));
      debugBelch("  total        : %5lu blocks (%lu MB)\n",
                 live_blocks + free_blocks, MB(live_blocks+free_blocks));
      if (leak) {
          debugBelch("\n  in system    : %5lu blocks (%lu MB)\n", 
                     mblocks_allocated * BLOCKS_PER_MBLOCK, mblocks_allocated);
      }
  }

  if (leak) {
      debugBelch("\n");
      findMemoryLeak();
  }
  ASSERT(n_alloc_blocks == live_blocks);
  ASSERT(!leak);
}


/* Full heap sanity check. */
void
checkSanity( void )
{
    nat g, s;

    if (RtsFlags.GcFlags.generations == 1) {
	checkHeap(g0s0->blocks);
	checkLargeObjects(g0s0->large_objects);
    } else {
	
	for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    for (s = 0; s < generations[g].n_steps; s++) {
		if (g == 0 && s == 0) { continue; }
		ASSERT(countBlocks(generations[g].steps[s].blocks)
		       == generations[g].steps[s].n_blocks);
		ASSERT(countBlocks(generations[g].steps[s].large_objects)
		       == generations[g].steps[s].n_large_blocks);
		checkHeap(generations[g].steps[s].blocks);
		checkLargeObjects(generations[g].steps[s].large_objects);
	    }
	}

	for (s = 0; s < n_nurseries; s++) {
	    ASSERT(countBlocks(nurseries[s].blocks)
		   == nurseries[s].n_blocks);
	    ASSERT(countBlocks(nurseries[s].large_objects)
		   == nurseries[s].n_large_blocks);
	}
	    
	checkFreeListSanity();
    }

#if defined(THREADED_RTS)
    // check the stacks too in threaded mode, because we don't do a
    // full heap sanity check in this case (see checkHeap())
    checkMutableLists(rtsTrue);
#else
    checkMutableLists(rtsFalse);
#endif
}

/* Nursery sanity check */
void
checkNurserySanity( step *stp )
{
    bdescr *bd, *prev;
    nat blocks = 0;

    prev = NULL;
    for (bd = stp->blocks; bd != NULL; bd = bd->link) {
	ASSERT(bd->u.back == prev);
	prev = bd;
	blocks += bd->blocks;
    }
    ASSERT(blocks == stp->n_blocks);
}

// handy function for use in gdb, because Bdescr() is inlined.
extern bdescr *_bdescr( StgPtr p );

bdescr *
_bdescr( StgPtr p )
{
    return Bdescr(p);
}

#endif
