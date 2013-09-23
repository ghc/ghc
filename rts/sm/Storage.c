/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * Storage manager front end
 *
 * Documentation on the architecture of the Storage Manager can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "GCThread.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "BlockAlloc.h"
#include "Weak.h"
#include "Sanity.h"
#include "Arena.h"
#include "Capability.h"
#include "Schedule.h"
#include "RetainerProfile.h"        // for counting memory blocks (memInventory)
#include "OSMem.h"
#include "Trace.h"
#include "GC.h"
#include "Evac.h"
#if defined(ios_HOST_OS)
#include "Hash.h"
#endif

#include <string.h>

#include "ffi.h"

/* 
 * All these globals require sm_mutex to access in THREADED_RTS mode.
 */
StgClosure    *caf_list         = NULL;
StgClosure    *revertible_caf_list = NULL;
rtsBool       keepCAFs;

W_ large_alloc_lim;    /* GC if n_large_blocks in any nursery
                        * reaches this. */

bdescr *exec_block;

generation *generations = NULL; /* all the generations */
generation *g0          = NULL; /* generation 0, for convenience */
generation *oldest_gen  = NULL; /* oldest generation, for convenience */

nursery *nurseries = NULL;     /* array of nurseries, size == n_capabilities */

#ifdef THREADED_RTS
/*
 * Storage manager mutex:  protects all the above state from
 * simultaneous access by two STG threads.
 */
Mutex sm_mutex;
#endif

static void allocNurseries (nat from, nat to);

static void
initGeneration (generation *gen, int g)
{
    gen->no = g;
    gen->collections = 0;
    gen->par_collections = 0;
    gen->failed_promotions = 0;
    gen->max_blocks = 0;
    gen->blocks = NULL;
    gen->n_blocks = 0;
    gen->n_words = 0;
    gen->live_estimate = 0;
    gen->old_blocks = NULL;
    gen->n_old_blocks = 0;
    gen->large_objects = NULL;
    gen->n_large_blocks = 0;
    gen->n_large_words = 0;
    gen->n_new_large_words = 0;
    gen->scavenged_large_objects = NULL;
    gen->n_scavenged_large_blocks = 0;
    gen->mark = 0;
    gen->compact = 0;
    gen->bitmap = NULL;
#ifdef THREADED_RTS
    initSpinLock(&gen->sync);
#endif
    gen->threads = END_TSO_QUEUE;
    gen->old_threads = END_TSO_QUEUE;
    gen->weak_ptr_list = NULL;
    gen->old_weak_ptr_list = NULL;
}

void
initStorage (void)
{
  nat g;

  if (generations != NULL) {
      // multi-init protection
      return;
  }

  initMBlocks();

  /* Sanity check to make sure the LOOKS_LIKE_ macros appear to be
   * doing something reasonable.
   */
  /* We use the NOT_NULL variant or gcc warns that the test is always true */
  ASSERT(LOOKS_LIKE_INFO_PTR_NOT_NULL((StgWord)&stg_BLOCKING_QUEUE_CLEAN_info));
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

  /* Initialise all generations */
  for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
      initGeneration(&generations[g], g);
  }

  /* A couple of convenience pointers */
  g0 = &generations[0];
  oldest_gen = &generations[RtsFlags.GcFlags.generations-1];

  /* Set up the destination pointers in each younger gen. step */
  for (g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
      generations[g].to = &generations[g+1];
  }
  oldest_gen->to = oldest_gen;
  
  /* The oldest generation has one step. */
  if (RtsFlags.GcFlags.compact || RtsFlags.GcFlags.sweep) {
      if (RtsFlags.GcFlags.generations == 1) {
          errorBelch("WARNING: compact/sweep is incompatible with -G1; disabled");
      } else {
          oldest_gen->mark = 1;
          if (RtsFlags.GcFlags.compact)
              oldest_gen->compact = 1;
      }
  }

  generations[0].max_blocks = 0;

  caf_list = END_OF_STATIC_LIST;
  revertible_caf_list = END_OF_STATIC_LIST;
   
  /* initialise the allocate() interface */
  large_alloc_lim = RtsFlags.GcFlags.minAllocAreaSize * BLOCK_SIZE_W;

  exec_block = NULL;

#ifdef THREADED_RTS
  initSpinLock(&gc_alloc_block_sync);
#ifdef PROF_SPIN
  whitehole_spin = 0;
#endif
#endif

  N = 0;

  storageAddCapabilities(0, n_capabilities);

  IF_DEBUG(gc, statDescribeGens());

  RELEASE_SM_LOCK;

  traceEventHeapInfo(CAPSET_HEAP_DEFAULT,
                     RtsFlags.GcFlags.generations,
                     RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE_W * sizeof(W_),
                     RtsFlags.GcFlags.minAllocAreaSize * BLOCK_SIZE_W * sizeof(W_),
                     MBLOCK_SIZE_W * sizeof(W_),
                     BLOCK_SIZE_W  * sizeof(W_));
}

void storageAddCapabilities (nat from, nat to)
{
    nat n, g, i;

    if (from > 0) {
        nurseries = stgReallocBytes(nurseries, to * sizeof(struct nursery_),
                                    "storageAddCapabilities");
    } else {
        nurseries = stgMallocBytes(to * sizeof(struct nursery_),
                                   "storageAddCapabilities");
    }

    // we've moved the nurseries, so we have to update the rNursery
    // pointers from the Capabilities.
    for (i = 0; i < to; i++) {
        capabilities[i]->r.rNursery = &nurseries[i];
    }

    /* The allocation area.  Policy: keep the allocation area
     * small to begin with, even if we have a large suggested heap
     * size.  Reason: we're going to do a major collection first, and we
     * don't want it to be a big one.  This vague idea is borne out by
     * rigorous experimental evidence.
     */
    allocNurseries(from, to);

    // allocate a block for each mut list
    for (n = from; n < to; n++) {
        for (g = 1; g < RtsFlags.GcFlags.generations; g++) {
            capabilities[n]->mut_lists[g] = allocBlock();
        }
    }

#if defined(THREADED_RTS) && defined(llvm_CC_FLAVOR)
    newThreadLocalKey(&gctKey);
#endif

    initGcThreads(from, to);
}


void
exitStorage (void)
{
    updateNurseriesStats();
    stat_exit();
}

void
freeStorage (rtsBool free_heap)
{
    stgFree(generations);
    if (free_heap) freeAllMBlocks();
#if defined(THREADED_RTS)
    closeMutex(&sm_mutex);
#endif
    stgFree(nurseries);
#if defined(THREADED_RTS) && defined(llvm_CC_FLAVOR)
    freeThreadLocalKey(&gctKey);
#endif
    freeGcThreads();
}

/* -----------------------------------------------------------------------------
   CAF management.

   The entry code for every CAF does the following:
     
      - builds a CAF_BLACKHOLE in the heap

      - calls newCaf, which atomically updates the CAF with
        IND_STATIC pointing to the CAF_BLACKHOLE

      - if newCaf returns zero, it re-enters the CAF (see Note [atomic
        CAF entry])

      - pushes an update frame pointing to the CAF_BLACKHOLE

   Why do we build an BLACKHOLE in the heap rather than just updating
   the thunk directly?  It's so that we only need one kind of update
   frame - otherwise we'd need a static version of the update frame
   too, and various other parts of the RTS that deal with update
   frames would also need special cases for static update frames.

   newCaf() does the following:
       
      - it updates the CAF with an IND_STATIC pointing to the
        CAF_BLACKHOLE, atomically.

      - it puts the CAF on the oldest generation's mutable list.
        This is so that we treat the CAF as a root when collecting
        younger generations.

   ------------------
   Note [atomic CAF entry]

   With THREADED_RTS, newCaf() is required to be atomic (see
   #5558). This is because if two threads happened to enter the same
   CAF simultaneously, they would create two distinct CAF_BLACKHOLEs,
   and so the normal threadPaused() machinery for detecting duplicate
   evaluation will not detect this.  Hence in lockCAF() below, we
   atomically lock the CAF with WHITEHOLE before updating it with
   IND_STATIC, and return zero if another thread locked the CAF first.
   In the event that we lost the race, CAF entry code will re-enter
   the CAF and block on the other thread's CAF_BLACKHOLE.

   ------------------
   Note [GHCi CAFs]

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

STATIC_INLINE StgWord lockCAF (StgClosure *caf, StgClosure *bh)
{
    const StgInfoTable *orig_info;

    orig_info = caf->header.info;

#ifdef THREADED_RTS
    const StgInfoTable *cur_info;

    if (orig_info == &stg_IND_STATIC_info ||
        orig_info == &stg_WHITEHOLE_info) {
        // already claimed by another thread; re-enter the CAF
        return 0;
    }

    cur_info = (const StgInfoTable *)
        cas((StgVolatilePtr)&caf->header.info,
            (StgWord)orig_info,
            (StgWord)&stg_WHITEHOLE_info);

    if (cur_info != orig_info) {
        // already claimed by another thread; re-enter the CAF
        return 0;
    }

    // successfully claimed by us; overwrite with IND_STATIC
#endif

    // For the benefit of revertCAFs(), save the original info pointer
    ((StgIndStatic *)caf)->saved_info  = orig_info;

    ((StgIndStatic*)caf)->indirectee = bh;
    write_barrier();
    SET_INFO(caf,&stg_IND_STATIC_info);

    return 1;
}

StgWord
newCAF(StgRegTable *reg, StgClosure *caf, StgClosure *bh)
{
    if (lockCAF(caf,bh) == 0) return 0;

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

        ACQUIRE_SM_LOCK; // caf_list is global, locked by sm_mutex
        ((StgIndStatic *)caf)->static_link = caf_list;
        caf_list = caf;
        RELEASE_SM_LOCK;
    }
    else
    {
        // Put this CAF on the mutable list for the old generation.
        ((StgIndStatic *)caf)->saved_info = NULL;
        if (oldest_gen->no != 0) {
            recordMutableCap(caf, regTableToCapability(reg), oldest_gen->no);
        }
    }
    return 1;
}

// External API for setting the keepCAFs flag. see #3900.
void
setKeepCAFs (void)
{
    keepCAFs = 1;
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
StgWord
newDynCAF (StgRegTable *reg STG_UNUSED, StgClosure *caf, StgClosure *bh)
{
    if (lockCAF(caf,bh) == 0) return 0;

    ACQUIRE_SM_LOCK;

    ((StgIndStatic *)caf)->static_link = revertible_caf_list;
    revertible_caf_list = caf;

    RELEASE_SM_LOCK;

    return 1;
}

/* -----------------------------------------------------------------------------
   Nursery management.
   -------------------------------------------------------------------------- */

static bdescr *
allocNursery (bdescr *tail, W_ blocks)
{
    bdescr *bd = NULL;
    W_ i, n;

    // We allocate the nursery as a single contiguous block and then
    // divide it into single blocks manually.  This way we guarantee
    // that the nursery blocks are adjacent, so that the processor's
    // automatic prefetching works across nursery blocks.  This is a
    // tiny optimisation (~0.5%), but it's free.

    while (blocks > 0) {
        n = stg_min(BLOCKS_PER_MBLOCK, blocks);
        // allocLargeChunk will prefer large chunks, but will pick up
        // small chunks if there are any available.  We must allow
        // single blocks here to avoid fragmentation (#7257)
        bd = allocLargeChunk(1, n);
        n = bd->blocks;
        blocks -= n;

        for (i = 0; i < n; i++) {
            initBdescr(&bd[i], g0, g0);

            bd[i].blocks = 1;
            bd[i].flags = 0;

            if (i > 0) {
                bd[i].u.back = &bd[i-1];
            } else {
                bd[i].u.back = NULL;
            }

            if (i+1 < n) {
                bd[i].link = &bd[i+1];
            } else {
                bd[i].link = tail;
                if (tail != NULL) {
                    tail->u.back = &bd[i];
                }
            }

            bd[i].free = bd[i].start;
        }

        tail = &bd[0];
    }

    return &bd[0];
}

static void
assignNurseriesToCapabilities (nat from, nat to)
{
    nat i;

    for (i = from; i < to; i++) {
        capabilities[i]->r.rCurrentNursery = nurseries[i].blocks;
        capabilities[i]->r.rCurrentAlloc   = NULL;
    }
}

static void
allocNurseries (nat from, nat to)
{ 
    nat i;

    for (i = from; i < to; i++) {
        nurseries[i].blocks =
            allocNursery(NULL, RtsFlags.GcFlags.minAllocAreaSize);
        nurseries[i].n_blocks =
            RtsFlags.GcFlags.minAllocAreaSize;
    }
    assignNurseriesToCapabilities(from, to);
}
      
void
clearNursery (Capability *cap)
{
    bdescr *bd;

    for (bd = nurseries[cap->no].blocks; bd; bd = bd->link) {
        cap->total_allocated += (W_)(bd->free - bd->start);
        bd->free = bd->start;
        ASSERT(bd->gen_no == 0);
        ASSERT(bd->gen == g0);
        IF_DEBUG(sanity,memset(bd->start, 0xaa, BLOCK_SIZE));
    }
}

void
resetNurseries (void)
{
    assignNurseriesToCapabilities(0, n_capabilities);
}

W_
countNurseryBlocks (void)
{
    nat i;
    W_ blocks = 0;

    for (i = 0; i < n_capabilities; i++) {
        blocks += nurseries[i].n_blocks;
    }
    return blocks;
}

static void
resizeNursery (nursery *nursery, W_ blocks)
{
  bdescr *bd;
  W_ nursery_blocks;

  nursery_blocks = nursery->n_blocks;
  if (nursery_blocks == blocks) return;

  if (nursery_blocks < blocks) {
      debugTrace(DEBUG_gc, "increasing size of nursery to %d blocks", 
                 blocks);
    nursery->blocks = allocNursery(nursery->blocks, blocks-nursery_blocks);
  } 
  else {
    bdescr *next_bd;
    
    debugTrace(DEBUG_gc, "decreasing size of nursery to %d blocks", 
               blocks);

    bd = nursery->blocks;
    while (nursery_blocks > blocks) {
        next_bd = bd->link;
        next_bd->u.back = NULL;
        nursery_blocks -= bd->blocks; // might be a large block
        freeGroup(bd);
        bd = next_bd;
    }
    nursery->blocks = bd;
    // might have gone just under, by freeing a large block, so make
    // up the difference.
    if (nursery_blocks < blocks) {
        nursery->blocks = allocNursery(nursery->blocks, blocks-nursery_blocks);
    }
  }
  
  nursery->n_blocks = blocks;
  ASSERT(countBlocks(nursery->blocks) == nursery->n_blocks);
}

// 
// Resize each of the nurseries to the specified size.
//
void
resizeNurseriesFixed (W_ blocks)
{
    nat i;
    for (i = 0; i < n_capabilities; i++) {
        resizeNursery(&nurseries[i], blocks);
    }
}

// 
// Resize the nurseries to the total specified size.
//
void
resizeNurseries (W_ blocks)
{
    // If there are multiple nurseries, then we just divide the number
    // of available blocks between them.
    resizeNurseriesFixed(blocks / n_capabilities);
}


/* -----------------------------------------------------------------------------
   move_STACK is called to update the TSO structure after it has been
   moved from one place to another.
   -------------------------------------------------------------------------- */

void
move_STACK (StgStack *src, StgStack *dest)
{
    ptrdiff_t diff;

    // relocate the stack pointer... 
    diff = (StgPtr)dest - (StgPtr)src; // In *words* 
    dest->sp = (StgPtr)dest->sp + diff;
}

/* -----------------------------------------------------------------------------
   allocate()

   This allocates memory in the current thread - it is intended for
   use primarily from STG-land where we have a Capability.  It is
   better than allocate() because it doesn't require taking the
   sm_mutex lock in the common case.

   Memory is allocated directly from the nursery if possible (but not
   from the current nursery block, so as not to interfere with
   Hp/HpLim).
   -------------------------------------------------------------------------- */

StgPtr
allocate (Capability *cap, W_ n)
{
    bdescr *bd;
    StgPtr p;

    TICK_ALLOC_HEAP_NOCTR(WDS(n));
    CCS_ALLOC(cap->r.rCCCS,n);
    
    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
        W_ req_blocks =  (W_)BLOCK_ROUND_UP(n*sizeof(W_)) / BLOCK_SIZE;

        // Attempting to allocate an object larger than maxHeapSize
        // should definitely be disallowed.  (bug #1791)
        if ((RtsFlags.GcFlags.maxHeapSize > 0 &&
             req_blocks >= RtsFlags.GcFlags.maxHeapSize) ||
            req_blocks >= HS_INT32_MAX)   // avoid overflow when
                                          // calling allocGroup() below
        {
            heapOverflow();
            // heapOverflow() doesn't exit (see #2592), but we aren't
            // in a position to do a clean shutdown here: we
            // either have to allocate the memory or exit now.
            // Allocating the memory would be bad, because the user
            // has requested that we not exceed maxHeapSize, so we
            // just exit.
            stg_exit(EXIT_HEAPOVERFLOW);
        }

        ACQUIRE_SM_LOCK
        bd = allocGroup(req_blocks);
        dbl_link_onto(bd, &g0->large_objects);
        g0->n_large_blocks += bd->blocks; // might be larger than req_blocks
        g0->n_new_large_words += n;
        RELEASE_SM_LOCK;
        initBdescr(bd, g0, g0);
        bd->flags = BF_LARGE;
        bd->free = bd->start + n;
        cap->total_allocated += n;
        return bd->start;
    }

    /* small allocation (<LARGE_OBJECT_THRESHOLD) */

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
            initBdescr(bd, g0, g0);
            bd->flags = 0;
            // If we had to allocate a new block, then we'll GC
            // pretty quickly now, because MAYBE_GC() will
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

    IF_DEBUG(sanity, ASSERT(*((StgWord8*)p) == 0xaa));
    return p;
}

/* ---------------------------------------------------------------------------
   Allocate a fixed/pinned object.

   We allocate small pinned objects into a single block, allocating a
   new block when the current one overflows.  The block is chained
   onto the large_object_list of generation 0.

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
allocatePinned (Capability *cap, W_ n)
{
    StgPtr p;
    bdescr *bd;

    // If the request is for a large object, then allocate()
    // will give us a pinned object anyway.
    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
        p = allocate(cap, n);
        Bdescr(p)->flags |= BF_PINNED;
        return p;
    }

    TICK_ALLOC_HEAP_NOCTR(WDS(n));
    CCS_ALLOC(cap->r.rCCCS,n);

    bd = cap->pinned_object_block;
    
    // If we don't have a block of pinned objects yet, or the current
    // one isn't large enough to hold the new object, get a new one.
    if (bd == NULL || (bd->free + n) > (bd->start + BLOCK_SIZE_W)) {

        // stash the old block on cap->pinned_object_blocks.  On the
        // next GC cycle these objects will be moved to
        // g0->large_objects.
        if (bd != NULL) {
            dbl_link_onto(bd, &cap->pinned_object_blocks);
            // add it to the allocation stats when the block is full
            cap->total_allocated += bd->free - bd->start;
        }

        // We need to find another block.  We could just allocate one,
        // but that means taking a global lock and we really want to
        // avoid that (benchmarks that allocate a lot of pinned
        // objects scale really badly if we do this).
        //
        // So first, we try taking the next block from the nursery, in
        // the same way as allocate(), but note that we can only take
        // an *empty* block, because we're about to mark it as
        // BF_PINNED | BF_LARGE.
        bd = cap->r.rCurrentNursery->link;
        if (bd == NULL || bd->free != bd->start) { // must be empty!
            // The nursery is empty, or the next block is non-empty:
            // allocate a fresh block (we can't fail here).

            // XXX in the case when the next nursery block is
            // non-empty we aren't exerting any pressure to GC soon,
            // so if this case ever happens then we could in theory
            // keep allocating for ever without calling the GC. We
            // can't bump g0->n_new_large_words because that will be
            // counted towards allocation, and we're already counting
            // our pinned obects as allocation in
            // collect_pinned_object_blocks in the GC.
            ACQUIRE_SM_LOCK;
            bd = allocBlock();
            RELEASE_SM_LOCK;
            initBdescr(bd, g0, g0);
        } else {
            // we have a block in the nursery: steal it
            cap->r.rCurrentNursery->link = bd->link;
            if (bd->link != NULL) {
                bd->link->u.back = cap->r.rCurrentNursery;
            }
            cap->r.rNursery->n_blocks -= bd->blocks;
        }

        cap->pinned_object_block = bd;
        bd->flags  = BF_PINNED | BF_LARGE | BF_EVACUATED;

        // The pinned_object_block remains attached to the capability
        // until it is full, even if a GC occurs.  We want this
        // behaviour because otherwise the unallocated portion of the
        // block would be forever slop, and under certain workloads
        // (allocating a few ByteStrings per GC) we accumulate a lot
        // of slop.
        //
        // So, the pinned_object_block is initially marked
        // BF_EVACUATED so the GC won't touch it.  When it is full,
        // we place it on the large_objects list, and at the start of
        // the next GC the BF_EVACUATED flag will be cleared, and the
        // block will be promoted as usual (if anything in it is
        // live).
    }

    p = bd->free;
    bd->free += n;
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
    if (p->header.info == &stg_MUT_VAR_CLEAN_info) {
        p->header.info = &stg_MUT_VAR_DIRTY_info;
        recordClosureMutated(cap,p);
    }
}

void
dirty_TVAR(Capability *cap, StgTVar *p)
{
    if (p->header.info == &stg_TVAR_CLEAN_info) {
        p->header.info = &stg_TVAR_DIRTY_info;
        recordClosureMutated(cap,(StgClosure*)p);
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
    if (tso->dirty == 0) {
        tso->dirty = 1;
        recordClosureMutated(cap,(StgClosure*)tso);
    }
    tso->_link = target;
}

void
setTSOPrev (Capability *cap, StgTSO *tso, StgTSO *target)
{
    if (tso->dirty == 0) {
        tso->dirty = 1;
        recordClosureMutated(cap,(StgClosure*)tso);
    }
    tso->block_info.prev = target;
}

void
dirty_TSO (Capability *cap, StgTSO *tso)
{
    if (tso->dirty == 0) {
        tso->dirty = 1;
        recordClosureMutated(cap,(StgClosure*)tso);
    }
}

void
dirty_STACK (Capability *cap, StgStack *stack)
{
    if (stack->dirty == 0) {
        stack->dirty = 1;
        recordClosureMutated(cap,(StgClosure*)stack);
    }
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
    recordClosureMutated(regTableToCapability(reg),p);
}

/* -----------------------------------------------------------------------------
 * Stats and stuff
 * -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * updateNurseriesStats()
 *
 * Update the per-cap total_allocated numbers with an approximation of
 * the amount of memory used in each cap's nursery.
 *
 * Since this update is also performed by clearNurseries() then we only
 * need this function for the final stats when the RTS is shutting down.
 * -------------------------------------------------------------------------- */

void updateNurseriesStats (void)
{
    nat i;

    for (i = 0; i < n_capabilities; i++) {
        capabilities[i]->total_allocated += countOccupied(nurseries[i].blocks);
    }
}

W_ countOccupied (bdescr *bd)
{
    W_ words;

    words = 0;
    for (; bd != NULL; bd = bd->link) {
        ASSERT(bd->free <= bd->start + bd->blocks * BLOCK_SIZE_W);
        words += bd->free - bd->start;
    }
    return words;
}

W_ genLiveWords (generation *gen)
{
    return gen->n_words + gen->n_large_words;
}

W_ genLiveBlocks (generation *gen)
{
    return gen->n_blocks + gen->n_large_blocks;
}

W_ gcThreadLiveWords (nat i, nat g)
{
    W_ words;

    words   = countOccupied(gc_threads[i]->gens[g].todo_bd);
    words  += countOccupied(gc_threads[i]->gens[g].part_list);
    words  += countOccupied(gc_threads[i]->gens[g].scavd_list);

    return words;
}

W_ gcThreadLiveBlocks (nat i, nat g)
{
    W_ blocks;

    blocks  = countBlocks(gc_threads[i]->gens[g].todo_bd);
    blocks += gc_threads[i]->gens[g].n_part_blocks;
    blocks += gc_threads[i]->gens[g].n_scavd_blocks;

    return blocks;
}

// Return an accurate count of the live data in the heap, excluding
// generation 0.
W_ calcLiveWords (void)
{
    nat g;
    W_ live;

    live = 0;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        live += genLiveWords(&generations[g]);
    }
    return live;
}

W_ calcLiveBlocks (void)
{
    nat g;
    W_ live;

    live = 0;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        live += genLiveBlocks(&generations[g]);
    }
    return live;
}

/* Determine which generation will be collected next, and approximate
 * the maximum amount of memory that will be required to do the GC,
 * taking into account data that will be copied, and the space needed
 * to store bitmaps and the mark stack.  Note: blocks_needed does not
 * include the blocks in the nursery.
 *
 * Assume: all data currently live will remain live.  Generationss
 * that will be collected next time will therefore need twice as many
 * blocks since all the data will be copied.
 */
extern W_ 
calcNeeded (rtsBool force_major, memcount *blocks_needed)
{
    W_ needed = 0, blocks;
    nat g, N;
    generation *gen;
    
    if (force_major) {
        N = RtsFlags.GcFlags.generations - 1;
    } else {
        N = 0;
    }

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        gen = &generations[g];

        blocks = gen->n_blocks // or: gen->n_words / BLOCK_SIZE_W (?)
               + gen->n_large_blocks;

        // we need at least this much space
        needed += blocks;
        
        // are we collecting this gen?
        if (g == 0 || // always collect gen 0
            blocks > gen->max_blocks)
        {
            N = stg_max(N,g);

            // we will collect this gen next time
            if (gen->mark) {
                //  bitmap:
                needed += gen->n_blocks / BITS_IN(W_);
                //  mark stack:
                needed += gen->n_blocks / 100;
            }
            if (gen->compact) {
                continue; // no additional space needed for compaction
            } else {
                needed += gen->n_blocks;
            }
        }
    }

    if (blocks_needed != NULL) {
        *blocks_needed = needed;
    }
    return N;
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

AdjustorWritable allocateExec (W_ bytes, AdjustorExecutable *exec_ret)
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
void freeExec (AdjustorExecutable addr)
{
    AdjustorWritable writable;
    writable = *((void**)addr - 1);
    ACQUIRE_SM_LOCK;
    ffi_closure_free (writable);
    RELEASE_SM_LOCK
}

#elif defined(ios_HOST_OS)

static HashTable* allocatedExecs;

AdjustorWritable allocateExec(W_ bytes, AdjustorExecutable *exec_ret)
{
    AdjustorWritable writ;
    ffi_closure* cl;
    if (bytes != sizeof(ffi_closure)) {
        barf("allocateExec: for ffi_closure only");
    }
    ACQUIRE_SM_LOCK;
    cl = writ = ffi_closure_alloc((size_t)bytes, exec_ret);
    if (cl != NULL) {
        if (allocatedExecs == NULL) {
            allocatedExecs = allocHashTable();
        }
        insertHashTable(allocatedExecs, (StgWord)*exec_ret, writ);
    }
    RELEASE_SM_LOCK;
    return writ;
}

AdjustorWritable execToWritable(AdjustorExecutable exec)
{
    AdjustorWritable writ;
    ACQUIRE_SM_LOCK;
    if (allocatedExecs == NULL ||
       (writ = lookupHashTable(allocatedExecs, (StgWord)exec)) == NULL) {
        RELEASE_SM_LOCK;
        barf("execToWritable: not found");
    }
    RELEASE_SM_LOCK;
    return writ;
}

void freeExec(AdjustorExecutable exec)
{
    AdjustorWritable writ;
    ffi_closure* cl;
    cl = writ = execToWritable(exec);
    ACQUIRE_SM_LOCK;
    removeHashTable(allocatedExecs, (StgWord)exec, writ);
    ffi_closure_free(cl);
    RELEASE_SM_LOCK
}

#else

AdjustorWritable allocateExec (W_ bytes, AdjustorExecutable *exec_ret)
{
    void *ret;
    W_ n;

    ACQUIRE_SM_LOCK;

    // round up to words.
    n  = (bytes + sizeof(W_) + 1) / sizeof(W_);

    if (n+1 > BLOCK_SIZE_W) {
        barf("allocateExec: can't handle large objects");
    }

    if (exec_block == NULL || 
        exec_block->free + n + 1 > exec_block->start + BLOCK_SIZE_W) {
        bdescr *bd;
        W_ pagesize = getPageSize();
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

#ifdef DEBUG

// handy function for use in gdb, because Bdescr() is inlined.
extern bdescr *_bdescr (StgPtr p);

bdescr *
_bdescr (StgPtr p)
{
    return Bdescr(p);
}

#endif
