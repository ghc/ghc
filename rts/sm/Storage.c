/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * Storage manager front end
 *
 * Documentation on the architecture of the Storage Manager can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage
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
#include "NonMoving.h"
#if defined(ios_HOST_OS)
#include "Hash.h"
#endif

#include <string.h>

#include "ffi.h"

/*
 * All these globals require sm_mutex to access in THREADED_RTS mode.
 */
StgIndStatic  *dyn_caf_list        = NULL;
StgIndStatic  *debug_caf_list      = NULL;
StgIndStatic  *revertible_caf_list = NULL;
bool           keepCAFs;

W_ large_alloc_lim;    /* GC if n_large_blocks in any nursery
                        * reaches this. */

bdescr *exec_block;

generation *generations = NULL; /* all the generations */
generation *g0          = NULL; /* generation 0, for convenience */
generation *oldest_gen  = NULL; /* oldest generation, for convenience */

/*
 * Array of nurseries, size == n_capabilities
 *
 * nursery[i] belongs to NUMA node (i % n_numa_nodes)
 * This is chosen to be the same convention as capabilities[i], so
 * that when not using nursery chunks (+RTS -n), we just map
 * capabilities to nurseries 1:1.
 */
nursery *nurseries = NULL;
uint32_t n_nurseries;

/*
 * When we are using nursery chunks, we need a separate next_nursery
 * pointer for each NUMA node.
 */
volatile StgWord next_nursery[MAX_NUMA_NODES];

#if defined(THREADED_RTS)
/*
 * Storage manager mutex:  protects all the above state from
 * simultaneous access by two STG threads.
 */
Mutex sm_mutex;
#endif

static void allocNurseries (uint32_t from, uint32_t to);
static void assignNurseriesToCapabilities (uint32_t from, uint32_t to);

void
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
    gen->compact_objects = NULL;
    gen->n_compact_blocks = 0;
    gen->compact_blocks_in_import = NULL;
    gen->n_compact_blocks_in_import = 0;
    gen->scavenged_large_objects = NULL;
    gen->n_scavenged_large_blocks = 0;
    gen->live_compact_objects = NULL;
    gen->n_live_compact_blocks = 0;
    gen->compact_blocks_in_import = NULL;
    gen->n_compact_blocks_in_import = 0;
    gen->mark = 0;
    gen->compact = 0;
    gen->bitmap = NULL;
#if defined(THREADED_RTS)
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
  uint32_t g, n;

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

  // Nonmoving heap uses oldest_gen so initialize it after initializing oldest_gen
  nonmovingInit();

#if defined(THREADED_RTS)
  // nonmovingAddCapabilities allocates segments, which requires taking the gc
  // sync lock, so initialize it before nonmovingAddCapabilities
  initSpinLock(&gc_alloc_block_sync);
#endif

  if (RtsFlags.GcFlags.useNonmoving)
      nonmovingAddCapabilities(n_capabilities);

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

  dyn_caf_list = (StgIndStatic*)END_OF_CAF_LIST;
  debug_caf_list = (StgIndStatic*)END_OF_CAF_LIST;
  revertible_caf_list = (StgIndStatic*)END_OF_CAF_LIST;

  if (RtsFlags.GcFlags.largeAllocLim > 0) {
      large_alloc_lim = RtsFlags.GcFlags.largeAllocLim * BLOCK_SIZE_W;
  } else {
      large_alloc_lim = RtsFlags.GcFlags.minAllocAreaSize * BLOCK_SIZE_W;
  }

  exec_block = NULL;

  N = 0;

  for (n = 0; n < n_numa_nodes; n++) {
      next_nursery[n] = n;
  }
  storageAddCapabilities(0, n_capabilities);

  IF_DEBUG(gc, statDescribeGens());

  RELEASE_SM_LOCK;

  traceEventHeapInfo(CAPSET_HEAP_DEFAULT,
                     RtsFlags.GcFlags.generations,
                     RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE,
                     RtsFlags.GcFlags.minAllocAreaSize * BLOCK_SIZE,
                     MBLOCK_SIZE,
                     BLOCK_SIZE);
}

void storageAddCapabilities (uint32_t from, uint32_t to)
{
    uint32_t n, g, i, new_n_nurseries;
    nursery *old_nurseries;

    if (RtsFlags.GcFlags.nurseryChunkSize == 0) {
        new_n_nurseries = to;
    } else {
        memcount total_alloc = to * RtsFlags.GcFlags.minAllocAreaSize;
        new_n_nurseries =
            stg_max(to, total_alloc / RtsFlags.GcFlags.nurseryChunkSize);
    }

    old_nurseries = nurseries;
    if (from > 0) {
        nurseries = stgReallocBytes(nurseries,
                                    new_n_nurseries * sizeof(struct nursery_),
                                    "storageAddCapabilities");
    } else {
        nurseries = stgMallocBytes(new_n_nurseries * sizeof(struct nursery_),
                                   "storageAddCapabilities");
    }

    // we've moved the nurseries, so we have to update the rNursery
    // pointers from the Capabilities.
    for (i = 0; i < from; i++) {
        uint32_t index = capabilities[i]->r.rNursery - old_nurseries;
        capabilities[i]->r.rNursery = &nurseries[index];
    }

    /* The allocation area.  Policy: keep the allocation area
     * small to begin with, even if we have a large suggested heap
     * size.  Reason: we're going to do a major collection first, and we
     * don't want it to be a big one.  This vague idea is borne out by
     * rigorous experimental evidence.
     */
    allocNurseries(n_nurseries, new_n_nurseries);
    n_nurseries = new_n_nurseries;

    /*
     * Assign each of the new capabilities a nursery.  Remember to start from
     * next_nursery, because we may have already consumed some of the earlier
     * nurseries.
     */
    assignNurseriesToCapabilities(from,to);

    // allocate a block for each mut list
    for (n = from; n < to; n++) {
        for (g = 1; g < RtsFlags.GcFlags.generations; g++) {
            capabilities[n]->mut_lists[g] =
                allocBlockOnNode(capNoToNumaNode(n));
        }
    }

    // Initialize NonmovingAllocators and UpdRemSets
    if (RtsFlags.GcFlags.useNonmoving) {
        nonmovingAddCapabilities(to);
        for (i = 0; i < to; ++i) {
            init_upd_rem_set(&capabilities[i]->upd_rem_set);
        }
    }

#if defined(THREADED_RTS) && defined(CC_LLVM_BACKEND) && (CC_SUPPORTS_TLS == 0)
    newThreadLocalKey(&gctKey);
#endif

    initGcThreads(from, to);
}


void
exitStorage (void)
{
    nonmovingExit();
    updateNurseriesStats();
    stat_exit();
}

void
freeStorage (bool free_heap)
{
    stgFree(generations);
    if (free_heap) freeAllMBlocks();
#if defined(THREADED_RTS)
    closeMutex(&sm_mutex);
#endif
    stgFree(nurseries);
#if defined(THREADED_RTS) && defined(CC_LLVM_BACKEND) && (CC_SUPPORTS_TLS == 0)
    freeThreadLocalKey(&gctKey);
#endif
    freeGcThreads();
}

/* -----------------------------------------------------------------------------
   Note [CAF management]
   ~~~~~~~~~~~~~~~~~~~~~

   The entry code for every CAF does the following:

      - calls newCAF, which builds a CAF_BLACKHOLE on the heap and atomically
        updates the CAF with IND_STATIC pointing to the CAF_BLACKHOLE

      - if newCAF returns zero, it re-enters the CAF (see Note [atomic
        CAF entry])

      - pushes an update frame pointing to the CAF_BLACKHOLE

   Why do we build a BLACKHOLE in the heap rather than just updating
   the thunk directly?  It's so that we only need one kind of update
   frame - otherwise we'd need a static version of the update frame
   too, and various other parts of the RTS that deal with update
   frames would also need special cases for static update frames.

   newCAF() does the following:

      - atomically locks the CAF (see [atomic CAF entry])

      - it builds a CAF_BLACKHOLE on the heap

      - it updates the CAF with an IND_STATIC pointing to the
        CAF_BLACKHOLE, atomically.

      - it puts the CAF on the oldest generation's mutable list.
        This is so that we treat the CAF as a root when collecting
        younger generations.

      - links the CAF onto the CAF list (see below)

   ------------------
   Note [atomic CAF entry]
   ~~~~~~~~~~~~~~~~~~~~~~~

   With THREADED_RTS, newCAF() is required to be atomic (see
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
   ~~~~~~~~~~~~~~~~

   For GHCI, we have additional requirements when dealing with CAFs:

      - we must *retain* all dynamically-loaded CAFs ever entered,
        just in case we need them again.
      - we must be able to *revert* CAFs that have been evaluated, to
        their pre-evaluated form.

      To do this, we use an additional CAF list.  When newCAF() is
      called on a dynamically-loaded CAF, we add it to the CAF list
      instead of the old-generation mutable list, and save away its
      old info pointer (in caf->saved_info) for later reversion.

      To revert all the CAFs, we traverse the CAF list and reset the
      info pointer to caf->saved_info, then throw away the CAF list.
      (see GC.c:revertCAFs()).

      -- SDM 29/1/01

   ------------------
   Note [Static objects under the nonmoving collector]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Static object management is a bit tricky under the nonmoving collector as we
   need to maintain a bit more state than in the moving collector. In
   particular, the moving collector uses the low bits of the STATIC_LINK field
   to determine whether the object has been moved to the scavenger's work list
   (see Note [STATIC_LINK fields] in Storage.h).

   However, the nonmoving collector also needs a place to keep its mark bit.
   This is problematic as we therefore need at least three bits of state
   but can assume only two bits are available in STATIC_LINK (due to 32-bit
   systems).

   To accommodate this we move handling of static objects entirely to the
   oldest generation when the nonmoving collector is in use. To do this safely
   and efficiently we allocate the blackhole created by lockCAF() directly in
   the non-moving heap. This means that the moving collector can completely
   ignore static objects in minor collections since they are guaranteed not to
   have any references into the moving heap. Of course, the blackhole itself
   likely will contain a reference into the moving heap but this is
   significantly easier to handle, being a heap-allocated object (see Note
   [Aging under the non-moving collector] in NonMoving.c for details).

   During the moving phase of a major collection we treat static objects
   as we do any other reference into the non-moving heap by pushing them
   to the non-moving mark queue (see Note [Aging under the non-moving
   collector]).

   This allows the non-moving collector to have full control over the flags
   in STATIC_LINK, which it uses as described in Note [STATIC_LINK fields]).
   This is implemented by NonMovingMark.c:bump_static_flag.

   In short, the plan is:

     - lockCAF allocates its blackhole in the nonmoving heap. This is important
       to ensure that we do not need to place the static object on the mut_list
       lest we would need somw way to ensure that it evacuate only once during
       a moving collection.

     - evacuate_static_object adds merely pushes objects to the mark queue

     - the nonmoving collector uses the flags in STATIC_LINK as its mark bit.

   -------------------------------------------------------------------------- */

STATIC_INLINE StgInd *
lockCAF (StgRegTable *reg, StgIndStatic *caf)
{
    const StgInfoTable *orig_info;
    Capability *cap = regTableToCapability(reg);
    StgInd *bh;

    orig_info = caf->header.info;

#if defined(THREADED_RTS)
    const StgInfoTable *cur_info;

    if (orig_info == &stg_IND_STATIC_info ||
        orig_info == &stg_WHITEHOLE_info) {
        // already claimed by another thread; re-enter the CAF
        return NULL;
    }

    cur_info = (const StgInfoTable *)
        cas((StgVolatilePtr)&caf->header.info,
            (StgWord)orig_info,
            (StgWord)&stg_WHITEHOLE_info);

    if (cur_info != orig_info) {
        // already claimed by another thread; re-enter the CAF
        return NULL;
    }

    // successfully claimed by us; overwrite with IND_STATIC
#endif

    // Push stuff that will become unreachable after updating to UpdRemSet to
    // maintain snapshot invariant
    const StgInfoTable *orig_info_tbl = INFO_PTR_TO_STRUCT(orig_info);
    // OSA: Assertions to make sure my understanding of static thunks is correct
    ASSERT(orig_info_tbl->type == THUNK_STATIC);
    // Secondly I think static thunks can't have payload: anything that they
    // reference should be in SRTs
    ASSERT(orig_info_tbl->layout.payload.ptrs == 0);
    // Because the payload is empty we just push the SRT
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        StgThunkInfoTable *thunk_info = itbl_to_thunk_itbl(orig_info_tbl);
        if (thunk_info->i.srt) {
            updateRemembSetPushClosure(cap, GET_SRT(thunk_info));
        }
    }

    // For the benefit of revertCAFs(), save the original info pointer
    caf->saved_info = orig_info;

    // Allocate the blackhole indirection closure
    if (RtsFlags.GcFlags.useNonmoving) {
        // See Note [Static objects under the nonmoving collector].
        ACQUIRE_SM_LOCK;
        bh = (StgInd *)nonmovingAllocate(cap, sizeofW(*bh));
        RELEASE_SM_LOCK;
        recordMutableCap((StgClosure*)bh,
                         regTableToCapability(reg), oldest_gen->no);
    } else {
        bh = (StgInd *)allocate(cap, sizeofW(*bh));
    }
    bh->indirectee = (StgClosure *)cap->r.rCurrentTSO;
    SET_HDR(bh, &stg_CAF_BLACKHOLE_info, caf->header.prof.ccs);
    // Ensure that above writes are visible before we introduce reference as CAF indirectee.
    write_barrier();

    caf->indirectee = (StgClosure *)bh;
    write_barrier();
    SET_INFO((StgClosure*)caf,&stg_IND_STATIC_info);

    return bh;
}

StgInd *
newCAF(StgRegTable *reg, StgIndStatic *caf)
{
    StgInd *bh;

    bh = lockCAF(reg, caf);
    if (!bh) return NULL;

    if(keepCAFs)
    {
        // Note [dyn_caf_list]
        // If we are in GHCi _and_ we are using dynamic libraries,
        // then we can't redirect newCAF calls to newRetainedCAF (see below),
        // so we make newCAF behave almost like newRetainedCAF.
        // The dynamic libraries might be used by both the interpreted
        // program and GHCi itself, so they must not be reverted.
        // This also means that in GHCi with dynamic libraries, CAFs are not
        // garbage collected. If this turns out to be a problem, we could
        // do another hack here and do an address range test on caf to figure
        // out whether it is from a dynamic library.

        ACQUIRE_SM_LOCK; // dyn_caf_list is global, locked by sm_mutex
        caf->static_link = (StgClosure*)dyn_caf_list;
        dyn_caf_list = (StgIndStatic*)((StgWord)caf | STATIC_FLAG_LIST);
        RELEASE_SM_LOCK;
    }
    else
    {
        // Put this CAF on the mutable list for the old generation.
        // N.B. the nonmoving collector works a bit differently: see
        // Note [Static objects under the nonmoving collector].
        if (oldest_gen->no != 0 && !RtsFlags.GcFlags.useNonmoving) {
            recordMutableCap((StgClosure*)caf,
                             regTableToCapability(reg), oldest_gen->no);
        }

#if defined(DEBUG)
        // In the DEBUG rts, we keep track of live CAFs by chaining them
        // onto a list debug_caf_list.  This is so that we can tell if we
        // ever enter a GC'd CAF, and emit a suitable barf().
        //
        // The saved_info field of the CAF is used as the link field for
        // debug_caf_list, because this field is only used by newDynCAF
        // for revertible CAFs, and we don't put those on the
        // debug_caf_list.
        ACQUIRE_SM_LOCK; // debug_caf_list is global, locked by sm_mutex
        ((StgIndStatic *)caf)->saved_info = (const StgInfoTable*)debug_caf_list;
        debug_caf_list = (StgIndStatic*)caf;
        RELEASE_SM_LOCK;
#endif
    }

    return bh;
}

// External API for setting the keepCAFs flag. see #3900.
void
setKeepCAFs (void)
{
    keepCAFs = 1;
}

// An alternate version of newCAF which is used for dynamically loaded
// object code in GHCi.  In this case we want to retain *all* CAFs in
// the object code, because they might be demanded at any time from an
// expression evaluated on the command line.
// Also, GHCi might want to revert CAFs, so we add these to the
// revertible_caf_list.
//
// The linker hackily arranges that references to newCAF from dynamic
// code end up pointing to newRetainedCAF.
//
StgInd* newRetainedCAF (StgRegTable *reg, StgIndStatic *caf)
{
    StgInd *bh;

    bh = lockCAF(reg, caf);
    if (!bh) return NULL;

    ACQUIRE_SM_LOCK;

    caf->static_link = (StgClosure*)revertible_caf_list;
    revertible_caf_list = (StgIndStatic*)((StgWord)caf | STATIC_FLAG_LIST);

    RELEASE_SM_LOCK;

    return bh;
}

// If we are using loadObj/unloadObj in the linker, then we want to
//
//  - retain all CAFs in statically linked code (keepCAFs == true),
//    because we might link a new object that uses any of these CAFs.
//
//  - GC CAFs in dynamically-linked code, so that we can detect when
//    a dynamically-linked object is unloadable.
//
// So for this case, we set keepCAFs to true, and link newCAF to newGCdCAF
// for dynamically-linked code.
//
StgInd* newGCdCAF (StgRegTable *reg, StgIndStatic *caf)
{
    StgInd *bh;

    bh = lockCAF(reg, caf);
    if (!bh) return NULL;

    // Put this CAF on the mutable list for the old generation.
    // N.B. the nonmoving collector works a bit differently:
    // see Note [Static objects under the nonmoving collector].
    if (oldest_gen->no != 0 && !RtsFlags.GcFlags.useNonmoving) {
        recordMutableCap((StgClosure*)caf,
                         regTableToCapability(reg), oldest_gen->no);
    }

    return bh;
}

/* -----------------------------------------------------------------------------
   Nursery management.
   -------------------------------------------------------------------------- */

static bdescr *
allocNursery (uint32_t node, bdescr *tail, W_ blocks)
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
        bd = allocLargeChunkOnNode(node, 1, n);
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

STATIC_INLINE void
assignNurseryToCapability (Capability *cap, uint32_t n)
{
    ASSERT(n < n_nurseries);
    cap->r.rNursery = &nurseries[n];
    cap->r.rCurrentNursery = nurseries[n].blocks;
    newNurseryBlock(nurseries[n].blocks);
    cap->r.rCurrentAlloc   = NULL;
    ASSERT(cap->r.rCurrentNursery->node == cap->node);
}

/*
 * Give each Capability a nursery from the pool. No need to do atomic increments
 * here, everything must be stopped to call this function.
 */
static void
assignNurseriesToCapabilities (uint32_t from, uint32_t to)
{
    uint32_t i, node;

    for (i = from; i < to; i++) {
        node = capabilities[i]->node;
        assignNurseryToCapability(capabilities[i], next_nursery[node]);
        next_nursery[node] += n_numa_nodes;
    }
}

static void
allocNurseries (uint32_t from, uint32_t to)
{
    uint32_t i;
    memcount n_blocks;

    if (RtsFlags.GcFlags.nurseryChunkSize) {
        n_blocks = RtsFlags.GcFlags.nurseryChunkSize;
    } else {
        n_blocks = RtsFlags.GcFlags.minAllocAreaSize;
    }

    for (i = from; i < to; i++) {
        nurseries[i].blocks = allocNursery(capNoToNumaNode(i), NULL, n_blocks);
        nurseries[i].n_blocks = n_blocks;
    }
}

void
resetNurseries (void)
{
    uint32_t n;

    for (n = 0; n < n_numa_nodes; n++) {
        next_nursery[n] = n;
    }
    assignNurseriesToCapabilities(0, n_capabilities);

#if defined(DEBUG)
    bdescr *bd;
    for (n = 0; n < n_nurseries; n++) {
        for (bd = nurseries[n].blocks; bd; bd = bd->link) {
            ASSERT(bd->gen_no == 0);
            ASSERT(bd->gen == g0);
            ASSERT(bd->node == capNoToNumaNode(n));
            IF_DEBUG(zero_on_gc, memset(bd->start, 0xaa, BLOCK_SIZE));
        }
    }
#endif
}

W_
countNurseryBlocks (void)
{
    uint32_t i;
    W_ blocks = 0;

    for (i = 0; i < n_nurseries; i++) {
        blocks += nurseries[i].n_blocks;
    }
    return blocks;
}

//
// Resize each of the nurseries to the specified size.
//
static void
resizeNurseriesEach (W_ blocks)
{
    uint32_t i, node;
    bdescr *bd;
    W_ nursery_blocks;
    nursery *nursery;

    for (i = 0; i < n_nurseries; i++) {
        nursery = &nurseries[i];
        nursery_blocks = nursery->n_blocks;
        if (nursery_blocks == blocks) continue;

        node = capNoToNumaNode(i);
        if (nursery_blocks < blocks) {
            debugTrace(DEBUG_gc, "increasing size of nursery to %d blocks",
                       blocks);
            nursery->blocks = allocNursery(node, nursery->blocks,
                                           blocks-nursery_blocks);
        }
        else
        {
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
                nursery->blocks = allocNursery(node, nursery->blocks,
                                               blocks-nursery_blocks);
            }
        }
        nursery->n_blocks = blocks;
        ASSERT(countBlocks(nursery->blocks) == nursery->n_blocks);
    }
}

void
resizeNurseriesFixed (void)
{
    uint32_t blocks;

    if (RtsFlags.GcFlags.nurseryChunkSize) {
        blocks = RtsFlags.GcFlags.nurseryChunkSize;
    } else {
        blocks = RtsFlags.GcFlags.minAllocAreaSize;
    }

    resizeNurseriesEach(blocks);
}

//
// Resize the nurseries to the total specified size.
//
void
resizeNurseries (W_ blocks)
{
    // If there are multiple nurseries, then we just divide the number
    // of available blocks between them.
    resizeNurseriesEach(blocks / n_nurseries);
}

bool
getNewNursery (Capability *cap)
{
    StgWord i;
    uint32_t node = cap->node;
    uint32_t n;

    for(;;) {
        i = next_nursery[node];
        if (i < n_nurseries) {
            if (cas(&next_nursery[node], i, i+n_numa_nodes) == i) {
                assignNurseryToCapability(cap, i);
                return true;
            }
        } else if (n_numa_nodes > 1) {
            // Try to find an unused nursery chunk on other nodes.  We'll get
            // remote memory, but the rationale is that avoiding GC is better
            // than avoiding remote memory access.
            bool lost = false;
            for (n = 0; n < n_numa_nodes; n++) {
                if (n == node) continue;
                i = next_nursery[n];
                if (i < n_nurseries) {
                    if (cas(&next_nursery[n], i, i+n_numa_nodes) == i) {
                        assignNurseryToCapability(cap, i);
                        return true;
                    } else {
                        lost = true; /* lost a race */
                    }
                }
            }
            if (!lost) return false;
        } else {
            return false;
        }
    }
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

STATIC_INLINE void
accountAllocation(Capability *cap, W_ n)
{
    TICK_ALLOC_HEAP_NOCTR(WDS(n));
    CCS_ALLOC(cap->r.rCCCS,n);
    if (cap->r.rCurrentTSO != NULL) {
        // cap->r.rCurrentTSO->alloc_limit -= n*sizeof(W_)
        ASSIGN_Int64((W_*)&(cap->r.rCurrentTSO->alloc_limit),
                     (PK_Int64((W_*)&(cap->r.rCurrentTSO->alloc_limit))
                      - n*sizeof(W_)));
    }

}

/* Note [slop on the heap]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 *
 * We use the term "slop" to refer to allocated memory on the heap which isn't
 * occupied by any closure. Usually closures are packet tightly into the heap
 * blocks, storage for one immediately following another. However there are
 * situations where slop is left behind:
 *
 * - Allocating large objects (BF_LARGE)
 *
 *   These are given an entire block, but if they don't fill the entire block
 *   the rest is slop. See allocateMightFail in Storage.c.
 *
 * - Allocating pinned objects with alignment (BF_PINNED)
 *
 *   These are packet into blocks like normal closures, however they
 *   can have alignment constraints and any memory that needed to be skipped for
 *   alignment becomes slop. See allocatePinned in Storage.c.
 *
 * - Shrinking (Small)Mutable(Byte)Array#
 *
 *    The size of these closures can be decreased after allocation, leaving any,
 *    now unused memory, behind as slop. See stg_resizzeMutableByteArrayzh,
 *    stg_shrinkSmallMutableArrayzh, and stg_shrinkMutableByteArrayzh in
 *    PrimOps.cmm.
 *
 *    This type of slop is extra tricky because it can also be pinned and
 *    large.
 *
 * - Overwriting closures
 *
 *   During GC the RTS overwrites closures with forwarding pointers, this can
 *   leave slop behind depending on the size of the closure being
 *   overwritten. See Note [zeroing slop when overwriting closures].
 *
 * Under various ways we actually zero slop so we can linearly scan over blocks
 * of closures. This trick is used by the sanity checking code and the heap
 * profiler, see Note [skipping slop in the heap profiler].
 *
 * When profiling we zero:
 *  - Pinned object alignment slop, see MEMSET_IF_PROFILING_W in allocatePinned.
 *  - Shrunk array slop, see OVERWRITING_CLOSURE_MUTABLE.
 *
 * When performing LDV profiling or using a (single threaded) debug RTS we zero
 * slop even when overwriting immutable closures, see Note [zeroing slop when
 * overwriting closures].
 */

/* -----------------------------------------------------------------------------
   StgPtr allocate (Capability *cap, W_ n)

   Allocates an area of memory n *words* large, from the nursery of
   the supplied Capability, or from the global block pool if the area
   requested is larger than LARGE_OBJECT_THRESHOLD.  Memory is not
   allocated from the current nursery block, so as not to interfere
   with Hp/HpLim.

   The address of the allocated memory is returned. allocate() never
   fails; if it returns, the returned value is a valid address.  If
   the nursery is already full, then another block is allocated from
   the global block pool.  If we need to get memory from the OS and
   that operation fails, then the whole process will be killed.
   -------------------------------------------------------------------------- */

/*
 * Allocate some n words of heap memory; terminating
 * on heap overflow
 */
StgPtr
allocate (Capability *cap, W_ n)
{
    StgPtr p = allocateMightFail(cap, n);
    if (p == NULL) {
        reportHeapOverflow();
        // heapOverflow() doesn't exit (see #2592), but we aren't
        // in a position to do a clean shutdown here: we
        // either have to allocate the memory or exit now.
        // Allocating the memory would be bad, because the user
        // has requested that we not exceed maxHeapSize, so we
        // just exit.
        stg_exit(EXIT_HEAPOVERFLOW);
    }
    return p;
}

/*
 * Allocate some n words of heap memory; returning NULL
 * on heap overflow
 */
StgPtr
allocateMightFail (Capability *cap, W_ n)
{
    bdescr *bd;
    StgPtr p;

    if (RTS_UNLIKELY(n >= LARGE_OBJECT_THRESHOLD/sizeof(W_))) {
        // The largest number of words such that
        // the computation of req_blocks will not overflow.
        W_ max_words = (HS_WORD_MAX & ~(BLOCK_SIZE-1)) / sizeof(W_);
        W_ req_blocks;

        if (n > max_words)
            req_blocks = HS_WORD_MAX; // signal overflow below
        else
            req_blocks = (W_)BLOCK_ROUND_UP(n*sizeof(W_)) / BLOCK_SIZE;

        // Attempting to allocate an object larger than maxHeapSize
        // should definitely be disallowed.  (bug #1791)
        if ((RtsFlags.GcFlags.maxHeapSize > 0 &&
             req_blocks >= RtsFlags.GcFlags.maxHeapSize) ||
            req_blocks >= HS_INT32_MAX)   // avoid overflow when
                                          // calling allocGroup() below
        {
            return NULL;
        }

        // Only credit allocation after we've passed the size check above
        accountAllocation(cap, n);

        ACQUIRE_SM_LOCK
        bd = allocGroupOnNode(cap->node,req_blocks);
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

    accountAllocation(cap, n);
    bd = cap->r.rCurrentAlloc;
    if (RTS_UNLIKELY(bd == NULL || bd->free + n > bd->start + BLOCK_SIZE_W)) {

        if (bd) finishedNurseryBlock(cap,bd);

        // The CurrentAlloc block is full, we need to find another
        // one.  First, we try taking the next block from the
        // nursery:
        bd = cap->r.rCurrentNursery->link;

        if (bd == NULL) {
            // The nursery is empty: allocate a fresh block (we can't
            // fail here).
            ACQUIRE_SM_LOCK;
            bd = allocBlockOnNode(cap->node);
            cap->r.rNursery->n_blocks++;
            RELEASE_SM_LOCK;
            initBdescr(bd, g0, g0);
            bd->flags = 0;
            // If we had to allocate a new block, then we'll GC
            // pretty quickly now, because MAYBE_GC() will
            // notice that CurrentNursery->link is NULL.
        } else {
            newNurseryBlock(bd);
            // we have a block in the nursery: take it and put
            // it at the *front* of the nursery list, and use it
            // to allocate() from.
            //
            // Previously the nursery looked like this:
            //
            //           CurrentNursery
            //                  /
            //                +-+    +-+
            // nursery -> ... |A| -> |B| -> ...
            //                +-+    +-+
            //
            // After doing this, it looks like this:
            //
            //                      CurrentNursery
            //                            /
            //            +-+           +-+
            // nursery -> |B| -> ... -> |A| -> ...
            //            +-+           +-+
            //             |
            //             CurrentAlloc
            //
            // The point is to get the block out of the way of the
            // advancing CurrentNursery pointer, while keeping it
            // on the nursery list so we don't lose track of it.
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

/**
 * Calculate the number of words we need to add to 'p' so it satisfies the
 * alignment constraint '(p + off) & (align-1) == 0'.
 */
#define ALIGN_WITH_OFF_W(p, align, off) \
    (((-((uintptr_t)p) - off) & (align-1)) / sizeof(W_))

/**
 * When profiling we zero the space used for alignment. This allows us to
 * traverse pinned blocks in the heap profiler.
 *
 * See Note [skipping slop in the heap profiler]
 */
#if defined(PROFILING)
#define MEMSET_IF_PROFILING_W(p, val, len_w) memset(p, val, (len_w) * sizeof(W_))
#else
#define MEMSET_IF_PROFILING_W(p, val, len_w) \
    do { (void)(p); (void)(val); (void)(len_w); } while(0)
#endif

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
   fills the allocated memory with a MutableByteArray#. Note that
   this returns NULL on heap overflow.
   ------------------------------------------------------------------------- */

StgPtr
allocatePinned (Capability *cap, W_ n /*words*/, W_ alignment /*bytes*/, W_ align_off /*bytes*/)
{
    StgPtr p;
    bdescr *bd;

    // Alignment and offset have to be a power of two
    ASSERT(alignment && !(alignment & (alignment - 1)));
    ASSERT(alignment >= sizeof(W_));

    ASSERT(!(align_off & (align_off - 1)));

    const StgWord alignment_w = alignment / sizeof(W_);

    // If the request is for a large object, then allocate()
    // will give us a pinned object anyway.
    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
        // For large objects we don't bother optimizing the number of words
        // allocated for alignment reasons. Here we just allocate the maximum
        // number of extra words we could possibly need to satisfy the alignment
        // constraint.
        p = allocateMightFail(cap, n + alignment_w - 1);
        if (p == NULL) {
            return NULL;
        } else {
            Bdescr(p)->flags |= BF_PINNED;
            W_ off_w = ALIGN_WITH_OFF_W(p, alignment, align_off);
            MEMSET_IF_PROFILING_W(p, 0, off_w);
            p += off_w;
            MEMSET_IF_PROFILING_W(p + n, 0, alignment_w - off_w - 1);
            return p;
        }
    }

    bd = cap->pinned_object_block;

    W_ off_w = 0;

    if(bd)
        off_w = ALIGN_WITH_OFF_W(bd->free, alignment, align_off);

    // If we don't have a block of pinned objects yet, or the current
    // one isn't large enough to hold the new object, get a new one.
    if (bd == NULL || (bd->free + off_w + n) > (bd->start + BLOCK_SIZE_W)) {

        // stash the old block on cap->pinned_object_blocks.  On the
        // next GC cycle these objects will be moved to
        // g0->large_objects.
        if (bd != NULL) {
            // add it to the allocation stats when the block is full
            finishedNurseryBlock(cap, bd);
            dbl_link_onto(bd, &cap->pinned_object_blocks);
        }

        // We need to find another block.  We could just allocate one,
        // but that means taking a global lock and we really want to
        // avoid that (benchmarks that allocate a lot of pinned
        // objects scale really badly if we do this).
        //
        // So first, we try taking the next block from the nursery, in
        // the same way as allocate().
        bd = cap->r.rCurrentNursery->link;
        if (bd == NULL) {
            // The nursery is empty: allocate a fresh block (we can't fail
            // here).
            ACQUIRE_SM_LOCK;
            bd = allocBlockOnNode(cap->node);
            RELEASE_SM_LOCK;
            initBdescr(bd, g0, g0);
        } else {
            newNurseryBlock(bd);
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

        off_w = ALIGN_WITH_OFF_W(bd->free, alignment, align_off);
    }

    p = bd->free;

    MEMSET_IF_PROFILING_W(p, 0, off_w);

    n += off_w;
    p += off_w;
    bd->free += n;

    accountAllocation(cap, n);

    return p;
}

/* -----------------------------------------------------------------------------
   Write Barriers
   -------------------------------------------------------------------------- */

/* These write barriers on heavily mutated objects serve two purposes:
 *
 * - Efficient maintenance of the generational invariant: Record whether or not
 *   we have added a particular mutable object to mut_list as they may contain
 *   references to younger generations.
 *
 * - Maintenance of the nonmoving collector's snapshot invariant: Record objects
 *   which are about to no longer be reachable due to mutation.
 *
 * In each case we record whether the object has been added to the mutable list
 * by way of either the info pointer or a dedicated "dirty" flag. The GC will
 * clear this flag and remove the object from mut_list (or rather, not re-add it)
 * to if it finds the object contains no references into any younger generation.
 *
 * Note that all dirty objects will be marked as clean during preparation for a
 * concurrent collection. Consequently, we can use the dirtiness flag to determine
 * whether or not we need to add overwritten pointers to the update remembered
 * set (since we need only write the value prior to the first update to maintain
 * the snapshot invariant).
 */

/*
   This is the write barrier for MUT_VARs, a.k.a. IORefs.  A
   MUT_VAR_CLEAN object is not on the mutable list; a MUT_VAR_DIRTY
   is.  When written to, a MUT_VAR_CLEAN turns into a MUT_VAR_DIRTY
   and is put on the mutable list.
*/
void
dirty_MUT_VAR(StgRegTable *reg, StgMutVar *mvar, StgClosure *old)
{
    Capability *cap = regTableToCapability(reg);
    // No barrier required here as no other heap object fields are read. See
    // note [Heap memory barriers] in SMP.h.
    if (mvar->header.info == &stg_MUT_VAR_CLEAN_info) {
        mvar->header.info = &stg_MUT_VAR_DIRTY_info;
        recordClosureMutated(cap, (StgClosure *) mvar);
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            // See Note [Dirty flags in the non-moving collector] in NonMoving.c
            updateRemembSetPushClosure_(reg, old);
        }
    }
}

/*
 * This is the write barrier for TVARs.
 * old is the pointer that we overwrote, which is required by the concurrent
 * garbage collector. Note that we, while StgTVars contain multiple pointers,
 * only overwrite one per dirty_TVAR call so we only need to take one old
 * pointer argument.
 */
void
dirty_TVAR(Capability *cap, StgTVar *p,
           StgClosure *old)
{
    // No barrier required here as no other heap object fields are read. See
    // note [Heap memory barriers] in SMP.h.
    if (p->header.info == &stg_TVAR_CLEAN_info) {
        p->header.info = &stg_TVAR_DIRTY_info;
        recordClosureMutated(cap,(StgClosure*)p);
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            // See Note [Dirty flags in the non-moving collector] in NonMoving.c
            updateRemembSetPushClosure(cap, old);
        }
    }
}

// Setting a TSO's link field with a write barrier.
// It is *not* necessary to call this function when
//    * setting the link field to END_TSO_QUEUE
//    * setting the link field of the currently running TSO, as it
//      will already be dirty.
void
setTSOLink (Capability *cap, StgTSO *tso, StgTSO *target)
{
    if (tso->dirty == 0) {
        tso->dirty = 1;
        recordClosureMutated(cap,(StgClosure*)tso);
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            updateRemembSetPushClosure(cap, (StgClosure *) tso->_link);
        }
    }
    tso->_link = target;
}

void
setTSOPrev (Capability *cap, StgTSO *tso, StgTSO *target)
{
    if (tso->dirty == 0) {
        tso->dirty = 1;
        recordClosureMutated(cap,(StgClosure*)tso);
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            updateRemembSetPushClosure(cap, (StgClosure *) tso->block_info.prev);
        }
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

    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushTSO(cap, tso);
    }
}

void
dirty_STACK (Capability *cap, StgStack *stack)
{
    // First push to upd_rem_set before we set stack->dirty since we
    // the nonmoving collector may already be marking the stack.
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushStack(cap, stack);
    }

    if (! (stack->dirty & STACK_DIRTY)) {
        stack->dirty = STACK_DIRTY;
        recordClosureMutated(cap,(StgClosure*)stack);
    }

}

/*
 * This is the concurrent collector's write barrier for MVARs. In the other
 * write barriers above this is folded into the dirty_* functions.  However, in
 * the case of MVars we need to separate the acts of adding the MVar to the
 * mutable list and adding its fields to the update remembered set.
 *
 * Specifically, the wakeup loop in stg_putMVarzh wants to freely mutate the
 * pointers of the MVar but needs to keep its lock, meaning we can't yet add it
 * to the mutable list lest the assertion checking for clean MVars on the
 * mutable list would fail.
 */
void
update_MVAR(StgRegTable *reg, StgClosure *p, StgClosure *old_val)
{
    Capability *cap = regTableToCapability(reg);
    IF_NONMOVING_WRITE_BARRIER_ENABLED {
        // See Note [Dirty flags in the non-moving collector] in NonMoving.c
        StgMVar *mvar = (StgMVar *) p;
        updateRemembSetPushClosure(cap, old_val);
        updateRemembSetPushClosure(cap, (StgClosure *) mvar->head);
        updateRemembSetPushClosure(cap, (StgClosure *) mvar->tail);
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
dirty_MVAR(StgRegTable *reg, StgClosure *p, StgClosure *old_val)
{
    Capability *cap = regTableToCapability(reg);
    update_MVAR(reg, p, old_val);
    recordClosureMutated(cap, p);
}

/* -----------------------------------------------------------------------------
 * Stats and stuff
 * -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * Note [allocation accounting]
 *
 *   - When cap->r.rCurrentNusery moves to a new block in the nursery,
 *     we add the size of the used portion of the previous block to
 *     cap->total_allocated. (see finishedNurseryBlock())
 *
 *   - When we start a GC, the allocated portion of CurrentNursery and
 *     CurrentAlloc are added to cap->total_allocated. (see
 *     updateNurseriesStats())
 *
 * -------------------------------------------------------------------------- */

//
// Calculate the total allocated memory since the start of the
// program.  Also emits events reporting the per-cap allocation
// totals.
//
StgWord
calcTotalAllocated (void)
{
    W_ tot_alloc = 0;
    W_ n;

    for (n = 0; n < n_capabilities; n++) {
        tot_alloc += capabilities[n]->total_allocated;

        traceEventHeapAllocated(capabilities[n],
                                CAPSET_HEAP_DEFAULT,
                                capabilities[n]->total_allocated * sizeof(W_));
    }

    return tot_alloc;
}

//
// Update the per-cap total_allocated numbers with an approximation of
// the amount of memory used in each cap's nursery.
//
void
updateNurseriesStats (void)
{
    uint32_t i;
    bdescr *bd;

    for (i = 0; i < n_capabilities; i++) {
        // The current nursery block and the current allocate block have not
        // yet been accounted for in cap->total_allocated, so we add them here.
        bd = capabilities[i]->r.rCurrentNursery;
        if (bd) finishedNurseryBlock(capabilities[i], bd);
        bd = capabilities[i]->r.rCurrentAlloc;
        if (bd) finishedNurseryBlock(capabilities[i], bd);
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
    return (gen->live_estimate ? gen->live_estimate : gen->n_words) +
        gen->n_large_words + gen->n_compact_blocks * BLOCK_SIZE_W;
}

W_ genLiveBlocks (generation *gen)
{
    return gen->n_blocks + gen->n_large_blocks + gen->n_compact_blocks;
}

W_ gcThreadLiveWords (uint32_t i, uint32_t g)
{
    W_ a, b, c;

    a = countOccupied(gc_threads[i]->gens[g].todo_bd);
    b = gc_threads[i]->gens[g].n_part_words;
    c = gc_threads[i]->gens[g].n_scavd_words;

//    debugBelch("cap %d, g%d, %ld %ld %ld\n", i, g, a, b, c);
    return a + b + c;
}

W_ gcThreadLiveBlocks (uint32_t i, uint32_t g)
{
    W_ blocks;

    blocks  = countBlocks(gc_threads[i]->gens[g].todo_bd);
    blocks += gc_threads[i]->gens[g].n_part_blocks;
    blocks += gc_threads[i]->gens[g].n_scavd_blocks;

    return blocks;
}

/* Determine which generation will be collected next, and approximate
 * the maximum amount of memory that will be required to do the GC,
 * taking into account data that will be copied, and the space needed
 * to store bitmaps and the mark stack.  Note: blocks_needed does not
 * include the blocks in the nursery.
 *
 * Assume: all data currently live will remain live.  Generations
 * that will be collected next time will therefore need twice as many
 * blocks since all the data will be copied.
 */
extern W_
calcNeeded (bool force_major, memcount *blocks_needed)
{
    W_ needed = 0;
    uint32_t N;

    if (force_major) {
        N = RtsFlags.GcFlags.generations - 1;
    } else {
        N = 0;
    }

    for (uint32_t g = 0; g < RtsFlags.GcFlags.generations; g++) {
        generation *gen = &generations[g];

        W_ blocks = gen->live_estimate ? (gen->live_estimate / BLOCK_SIZE_W) : gen->n_blocks;
        blocks += gen->n_large_blocks
                + gen->n_compact_blocks;

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
            if (gen->compact || (RtsFlags.GcFlags.useNonmoving && gen == oldest_gen)) {
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

StgWord calcTotalLargeObjectsW (void)
{
    uint32_t g;
    StgWord totalW = 0;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        totalW += generations[g].n_large_words;
    }
    return totalW;
}

StgWord calcTotalCompactW (void)
{
    uint32_t g;
    StgWord totalW = 0;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        totalW += generations[g].n_compact_blocks * BLOCK_SIZE_W;
    }
    return totalW;
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

#if (defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)) && defined(ios_HOST_OS)
#include <libkern/OSCacheControl.h>
#endif

/* __builtin___clear_cache is supported since GNU C 4.3.6.
 * We pick 4.4 to simplify condition a bit.
 */
#define GCC_HAS_BUILTIN_CLEAR_CACHE (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4))

#if defined(__clang__)
/* clang defines __clear_cache as a builtin on some platforms.
 * For example on armv7-linux-androideabi. The type slightly
 * differs from gcc.
 */
extern void __clear_cache(void * begin, void * end);
#elif defined(__GNUC__) && !GCC_HAS_BUILTIN_CLEAR_CACHE
/* __clear_cache is a libgcc function.
 * It existed before __builtin___clear_cache was introduced.
 * See #8562.
 */
extern void __clear_cache(char * begin, char * end);
#endif /* __GNUC__ */

/* On ARM and other platforms, we need to flush the cache after
   writing code into memory, so the processor reliably sees it. */
void flushExec (W_ len, AdjustorExecutable exec_addr)
{
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
  /* x86 doesn't need to do anything, so just suppress some warnings. */
  (void)len;
  (void)exec_addr;
#elif (defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)) && defined(ios_HOST_OS)
  /* On iOS we need to use the special 'sys_icache_invalidate' call. */
  sys_icache_invalidate(exec_addr, len);
#elif defined(__clang__)
  unsigned char* begin = (unsigned char*)exec_addr;
  unsigned char* end   = begin + len;
# if __has_builtin(__builtin___clear_cache)
  __builtin___clear_cache((void*)begin, (void*)end);
# else
  __clear_cache((void*)begin, (void*)end);
# endif
#elif defined(__GNUC__)
  /* For all other platforms, fall back to a libgcc builtin. */
  unsigned char* begin = (unsigned char*)exec_addr;
  unsigned char* end   = begin + len;
# if GCC_HAS_BUILTIN_CLEAR_CACHE
  __builtin___clear_cache((void*)begin, (void*)end);
# else
  /* For all other platforms, fall back to a libgcc builtin. */
  __clear_cache((void*)begin, (void*)end);
# endif
#else
#error Missing support to flush the instruction cache
#endif
}

#if defined(linux_HOST_OS) || defined(netbsd_HOST_OS)

// On Linux we need to use libffi for allocating executable memory,
// because it knows how to work around the restrictions put in place
// by SELinux. The same goes for NetBSD where it is prohibited to
// mark a page mapping both writable and executable at the same time.

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
        setExecutable(bd->start, bd->blocks * BLOCK_SIZE, true);
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
            setExecutable(bd->start, bd->blocks * BLOCK_SIZE, false);
            freeGroup(bd);
        } else {
            bd->free = bd->start;
        }
    }

    RELEASE_SM_LOCK
}

#endif /* switch(HOST_OS) */

#if defined(DEBUG)

// handy function for use in gdb, because Bdescr() is inlined.
extern bdescr *_bdescr (StgPtr p);

bdescr *
_bdescr (StgPtr p)
{
    return Bdescr(p);
}

#endif
