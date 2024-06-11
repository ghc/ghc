/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Sweep phase
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "NonMovingSweep.h"
#include "NonMoving.h"
#include "NonMovingMark.h" // for nonmovingIsAlive
#include "Capability.h"
#include "GCThread.h" // for GCUtils.h
#include "GCUtils.h"
#include "Storage.h"
#include "Trace.h"
#include "StableName.h"
#include "CNF.h" // compactFree

// On which list should a particular segment be placed?
enum SweepResult {
    SEGMENT_FREE,     // segment is empty: place on free list
    SEGMENT_PARTIAL,  // segment is partially filled: place on active list
    SEGMENT_FILLED    // segment is full: place on filled list
};

// Determine which list a marked segment should be placed on and initialize
// next_free indices as appropriate. Additionally, we must clear the mark
// bitmap entries associated with swept blocks.
GNUC_ATTR_HOT static enum SweepResult
nonmovingSweepSegment(struct NonmovingSegment *seg)
{
    ASSERT_SEGMENT_STATE(seg, FILLED_SWEEPING);
    const nonmoving_block_idx blk_cnt = nonmovingSegmentBlockCount(seg);
    bool found_free = false;
    bool found_live = false;

    for (nonmoving_block_idx i = 0; i < blk_cnt; ++i)
    {
        if (seg->bitmap[i] == nonmovingMarkEpoch) {
            found_live = true;
        } else {
            seg->bitmap[i] = 0;
            if (!found_free) {
                // This is the first free block we've found; set next_free,
                // next_free_snap, and the scan pointer.
                found_free = true;
                seg->next_free = i;
                nonmovingSegmentInfo(seg)->next_free_snap = i;
                Bdescr((P_)seg)->u.scan = (P_)nonmovingSegmentGetBlock(seg, i);
            }
        }

        if (found_free && found_live) {
            // zero the remaining dead objects' mark bits
            for (; i < nonmovingSegmentBlockCount(seg); ++i) {
                if (seg->bitmap[i] != nonmovingMarkEpoch) {
                    seg->bitmap[i] = 0;
                }
            }
            return SEGMENT_PARTIAL;
        }
    }

    if (found_live) {
        return SEGMENT_FILLED;
    } else {
        ASSERT(seg->next_free == 0);
        ASSERT(nonmovingSegmentInfo(seg)->next_free_snap == 0);
        return SEGMENT_FREE;
    }
}

#if defined(DEBUG)

void nonmovingGcCafs(void)
{
    uint32_t i = 0;
    StgIndStatic *next;

    for (StgIndStatic *caf = debug_caf_list_snapshot;
         caf != (StgIndStatic*) END_OF_CAF_LIST;
         caf = next)
    {
        next = (StgIndStatic*)caf->saved_info;

        const StgInfoTable *info = get_itbl((StgClosure*)caf);
        ASSERT(info->type == IND_STATIC);

        StgWord flag = ((StgWord) caf->static_link) & STATIC_BITS;
        if (flag != 0 && flag != static_flag) {
            debugTrace(DEBUG_gccafs, "CAF gc'd at 0x%p", caf);
            SET_INFO((StgClosure*)caf, &stg_GCD_CAF_info); // stub it
        } else {
            // CAF is alive, move it back to the debug_caf_list
            ++i;
            debugTrace(DEBUG_gccafs, "CAF alive at 0x%p", caf);
            ACQUIRE_SM_LOCK; // debug_caf_list is global, locked by sm_mutex
            caf->saved_info = (const StgInfoTable*)debug_caf_list;
            debug_caf_list = caf;
            RELEASE_SM_LOCK;
        }
    }

    debugTrace(DEBUG_gccafs, "%d CAFs live", i);
    debug_caf_list_snapshot = (StgIndStatic*)END_OF_CAF_LIST;
}

#endif

void
nonmovingClearSegment(struct NonmovingSegment* seg)
{
    size_t end = ((size_t)seg) + NONMOVING_SEGMENT_SIZE;
    memset(&seg->bitmap, 0, end - (size_t)&seg->bitmap);
}

void
nonmovingClearSegmentFreeBlocks(struct NonmovingSegment* seg)
{
    unsigned int block_size = nonmovingSegmentBlockSize(seg);
    for (unsigned int p_idx = 0; p_idx < nonmovingSegmentBlockCount(seg); ++p_idx) {
        // N.B. nonmovingSweepSegment helpfully clears the bitmap entries of
        // dead blocks
        if (nonmovingGetMark(seg, p_idx) == 0) {
            memset(nonmovingSegmentGetBlock(seg, p_idx), 0, block_size);
        }
    }
}

GNUC_ATTR_HOT void nonmovingSweep(void)
{
    while (nonmovingHeap.sweep_list) {
        struct NonmovingSegment *seg = nonmovingHeap.sweep_list;

        // Pushing the segment to one of the free/active/filled segments
        // updates the link field, so update sweep_list here
        nonmovingHeap.sweep_list = seg->link;

        enum SweepResult ret = nonmovingSweepSegment(seg);

        switch (ret) {
        case SEGMENT_FREE:
            IF_DEBUG(sanity, nonmovingClearSegment(seg));
            nonmovingPushFreeSegment(seg);
            break;
        case SEGMENT_PARTIAL:
            IF_DEBUG(sanity, nonmovingClearSegmentFreeBlocks(seg));
            nonmovingPushActiveSegment(seg);
            break;
        case SEGMENT_FILLED:
            nonmovingPushFilledSegment(seg);
            break;
        default:
            barf("nonmovingSweep: weird sweep return: %d\n", ret);
        }
    }
}

/* Must a closure remain on the mutable list?
 *
 * A closure must remain if any of the following applies:
 *
 *  1. it contains references to a younger generation
 *  2. it's a mutable closure (e.g. mutable array or MUT_PRIM)
 */
static bool is_closure_clean(StgClosure *p)
{
    const StgInfoTable *info = get_itbl(p);

#define CLEAN(ptr) (!HEAP_ALLOCED((StgClosure*) ptr) || Bdescr((StgPtr) ptr)->gen == oldest_gen)

    switch (info->type) {
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = ((StgMVar *)p);
        if (!CLEAN(mvar->head)) goto dirty_MVAR;
        if (!CLEAN(mvar->tail)) goto dirty_MVAR;
        if (!CLEAN(mvar->value)) goto dirty_MVAR;
        mvar->header.info = &stg_MVAR_CLEAN_info;
        return true;

dirty_MVAR:
        mvar->header.info = &stg_MVAR_DIRTY_info;
        return false;
    }

    case TVAR:
    {
        StgTVar *tvar = ((StgTVar *)p);
        if (!CLEAN(tvar->current_value)) goto dirty_TVAR;
        if (!CLEAN(tvar->first_watch_queue_entry)) goto dirty_TVAR;
        tvar->header.info = &stg_TVAR_CLEAN_info;
        return true;

dirty_TVAR:
        tvar->header.info = &stg_TVAR_DIRTY_info;
        return false;
    }

    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    {
        StgPtr end = (StgPtr)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (StgPtr q = (StgPtr)((StgThunk *)p)->payload; q < end; q++) {
            if (!CLEAN(*q)) return false;
        }
        return true;
    }

    case FUN:
    case FUN_1_0:                       // hardly worth specialising these guys
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case PRIM:
    {
        StgPtr end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (StgPtr q = (StgPtr)((StgClosure *)p)->payload; q < end; q++) {
            if (!CLEAN(*q)) return false;
        }
        return true;
    }

    case WEAK:
        return false; // TODO

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        if (!CLEAN(((StgMutVar *)p)->var)) {
            p->header.info = &stg_MUT_VAR_DIRTY_info;
            return false;
        } else {
            p->header.info = &stg_MUT_VAR_CLEAN_info;
            return true;
        }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        if (!CLEAN(bq->bh)) goto dirty_BLOCKING_QUEUE;
        if (!CLEAN(bq->owner)) goto dirty_BLOCKING_QUEUE;
        if (!CLEAN(bq->queue)) goto dirty_BLOCKING_QUEUE;
        if (!CLEAN(bq->link)) goto dirty_BLOCKING_QUEUE;
        bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
        return true;

dirty_BLOCKING_QUEUE:
        bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
        return false;
    }

    case THUNK_SELECTOR:
        return CLEAN(((StgSelector *) p)->selectee);

    case ARR_WORDS:
        return true;

    default:
        // TODO: the rest
        return false;
    }
#undef CLEAN
}

/* N.B. This happens during the pause so we own all capabilities. */
void nonmovingSweepMutLists(void)
{
    for (uint32_t n = 0; n < getNumCapabilities(); n++) {
        Capability *cap = getCapability(n);
        bdescr *old_mut_list = cap->mut_lists[oldest_gen->no];
        cap->mut_lists[oldest_gen->no] = allocBlockOnNode_lock(cap->node);
        for (bdescr *bd = old_mut_list; bd; bd = bd->link) {
            for (StgPtr p = bdescr_start(bd); p < bd->free; p++) {
                StgClosure **q = (StgClosure**)p;
                ASSERT(Bdescr((StgPtr) *q)->gen == oldest_gen);
                if (nonmovingIsAlive(*q) && !is_closure_clean(*q)) {
                    recordMutableCap(*q, cap, oldest_gen->no);
                }
            }
        }
        freeChain_lock(old_mut_list);
    }
}

/* A variant of freeChain_lock that will only hold the lock for at most max_dur
 * freed blocks to ensure that we don't starve other lock users (e.g. the
 * mutator).
 */
static void freeChain_lock_max(bdescr *bd, int max_dur)
{
  ACQUIRE_SM_LOCK;
  bdescr *next_bd;
  int i = 0;
  while (bd != NULL) {
    next_bd = bd->link;
    freeGroup(bd);
    bd = next_bd;
    if (i == max_dur) {
#if defined(THREADED_RTS)
        RELEASE_SM_LOCK;
        yieldThread();
        ACQUIRE_SM_LOCK;
#endif
        i = 0;
    }
    i++;
  }
  RELEASE_SM_LOCK;
}

void nonmovingSweepLargeObjects(void)
{
    freeChain_lock_max(nonmoving_large_objects, 10000);
    nonmoving_large_objects = nonmoving_marked_large_objects;
    n_nonmoving_large_blocks = n_nonmoving_marked_large_blocks;
    nonmoving_marked_large_objects = NULL;
    n_nonmoving_marked_large_blocks = 0;
}

void nonmovingSweepCompactObjects(void)
{
    bdescr *next;
    ACQUIRE_SM_LOCK;
    for (bdescr *bd = nonmoving_compact_objects; bd; bd = next) {
        next = bd->link;
        compactFree(((StgCompactNFDataBlock*) bdescr_start(bd))->owner);
    }
    RELEASE_SM_LOCK;
    nonmoving_compact_objects = nonmoving_marked_compact_objects;
    n_nonmoving_compact_blocks = n_nonmoving_marked_compact_blocks;
    nonmoving_marked_compact_objects = NULL;
    n_nonmoving_marked_compact_blocks = 0;
}

// Helper for nonmovingSweepStableNameTable. Essentially nonmovingIsAlive,
// but works when the object died in moving heap, see
// nonmovingSweepStableNameTable
static bool is_alive(StgClosure *p)
{
    if (!HEAP_ALLOCED_GC(p)) {
        return true;
    }

    if (nonmovingClosureBeingSwept(p)) {
        return nonmovingIsAlive(p);
    } else {
        // We don't want to sweep any stable names which weren't in the
        // set of segments that we swept.
        // See Note [Sweeping stable names in the concurrent collector]
        return true;
    }
}

void nonmovingSweepStableNameTable(void)
{
    // See comments in gcStableTables

    /* Note [Sweeping stable names in the concurrent collector]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * When collecting concurrently we need to take care to avoid freeing
     * stable names the we didn't sweep this collection cycle. For instance,
     * consider the following situation:
     *
     *  1. We take a snapshot and start collection
     *  2. A mutator allocates a new object, then makes a stable name for it
     *  3. The mutator performs a minor GC and promotes the new object to the nonmoving heap
     *  4. The GC thread gets to the sweep phase and, when traversing the stable
     *     name table, finds the new object unmarked. It then assumes that the
     *     object is dead and removes the stable name from the stable name table.
     *
     */

    // FIXME: We can't use nonmovingIsAlive here without first using isAlive:
    // a stable name can die during moving heap collection and we can't use
    // nonmovingIsAlive on those objects. Inefficient.

    stableNameLock();
    FOR_EACH_STABLE_NAME(
        p, {
            if (p->sn_obj != NULL) {
                if (!is_alive((StgClosure*)p->sn_obj)) {
                    p->sn_obj = NULL; // Just to make an assertion happy
                    freeSnEntry(p);
                } else if (p->addr != NULL) {
                    if (!is_alive((StgClosure*)p->addr)) {
                        p->addr = NULL;
                    }
                }
            }
        });
    stableNameUnlock();
}
