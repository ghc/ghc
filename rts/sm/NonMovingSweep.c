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

// On which list should a particular segment be placed?
enum SweepResult {
    SEGMENT_FREE,     // segment is empty: place on free list
    SEGMENT_PARTIAL,  // segment is partially filled: place on active list
    SEGMENT_FILLED    // segment is full: place on filled list
};

// Determine which list a marked segment should be placed on and initialize
// next_free indices as appropriate.
GNUC_ATTR_HOT static enum SweepResult
nonmovingSweepSegment(struct NonmovingSegment *seg)
{
    bool found_free = false;
    bool found_live = false;

    for (nonmoving_block_idx i = 0;
         i < nonmovingSegmentBlockCount(seg);
         ++i)
    {
        if (seg->bitmap[i] == nonmovingMarkEpoch) {
            found_live = true;
        } else if (!found_free) {
            found_free = true;
            seg->next_free = i;
            seg->next_free_snap = i;
            Bdescr((P_)seg)->u.scan = (P_)nonmovingSegmentGetBlock(seg, i);
            seg->bitmap[i] = 0;
        } else {
            seg->bitmap[i] = 0;
        }

        if (found_free && found_live) {
            // zero the remaining dead object's mark bits
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
        ASSERT(seg->next_free_snap == 0);
        return SEGMENT_FREE;
    }
}

#if defined(DEBUG)

void nonmovingGcCafs()
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

static void
clear_segment(struct NonmovingSegment* seg)
{
    size_t end = ((size_t)seg) + NONMOVING_SEGMENT_SIZE;
    memset(&seg->bitmap, 0, end - (size_t)&seg->bitmap);
}

static void
clear_segment_free_blocks(struct NonmovingSegment* seg)
{
    unsigned int block_size = nonmovingSegmentBlockSize(seg);
    for (unsigned int p_idx = 0; p_idx < nonmovingSegmentBlockCount(seg); ++p_idx) {
        // after mark, so bit not set == dead
        if (nonmovingGetMark(seg, p_idx) == 0) {
            memset(nonmovingSegmentGetBlock(seg, p_idx), 0, block_size);
        }
    }
}

#endif

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
            IF_DEBUG(sanity, clear_segment(seg));
            nonmovingPushFreeSegment(seg);
            break;
        case SEGMENT_PARTIAL:
            IF_DEBUG(sanity, clear_segment_free_blocks(seg));
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

/* N.B. This happens during the pause so we own all capabilities. */
void nonmovingSweepMutLists()
{
    for (uint32_t n = 0; n < n_capabilities; n++) {
        Capability *cap = capabilities[n];
        bdescr *old_mut_list = cap->mut_lists[oldest_gen->no];
        cap->mut_lists[oldest_gen->no] = allocBlockOnNode_sync(cap->node);
        for (bdescr *bd = old_mut_list; bd; bd = bd->link) {
            for (StgPtr p = bd->start; p < bd->free; p++) {
                StgClosure **q = (StgClosure**)p;
                if (nonmovingIsAlive(*q)) {
                    recordMutableCap(*q, cap, oldest_gen->no);
                }
            }
        }
        freeChain(old_mut_list);
    }
}

void nonmovingSweepLargeObjects()
{
    freeChain_lock(nonmoving_large_objects);
    nonmoving_large_objects = nonmoving_marked_large_objects;
    n_nonmoving_large_blocks = n_nonmoving_marked_large_blocks;
    nonmoving_marked_large_objects = NULL;
    n_nonmoving_marked_large_blocks = 0;
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

void nonmovingSweepStableNameTable()
{
    // See comments in gcStableTables

    /* Note [Sweeping stable names in the concurrent collector]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     *
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
