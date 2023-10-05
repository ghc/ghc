/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Accounting census
 *
 * This is a simple space accounting census useful for characterising
 * fragmentation in the nonmoving heap.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "NonMoving.h"
#include "Trace.h"
#include "NonMovingCensus.h"

// N.B. This may miss segments in the event of concurrent mutation (e.g. if a
// mutator retires its current segment to the filled list).
//
// all_stopped is whether we can guarantee that all mutators and minor GCs are
// stopped. In this case is safe to look at active and current segments so we can
// also collect statistics on live words.
static struct NonmovingAllocCensus
nonmovingAllocatorCensus_(uint32_t alloc_idx, bool collect_live_words)
{
    struct NonmovingAllocCensus census = {collect_live_words, 0, 0, 0, 0};
    struct NonmovingAllocator *alloc = &nonmovingHeap.allocators[alloc_idx];

    // filled segments
    for (struct NonmovingSegment *seg = alloc->filled;
         seg != NULL;
         seg = seg->link)
    {
        unsigned int n = nonmovingSegmentBlockCount(seg);
        census.n_filled_segs++;
        census.n_live_blocks += n;
        if (collect_live_words) {
            for (unsigned int i=0; i < n; i++) {
                StgClosure *c = (StgClosure *) nonmovingSegmentGetBlock(seg, i);
                census.n_live_words += closure_sizeW(c);
            }
        }
    }

    // active segments
    for (struct NonmovingSegment *seg = alloc->active;
         seg != NULL;
         seg = seg->link)
    {
        census.n_active_segs++;
        unsigned int n = nonmovingSegmentBlockCount(seg);
        for (unsigned int i=0; i < n; i++) {
            if (nonmovingGetMark(seg, i) == nonmovingMarkEpoch) {
                StgClosure *c = (StgClosure *) nonmovingSegmentGetBlock(seg, i);
                if (collect_live_words)
                    census.n_live_words += closure_sizeW(c);
                census.n_live_blocks++;
            }
        }
    }

    // current segments
    for (unsigned int cap_n=0; cap_n < getNumCapabilities(); cap_n++)
    {
        Capability *cap = getCapability(cap_n);
        struct NonmovingSegment *seg = cap->current_segments[alloc_idx];
        unsigned int n = nonmovingSegmentBlockCount(seg);
        for (unsigned int i=0; i < n; i++) {
            if (nonmovingGetMark(seg, i)) {
                StgClosure *c = (StgClosure *) nonmovingSegmentGetBlock(seg, i);
                if (collect_live_words)
                    census.n_live_words += closure_sizeW(c);
                census.n_live_blocks++;
            }
        }
    }
    return census;
}

/* This must not be used when mutators are active since it assumes that
 * all blocks in nonmoving heap are valid closures.
 */
struct NonmovingAllocCensus
nonmovingAllocatorCensusWithWords(uint32_t alloc_idx)
{
    return nonmovingAllocatorCensus_(alloc_idx, true);
}

struct NonmovingAllocCensus
nonmovingAllocatorCensus(uint32_t alloc_idx)
{
    return nonmovingAllocatorCensus_(alloc_idx, false);
}


static void print_alloc_census(int i, struct NonmovingAllocCensus census)
{
    uint32_t blk_size = 1 << (i + NONMOVING_ALLOCA0);
    int sz_min = 1 << (i + NONMOVING_ALLOCA0 - 1);
    int sz_max = 1 << (i + NONMOVING_ALLOCA0);
    (void) sz_min; (void) sz_max;

    if (census.collected_live_words) {
        // We define occupancy as the fraction of space that is used for useful
        // data (that is, live and not slop).
        double occupancy = 100.0 * census.n_live_words * sizeof(W_)
            / (census.n_live_blocks * blk_size);
        if (census.n_live_blocks == 0) occupancy = 100;
        (void) occupancy; // silence warning if !DEBUG
        debugTrace(DEBUG_nonmoving_gc,
                   "Allocator %d (%d bytes - %d bytes): "
                   "%"PRIu32" active segs, %"PRIu32" filled segs, %"PRIu32" live blocks, "
                   "%"PRIu32" live words (%2.1f%% occupancy)",
                   i, sz_min, sz_max,
                   census.n_active_segs,
                   census.n_filled_segs,
                   census.n_live_blocks,
                   census.n_live_words,
                   occupancy);
    } else {
        debugTrace(DEBUG_nonmoving_gc,
                   "Allocator %d (%d bytes - %d bytes): "
                   "%"PRIu32" active segs, %"PRIu32" filled segs, %"PRIu32" live blocks",
                   i, sz_min, sz_max,
                   census.n_active_segs,
                   census.n_filled_segs,
                   census.n_live_blocks);
    }
}

void nonmovingPrintAllocatorCensus(bool collect_live_words)
{
    if (!RtsFlags.GcFlags.useNonmoving)
        return;

    for (int i=0; i < nonmoving_alloca_cnt; i++) {
        struct NonmovingAllocCensus census =
            nonmovingAllocatorCensus_(i, collect_live_words);

        print_alloc_census(i, census);
    }
}

void nonmovingTraceAllocatorCensus(void)
{
#if defined(TRACING)
    if (!RtsFlags.GcFlags.useNonmoving && !TRACE_nonmoving_gc)
        return;

    for (int i=0; i < nonmoving_alloca_cnt; i++) {
        const struct NonmovingAllocCensus census = nonmovingAllocatorCensus(i);
        const uint32_t blk_size = nonmovingHeap.allocators[i].block_size;
        traceNonmovingHeapCensus(blk_size, &census);
    }
#endif
}
