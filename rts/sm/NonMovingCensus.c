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
nonmovingAllocatorCensus_(struct NonmovingAllocator *alloc, bool collect_live_words)
{
    struct NonmovingAllocCensus census = {0, 0, 0, 0};

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

    for (struct NonmovingSegment *seg = alloc->active;
         seg != NULL;
         seg = seg->link)
    {
        census.n_active_segs++;
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

    for (unsigned int cap=0; cap < n_capabilities; cap++)
    {
        struct NonmovingSegment *seg = alloc->current[cap];
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
nonmovingAllocatorCensusWithWords(struct NonmovingAllocator *alloc)
{
    return nonmovingAllocatorCensus_(alloc, true);
}

struct NonmovingAllocCensus
nonmovingAllocatorCensus(struct NonmovingAllocator *alloc)
{
    return nonmovingAllocatorCensus_(alloc, false);
}


void nonmovingPrintAllocatorCensus()
{
    if (!RtsFlags.GcFlags.useNonmoving)
        return;

    for (int i=0; i < NONMOVING_ALLOCA_CNT; i++) {
        struct NonmovingAllocCensus census =
            nonmovingAllocatorCensus(nonmovingHeap.allocators[i]);

        uint32_t blk_size = 1 << (i + NONMOVING_ALLOCA0);
        // We define occupancy as the fraction of space that is used for useful
        // data (that is, live and not slop).
        double occupancy = 100.0 * census.n_live_words * sizeof(W_)
            / (census.n_live_blocks * blk_size);
        if (census.n_live_blocks == 0) occupancy = 100;
        (void) occupancy; // silence warning if !DEBUG
        debugTrace(DEBUG_nonmoving_gc, "Allocator %d (%d bytes - %d bytes): "
                   "%d active segs, %d filled segs, %d live blocks, %d live words "
                   "(%2.1f%% occupancy)",
                   i, 1 << (i + NONMOVING_ALLOCA0 - 1), 1 << (i + NONMOVING_ALLOCA0),
                   census.n_active_segs, census.n_filled_segs, census.n_live_blocks, census.n_live_words,
                   occupancy);
    }
}

void nonmovingTraceAllocatorCensus()
{
#if defined(TRACING)
    if (!RtsFlags.GcFlags.useNonmoving && !TRACE_nonmoving_gc)
        return;

    for (int i=0; i < NONMOVING_ALLOCA_CNT; i++) {
        const struct NonmovingAllocCensus census =
            nonmovingAllocatorCensus(nonmovingHeap.allocators[i]);
        const uint32_t log_blk_size = i + NONMOVING_ALLOCA0;
        traceNonmovingHeapCensus(log_blk_size, &census);
    }
#endif
}
