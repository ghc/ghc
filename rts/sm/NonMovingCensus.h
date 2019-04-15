/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Accounting census
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "NonMoving.h"

struct NonmovingAllocCensus {
    uint32_t n_active_segs;
    uint32_t n_filled_segs;
    uint32_t n_live_blocks;
    uint32_t n_live_words;
};


struct NonmovingAllocCensus
nonmovingAllocatorCensusWithWords(struct NonmovingAllocator *alloc);

struct NonmovingAllocCensus
nonmovingAllocatorCensus(struct NonmovingAllocator *alloc);

void nonmovingPrintAllocatorCensus(void);
void nonmovingTraceAllocatorCensus(void);
