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
    bool collected_live_words;
    uint32_t n_active_segs;
    uint32_t n_filled_segs;
    uint32_t n_live_blocks;
    uint32_t n_live_words;
};


struct NonmovingAllocCensus
nonmovingAllocatorCensusWithWords(uint32_t alloc_idx);

struct NonmovingAllocCensus
nonmovingAllocatorCensus(uint32_t alloc_idx);

void nonmovingPrintAllocatorCensus(bool collect_live_words);
void nonmovingTraceAllocatorCensus(void);
