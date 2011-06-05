/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2008 
 *
 * Simple mark/sweep, collecting whole blocks.
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "BlockAlloc.h"
#include "Sweep.h"
#include "Compact.h"
#include "Trace.h"
#include "GCUtils.h"

void
sweep(generation *gen)
{
    bdescr *bd, *prev, *next;
    nat i;
    nat freed, resid, fragd, blocks, live;
    
    ASSERT(countBlocks(gen->old_blocks) == gen->n_old_blocks);

    live = 0; // estimate of live data in this gen
    freed = 0;
    fragd = 0;
    blocks = 0;
    prev = NULL;
    for (bd = gen->old_blocks; bd != NULL; bd = next)
    {
        next = bd->link;

        if (!(bd->flags & BF_MARKED)) { 
            prev = bd;
            continue;
        }

        blocks++;
        resid = 0;
        for (i = 0; i < BLOCK_SIZE_W / BITS_IN(W_); i++)
        {
            if (bd->u.bitmap[i] != 0) resid++;
        }
        live += resid * BITS_IN(W_);

        if (resid == 0)
        {
            freed++;
            gen->n_old_blocks--;
            if (prev == NULL) {
                gen->old_blocks = next;
            } else {
                prev->link = next;
            }
            freeGroup(bd);
        }
        else
        {
            prev = bd;
            if (resid < (BLOCK_SIZE_W * 3) / (BITS_IN(W_) * 4)) {
                fragd++;
                bd->flags |= BF_FRAGMENTED;
            }

            bd->flags |= BF_SWEPT;
        }
    }

    gen->live_estimate = live;

    debugTrace(DEBUG_gc, "sweeping: %d blocks, %d were copied, %d freed (%d%%), %d are fragmented, live estimate: %ld%%",
          gen->n_old_blocks + freed,
          gen->n_old_blocks - blocks + freed,
          freed,
          blocks == 0 ? 0 : (freed * 100) / blocks,
          fragd, 
          (unsigned long)((blocks - freed) == 0 ? 0 : ((live / BLOCK_SIZE_W) * 100) / (blocks - freed)));

    ASSERT(countBlocks(gen->old_blocks) == gen->n_old_blocks);
}


typedef enum { Empty, LiveLocal, LiveGlobal } IsEmpty;

static IsEmpty
empty (bdescr *bd)
{
    StgPtr p;
    StgWord flag, size;
    IsEmpty is_empty;
    
    is_empty = Empty;
    p = bd->start;
    while (p < bd->free) {
        flag = *p;
        p += 1;
        switch (flag) {
        case 0: // not global
            size = closure_sizeW((StgClosure*)p);
            if (!is_marked(p,bd)) {
                *(p-1) = size+2;
            } else {
                is_empty = LiveLocal;
            }
            p += size;
            break;
        case 1: // global
#ifndef DEBUG
            // in DEBUG, we need to sweep the whole lot, for sanity checking
            return LiveGlobal;
#else
            is_empty = LiveGlobal;
            p += closure_sizeW((StgClosure*)p);
#endif
            break;
        default: // reclaimed free space of size 'flag-2'
            p += flag-2;
            break;
        }
    }

    return is_empty;
}

void
sweepPrimArea (generation *gen)
{
    bdescr *bd, *prev, *next;

    prev = NULL;
    for (bd = gen->prim_blocks; bd != NULL; bd = next)
    {
        next = bd->link;

        // The BF_GLOBAL flag indicates that this block contains one
        // or more global objects.  There's no point in sweeping it,
        // because we can't free the block.
#ifndef DEBUG
        // Not in DEBUG mode: we might need to sanity-check the prim
        // heap, so we need to sweep the free areas.
        if (bd->flags & BF_GLOBAL) {
            prev = bd;
            continue;
        }
#endif

        switch (empty(bd)) {
        case Empty:
            if (prev == NULL) {
                gen->prim_blocks = next;
            } else {
                prev->link = next;
            }
            debugTrace(DEBUG_gc, "sweepPrimArea: free block at %p", bd->start);
            gen->n_prim_blocks -= bd->blocks;
            freeGroup_sync(bd);
            break;

        case LiveGlobal:
            bd->flags |= BF_GLOBAL;
            // fall through
        case LiveLocal:
            prev = bd;
            break;
        }
    }

    IF_DEBUG(sanity, ASSERT(countBlocks(gen->prim_blocks) == gen->n_prim_blocks));
    
}
