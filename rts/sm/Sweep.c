/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2008 
 *
 * Simple mark/sweep, collecting whole blocks.
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "BlockAlloc.h"
#include "Sweep.h"
#include "Trace.h"

void
sweep(generation *gen)
{
    bdescr *bd, *prev, *next;
    nat i;
    W_ freed, resid, fragd, blocks, live;
    
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
