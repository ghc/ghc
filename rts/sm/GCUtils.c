/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: utilities
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"
#include "GC.h"
#include "GCUtils.h"

/* -----------------------------------------------------------------------------
   Allocate a new to-space block in the given step.
   -------------------------------------------------------------------------- */

bdescr *
gc_alloc_block(step *stp)
{
    bdescr *bd = allocBlock();
    bd->gen_no = stp->gen_no;
    bd->step = stp;
    bd->link = NULL;

    // blocks in to-space in generations up to and including N
    // get the BF_EVACUATED flag.
    if (stp->gen_no <= N) {
	bd->flags = BF_EVACUATED;
    } else {
	bd->flags = 0;
    }

    // Start a new to-space block, chain it on after the previous one.
    if (stp->hp_bd != NULL) {
	stp->hp_bd->free = stp->hp;
	stp->hp_bd->link = bd;
    }

    stp->hp_bd = bd;
    stp->hp    = bd->start;
    stp->hpLim = stp->hp + BLOCK_SIZE_W;

    stp->n_blocks++;
    new_blocks++;

    return bd;
}

bdescr *
gc_alloc_scavd_block(step *stp)
{
    bdescr *bd = allocBlock();
    bd->gen_no = stp->gen_no;
    bd->step = stp;

    // blocks in to-space in generations up to and including N
    // get the BF_EVACUATED flag.
    if (stp->gen_no <= N) {
	bd->flags = BF_EVACUATED;
    } else {
	bd->flags = 0;
    }

    bd->link = stp->blocks;
    stp->blocks = bd;

    if (stp->scavd_hp != NULL) {
	Bdescr(stp->scavd_hp)->free = stp->scavd_hp;
    }
    stp->scavd_hp    = bd->start;
    stp->scavd_hpLim = stp->scavd_hp + BLOCK_SIZE_W;

    stp->n_blocks++;
    new_scavd_blocks++;

    return bd;
}

