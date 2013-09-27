/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: utilities
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
#include "Storage.h"
#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"
#include "GCUtils.h"
#include "Printer.h"
#include "Trace.h"
#ifdef THREADED_RTS
#include "WSDeque.h"
#endif

#ifdef THREADED_RTS
SpinLock gc_alloc_block_sync;
#endif

bdescr *
allocBlock_sync(void)
{
    bdescr *bd;
    ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
    bd = allocBlock();
    RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
    return bd;
}

static bdescr *
allocGroup_sync(nat n)
{
    bdescr *bd;
    ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
    bd = allocGroup(n);
    RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
    return bd;
}


#if 0
static void
allocBlocks_sync(nat n, bdescr **hd, bdescr **tl, 
                 nat gen_no, step *stp,
                 StgWord32 flags)
{
    bdescr *bd;
    nat i;
    ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
    bd = allocGroup(n);
    for (i = 0; i < n; i++) {
        bd[i].blocks = 1;
        bd[i].gen_no = gen_no;
        bd[i].step = stp;
        bd[i].flags = flags;
        bd[i].link = &bd[i+1];
        bd[i].u.scan = bd[i].free = bd[i].start;
    }
    *hd = bd;
    *tl = &bd[n-1];
    RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
}
#endif

void
freeChain_sync(bdescr *bd)
{
    ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
    freeChain(bd);
    RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
}

/* -----------------------------------------------------------------------------
   Workspace utilities
   -------------------------------------------------------------------------- */

bdescr *
grab_local_todo_block (gen_workspace *ws)
{
    bdescr *bd;

    bd = ws->todo_overflow;
    if (bd != NULL)
    {
        ws->todo_overflow = bd->link;
        bd->link = NULL;
        ws->n_todo_overflow--;
	return bd;
    }

    bd = popWSDeque(ws->todo_q);
    if (bd != NULL)
    {
	ASSERT(bd->link == NULL);
	return bd;
    }

    return NULL;
}

#if defined(THREADED_RTS)
bdescr *
steal_todo_block (nat g)
{
    nat n;
    bdescr *bd;

    // look for work to steal
    for (n = 0; n < n_gc_threads; n++) {
        if (n == gct->thread_index) continue;
        bd = stealWSDeque(gc_threads[n]->gens[g].todo_q);
        if (bd) {
            return bd;
        }
    }
    return NULL;
}
#endif

void
push_scanned_block (bdescr *bd, gen_workspace *ws)
{
    ASSERT(bd != NULL);
    ASSERT(bd->link == NULL);
    ASSERT(bd->gen == ws->gen);
    ASSERT(bd->u.scan == bd->free);

    if (bd->start + bd->blocks * BLOCK_SIZE_W - bd->free > WORK_UNIT_WORDS)
    {
        // a partially full block: put it on the part_list list.
        bd->link = ws->part_list;
        ws->part_list = bd;
        ws->n_part_blocks += bd->blocks;
        IF_DEBUG(sanity, 
                 ASSERT(countBlocks(ws->part_list) == ws->n_part_blocks));
    }
    else
    {
        // put the scan block on the ws->scavd_list.
        bd->link = ws->scavd_list;
        ws->scavd_list = bd;
        ws->n_scavd_blocks += bd->blocks;
        IF_DEBUG(sanity, 
                 ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks));
    }
}

StgPtr
todo_block_full (nat size, gen_workspace *ws)
{
    rtsBool urgent_to_push, can_extend;
    StgPtr p;
    bdescr *bd;

    // todo_free has been pre-incremented by Evac.c:alloc_for_copy().  We
    // are expected to leave it bumped when we've finished here.
    ws->todo_free -= size;

    bd = ws->todo_bd;

    ASSERT(bd != NULL);
    ASSERT(bd->link == NULL);
    ASSERT(bd->gen == ws->gen);

    // We intentionally set ws->todo_lim lower than the full size of
    // the block, so that we can push out some work to the global list
    // and get the parallel threads working as soon as possible.
    //
    // So when ws->todo_lim is reached, we end up here and have to
    // decide whether it's worth pushing out the work we have or not.
    // If we have enough room in the block to evacuate the current
    // object, and it's not urgent to push this work, then we just
    // extend the limit and keep going.  Where "urgent" is defined as:
    // the global pool is empty, and there's enough work in this block
    // to make it worth pushing.
    //
    urgent_to_push =
        looksEmptyWSDeque(ws->todo_q) &&
        (ws->todo_free - bd->u.scan >= WORK_UNIT_WORDS / 2);

    // We can extend the limit for the current block if there's enough
    // room for the current object, *and* we're not into the second or
    // subsequent block of a large block.  The second condition occurs
    // when we evacuate an object that is larger than a block.  In
    // that case, alloc_todo_block() sets todo_lim to be exactly the
    // size of the large object, and we don't evacuate any more
    // objects into this block.  The reason is that the rest of the GC
    // is not set up to handle objects that start in the second or
    // later blocks of a group.  We just about manage this in the
    // nursery (see scheduleHandleHeapOverflow()) so evacuate() can
    // handle this, but other parts of the GC can't.  We could
    // probably fix this, but it's a rare case anyway.
    //
    can_extend =
        ws->todo_free + size <= bd->start + bd->blocks * BLOCK_SIZE_W
        && ws->todo_free < ws->todo_bd->start + BLOCK_SIZE_W;

    if (!urgent_to_push && can_extend)
    {
        ws->todo_lim = stg_min(bd->start + bd->blocks * BLOCK_SIZE_W,
                               ws->todo_lim + stg_max(WORK_UNIT_WORDS,size));
        debugTrace(DEBUG_gc, "increasing limit for %p to %p",
                   bd->start, ws->todo_lim);
        p = ws->todo_free;
        ws->todo_free += size;

        return p;
    }
    
    gct->copied += ws->todo_free - bd->free;
    bd->free = ws->todo_free;

    ASSERT(bd->u.scan >= bd->start && bd->u.scan <= bd->free);

    // If this block is not the scan block, we want to push it out and
    // make room for a new todo block.
    if (bd != gct->scan_bd)
    {
        // If this block does not have enough space to allocate the
        // current object, but it also doesn't have any work to push, then 
        // push it on to the scanned list.
        if (bd->u.scan == bd->free)
        {
            if (bd->free == bd->start) {
                // Normally the block would not be empty, because then
                // there would be enough room to copy the current
                // object.  However, if the object we're copying is
                // larger than a block, then we might have an empty
                // block here.
                freeGroup(bd);
            } else {
                push_scanned_block(bd, ws);
            }
        }
        // Otherwise, push this block out to the global list.
        else 
        {
            DEBUG_ONLY( generation *gen );
            DEBUG_ONLY( gen = ws->gen );
            debugTrace(DEBUG_gc, "push todo block %p (%ld words), step %d, todo_q: %ld", 
                  bd->start, (unsigned long)(bd->free - bd->u.scan),
                  gen->no, dequeElements(ws->todo_q));

            if (!pushWSDeque(ws->todo_q, bd)) {
                bd->link = ws->todo_overflow;
                ws->todo_overflow = bd;
                ws->n_todo_overflow++;
            }
        }
    }

    ws->todo_bd   = NULL;
    ws->todo_free = NULL;
    ws->todo_lim  = NULL;

    alloc_todo_block(ws, size);

    p = ws->todo_free;
    ws->todo_free += size;
    return p;
}

StgPtr
alloc_todo_block (gen_workspace *ws, nat size)
{
    bdescr *bd/*, *hd, *tl */;

    // Grab a part block if we have one, and it has enough room
    bd = ws->part_list;
    if (bd != NULL &&
        bd->start + bd->blocks * BLOCK_SIZE_W - bd->free > (int)size)
    {
        ws->part_list = bd->link;
        ws->n_part_blocks -= bd->blocks;
    }
    else
    {
        // blocks in to-space get the BF_EVACUATED flag.

//        allocBlocks_sync(16, &hd, &tl, 
//                         ws->step->gen_no, ws->step, BF_EVACUATED);
//
//        tl->link = ws->part_list;
//        ws->part_list = hd->link;
//        ws->n_part_blocks += 15;
//
//        bd = hd;

        if (size > BLOCK_SIZE_W) {
            bd = allocGroup_sync((W_)BLOCK_ROUND_UP(size*sizeof(W_))
                                 / BLOCK_SIZE);
        } else {
            bd = allocBlock_sync();
        }
        initBdescr(bd, ws->gen, ws->gen->to);
        bd->flags = BF_EVACUATED;
        bd->u.scan = bd->free = bd->start;
    }

    bd->link = NULL;

    ws->todo_bd = bd;
    ws->todo_free = bd->free;
    ws->todo_lim  = stg_min(bd->start + bd->blocks * BLOCK_SIZE_W,
                            bd->free + stg_max(WORK_UNIT_WORDS,size));

    debugTrace(DEBUG_gc, "alloc new todo block %p for gen  %d", 
               bd->free, ws->gen->no);

    return ws->todo_free;
}

/* -----------------------------------------------------------------------------
 * Debugging
 * -------------------------------------------------------------------------- */

#if DEBUG
void
printMutableList(bdescr *bd)
{
    StgPtr p;

    debugBelch("mutable list %p: ", bd);

    for (; bd != NULL; bd = bd->link) {
	for (p = bd->start; p < bd->free; p++) {
	    debugBelch("%p (%s), ", (void *)*p, info_type((StgClosure *)*p));
	}
    }
    debugBelch("\n");
}
#endif /* DEBUG */
