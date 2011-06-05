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
    // GC_LOCAL uses the ordinary locking protocol for the block
    // allocator because it runs concurrently with the mutator.
    if (gct->gc_type == GC_LOCAL) {
        return allocBlock_lock();
    } else {
        bdescr *bd;
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        bd = allocBlock();
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
        return bd;
    }
}

bdescr *
allocGroup_sync(nat n)
{
    // GC_LOCAL uses the ordinary locking protocol for the block
    // allocator because it runs concurrently with the mutator.
    if (gct->gc_type == GC_LOCAL) {
        return allocGroup_lock(n);
    } else {
        bdescr *bd;
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        bd = allocGroup(n);
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
        return bd;
    }
}


#define ALLOCBLOCKS 4
#if ALLOCBLOCKS
static void
allocChain_sync(nat n, bdescr **hd, bdescr **tl,
                 generation *gen, StgWord32 flags)
{
    bdescr *bd;
    nat i;

    if (gct->gc_type == GC_LOCAL) {
        ACQUIRE_SM_LOCK;
    } else {
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
    }

    bd = allocGroup(n);

    // NB. hold the lock while we fiddle with the block group,
    // otherwise we can invalidate invariants of the block allocator.
    // e.g. the block allocator assumes that bd->blocks doesn't change
    // for allocated blocks, so that it can do free list coalescing.
    for (i = 0; i < n; i++) {
        bd[i].blocks = 1;
        initBdescr(&bd[i], gen, gen->to);
        bd[i].flags = flags;
        bd[i].link = &bd[i+1];
        bd[i].u.scan = bd[i].free = bd[i].start;
    }
    *hd = bd;
    *tl = &bd[n-1];

    if (gct->gc_type == GC_LOCAL) {
        RELEASE_SM_LOCK;
    } else {
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
    }
}
#endif

void
freeChain_sync(bdescr *bd)
{
    // GC_LOCAL uses the ordinary locking protocol for the block
    // allocator because it runs concurrently with the mutator.
    if (gct->gc_type == GC_LOCAL) {
        return freeChain_lock(bd);
    } else {
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        freeChain(bd);
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
    }
}

void
freeGroup_sync(bdescr *bd)
{
    // GC_LOCAL uses the ordinary locking protocol for the block
    // allocator because it runs concurrently with the mutator.
    if (gct->gc_type == GC_LOCAL) {
        return freeGroup_lock(bd);
    } else {
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        freeGroup(bd);
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
    }
}

/* -----------------------------------------------------------------------------
   Workspace utilities
   -------------------------------------------------------------------------- */

bdescr *
grab_local_todo_block (gen_workspace *ws)
{
    bdescr *bd;
    generation *gen;

    gen = ws->gen;

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
        if (n == gct->index) continue;
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
    StgPtr p;
    bdescr *bd;

    // todo_free has been pre-incremented by Evac.c:alloc_for_copy().  We
    // are expected to leave it bumped when we've finished here.
    ws->todo_free -= size;

    bd = ws->todo_bd;

    ASSERT(bd != NULL);
    ASSERT(bd->link == NULL);
    ASSERT(bd->gen == ws->gen);

    // If the global list is not empty, or there's not much work in
    // this block to push, and there's enough room in
    // this block to evacuate the current object, then just increase
    // the limit.
    if (!looksEmptyWSDeque(ws->todo_q) || 
        (ws->todo_free - bd->u.scan < WORK_UNIT_WORDS / 2)) {
        if (ws->todo_free + size < bd->start + bd->blocks * BLOCK_SIZE_W) {
            ws->todo_lim = stg_min(bd->start + bd->blocks * BLOCK_SIZE_W,
                                   ws->todo_lim + stg_max(WORK_UNIT_WORDS,size));
            debugTrace(DEBUG_gc, "increasing limit for %p to %p", bd->start, ws->todo_lim);
            p = ws->todo_free;
            ws->todo_free += size;
            return p;
        }
    }
    
    bd->free = ws->todo_free;

    ASSERT(bd->u.scan >= bd->start && bd->u.scan <= bd->free);

    // If this block is not the scan block, we want to push it out and
    // make room for a new todo block.
    if (bd != gct->scan_bd)
    {
        // If this block does not have enough space to allocate the
        // current object, but it also doesn't have any work to push, then 
        // push it on to the scanned list.  It cannot be empty, because
        // then there would be enough room to copy the current object.
        if (bd->u.scan == bd->free)
        {
            ASSERT(bd->free != bd->start);
            push_scanned_block(bd, ws);
        }
        // Otherwise, push this block out to the global list.
        else 
        {
            generation *gen;
            gen = ws->gen;
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
    bdescr *bd;

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

        if (size > BLOCK_SIZE_W) {
            bd = allocGroup_sync((lnat)BLOCK_ROUND_UP(size*sizeof(W_))
                                 / BLOCK_SIZE);
        } else {

#if ALLOCBLOCKS
            bdescr *hd, *tl;

            allocChain_sync(ALLOCBLOCKS, &hd, &tl, ws->gen, BF_EVACUATED);
            tl->link = ws->part_list;
            ws->part_list = hd->link;
            ws->n_part_blocks += ALLOCBLOCKS-1;

            bd = hd;
#else
            bd = allocBlock_sync();
#endif
        }
        initBdescr(bd, ws->gen, ws->gen->to);
        bd->flags = BF_EVACUATED;
        bd->u.scan = bd->free = bd->start;
    }

    bd->link = NULL;

    ws->todo_bd = bd;
    ws->todo_free = bd->free;

    if (gct->gc_type == GC_PAR && work_stealing) {
        // use a smaller limit if we're doing load-balancing, so we
        // can share available work more quickly.
        ws->todo_lim  = stg_min(bd->start + bd->blocks * BLOCK_SIZE_W,
                                bd->free + stg_max(WORK_UNIT_WORDS,size));
    } else {
        ws->todo_lim = bd->start + bd->blocks * BLOCK_SIZE_W;
    }

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
