/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: utilities
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "BlockAlloc.h"
#include "Storage.h"
#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"
#include "GCUtils.h"
#include "Printer.h"
#include "Trace.h"
#if defined(THREADED_RTS)
#include "WSDeque.h"
#endif

#if defined(THREADED_RTS)
SpinLock gc_alloc_block_sync;
#endif

static void push_todo_block(bdescr *bd, gen_workspace *ws);

bdescr* allocGroup_sync(uint32_t n)
{
    bdescr *bd;
    uint32_t node = capNoToNumaNode(gct->thread_index);
    ACQUIRE_ALLOC_BLOCK_SPIN_LOCK();
    bd = allocGroupOnNode(node,n);
    RELEASE_ALLOC_BLOCK_SPIN_LOCK();
    return bd;
}

bdescr* allocGroupOnNode_sync(uint32_t node, uint32_t n)
{
    bdescr *bd;
    ACQUIRE_ALLOC_BLOCK_SPIN_LOCK();
    bd = allocGroupOnNode(node,n);
    RELEASE_ALLOC_BLOCK_SPIN_LOCK();
    return bd;
}

static uint32_t
allocBlocks_sync(uint32_t n, bdescr **hd)
{
    bdescr *bd;
    uint32_t i;
    uint32_t node = capNoToNumaNode(gct->thread_index);
    ACQUIRE_ALLOC_BLOCK_SPIN_LOCK();
    bd = allocLargeChunkOnNode(node,1,n);
    // NB. allocLargeChunk, rather than allocGroup(n), to allocate in a
    // fragmentation-friendly way.
    n = bd->blocks;
    for (i = 0; i < n; i++) {
        bd[i].blocks = 1;
        bd[i].link = &bd[i+1];
        bd[i].free_off = 0;
    }
    bd[n-1].link = NULL;
    // We have to hold the lock until we've finished fiddling with the metadata,
    // otherwise the block allocator can get confused.
    RELEASE_ALLOC_BLOCK_SPIN_LOCK();
    *hd = bd;
    return n;
}

void
freeChain_sync(bdescr *bd)
{
    ACQUIRE_ALLOC_BLOCK_SPIN_LOCK();
    freeChain(bd);
    RELEASE_ALLOC_BLOCK_SPIN_LOCK();
}

void
freeGroup_sync(bdescr *bd)
{
    ACQUIRE_ALLOC_BLOCK_SPIN_LOCK();
    freeGroup(bd);
    RELEASE_ALLOC_BLOCK_SPIN_LOCK();
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
steal_todo_block (uint32_t g)
{
    uint32_t n;
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
    ASSERT(bd->gen_no == ws->gen->no);
    ASSERT(bd->u.scan == bdescr_free(bd));

    if (bd->blocks == 1 &&
        (BLOCK_SIZE_W - WORK_UNIT_WORDS) * sizeof(StgWord) > bd->free_off)
    {
        // A partially full block: put it on the part_list list.
        // Only for single objects - see Note [big objects]
        bd->link = ws->part_list;
        ws->part_list = bd;
        ws->n_part_blocks += bd->blocks;
        ws->n_part_words += bd->free_off / sizeof(StgWord);
        IF_DEBUG(sanity,
                 ASSERT(countBlocks(ws->part_list) == ws->n_part_blocks));
    }
    else
    {
        // put the scan block on the ws->scavd_list.
        bd->link = ws->scavd_list;
        ws->scavd_list = bd;
        ws->n_scavd_blocks += bd->blocks;
        ws->n_scavd_words += bd->free_off / sizeof(StgWord);
        IF_DEBUG(sanity,
                 ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks));
    }
}

void
push_todo_block(bdescr *bd, gen_workspace *ws)
{
    debugTrace(DEBUG_gc, "push todo block %p (%ld words), step %d, todo_q: %ld",
            bdescr_start(bd), (unsigned long)(bdescr_free(bd) - bd->u.scan),
            ws->gen->no, dequeElements(ws->todo_q));

    ASSERT(bd->link == NULL);

    if(!pushWSDeque(ws->todo_q, bd)) {
        bd->link = ws->todo_overflow;
        ws->todo_overflow = bd;
        ws->n_todo_overflow++;

        // In theory, if a gc thread pushes more blocks to it's todo_q than it
        // pops, the todo_overflow list will continue to grow. Other gc threads
        // can't steal from the todo_overflwo list, so they may be idle as the
        // first gc thread works diligently on it's todo_overflow list. In
        // practice this has not been observed to occur.
        //
        // The max_n_todo_overflow counter will allow us to observe large
        // todo_overflow lists if they ever arise. As of now I've not observed
        // any nonzero max_n_todo_overflow samples.
        gct->max_n_todo_overflow =
            stg_max(gct->max_n_todo_overflow, ws->n_todo_overflow);
    }

#if defined(THREADED_RTS)
    notifyTodoBlock();
#endif
}

/* Note [big objects]
   ~~~~~~~~~~~~~~~~~~
   We can get an ordinary object (CONSTR, FUN, THUNK etc.) that is
   larger than a block (see #7919).  Let's call these "big objects".
   These objects don't behave like large objects - they live in
   ordinary heap space (not the large_objects list), and are copied by
   evacuate().

   Clearly to copy one of these objects we need a block group, not an
   ordinary block, so when alloc_todo_block() will correctly allocate a
   block group.

   The question is what to do with the space that is left at the end
   of the block group after copying the big object into it.  We could
   continue to copy more objects into that space, but unfortunately
   the rest of the GC is not set up to handle objects that start in
   the second or later blocks of a group.  We just about manage this
   in the nursery (see scheduleHandleHeapOverflow()) so evacuate() can
   handle this, but other parts of the GC can't.  We could probably
   fix this, but it's a rare case, so for now we ensure that we never
   copy objects into the second and subsequent blocks of a block
   group.

   To ensure this:
    - alloc_todo_block() sets todo_lim to be exactly the size of the
      large object
    - push_scanned_block doesn't put these blocks on the part_list
*/

StgPtr
todo_block_full (uint32_t size, gen_workspace *ws)
{
    bool urgent_to_push, can_extend;
    StgPtr p;

    // todo_free has been pre-incremented by Evac.c:alloc_for_copy().  We
    // are expected to leave it bumped when we've finished here.
    ws->todo_free -= size;

    bdescr *const bd = ws->todo_bd;
    const StgPtr start = bdescr_start(bd);

    ASSERT(bd != NULL);
    ASSERT(bd->link == NULL);
    ASSERT(bd->gen_no == ws->gen->no);

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
    // subsequent block of a large block (see Note [big objects]).
    can_extend =
        ws->todo_free + size <= start + bd->blocks * BLOCK_SIZE_W
        && ws->todo_free < bdescr_start(ws->todo_bd) + BLOCK_SIZE_W;

    if (!urgent_to_push && can_extend)
    {
        ws->todo_lim = stg_min(start + bd->blocks * BLOCK_SIZE_W,
                               ws->todo_lim + stg_max(WORK_UNIT_WORDS,size));
        debugTrace(DEBUG_gc, "increasing limit for %p to %p",
                   start, ws->todo_lim);
        p = ws->todo_free;
        ws->todo_free += size;

        return p;
    }

    gct->copied += ws->todo_free - bdescr_free(bd);
    bdescr_set_free(bd, ws->todo_free);

    ASSERT(bd->u.scan >= bdescr_start(bd) && bd->u.scan <= bdescr_free(bd));

    // If this block is not the scan block, we want to push it out and
    // make room for a new todo block.
    if (bd != gct->scan_bd)
    {
        // If this block does not have enough space to allocate the
        // current object, but it also doesn't have any work to push, then
        // push it on to the scanned list.
        if (bd->u.scan == bdescr_free(bd))
        {
            if (bd->free_off == 0) {
                // Normally the block would not be empty, because then
                // there would be enough room to copy the current
                // object.  However, if the object we're copying is
                // larger than a block, then we might have an empty
                // block here.
                freeGroup_sync(bd);
            } else {
                push_scanned_block(bd, ws);
            }
        }
        // Otherwise, push this block out to the global list.
        else
        {
            push_todo_block(bd, ws);
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
alloc_todo_block (gen_workspace *ws, uint32_t size)
{
    bdescr *bd/*, *hd, *tl */;

    // Grab a part block if we have one, and it has enough room
    bd = ws->part_list;
    if (bd != NULL &&
        bd->blocks * BLOCK_SIZE_W > bd->free_off / sizeof(W_) + size)
    {
        ws->part_list = bd->link;
        ws->n_part_blocks -= bd->blocks;
        ws->n_part_words -= bd->free_off / sizeof(W_);
    }
    else
    {
        if (size > BLOCK_SIZE_W) {
            bd = allocGroup_sync((W_)BLOCK_ROUND_UP(size*sizeof(W_))
                                 / BLOCK_SIZE);
        } else {
            if (gct->free_blocks) {
                bd = gct->free_blocks;
                gct->free_blocks = bd->link;
            } else {
                // We allocate in chunks of at most 16 blocks, use one
                // block to satisfy the allocation request and place
                // the rest on `gct->free_blocks` for future use.
                StgWord chunk_size = 16;
                StgWord n_blocks = stg_min(chunk_size, 1 << (MBLOCK_SHIFT - BLOCK_SHIFT - 1));
                allocBlocks_sync(n_blocks, &bd);
                gct->free_blocks = bd->link;
            }
        }
        initBdescr(bd, ws->gen, ws->gen->to);
        RELAXED_STORE(&bd->u.scan, bdescr_start(bd));
        // blocks in to-space get the BF_EVACUATED flag.
        // RELEASE here to ensure that bd->gen is visible to other cores.
        RELEASE_STORE(&bd->flags, BF_EVACUATED);
    }

    bd->link = NULL;

    ws->todo_bd = bd;
    ws->todo_free = bdescr_free(bd);
    ws->todo_lim  = stg_min(bdescr_start(bd) + bd->blocks * BLOCK_SIZE_W,
                            ws->todo_free + stg_max(WORK_UNIT_WORDS,size));
                     // See Note [big objects]

    debugTrace(DEBUG_gc, "alloc new todo block %p for gen  %d",
               bdescr_free(bd), ws->gen->no);

    return ws->todo_free;
}
