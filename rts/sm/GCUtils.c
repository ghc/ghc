/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: utilities
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "GC.h"
#include "GCUtils.h"
#include "Printer.h"
#include "Trace.h"

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

/* -----------------------------------------------------------------------------
   Workspace utilities
   -------------------------------------------------------------------------- */

bdescr *
grab_todo_block (step_workspace *ws)
{
    bdescr *bd;
    step *stp;

    stp = ws->stp;
    bd = NULL;

    if (ws->buffer_todo_bd)
    {
	bd = ws->buffer_todo_bd;
	ASSERT(bd->link == NULL);
	ws->buffer_todo_bd = NULL;
	return bd;
    }

    ACQUIRE_SPIN_LOCK(&stp->sync_todo);
    if (stp->todos) {
	bd = stp->todos;
	stp->todos = bd->link;
        stp->n_todos--;
	bd->link = NULL;
    }	
    RELEASE_SPIN_LOCK(&stp->sync_todo);
    return bd;
}

static void
push_todo_block (bdescr *bd, step *stp)
{
    ASSERT(bd->link == NULL);
    ACQUIRE_SPIN_LOCK(&stp->sync_todo);
    bd->link = stp->todos;
    stp->todos = bd;
    stp->n_todos++;
    trace(TRACE_gc, "step %d, n_todos: %d", stp->abs_no, stp->n_todos);
    RELEASE_SPIN_LOCK(&stp->sync_todo);
}

void
push_scan_block (bdescr *bd, step_workspace *ws)
{
    ASSERT(bd != NULL);
    ASSERT(bd->link == NULL);

    // update stats: this is a block that has been copied & scavenged
    copied += bd->free - bd->start;

    // put the scan block on the ws->scavd_list.
    bd->link = ws->scavd_list;
    ws->scavd_list = bd;
    ws->n_scavd_blocks ++;

    IF_DEBUG(sanity, 
	     ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks));
}

StgPtr
gc_alloc_todo_block (step_workspace *ws)
{
    bdescr *bd;

    if (ws->todo_bd != NULL) {
        ws->todo_bd->free = ws->todo_free;
    }

    // If we already have a todo block, it must be full, so we push it
    // out: first to the buffer_todo_bd, then to the step.  BUT, don't
    // push out the block out if it is already the scan block.
    if (ws->todo_bd != NULL && ws->scan_bd != ws->todo_bd) {
	ASSERT(ws->todo_bd->link == NULL);
        if (ws->buffer_todo_bd == NULL) {
            // If the global todo list is empty, push this block
            // out immediately rather than caching it in
            // buffer_todo_bd, because there might be other threads
            // waiting for work.
            if (ws->stp->todos == NULL) {
                push_todo_block(ws->todo_bd, ws->stp);
            } else {
                ws->buffer_todo_bd = ws->todo_bd;
            }
        } else {            
	    ASSERT(ws->buffer_todo_bd->link == NULL);
	    push_todo_block(ws->buffer_todo_bd, ws->stp);
            ws->buffer_todo_bd = ws->todo_bd;
        }
	ws->todo_bd = NULL;
    }	    

    bd = allocBlock_sync();

    bd->gen_no = ws->stp->gen_no;
    bd->step = ws->stp;
    bd->link = NULL;

    // blocks in to-space in generations up to and including N
    // get the BF_EVACUATED flag.
    if (ws->stp->gen_no <= N) {
	bd->flags = BF_EVACUATED;
    } else {
	bd->flags = 0;
    }
	
    ws->todo_bd = bd;
    ws->todo_free = bd->start;
    ws->todo_lim  = bd->start + BLOCK_SIZE_W;

    return ws->todo_free;
}

/* -----------------------------------------------------------------------------
 * Debugging
 * -------------------------------------------------------------------------- */

#if DEBUG
void
printMutableList(generation *gen)
{
    bdescr *bd;
    StgPtr p;

    debugBelch("mutable list %p: ", gen->mut_list);

    for (bd = gen->mut_list; bd != NULL; bd = bd->link) {
	for (p = bd->start; p < bd->free; p++) {
	    debugBelch("%p (%s), ", (void *)*p, info_type((StgClosure *)*p));
	}
    }
    debugBelch("\n");
}
#endif /* DEBUG */
