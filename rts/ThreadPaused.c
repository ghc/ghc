/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Tidying up a thread when it stops running
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"
#include "Updates.h"
#include "RaiseAsync.h"
#include "Trace.h"
#include "RtsFlags.h"

#include <string.h> // for memmove()

/* -----------------------------------------------------------------------------
 * Stack squeezing
 *
 * Code largely pinched from old RTS, then hacked to bits.  We also do
 * lazy black holing here.
 *
 * -------------------------------------------------------------------------- */

struct stack_gap { StgWord gap_size; struct stack_gap *next_gap; };

static void
stackSqueeze(StgTSO *tso, StgPtr bottom)
{
    StgPtr frame;
    rtsBool prev_was_update_frame;
    StgClosure *updatee = NULL;
    StgRetInfoTable *info;
    StgWord current_gap_size;
    struct stack_gap *gap;

    // Stage 1: 
    //    Traverse the stack upwards, replacing adjacent update frames
    //    with a single update frame and a "stack gap".  A stack gap
    //    contains two values: the size of the gap, and the distance
    //    to the next gap (or the stack top).

    frame = tso->sp;

    ASSERT(frame < bottom);
    
    prev_was_update_frame = rtsFalse;
    current_gap_size = 0;
    gap = (struct stack_gap *) (tso->sp - sizeofW(StgUpdateFrame));

    while (frame < bottom) {
	
	info = get_ret_itbl((StgClosure *)frame);
	switch (info->i.type) {

	case UPDATE_FRAME:
	{ 
	    StgUpdateFrame *upd = (StgUpdateFrame *)frame;

	    if (prev_was_update_frame) {

		TICK_UPD_SQUEEZED();
		/* wasn't there something about update squeezing and ticky to be
		 * sorted out?  oh yes: we aren't counting each enter properly
		 * in this case.  See the log somewhere.  KSW 1999-04-21
		 *
		 * Check two things: that the two update frames don't point to
		 * the same object, and that the updatee_bypass isn't already an
		 * indirection.  Both of these cases only happen when we're in a
		 * block hole-style loop (and there are multiple update frames
		 * on the stack pointing to the same closure), but they can both
		 * screw us up if we don't check.
		 */
		if (upd->updatee != updatee && !closure_IND(upd->updatee)) {
		    UPD_IND_NOLOCK(upd->updatee, updatee);
		}

		// now mark this update frame as a stack gap.  The gap
		// marker resides in the bottom-most update frame of
		// the series of adjacent frames, and covers all the
		// frames in this series.
		current_gap_size += sizeofW(StgUpdateFrame);
		((struct stack_gap *)frame)->gap_size = current_gap_size;
		((struct stack_gap *)frame)->next_gap = gap;

		frame += sizeofW(StgUpdateFrame);
		continue;
	    } 

	    // single update frame, or the topmost update frame in a series
	    else {
		prev_was_update_frame = rtsTrue;
		updatee = upd->updatee;
		frame += sizeofW(StgUpdateFrame);
		continue;
	    }
	}
	    
	default:
	    prev_was_update_frame = rtsFalse;

	    // we're not in a gap... check whether this is the end of a gap
	    // (an update frame can't be the end of a gap).
	    if (current_gap_size != 0) {
		gap = (struct stack_gap *) (frame - sizeofW(StgUpdateFrame));
	    }
	    current_gap_size = 0;

	    frame += stack_frame_sizeW((StgClosure *)frame);
	    continue;
	}
    }

    if (current_gap_size != 0) {
	gap = (struct stack_gap *) (frame - sizeofW(StgUpdateFrame));
    }

    // Now we have a stack with gaps in it, and we have to walk down
    // shoving the stack up to fill in the gaps.  A diagram might
    // help:
    //
    //    +| ********* |
    //     | ********* | <- sp
    //     |           |
    //     |           | <- gap_start
    //     | ......... |                |
    //     | stack_gap | <- gap         | chunk_size
    //     | ......... |                | 
    //     | ......... | <- gap_end     v
    //     | ********* | 
    //     | ********* | 
    //     | ********* | 
    //    -| ********* | 
    //
    // 'sp'  points the the current top-of-stack
    // 'gap' points to the stack_gap structure inside the gap
    // *****   indicates real stack data
    // .....   indicates gap
    // <empty> indicates unused
    //
    {
	void *sp;
	void *gap_start, *next_gap_start, *gap_end;
	nat chunk_size;

	next_gap_start = (void *)((unsigned char*)gap + sizeof(StgUpdateFrame));
	sp = next_gap_start;

	while ((StgPtr)gap > tso->sp) {

	    // we're working in *bytes* now...
	    gap_start = next_gap_start;
	    gap_end = (void*) ((unsigned char*)gap_start - gap->gap_size * sizeof(W_));

	    gap = gap->next_gap;
	    next_gap_start = (void *)((unsigned char*)gap + sizeof(StgUpdateFrame));

	    chunk_size = (unsigned char*)gap_end - (unsigned char*)next_gap_start;
	    sp -= chunk_size;
	    memmove(sp, next_gap_start, chunk_size);
	}

	tso->sp = (StgPtr)sp;
    }
}    

/* -----------------------------------------------------------------------------
 * Pausing a thread
 * 
 * We have to prepare for GC - this means doing lazy black holing
 * here.  We also take the opportunity to do stack squeezing if it's
 * turned on.
 * -------------------------------------------------------------------------- */
void
threadPaused(Capability *cap, StgTSO *tso)
{
    StgClosure *frame;
    StgRetInfoTable *info;
    StgClosure *bh;
    StgPtr stack_end;
    nat words_to_squeeze = 0;
    nat weight           = 0;
    nat weight_pending   = 0;
    rtsBool prev_was_update_frame = rtsFalse;
    
    // Check to see whether we have threads waiting to raise
    // exceptions, and we're not blocking exceptions, or are blocked
    // interruptibly.  This is important; if a thread is running with
    // TSO_BLOCKEX and becomes blocked interruptibly, this is the only
    // place we ensure that the blocked_exceptions get a chance.
    maybePerformBlockedException (cap, tso);
    if (tso->what_next == ThreadKilled) { return; }

    stack_end = &tso->stack[tso->stack_size];
    
    frame = (StgClosure *)tso->sp;

    while (1) {
	// If we've already marked this frame, then stop here.
	if (frame->header.info == (StgInfoTable *)&stg_marked_upd_frame_info) {
	    goto end;
	}

	info = get_ret_itbl(frame);
	
	switch (info->i.type) {
	    
	case UPDATE_FRAME:

	    SET_INFO(frame, (StgInfoTable *)&stg_marked_upd_frame_info);

	    bh = ((StgUpdateFrame *)frame)->updatee;

	    if (closure_IND(bh) || bh->header.info == &stg_BLACKHOLE_info) {
		debugTrace(DEBUG_squeeze,
			   "suspending duplicate work: %ld words of stack",
			   (long)((StgPtr)frame - tso->sp));

		// If this closure is already an indirection, then
		// suspend the computation up to this point:
		suspendComputation(cap,tso,(StgPtr)frame);

		// Now drop the update frame, and arrange to return
		// the value to the frame underneath:
		tso->sp = (StgPtr)frame + sizeofW(StgUpdateFrame) - 2;
		tso->sp[1] = (StgWord)bh;
		tso->sp[0] = (W_)&stg_enter_info;

		// And continue with threadPaused; there might be
		// yet more computation to suspend.
		threadPaused(cap,tso);
		return;
	    }

	    if (bh->header.info != &stg_CAF_BLACKHOLE_info) {
#if (!defined(LAZY_BLACKHOLING)) && defined(DEBUG)
		debugBelch("Unexpected lazy BHing required at 0x%04lx\n",(long)bh);
#endif
		// zero out the slop so that the sanity checker can tell
		// where the next closure is.
		DEBUG_FILL_SLOP(bh);
#ifdef PROFILING
		// @LDV profiling
		// We pretend that bh is now dead.
		LDV_recordDead_FILL_SLOP_DYNAMIC((StgClosure *)bh);
#endif
		SET_INFO(bh,&stg_BLACKHOLE_info);

		// We pretend that bh has just been created.
		LDV_RECORD_CREATE(bh);
	    }
	    
	    frame = (StgClosure *) ((StgUpdateFrame *)frame + 1);
	    if (prev_was_update_frame) {
		words_to_squeeze += sizeofW(StgUpdateFrame);
		weight += weight_pending;
		weight_pending = 0;
	    }
	    prev_was_update_frame = rtsTrue;
	    break;
	    
	case STOP_FRAME:
	    goto end;
	    
	    // normal stack frames; do nothing except advance the pointer
	default:
	{
	    nat frame_size = stack_frame_sizeW(frame);
	    weight_pending += frame_size;
	    frame = (StgClosure *)((StgPtr)frame + frame_size);
	    prev_was_update_frame = rtsFalse;
	}
	}
    }

end:
    debugTrace(DEBUG_squeeze, 
	       "words_to_squeeze: %d, weight: %d, squeeze: %s", 
	       words_to_squeeze, weight, 
	       weight < words_to_squeeze ? "YES" : "NO");

    // Should we squeeze or not?  Arbitrary heuristic: we squeeze if
    // the number of words we have to shift down is less than the
    // number of stack words we squeeze away by doing so.
    if (RtsFlags.GcFlags.squeezeUpdFrames == rtsTrue &&
	weight < words_to_squeeze) {
	stackSqueeze(tso, (StgPtr)frame);
    }
}
