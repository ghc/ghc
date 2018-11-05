/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Tidying up a thread when it stops running
 *
 * ---------------------------------------------------------------------------*/

// #include "PosixSource.h"
#include "Rts.h"

#include "ThreadPaused.h"
#include "sm/Storage.h"
#include "Updates.h"
#include "RaiseAsync.h"
#include "Trace.h"
#include "Threads.h"

#include <string.h> // for memmove()

/* -----------------------------------------------------------------------------
 * Stack squeezing
 *
 * Code largely pinched from old RTS, then hacked to bits.  We also do
 * lazy black holing here.
 *
 * -------------------------------------------------------------------------- */


struct stack_gap { StgWord gap_size; struct stack_gap *next_gap; };

static struct stack_gap *
updateAdjacentFrames (Capability *cap, StgTSO *tso, StgUpdateFrame *upd,
                      uint32_t count, struct stack_gap *next)
{
    StgClosure *updatee;
    struct stack_gap *gap;
    uint32_t i;

    // The first one (highest address) is the frame we take the
    // "master" updatee from; all the others will be made indirections
    // to this one.  It is essential that we do it this way around: we
    // used to make the lowest-addressed frame the "master" frame and
    // shuffle it down, but a bad case cropped up (#5505) where this
    // happened repeatedly, generating a chain of indirections which
    // the GC repeatedly traversed (indirection chains longer than one
    // are not supposed to happen).  So now after identifying a block
    // of adjacent update frames we walk downwards again updating them
    // all to point to the highest one, before squeezing out all but
    // the highest one.
    updatee = upd->updatee;
    count--;

    upd--;
    gap = (struct stack_gap*)upd;

    for (i = count; i > 0; i--, upd--) {
        /*
         * Check two things: that the two update frames
         * don't point to the same object, and that the
         * updatee_bypass isn't already an indirection.
         * Both of these cases only happen when we're in a
         * block hole-style loop (and there are multiple
         * update frames on the stack pointing to the same
         * closure), but they can both screw us up if we
         * don't check.
         */
        if (upd->updatee != updatee && !closure_IND(upd->updatee)) {
            updateThunk(cap, tso, upd->updatee, updatee);
        }
    }

    gap->gap_size = count * sizeofW(StgUpdateFrame);
    gap->next_gap = next;

    return gap;
}

static void
stackSqueeze(Capability *cap, StgTSO *tso, StgPtr bottom)
{
    StgPtr frame;
    uint32_t adjacent_update_frames;
    struct stack_gap *gap;

    // Stage 1:
    //    Traverse the stack upwards, replacing adjacent update frames
    //    with a single update frame and a "stack gap".  A stack gap
    //    contains two values: the size of the gap, and the distance
    //    to the next gap (or the stack top).

    frame = tso->stackobj->sp;

    ASSERT(frame < bottom);

    adjacent_update_frames = 0;
    gap = (struct stack_gap *) (frame - sizeofW(StgUpdateFrame));

    while (frame <= bottom)
    {
        switch (get_ret_itbl((StgClosure *)frame)->i.type) {

        case UPDATE_FRAME:
        {
            if (adjacent_update_frames > 0) {
                TICK_UPD_SQUEEZED();
            }
            adjacent_update_frames++;

            frame += sizeofW(StgUpdateFrame);
            continue;
        }

        default:
            // we're not in a gap... check whether this is the end of a gap
            // (an update frame can't be the end of a gap).
            if (adjacent_update_frames > 1) {
                gap = updateAdjacentFrames(cap, tso,
                                           (StgUpdateFrame*)(frame - sizeofW(StgUpdateFrame)),
                                           adjacent_update_frames, gap);
            }
            adjacent_update_frames = 0;

            frame += stack_frame_sizeW((StgClosure *)frame);
            continue;
        }
    }

    if (adjacent_update_frames > 1) {
        gap = updateAdjacentFrames(cap, tso,
                                   (StgUpdateFrame*)(frame - sizeofW(StgUpdateFrame)),
                                   adjacent_update_frames, gap);
    }

    // Now we have a stack with gap-structs in it, and we have to walk down
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
    // 'sp'  points the current top-of-stack
    // 'gap' points to the stack_gap structure inside the gap
    // *****   indicates real stack data
    // .....   indicates gap
    // <empty> indicates unused
    //
    {
        StgWord8 *sp;
        StgWord8 *gap_start, *next_gap_start, *gap_end;
        uint32_t chunk_size;

        next_gap_start = (StgWord8*)gap + sizeof(StgUpdateFrame);
        sp = next_gap_start;

        while ((StgPtr)gap > tso->stackobj->sp) {

            // we're working in *bytes* now...
            gap_start = next_gap_start;
            gap_end = gap_start - gap->gap_size * sizeof(W_);

            gap = gap->next_gap;
            next_gap_start = (StgWord8*)gap + sizeof(StgUpdateFrame);

            chunk_size = gap_end - next_gap_start;
            sp -= chunk_size;
            memmove(sp, next_gap_start, chunk_size);
        }

        tso->stackobj->sp = (StgPtr)sp;
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
    const StgRetInfoTable *info;
    const StgInfoTable *bh_info;
    const StgInfoTable *cur_bh_info USED_IF_THREADS;
    StgClosure *bh;
    StgPtr stack_end;
    uint32_t words_to_squeeze = 0;
    uint32_t weight           = 0;
    uint32_t weight_pending   = 0;
    bool prev_was_update_frame = false;
    StgWord heuristic_says_squeeze;

    // Check to see whether we have threads waiting to raise
    // exceptions, and we're not blocking exceptions, or are blocked
    // interruptibly.  This is important; if a thread is running with
    // TSO_BLOCKEX and becomes blocked interruptibly, this is the only
    // place we ensure that the blocked_exceptions get a chance.
    maybePerformBlockedException (cap, tso);
    if (tso->what_next == ThreadKilled) { return; }

    // NB. Updatable thunks *must* be blackholed, either by eager blackholing or
    // lazy blackholing.  See Note [upd-black-hole] in sm/Scav.c.

    stack_end = tso->stackobj->stack + tso->stackobj->stack_size;

    frame = (StgClosure *)tso->stackobj->sp;

    while ((P_)frame < stack_end) {
        info = get_ret_itbl(frame);

        switch (info->i.type) {

        case UPDATE_FRAME:

            // If we've already marked this frame, then stop here.
            if (frame->header.info == (StgInfoTable *)&stg_marked_upd_frame_info) {
                if (prev_was_update_frame) {
                    words_to_squeeze += sizeofW(StgUpdateFrame);
                    weight += weight_pending;
                    weight_pending = 0;
                }
                goto end;
            }

            SET_INFO(frame, (StgInfoTable *)&stg_marked_upd_frame_info);

            bh = ((StgUpdateFrame *)frame)->updatee;
            bh_info = bh->header.info;

#if defined(THREADED_RTS)
        retry:
#endif
            // Note [suspend duplicate work]
            //
            // If the info table is a WHITEHOLE or a BLACKHOLE, then
            // another thread has claimed it (via the SET_INFO()
            // below), or is in the process of doing so.  In that case
            // we want to suspend the work that the current thread has
            // done on this thunk and wait until the other thread has
            // finished.
            //
            // If eager blackholing is taking place, it could be the
            // case that the blackhole points to the current
            // TSO. e.g.:
            //
            //    this thread                   other thread
            //    --------------------------------------------------------
            //                                  c->indirectee = other_tso;
            //                                  c->header.info = EAGER_BH
            //                                  threadPaused():
            //                                    c->header.info = WHITEHOLE
            //                                    c->indirectee = other_tso
            //    c->indirectee = this_tso;
            //    c->header.info = EAGER_BH
            //                                    c->header.info = BLACKHOLE
            //    threadPaused()
            //    *** c->header.info is now BLACKHOLE,
            //        c->indirectee  points to this_tso
            //
            // So in this case do *not* suspend the work of the
            // current thread, because the current thread will become
            // deadlocked on itself.  See #5226 for an instance of
            // this bug.
            //
            // Note that great care is required when entering computations
            // suspended by this mechanism. See Note [AP_STACKs must be eagerly
            // blackholed] for details.
            if (((bh_info == &stg_BLACKHOLE_info)
                 && ((StgInd*)bh)->indirectee != (StgClosure*)tso)
                || (bh_info == &stg_WHITEHOLE_info))
            {
                debugTrace(DEBUG_squeeze,
                           "suspending duplicate work: %ld words of stack",
                           (long)((StgPtr)frame - tso->stackobj->sp));

                // If this closure is already an indirection, then
                // suspend the computation up to this point.
                // NB. check raiseAsync() to see what happens when
                // we're in a loop (#2783).
                suspendComputation(cap,tso,(StgUpdateFrame*)frame);

                // Now drop the update frame, and arrange to return
                // the value to the frame underneath:
                tso->stackobj->sp = (StgPtr)frame + sizeofW(StgUpdateFrame) - 2;
                tso->stackobj->sp[1] = (StgWord)bh;
                ASSERT(bh->header.info != &stg_TSO_info);
                tso->stackobj->sp[0] = (W_)&stg_enter_info;

                // And continue with threadPaused; there might be
                // yet more computation to suspend.
                frame = (StgClosure *)(tso->stackobj->sp + 2);
                prev_was_update_frame = false;
                continue;
            }

            // zero out the slop so that the sanity checker can tell
            // where the next closure is.
            OVERWRITING_CLOSURE(bh);

            // an EAGER_BLACKHOLE or CAF_BLACKHOLE gets turned into a
            // BLACKHOLE here.
#if defined(THREADED_RTS)
            // first we turn it into a WHITEHOLE to claim it, and if
            // successful we write our TSO and then the BLACKHOLE info pointer.
            cur_bh_info = (const StgInfoTable *)
                cas((StgVolatilePtr)&bh->header.info,
                    (StgWord)bh_info,
                    (StgWord)&stg_WHITEHOLE_info);

            if (cur_bh_info != bh_info) {
                bh_info = cur_bh_info;
#if defined(PROF_SPIN)
                ++whitehole_threadPaused_spin;
#endif
                busy_wait_nop();
                goto retry;
            }
#endif

            // The payload of the BLACKHOLE points to the TSO
            ((StgInd *)bh)->indirectee = (StgClosure *)tso;
            write_barrier();
            SET_INFO(bh,&stg_BLACKHOLE_info);

            // .. and we need a write barrier, since we just mutated the closure:
            recordClosureMutated(cap,bh);

            // We pretend that bh has just been created.
            LDV_RECORD_CREATE(bh);

            frame = (StgClosure *) ((StgUpdateFrame *)frame + 1);
            if (prev_was_update_frame) {
                words_to_squeeze += sizeofW(StgUpdateFrame);
                weight += weight_pending;
                weight_pending = 0;
            }
            prev_was_update_frame = true;
            break;

        case UNDERFLOW_FRAME:
        case STOP_FRAME:
            goto end;

            // normal stack frames; do nothing except advance the pointer
        default:
        {
            uint32_t frame_size = stack_frame_sizeW(frame);
            weight_pending += frame_size;
            frame = (StgClosure *)((StgPtr)frame + frame_size);
            prev_was_update_frame = false;
        }
        }
    }

end:
    // Should we squeeze or not?  Arbitrary heuristic: we squeeze if
    // the number of words we have to shift down is less than the
    // number of stack words we squeeze away by doing so.
    // The threshold was bumped from 5 to 8 as a result of #2797
    heuristic_says_squeeze = ((weight <= 8 && words_to_squeeze > 0)
                            || weight < words_to_squeeze);

    debugTrace(DEBUG_squeeze,
        "words_to_squeeze: %d, weight: %d, squeeze: %s",
        words_to_squeeze, weight,
        heuristic_says_squeeze ? "YES" : "NO");

    if (RtsFlags.GcFlags.squeezeUpdFrames == true &&
        heuristic_says_squeeze) {
        stackSqueeze(cap, tso, (StgPtr)frame);
        tso->flags |= TSO_SQUEEZED;
        // This flag tells threadStackOverflow() that the stack was
        // squeezed, because it may not need to be expanded.
    } else {
        tso->flags &= ~TSO_SQUEEZED;
    }
}
