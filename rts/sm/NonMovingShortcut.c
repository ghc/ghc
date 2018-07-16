/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2019
 *
 * Non-moving garbage collector and allocator:
 * Indirection short-cutting and the selector optimisation
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "GC.h"
#include "SMPClosureOps.h"
#include "NonMovingMark.h"
#include "NonMovingShortcut.h"
#include "Printer.h"

#define MAX_THUNK_SELECTOR_DEPTH 16

//#define SELECTOR_OPT_DEBUG

#if defined(SELECTOR_OPT_DEBUG)
static void
print_selector_chain(StgClosure *p)
{
    debugBelch("Selector chain: %p", (void*)p);
    StgClosure *next = p->payload[0];
    while (next != NULL) {
        debugBelch(", %p", next);
        next = next->payload[0];
    }
    debugBelch("\n");
}
#endif

static void
update_selector_chain(
        StgClosure *chain,
        StgClosure **origin,
        StgSelector * const p0,
        StgClosure * const val
) {
    ASSERT(val != NULL);

    // Make sure we don't introduce non-moving-to-moving pointers here.
    ASSERT(isNonmovingClosure(val));

    // This case we can't handle because we don't know info ptr of the closure
    // before we locked it.
    ASSERT(chain != val);

#if defined(SELECTOR_OPT_DEBUG)
    if (chain != NULL) {
        print_selector_chain(chain);
        debugBelch("Value: ");
        printClosure(val);
    }
#endif

    while (chain) {
        // debugBelch("chain: %p\n", (void*)chain);

        StgClosure *next = chain->payload[0];

        // We only update closures in the non-moving heap
        ASSERT(isNonmovingClosure(chain));

        ((StgInd*)chain)->indirectee = val;
        unlockClosure((StgClosure*)chain, &stg_IND_info);

        chain = next;
    }

    if (origin != NULL && (StgClosure*)p0 != val) {
        cas((StgVolatilePtr)origin, (StgWord)p0, (StgWord)val);
    }
}

// Returns value of the selector thunk. The value is a non-moving closure. If
// it's not possible to evaluate the selector thunk the return value will be the
// selector itself.
static StgClosure*
nonmoving_eval_thunk_selector_(
        MarkQueue *queue,
        StgSelector * const p0,
        StgClosure ** const origin,
        int depth
) {
    // This function should only be called on non-moving objects.
    ASSERT(HEAP_ALLOCED_GC((P_)p0) && isNonmovingClosure((StgClosure*)p0));

    markQueuePushClosure(queue, (StgClosure*)p0, NULL);

    // INVARIANT: A non-moving object. Locked (below).
    StgClosure *p = (StgClosure*)p0;

    // Chain of non-moving selectors to update. These will be INDs to `p` when
    // we reach to a value. INVARIANT: All objects in the chain are locked, and
    // in the non-moving heap.
    StgClosure *chain = NULL;

    // Variables to update: p.
selector_changed:
    ;

    // debugBelch("Selector changed: %p\n", (void*)p);

    // Lock the selector to avoid concurrent modification in mutators
    const StgInfoTable *selector_info_ptr = lockClosure((StgClosure*)p);
    StgInfoTable *selector_info_tbl = INFO_PTR_TO_STRUCT(selector_info_ptr);

    if (selector_info_tbl->type != THUNK_SELECTOR) {
        // Selector updated in the meantime, or we reached to a value. Update
        // the chain.
        unlockClosure(p, selector_info_ptr);
        update_selector_chain(chain, origin, p0, p);
        return p;
    }

    // The closure is locked and it's a selector thunk. If the selectee is a
    // CONSTR we do the selection here and the In the selected value will be the
    // value of this selector thunk.
    //
    // Two cases:
    //
    // - If the selected value is also a selector thunk, then we loop and
    //   evaluate it. The final value will be the value of both the current
    //   selector and the selected value (which is also a selector thunk).
    //
    // - If the selectee is a selector thunk, we recursively evaluate it (up to
    //   a certain depth, specified with MAX_THUNK_SELECTOR_DEPTH), then do the
    //   selection on the value of it.

    //
    // Do the selection
    //

    uint32_t field = selector_info_tbl->layout.selector_offset;
    StgClosure *selectee = UNTAG_CLOSURE(((StgSelector*)p)->selectee);

    // Variables to update: selectee
selectee_changed:
    // debugBelch("Selectee changed: %p\n", (void*)selectee);

    if (!isNonmovingClosure(selectee)) {
        // The selectee is a moving object, and it may be moved by a concurrent
        // minor GC while we read the info table and fields, so don't try to
        // read the fields, just update the chain.
        unlockClosure(p, selector_info_ptr);
        update_selector_chain(chain, origin, p0, p);
        return p;
    }

    // Selectee is a non-moving object, mark it.
    markQueuePushClosure(queue, selectee, NULL);

    const StgInfoTable *selectee_info_tbl = get_itbl(selectee);
    switch (selectee_info_tbl->type) {
        case WHITEHOLE: {
            // Probably a loop. Abort.
            unlockClosure(p, selector_info_ptr);
            update_selector_chain(chain, origin, p0, p);
            return p;
        }

        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_0_1:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_0_2:
        case CONSTR_NOCAF: {
            // Selectee is a constructor in the non-moving heap.
            // Select the field.

            // Check that the size is in range.
            ASSERT(field < (StgWord32)(selectee_info_tbl->layout.payload.ptrs +
                                       selectee_info_tbl->layout.payload.nptrs));

            StgClosure *val = UNTAG_CLOSURE(selectee->payload[field]);

            // `val` is the value of this selector thunk. We need to check a
            // few cases:
            //
            // - If `val` is in the moving heap, we stop here and update the
            //   chain. All updated objects should be added to the mut_list.
            //   (TODO (osa): What happens if the value is evacuated as we do
            //   this?)
            //
            // - If `val` is in the non-moving heap, we check if it's also a
            //   selector. If it is we add it to the chain and loop.

            // Follow indirections. Variables to update: `val`.
        val_changed:
            if (!isNonmovingClosure(val)) {
                // The selected value is a moving object, so we won't be
                // updating the chain to this object.
                unlockClosure(p, selector_info_ptr);
                update_selector_chain(chain, origin, p0, p);
                return p;
            }

            switch (get_itbl(val)->type) {
            case IND:
            case IND_STATIC:
                ;
                // Follow the indirection
                StgClosure *indirectee = UNTAG_CLOSURE(((StgInd*)val)->indirectee);
                if (isNonmovingClosure(indirectee)) {
                    val = UNTAG_CLOSURE(((StgInd*)val)->indirectee);
                    goto val_changed;
                } else {
                    unlockClosure(p, selector_info_ptr);
                    update_selector_chain(chain, origin, p0, p);
                    return p;
                }
            case THUNK_SELECTOR:
                // Value of the selector thunk is again a selector thunk in the
                // non-moving heap. Add the current selector to the chain and
                // loop.
                p->payload[0] = chain;
                chain = p;
                p = val;
                goto selector_changed;
            default:
                // Found a value, add the current selector to the chain and
                // update it.
                p->payload[0] = chain;
                chain = p;
                update_selector_chain(chain, origin, p0, val);
                return val;
            }
        }

        case IND:
        case IND_STATIC: {
            StgClosure *indirectee = UNTAG_CLOSURE(((StgInd *)selectee)->indirectee);
            if (isNonmovingClosure(indirectee)) {
                selectee = indirectee;
                goto selectee_changed;
            } else {
                unlockClosure(p, selector_info_ptr);
                update_selector_chain(chain, origin, p0, p);
                return p;
            }
        }

        case BLACKHOLE: {
            StgClosure *indirectee = ((StgInd*)selectee)->indirectee;

            if (!isNonmovingClosure(UNTAG_CLOSURE(indirectee))) {
                unlockClosure(p, selector_info_ptr);
                update_selector_chain(chain, origin, p0, p);
                return p;
            }

            // Establish whether this BH has been updated, and is now an
            // indirection, as in evacuate().
            if (GET_CLOSURE_TAG(indirectee) == 0) {
                const StgInfoTable *i = indirectee->header.info;
                if (i == &stg_TSO_info
                    || i == &stg_WHITEHOLE_info
                    || i == &stg_BLOCKING_QUEUE_CLEAN_info
                    || i == &stg_BLOCKING_QUEUE_DIRTY_info) {
                    unlockClosure(p, selector_info_ptr);
                    update_selector_chain(chain, origin, p0, p);
                    return (StgClosure*)p;
                }
                ASSERT(i != &stg_IND_info); // TODO not sure about this part
            }

            // It's an indirection, follow it.
            selectee = UNTAG_CLOSURE(indirectee);
            goto selectee_changed;
        }

        case AP:
        case AP_STACK:
        case THUNK:
        case THUNK_1_0:
        case THUNK_0_1:
        case THUNK_2_0:
        case THUNK_1_1:
        case THUNK_0_2:
        case THUNK_STATIC: {
            // Not evaluated yet
            unlockClosure(p, selector_info_ptr);
            update_selector_chain(chain, origin, p0, p);
            return (StgClosure*)p;
        }

        case THUNK_SELECTOR: {
            // Selectee is a selector thunk. Evaluate it if we haven't reached
            // the recursion limit yet.
            if (depth < MAX_THUNK_SELECTOR_DEPTH) {
                StgClosure *new_selectee =
                    UNTAG_CLOSURE(nonmoving_eval_thunk_selector_(
                                queue, (StgSelector*)selectee, NULL, depth+1));
                ASSERT(isNonmovingClosure(new_selectee));
                if (selectee == new_selectee) {
                    unlockClosure(p, selector_info_ptr);
                    update_selector_chain(chain, origin, p0, p);
                    return (StgClosure*)p;
                } else {
                    selectee = new_selectee;
                    goto selectee_changed;
                }
            } else {
                // Recursion limit reached
                unlockClosure(p, selector_info_ptr);
                update_selector_chain(chain, origin, p0, p);
                return (StgClosure*)p;
            }
        }

        default: {
            barf("nonmoving_eval_thunk_selector: strange selectee %d",
                 (int)(selectee_info_tbl->type));
        }
    }
}

void
nonmoving_eval_thunk_selector(MarkQueue *queue, StgSelector *p, StgClosure **origin)
{
    nonmoving_eval_thunk_selector_(queue, p, origin, 0);
}
