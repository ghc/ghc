/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001,2019
 * Author: Sungwoo Park, Daniel Gröber
 *
 * Generalised profiling heap traversal.
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include "rts/PosixSource.h"
#include "Rts.h"
#include "sm/Storage.h"
#include <string.h>

#include "TraverseHeap.h"

const stackData nullStackData;

StgWord getTravData(const StgClosure *c)
{
    const StgWord hp_hdr = c->header.prof.hp.trav;
    return hp_hdr & (STG_WORD_MAX ^ 1);
}

void setTravData(const traverseState *ts, StgClosure *c, StgWord w)
{
    c->header.prof.hp.trav = w | ts->flip;
}

bool isTravDataValid(const traverseState *ts, const StgClosure *c)
{
    return (c->header.prof.hp.trav & 1) == ts->flip;
}

#if defined(DEBUG)
unsigned int g_traversalDebugLevel = 0;
static void debug(const char *s, ...)
{
    va_list ap;

    if(g_traversalDebugLevel == 0)
        return;

    va_start(ap,s);
    vdebugBelch(s, ap);
    va_end(ap);
}
#else
static void debug(const char *s STG_UNUSED, ...) {}
#endif

// number of blocks allocated for one stack
#define BLOCKS_IN_STACK 1

/* -----------------------------------------------------------------------------
 * Add a new block group to the stack.
 * Invariants:
 *  currentStack->link == s.
 * -------------------------------------------------------------------------- */
STATIC_INLINE void
newStackBlock( traverseState *ts, bdescr *bd )
{
    ts->currentStack = bd;
    ts->stackTop     = (stackElement *)(bd->start + BLOCK_SIZE_W * bd->blocks);
    ts->stackBottom  = (stackElement *)bd->start;
    ts->stackLimit   = (stackElement *)ts->stackTop;
    bd->free     = (StgPtr)ts->stackLimit;
}

/* -----------------------------------------------------------------------------
 * Return to the previous block group.
 * Invariants:
 *   s->link == currentStack.
 * -------------------------------------------------------------------------- */
STATIC_INLINE void
returnToOldStack( traverseState *ts, bdescr *bd )
{
    ts->currentStack = bd;
    ts->stackTop = (stackElement *)bd->free;
    ts->stackBottom = (stackElement *)bd->start;
    ts->stackLimit = (stackElement *)(bd->start + BLOCK_SIZE_W * bd->blocks);
    bd->free = (StgPtr)ts->stackLimit;
}

/**
 *  Initializes the traversal work-stack.
 */
void
initializeTraverseStack( traverseState *ts )
{
    if (ts->firstStack != NULL) {
        freeChain(ts->firstStack);
    }

    ts->firstStack = allocGroup(BLOCKS_IN_STACK);
    ts->firstStack->link = NULL;
    ts->firstStack->u.back = NULL;

    ts->stackSize = 0;
    ts->maxStackSize = 0;

    newStackBlock(ts, ts->firstStack);
}

/**
 * Frees all the block groups in the traversal works-stack.
 *
 * Invariants:
 *   firstStack != NULL
 */
void
closeTraverseStack( traverseState *ts )
{
    freeChain(ts->firstStack);
    ts->firstStack = NULL;
}

/**
 * Returns the largest stack size encountered during the traversal.
 */
int
getTraverseStackMaxSize(traverseState *ts)
{
    return ts->maxStackSize;
}

/**
 * Returns true if the whole stack is empty.
 **/
STATIC_INLINE bool
isEmptyWorkStack( traverseState *ts )
{
    return (ts->firstStack == ts->currentStack) && ts->stackTop == ts->stackLimit;
}

/**
 * Returns size of stack
 */
W_
traverseWorkStackBlocks(traverseState *ts)
{
    bdescr* bd;
    W_ res = 0;

    for (bd = ts->firstStack; bd != NULL; bd = bd->link)
      res += bd->blocks;

    return res;
}

/**
 * Initializes *info from ptrs and payload.
 *
 * Invariants:
 *
 *   payload[] begins with ptrs pointers followed by non-pointers.
 */
STATIC_INLINE void
init_ptrs( stackPos *info, uint32_t ptrs, StgPtr payload )
{
    info->type              = posTypePtrs;
    info->next.ptrs.pos     = 0;
    info->next.ptrs.ptrs    = ptrs;
    info->next.ptrs.payload = payload;
}

/**
 * Find the next object from *info.
 */
STATIC_INLINE StgClosure *
find_ptrs( stackPos *info )
{
    if (info->next.ptrs.pos < info->next.ptrs.ptrs) {
        return (StgClosure *)info->next.ptrs.payload[info->next.ptrs.pos++];
    } else {
        return NULL;
    }
}

/**
 *  Initializes *info from SRT information stored in *infoTable.
 */
STATIC_INLINE void
init_srt_fun( stackPos *info, const StgFunInfoTable *infoTable )
{
    info->type = posTypeSRT;
    if (infoTable->i.srt) {
        info->next.srt.srt = (StgClosure*)GET_FUN_SRT(infoTable);
    } else {
        info->next.srt.srt = NULL;
    }
}

STATIC_INLINE void
init_srt_thunk( stackPos *info, const StgThunkInfoTable *infoTable )
{
    info->type = posTypeSRT;
    if (infoTable->i.srt) {
        info->next.srt.srt = (StgClosure*)GET_SRT(infoTable);
    } else {
        info->next.srt.srt = NULL;
    }
}

/**
 * Find the next object from *info.
 */
STATIC_INLINE StgClosure *
find_srt( stackPos *info )
{
    StgClosure *c;
    if (info->type == posTypeSRT) {
        c = info->next.srt.srt;
        info->next.srt.srt = NULL;
        return c;
    }

    return NULL;
}

/**
 * Push a set of closures, represented by a single 'stackElement', onto the
 * traversal work-stack.
 */
static stackElement*
pushStackElement(traverseState *ts, const stackElement se)
{
    bdescr *nbd;      // Next Block Descriptor
    if (ts->stackTop - 1 < ts->stackBottom) {
        debug("pushStackElement() to the next stack.\n");

        // currentStack->free is updated when the active stack is switched
        // to the next stack.
        ts->currentStack->free = (StgPtr)ts->stackTop;

        if (ts->currentStack->link == NULL) {
            nbd = allocGroup(BLOCKS_IN_STACK);
            nbd->link = NULL;
            nbd->u.back = ts->currentStack;
            ts->currentStack->link = nbd;
        } else
            nbd = ts->currentStack->link;

        newStackBlock(ts, nbd);
    }

    // adjust stackTop (actual push)
    ts->stackTop--;
    // If the size of stackElement was huge, we would better replace the
    // following statement by either a memcpy() call or a switch statement
    // on the type of the element. Currently, the size of stackElement is
    // small enough (5 words) that this direct assignment seems to be enough.
    *ts->stackTop = se;

    ts->stackSize++;
    if (ts->stackSize > ts->maxStackSize) ts->maxStackSize = ts->stackSize;
    ASSERT(ts->stackSize >= 0);
    debug("stackSize = %d\n", ts->stackSize);

    return ts->stackTop;
}

/**
 * Push a single closure onto the traversal work-stack.
 *
 *  cp   - object's parent
 *  c    - closure
 *  data - data associated with closure.
 */
inline void
traversePushClosure(traverseState *ts, StgClosure *c, StgClosure *cp, stackElement *sep, stackData data) {
    stackElement se;

    se.c = c;
    se.info.next.cp = cp;
    se.sep = sep;
    se.data = data;
    se.accum = (stackAccum)(StgWord)0;
    se.info.type = posTypeFresh;

    pushStackElement(ts, se);
};

void
traversePushRoot(traverseState *ts, StgClosure *c, StgClosure *cp, stackData data)
{
    traversePushClosure(ts, c, cp, NULL, data);
};

/**
 * Push an empty stackElement onto the traversal work-stack for the sole purpose
 * of triggering the return callback 'traversalState.return_cb' for the closure
 * '*c' when traversing of it's children is complete.
 *
 * This is needed for code-paths which don't inherently have to push a
 * stackElement. c.f. traverseWorkStack.
 *
 * When return_cb is NULL this function does nothing.
 */
STATIC_INLINE stackElement *
traversePushReturn(traverseState *ts, StgClosure *c, stackAccum acc, stackElement *sep)
{
    if(!ts->return_cb)
        return sep;

    stackElement se;
    se.c = c;
    se.info.next.cp = NULL;
    se.accum = acc;
    se.sep = sep;
    memset(&se.data, 0, sizeof(se.data));
    // return frames never emit closures, traversePop just skips over them. So
    // the data field is simply never used.
    se.info.type = posTypeEmpty;
    return pushStackElement(ts, se);
};

/**
 * traverseGetChildren() extracts the first child of 'c' in 'first_child' and if
 * 'other_children' is true returns a stackElement in 'se' which
 * conceptually contains all remaining children of 'c'.
 *
 * If 'c' has no children, 'first_child' is set to NULL, other_children is set
 * to false and nothing is returned in 'se'.
 *
 * If 'c' has only one child, 'first_child' is set to that child, other_children
 * is set to false and nothing is returned in 'se'.
 *
 * Otherwise 'other_children' is set to true and a stackElement representing the
 * other children is returned in 'se'.
 *
 * Note that when 'se' is set only the fields fields 'se.c' and 'se.info'
 * are initialized. It is the caller's responsibility to initialize the rest.
 *
 * Invariants:
 *
 *  - 'c' is not any of TSO, AP, PAP, AP_STACK, or CONTINUATION, which means
 *    that there cannot be any stack objects.
 *
 * Note: SRTs are considered to be children as well.
 */
STATIC_INLINE void
traverseGetChildren(StgClosure *c, StgClosure **first_child, bool *other_children, stackElement *se)
{
    ASSERT(get_itbl(c)->type != TSO);
    ASSERT(get_itbl(c)->type != AP_STACK);

    //
    // fill in se
    //

    se->c = c;

    *other_children = false;

    // fill in se->info
    switch (get_itbl(c)->type) {
        // no child, no SRT
    case CONSTR_0_1:
    case CONSTR_0_2:
    case ARR_WORDS:
    case COMPACT_NFDATA:
        *first_child = NULL;
        return;

        // one child (fixed), no SRT
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        *first_child = ((StgMutVar *)c)->var;
        return;
    case THUNK_SELECTOR:
        *first_child = ((StgSelector *)c)->selectee;
        return;
    case BLACKHOLE:
        *first_child = ((StgInd *)c)->indirectee;
        return;
    case CONSTR_1_0:
    case CONSTR_1_1:
        *first_child = c->payload[0];
        return;

        // For CONSTR_2_0 and MVAR, we use se->info.step to record the position
        // of the next child. We do not write a separate initialization code.
        // Also we do not have to initialize info.type;

        // two children (fixed), no SRT
        // need to push a stackElement, but nothing to store in se->info
    case CONSTR_2_0:
        *first_child = c->payload[0];         // return the first pointer
        se->info.type = posTypeStep;
        se->info.next.step = 2;            // 2 = second
        break;

        // three children (fixed), no SRT
        // need to push a stackElement
    case MVAR_CLEAN:
    case MVAR_DIRTY:
        // head must be TSO and the head of a linked list of TSOs.
        // Shoule it be a child? Seems to be yes.
        *first_child = (StgClosure *)((StgMVar *)c)->head;
        se->info.type = posTypeStep;
        se->info.next.step = 2;            // 2 = second
        break;

        // three children (fixed), no SRT
    case WEAK:
        *first_child = ((StgWeak *)c)->key;
        se->info.type = posTypeStep;
        se->info.next.step = 2;
        break;

        // layout.payload.ptrs, no SRT
    case TVAR:
    case CONSTR:
    case CONSTR_NOCAF:
    case PRIM:
    case MUT_PRIM:
    case BCO:
        init_ptrs(&se->info, get_itbl(c)->layout.payload.ptrs,
                  (StgPtr)c->payload);
        *first_child = find_ptrs(&se->info);
        if (*first_child == NULL)
            return;   // no child
        break;

        // StgMutArrPtr.ptrs, no SRT
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        init_ptrs(&se->info, ((StgMutArrPtrs *)c)->ptrs,
                  (StgPtr)(((StgMutArrPtrs *)c)->payload));
        *first_child = find_ptrs(&se->info);
        if (*first_child == NULL)
            return;
        break;

        // StgMutArrPtr.ptrs, no SRT
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        init_ptrs(&se->info, ((StgSmallMutArrPtrs *)c)->ptrs,
                  (StgPtr)(((StgSmallMutArrPtrs *)c)->payload));
        *first_child = find_ptrs(&se->info);
        if (*first_child == NULL)
            return;
        break;

    // layout.payload.ptrs, SRT
    case FUN_STATIC:
    case FUN:           // *c is a heap object.
    case FUN_2_0:
        init_ptrs(&se->info, get_itbl(c)->layout.payload.ptrs, (StgPtr)c->payload);
        *first_child = find_ptrs(&se->info);
        if (*first_child == NULL)
            // no child from ptrs, so check SRT
            goto fun_srt_only;
        break;

    case THUNK:
    case THUNK_2_0:
        init_ptrs(&se->info, get_itbl(c)->layout.payload.ptrs,
                  (StgPtr)((StgThunk *)c)->payload);
        *first_child = find_ptrs(&se->info);
        if (*first_child == NULL)
            // no child from ptrs, so check SRT
            goto thunk_srt_only;
        break;

        // 1 fixed child, SRT
    case FUN_1_0:
    case FUN_1_1:
        *first_child = c->payload[0];
        ASSERT(*first_child != NULL);
        init_srt_fun(&se->info, get_fun_itbl(c));
        break;

    case THUNK_1_0:
    case THUNK_1_1:
        *first_child = ((StgThunk *)c)->payload[0];
        ASSERT(*first_child != NULL);
        init_srt_thunk(&se->info, get_thunk_itbl(c));
        break;

    case FUN_0_1:      // *c is a heap object.
    case FUN_0_2:
    fun_srt_only:
        init_srt_fun(&se->info, get_fun_itbl(c));
        *first_child = find_srt(&se->info);
        if (*first_child == NULL)
            return;     // no child
        break;

    // SRT only
    case THUNK_STATIC:
        ASSERT(get_itbl(c)->srt != 0);
        /* fall-thru */
    case THUNK_0_1:
    case THUNK_0_2:
    thunk_srt_only:
        init_srt_thunk(&se->info, get_thunk_itbl(c));
        *first_child = find_srt(&se->info);
        if (*first_child == NULL)
            return;     // no child
        break;

    case TREC_CHUNK:
        *first_child = (StgClosure *)((StgTRecChunk *)c)->prev_chunk;
        se->info.type = posTypeStep;
        se->info.next.step = 0;  // entry no.
        break;

        // cannot appear
    case PAP:
    case AP:
    case AP_STACK:
    case CONTINUATION:
    case TSO:
    case STACK:
    case IND_STATIC:
        // stack objects
    case UPDATE_FRAME:
    case CATCH_FRAME:
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case ANN_FRAME:
        // invalid objects
    case IND:
    case INVALID_OBJECT:
    default:
        barf("Invalid object *c in push(): %d", get_itbl(c)->type);
        return;
    }

    // se->info.next.cp has to be initialized when type==posTypeFresh. We don't
    // do that here though. So type must be !=posTypeFresh.
    ASSERT(se->info.type != posTypeFresh);

    *other_children = true;
}

STATIC_INLINE void
popStackElement(traverseState *ts) {
    debug("popStackElement(): stackTop = 0x%x\n", ts->stackTop);

    ASSERT(ts->stackTop != ts->stackLimit);
    ASSERT(!isEmptyWorkStack(ts));

    // <= (instead of <) is wrong!
    if (ts->stackTop + 1 < ts->stackLimit) {
        ts->stackTop++;

        ts->stackSize--;
        if (ts->stackSize > ts->maxStackSize) ts->maxStackSize = ts->stackSize;
        ASSERT(ts->stackSize >= 0);
        debug("stackSize = (--) %d\n", ts->stackSize);

        return;
    }

    bdescr *pbd;    // Previous Block Descriptor

    debug("popStackElement() to the previous stack.\n");

    ASSERT(ts->stackTop + 1 == ts->stackLimit);
    ASSERT(ts->stackBottom == (stackElement *)ts->currentStack->start);

    if (ts->firstStack == ts->currentStack) {
        // The stack is completely empty.
        ts->stackTop++;
        ASSERT(ts->stackTop == ts->stackLimit);

        ts->stackSize--;
        if (ts->stackSize > ts->maxStackSize) ts->maxStackSize = ts->stackSize;
        ASSERT(ts->stackSize >= 0);
        debug("stackSize = %d\n", ts->stackSize);

        return;
    }

    // currentStack->free is updated when the active stack is switched back
    // to the previous stack.
    ts->currentStack->free = (StgPtr)ts->stackLimit;

    // find the previous block descriptor
    pbd = ts->currentStack->u.back;
    ASSERT(pbd != NULL);

    returnToOldStack(ts, pbd);

    ts->stackSize--;
    if (ts->stackSize > ts->maxStackSize) ts->maxStackSize = ts->stackSize;
    ASSERT(ts->stackSize >= 0);
    debug("stackSize = %d\n", ts->stackSize);
}

/**
 *  callReturnAndPopStackElement(): Call 'traversalState.return_cb' and remove a
 *  depleted stackElement from the top of the traversal work-stack.
 *
 *  Invariants:
 *    stackTop cannot be equal to stackLimit unless the whole stack is
 *    empty, in which case popStackElement() is not allowed.
 */
static void
callReturnAndPopStackElement(traverseState *ts)
{
    stackElement *se = ts->stackTop;

    if(ts->return_cb)
        ts->return_cb(se->c, se->accum,
                      se->sep->c, &se->sep->accum);

    popStackElement(ts);
}

/**
 *  Finds the next object to be considered for retainer profiling and store
 *  its pointer to *c.
 *
 *  If the unprocessed object was stored in the stack (posTypeFresh), the
 *  this object is returned as-is. Otherwise Test if the topmost stack
 *  element indicates that more objects are left,
 *  and if so, retrieve the next object and store its pointer to *c. Also,
 *  set *cp and *data appropriately, both of which are stored in the stack
 *  element.  The topmost stack element is then overwritten so it denotes the
 *  next object.
 *
 *  If the topmost stack element indicates no more objects are left, pop
 *  off the stack element until either an object can be retrieved or
 *  the work-stack becomes empty, indicated by true returned by
 *  isEmptyWorkStack(), in which case *c is set to NULL.
 *
 *  Note:
 *
 *    It is okay to call this function even when the work-stack is empty.
 */
STATIC_INLINE void
traversePop(traverseState *ts, StgClosure **c, StgClosure **cp, stackData *data, stackElement **sep)
{
    stackElement *se;

    debug("traversePop(): stackTop = 0x%x\n", ts->stackTop);

    // Is this the last internal sub-element?
    bool last = false;

    *c = NULL;

    do {
        if (isEmptyWorkStack(ts)) {
            *c = NULL;
            return;
        }

        // Note: Below every `break`, where the loop condition is true, must be
        // accompanied by a popStackElement()/callReturnAndPopStackElement()
        // call otherwise this is an infinite loop.
        se = ts->stackTop;

        *sep = se->sep;

        // If this is a top-level element, you should pop that out.
        if (se->info.type == posTypeFresh) {
            *cp = se->info.next.cp;
            *c = se->c;
            *data = se->data;
            popStackElement(ts);
            return;
        } else if (se->info.type == posTypeEmpty) {
            callReturnAndPopStackElement(ts);
            continue;
        }

        // Note: The first ptr of all of these was already returned as
        // *fist_child in push(), so we always start with the second field.
        switch (get_itbl(se->c)->type) {
            // two children (fixed), no SRT
            // nothing in se.info
        case CONSTR_2_0:
            *c = se->c->payload[1];
            last = true;
            goto out;

            // three children (fixed), no SRT
            // need to push a stackElement
        case MVAR_CLEAN:
        case MVAR_DIRTY:
            if (se->info.next.step == 2) {
                *c = (StgClosure *)((StgMVar *)se->c)->tail;
                se->info.next.step++;             // move to the next step
                // no popStackElement
            } else {
                *c = ((StgMVar *)se->c)->value;
                last = true;
            }
            goto out;

            // three children (fixed), no SRT
        case WEAK:
            if (se->info.next.step == 2) {
                *c = ((StgWeak *)se->c)->value;
                se->info.next.step++;
                // no popStackElement
            } else {
                *c = ((StgWeak *)se->c)->finalizer;
                last = true;
            }
            goto out;

        case TREC_CHUNK: {
            // These are pretty complicated: we have N entries, each
            // of which contains 3 fields that we want to follow.  So
            // we divide the step counter: the 2 low bits indicate
            // which field, and the rest of the bits indicate the
            // entry number (starting from zero).
            TRecEntry *entry;
            StgWord step = se->info.next.step;
            uint32_t entry_no = step >> 2;
            uint32_t field_no = step & 3;

            entry = &((StgTRecChunk *)se->c)->entries[entry_no];
            if (field_no == 0) {
                *c = (StgClosure *)entry->tvar;
            } else if (field_no == 1) {
                *c = entry->expected_value;
            } else {
                *c = entry->new_value;
            }

            se->info.next.step = ++step;

            entry_no = step >> 2;
            if (entry_no == ((StgTRecChunk *)se->c)->next_entry_idx) {
                se->info.type = posTypeEmpty;
                continue;
            }
            goto out;
        }

        case TVAR:
        case CONSTR:
        case PRIM:
        case MUT_PRIM:
        case BCO:
            // StgMutArrPtr.ptrs, no SRT
        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
        case MUT_ARR_PTRS_FROZEN_CLEAN:
        case MUT_ARR_PTRS_FROZEN_DIRTY:
        case SMALL_MUT_ARR_PTRS_CLEAN:
        case SMALL_MUT_ARR_PTRS_DIRTY:
        case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
        case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
            *c = find_ptrs(&se->info);
            if (*c == NULL) {
                se->info.type = posTypeEmpty;
                continue;
            }
            goto out;

            // layout.payload.ptrs, SRT
        case FUN:         // always a heap object
        case FUN_STATIC:
        case FUN_2_0:
            if (se->info.type == posTypePtrs) {
                *c = find_ptrs(&se->info);
                if (*c != NULL) {
                    goto out;
                }
                init_srt_fun(&se->info, get_fun_itbl(se->c));
            }
            goto do_srt;

        case THUNK:
        case THUNK_2_0:
            if (se->info.type == posTypePtrs) {
                *c = find_ptrs(&se->info);
                if (*c != NULL) {
                    goto out;
                }
                init_srt_thunk(&se->info, get_thunk_itbl(se->c));
            }
            goto do_srt;

            // SRT
        do_srt:
        case THUNK_STATIC:
        case FUN_0_1:
        case FUN_0_2:
        case THUNK_0_1:
        case THUNK_0_2:
        case FUN_1_0:
        case FUN_1_1:
        case THUNK_1_0:
        case THUNK_1_1:
            *c = find_srt(&se->info);
            if(*c == NULL) {
                se->info.type = posTypeEmpty;
                continue;
            }
            goto out;

            // no child (fixed), no SRT
        case CONSTR_0_1:
        case CONSTR_0_2:
        case ARR_WORDS:
            // one child (fixed), no SRT
        case MUT_VAR_CLEAN:
        case MUT_VAR_DIRTY:
        case THUNK_SELECTOR:
        case CONSTR_1_1:
            // cannot appear
        case PAP:
        case AP:
        case AP_STACK:
        case CONTINUATION:
        case TSO:
        case STACK:
        case IND_STATIC:
        case CONSTR_NOCAF:
            // stack objects
        case UPDATE_FRAME:
        case CATCH_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case RET_BCO:
        case RET_SMALL:
        case RET_BIG:
        case ANN_FRAME:
            // invalid objects
        case IND:
        case INVALID_OBJECT:
        default:
            barf("Invalid object *c in traversePop(): %d", get_itbl(se->c)->type);
            return;
        }
    } while (*c == NULL);

out:

    ASSERT(*c != NULL);

    *cp = se->c;
    *data = se->data;
    *sep = se;

    if(last && ts->return_cb)
        se->info.type = posTypeEmpty;
    else if(last)
        popStackElement(ts);

    return;

}

/**
 * Make sure a closure's profiling data is initialized to zero if it does not
 * conform to the current value of the flip bit, returns true in this case.
 *
 * See Note [Profiling heap traversal visited bit].
 */
bool
traverseMaybeInitClosureData(const traverseState* ts, StgClosure *c)
{
    if (!isTravDataValid(ts, c)) {
        setTravData(ts, c, 0);
        return true;
    }
    return false;
}

/**
 * Call traversePushClosure for each of the closures covered by a large bitmap.
 */
static void
traverseLargeBitmap(traverseState *ts, StgPtr p, StgLargeBitmap *large_bitmap,
    uint32_t size, StgClosure *c, stackElement *sep, stackData data)
{
    uint32_t i, b;
    StgWord bitmap;

    b = 0;
    bitmap = large_bitmap->bitmap[b];
    for (i = 0; i < size; ) {
        if ((bitmap & 1) == 0) {
            traversePushClosure(ts, (StgClosure *)*p, c, sep, data);
        }
        i++;
        p++;
        if (i % BITS_IN(W_) == 0) {
            b++;
            bitmap = large_bitmap->bitmap[b];
        } else {
            bitmap = bitmap >> 1;
        }
    }
}

STATIC_INLINE StgPtr
traverseSmallBitmap (traverseState *ts, StgPtr p, uint32_t size, StgWord bitmap,
                     StgClosure *c, stackElement *sep, stackData data)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            traversePushClosure(ts, (StgClosure *)*p, c, sep, data);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

/**
 *  traversePushStack(ts, cp, data, stackStart, stackEnd) pushes all the objects
 *  in the STG stack-chunk from stackStart to stackEnd onto the traversal
 *  work-stack with 'c' and 'data' being their parent and associated data,
 *  respectively.
 *
 *  Invariants:
 *
 *    *cp is one of the following: TSO, AP_STACK.
 *
 *    stackStart < stackEnd.
 *
 *    If *c is TSO, its state is not ThreadComplete,or ThreadKilled,
 *    which means that its stack is ready to process.
 *
 *  Note:
 *
 *    This code was almost plagiarized from GC.c! For each pointer,
 *    traversePushClosure() is invoked instead of evacuate().
 */
static void
traversePushStack(traverseState *ts, StgClosure *cp, stackElement *sep,
                  stackData data, StgPtr stackStart, StgPtr stackEnd)
{
    StgPtr p;
    const StgRetInfoTable *info;
    StgWord bitmap;
    uint32_t size;

    ASSERT(get_itbl(cp)->type == STACK);

    p = stackStart;
    while (p < stackEnd) {
        info = get_ret_itbl((StgClosure *)p);

        switch(info->i.type) {

        case UPDATE_FRAME:
            traversePushClosure(ts, ((StgUpdateFrame *)p)->updatee, cp, sep, data);
            p += sizeofW(StgUpdateFrame);
            continue;

        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case CATCH_FRAME:
        case CATCH_STM_FRAME:
        case CATCH_RETRY_FRAME:
        case ATOMICALLY_FRAME:
        case RET_SMALL:
        case ANN_FRAME:
            bitmap = BITMAP_BITS(info->i.layout.bitmap);
            size   = BITMAP_SIZE(info->i.layout.bitmap);
            p++;
            p = traverseSmallBitmap(ts, p, size, bitmap, cp, sep, data);

        follow_srt:
            if (info->i.srt) {
                traversePushClosure(ts, GET_SRT(info), cp, sep, data);
            }
            continue;

        case RET_BCO: {
            StgBCO *bco;

            p++;
            traversePushClosure(ts, (StgClosure*)*p, cp, sep, data);
            bco = (StgBCO *)*p;
            p++;
            size = BCO_BITMAP_SIZE(bco);
            traverseLargeBitmap(ts, p, BCO_BITMAP(bco), size, cp, sep, data);
            p += size;
            continue;
        }

            // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
        case RET_BIG:
            size = GET_LARGE_BITMAP(&info->i)->size;
            p++;
            traverseLargeBitmap(ts, p, GET_LARGE_BITMAP(&info->i),
                                size, cp, sep, data);
            p += size;
            // and don't forget to follow the SRT
            goto follow_srt;

        case RET_FUN: {
            StgRetFun *ret_fun = (StgRetFun *)p;
            const StgFunInfoTable *fun_info;

            traversePushClosure(ts, ret_fun->fun, cp, sep, data);
            fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(ret_fun->fun));

            p = (P_)&ret_fun->payload;
            switch (fun_info->f.fun_type) {
            case ARG_GEN:
                bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
                size = BITMAP_SIZE(fun_info->f.b.bitmap);
                p = traverseSmallBitmap(ts, p, size, bitmap, cp, sep, data);
                break;
            case ARG_GEN_BIG:
                size = GET_FUN_LARGE_BITMAP(fun_info)->size;
                traverseLargeBitmap(ts, p, GET_FUN_LARGE_BITMAP(fun_info),
                                    size, cp, sep, data);
                p += size;
                break;
            default:
                bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
                size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
                p = traverseSmallBitmap(ts, p, size, bitmap, cp, sep, data);
                break;
            }
            goto follow_srt;
        }

        default:
            barf("Invalid object found in traversePushStack(): %d",
                 (int)(info->i.type));
        }
    }
}

/**
 * Call traversePushClosure for each of the children of a PAP/AP
 */
static StgPtr
traversePAP (traverseState *ts,
                    StgClosure *pap,    /* NOT tagged */
                    stackElement *sep,
                    stackData data,
                    StgClosure *fun,    /* tagged */
                    StgClosure** payload, StgWord n_args)
{
    StgPtr p;
    StgWord bitmap;
    const StgFunInfoTable *fun_info;

    traversePushClosure(ts, fun, pap, sep, data);
    fun = UNTAG_CLOSURE(fun);
    fun_info = get_fun_itbl(fun);
    ASSERT(fun_info->i.type != PAP);

    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        p = traverseSmallBitmap(ts, p, n_args, bitmap,
                                pap, sep, data);
        break;
    case ARG_GEN_BIG:
        traverseLargeBitmap(ts, p, GET_FUN_LARGE_BITMAP(fun_info),
                            n_args, pap, sep, data);
        p += n_args;
        break;
    case ARG_BCO:
        traverseLargeBitmap(ts, (StgPtr)payload, BCO_BITMAP(fun),
                            n_args, pap, sep, data);
        p += n_args;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        p = traverseSmallBitmap(ts, p, n_args, bitmap, pap, sep, data);
        break;
    }
    return p;
}

static void
resetMutableObjects(traverseState* ts)
{
    uint32_t g, n;
    bdescr *bd;
    StgPtr ml;

    // The following code resets the 'trav' field of each unvisited mutable
    // object.
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        // NOT true: even G0 has a block on its mutable list
        // ASSERT(g != 0 || (generations[g].mut_list == NULL));

        // Traversing through mut_list is necessary
        // because we can find MUT_VAR objects which have not been
        // visited during heap traversal.
        for (n = 0; n < getNumCapabilities(); n++) {
          for (bd = getCapability(n)->mut_lists[g]; bd != NULL; bd = bd->link) {
            for (ml = bd->start; ml < bd->free; ml++) {
                traverseMaybeInitClosureData(ts, (StgClosure *)*ml);
            }
          }
        }
    }
}

/**
 * Traverse all closures on the traversal work-stack, calling 'visit_cb' on each
 * closure. See 'visitClosure_cb' for details.
 */
void
traverseWorkStack(traverseState *ts, visitClosure_cb visit_cb)
{
    // first_child = first child of c
    StgClosure *c, *cp, *first_child;
    stackData data, child_data;
    StgWord typeOfc;
    stackElement *sep;
    bool other_children;

    // c = Current closure                           (possibly tagged)
    // cp = Current closure's Parent                 (NOT tagged)
    // data = current closures' associated data      (NOT tagged)
    // child_data = data to associate with current closure's children

loop:
    traversePop(ts, &c, &cp, &data, &sep);

    if (c == NULL) {
        debug("maxStackSize= %d\n", ts->maxStackSize);
        return;
    }

inner_loop:
    c = UNTAG_CLOSURE(c);

    typeOfc = get_itbl(c)->type;

    // special cases
    switch (typeOfc) {
    case TSO:
        if (((StgTSO *)c)->what_next == ThreadComplete ||
            ((StgTSO *)c)->what_next == ThreadKilled) {
            debug("ThreadComplete or ThreadKilled encountered in traverseWorkStack()\n");
            goto loop;
        }
        break;

    case IND_STATIC:
        // We just skip IND_STATIC, so it's never visited.
        c = ((StgIndStatic *)c)->indirectee;
        goto inner_loop;

    case CONSTR_NOCAF:
        // static objects with no pointers out, so goto loop.

        // It is not just enough not to visit *c; it is
        // mandatory because CONSTR_NOCAF are not reachable from
        // scavenged_static_objects, the list from which is assumed to traverse
        // all static objects after major garbage collections.
        goto loop;

    case THUNK_STATIC:
        if (get_itbl(c)->srt == 0) {
            // No need to visit *c; no dynamic objects are reachable from it.
            //
            // Static objects: if we traverse all the live closures,
            // including static closures, during each heap census then
            // we will observe that some static closures appear and
            // disappear.  eg. a closure may contain a pointer to a
            // static function 'f' which is not otherwise reachable
            // (it doesn't indirectly point to any CAFs, so it doesn't
            // appear in any SRTs), so we would find 'f' during
            // traversal.  However on the next sweep there may be no
            // closures pointing to 'f'.
            //
            // We must therefore ignore static closures whose SRT is
            // empty, because these are exactly the closures that may
            // "appear".  A closure with a non-empty SRT, and which is
            // still required, will always be reachable.
            //
            // But what about CONSTR?  Surely these may be able
            // to appear, and they don't have SRTs, so we can't
            // check.  So for now, we're calling
            // resetStaticObjectForProfiling() from the
            // garbage collector to reset the retainer sets in all the
            // reachable static objects.
            goto loop;
        }
        /* fall-thru */

    case FUN_STATIC: {
        const StgInfoTable *info = get_itbl(c);
        if (info->srt == 0 && info->layout.payload.ptrs == 0) {
            goto loop;
        } else {
            break;
        }
    }

    default:
        break;
    }

    stackAccum accum = {};

    // If this is the first visit to c, initialize its data.
    bool first_visit = traverseMaybeInitClosureData(ts, c);
    bool traverse_children = first_visit;
    if(visit_cb)
        traverse_children = visit_cb(c, cp, data, first_visit,
                                     &accum, &child_data);
    if(!traverse_children)
        goto loop;

    // process child

    // Special case closures: we process these all in one go rather
    // than attempting to save the current position, because doing so
    // would be hard.
    switch (typeOfc) {
    case STACK:
        sep = traversePushReturn(ts, c, accum, sep);
        traversePushStack(ts, c, sep, child_data,
                    ((StgStack *)c)->sp,
                    ((StgStack *)c)->stack + ((StgStack *)c)->stack_size);
        goto loop;

    case TSO:
    {
        StgTSO *tso = (StgTSO *)c;

        sep = traversePushReturn(ts, c, accum, sep);

        traversePushClosure(ts, (StgClosure *) tso->stackobj, c, sep, child_data);
        traversePushClosure(ts, (StgClosure *) tso->blocked_exceptions, c, sep, child_data);
        traversePushClosure(ts, (StgClosure *) tso->bq, c, sep, child_data);
        traversePushClosure(ts, (StgClosure *) tso->trec, c, sep, child_data);
        switch (ACQUIRE_LOAD(&tso->why_blocked)) {
        case BlockedOnMVar:
        case BlockedOnMVarRead:
        case BlockedOnBlackHole:
        case BlockedOnMsgThrowTo:
            traversePushClosure(ts, tso->block_info.closure, c, sep, child_data);
            break;
        default:
            break;
        }
        goto loop;
    }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)c;

        sep = traversePushReturn(ts, c, accum, sep);

        traversePushClosure(ts, (StgClosure *) bq->link, c, sep, child_data);
        traversePushClosure(ts, (StgClosure *) bq->bh, c, sep, child_data);
        traversePushClosure(ts, (StgClosure *) bq->owner, c, sep, child_data);
        goto loop;
    }

    case PAP:
    {
        StgPAP *pap = (StgPAP *)c;

        sep = traversePushReturn(ts, c, accum, sep);

        traversePAP(ts, c, sep, child_data, pap->fun, pap->payload, pap->n_args);
        goto loop;
    }

    case AP:
    {
        StgAP *ap = (StgAP *)c;

        sep = traversePushReturn(ts, c, accum, sep);

        traversePAP(ts, c, sep, child_data, ap->fun, ap->payload, ap->n_args);
        goto loop;
    }

    case AP_STACK:
        sep = traversePushReturn(ts, c, accum, sep);

        traversePushClosure(ts, ((StgAP_STACK *)c)->fun, c, sep, child_data);
        traversePushStack(ts, c, sep, child_data,
                    (StgPtr)((StgAP_STACK *)c)->payload,
                    (StgPtr)((StgAP_STACK *)c)->payload +
                             ((StgAP_STACK *)c)->size);
        goto loop;

    case CONTINUATION:
    {
        StgContinuation *cont = (StgContinuation *)c;
        traversePushStack(ts, c, sep, child_data,
                          cont->stack, cont->stack + cont->stack_size);
        goto loop;
    }
    }

    stackElement se;
    traverseGetChildren(c, &first_child, &other_children, &se);

    // If first_child is null, c has no child.
    // If first_child is not null, the top stack element points to the next
    // object.
    if(first_child == NULL && ts->return_cb) { // no children
        // This is only true when we're pushing additional return frames onto
        // the stack due to return_cb, so don't get any funny ideas about
        // replacing 'cp' by sep.
        ASSERT(sep->c == cp);
        ts->return_cb(c, accum, cp, &sep->accum);
        goto loop;
    } else if (first_child == NULL) { // no children
        goto loop;
    } else if(!other_children) {      // one child
        // Pushing a return frame for one child is pretty inefficent. We could
        // optimize this by storing a pointer to cp in c's profiling header
        // instead. I tested this out in a Haskell prototype of this code and it
        // works out but is rather fiddly.
        //
        // See Haskell model code here:
        //
        //     https://gitlab.haskell.org/ghc/ghc/snippets/1461
        sep = traversePushReturn(ts, c, accum, sep);
    } else {                          // many children
        se.sep = sep;
        se.data = child_data;
        se.accum = accum;

        sep = pushStackElement(ts, se);
    }

    // (c, cp, data) = (first_child, c, child_data)
    data = child_data;
    cp = c;
    c = first_child;
    goto inner_loop;
}

/**
 * This function flips the 'flip' bit and hence every closure's profiling data
 * will be reset to zero upon visiting. See Note [Profiling heap traversal
 * visited bit].
 */
void
traverseInvalidateClosureData(traverseState* ts)
{
    // First make sure any unvisited mutable objects are valid so they're
    // invalidated by the flip below
    resetMutableObjects(ts);

    // Then flip the flip bit, invalidating all closures.
    ts->flip = ts->flip ^ 1;
}

/**
 * Traverse all static objects and invalidate their traversal-data. This ensures
 * that when doing the actual traversal no static closures will seem to have
 * been visited already because they weren't visited in the last run.
 *
 * This function must be called before zeroing all objects reachable from
 * scavenged_static_objects in the case of major garbage collections. See
 * GarbageCollect() in GC.c.
 *
 * Note:
 *
 *   The mut_once_list of the oldest generation must also be traversed?
 *
 *   Why? Because if the evacuation of an object pointed to by a static
 *   indirection object fails, it is put back to the mut_once_list of the oldest
 *   generation.
 *
 *   However, this is not necessary because any static indirection objects are
 *   just traversed through to reach dynamic objects. In other words, they are
 *   never visited during traversal.
 */
void
resetStaticObjectForProfiling( const traverseState *ts, StgClosure *static_objects )
{
    uint32_t count = 0;
    StgClosure *p;

    p = static_objects;
    while (p != END_OF_STATIC_OBJECT_LIST) {
        p = UNTAG_STATIC_LIST_PTR(p);
        count++;

        switch (get_itbl(p)->type) {
        case IND_STATIC:
            // Since we do not compute the retainer set of any
            // IND_STATIC object, we don't have to reset its retainer
            // field.
            p = (StgClosure*)*IND_STATIC_LINK(p);
            break;
        case THUNK_STATIC:
            traverseMaybeInitClosureData(ts, p);
            p = (StgClosure*)*THUNK_STATIC_LINK(p);
            break;
        case FUN_STATIC:
        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_NOCAF:
            traverseMaybeInitClosureData(ts, p);
            p = (StgClosure*)*STATIC_LINK(get_itbl(p), p);
            break;
        default:
            barf("resetStaticObjectForProfiling: %p (%lu)",
                 p, (unsigned long)get_itbl(p)->type);
            break;
        }
    }

    debug("count in scavenged_static_objects = %d\n", count);
}

#endif /* PROFILING */
