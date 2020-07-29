/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019,2020
 * Author: Daniel Gröber
 *
 * Generalised profiling heap traversal.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(PROFILING)

#include <rts/Types.h>

/**
 * Tagged-union representing a position pointer into a stackElement's closure.
 *
 * For some closure types we want to have a single stackElement represent
 * multiple objects to save space on the stack, to do this we remember which of
 * a stackElement's closure's children is the next one we want to be returned by
 * traversePop().
 *
 * Overall traverseGetChildren() initializes a stackElement's stackPos, and
 * traversePop() then uses stackPos to determine which of a closure's children
 * to return in this case.
 */
typedef struct stackPos_ {
    enum {
        posTypeStep,
        /*< Object with fixed layout. Keeps a counter to keep track of which
         * object should be returned next. (uses stackPos.next.step) */

        posTypePtrs,
        /*< Description of the pointers-first heap object. Keeps information
         * about layout. (uses stackPos.next.ptrs) */

        posTypeSRT,
        /*< Keeps SRT bitmap (uses stackPos.next.srt) */

        posTypeFresh,
        /*< Keeps a new object that was not inspected yet. Keeps a parent
         * element (uses stackPos.next.cp) */

        posTypeEmpty
        /*< This stackElement is empty and doesn't represent any closure, we
         *  just keep it around so we can call returnClosure_cb. */
    } type;
    union {
        // fixed layout or layout specified by a field in the closure
        StgWord step;

        // layout.payload
        struct {
            // See StgClosureInfo in InfoTables.h
            StgHalfWord pos;
            StgHalfWord ptrs;
            StgPtr payload;
        } ptrs;

        // SRT
        StgClosure *srt;

        // parent of the current closure, used only when posTypeFresh is set
        StgClosure *cp;
    } next;
} stackPos;

typedef union stackData_ {
     /**
      * Most recent retainer for the corresponding closure on the stack.
      */
    CostCentreStack *c_child_r;
} stackData;

extern const stackData nullStackData;

typedef union stackAccum_ {
} stackAccum;

/**
 * An element of the traversal work-stack. Besides the closure itself this also
 * stores it's parent, associated data and an accumulator.
 *
 * When 'info.type == posTypeFresh' a 'stackElement' represents just one
 * closure, namely 'c' with its parent stored as 'info.next.cp'.
 *
 * Otherwise this stack element specifies the list of child closures of 'c'. The
 * 'info' field represents an offset into this list, among other things. This is
 * to support returning a closure's children one-by-one without pushing one
 * element per child onto the stack which would use more space on the stack.
 *
 * See traverseGetChildren() and traversePop() for the code dealing with this
 * structure.
 *
 */
typedef struct stackElement_ {
    stackPos info;
    StgClosure *c;
    struct stackElement_ *sep; // stackElement of parent closure
    stackData data;
    stackAccum accum;
} stackElement;

typedef struct traverseState_ {
    /**
     * Invariants:
     *
     *    firstStack points to the first block group.
     *
     *    currentStack points to the block group currently being used.
     *
     *    currentStack->free == stackLimit.
     *
     *    stackTop points to the topmost byte in the stack of currentStack.
     *
     *    Unless the whole stack is empty, stackTop must point to the topmost
     *    object (or byte) in the whole stack. Thus, it is only when the whole
     *    stack is empty that stackTop == stackLimit (not during the execution
     *    of pushStackElement() and popStackElement()).
     *
     *    stackBottom == currentStack->start.
     *
     *    stackLimit
     *      == currentStack->start + BLOCK_SIZE_W * currentStack->blocks.
     *
     *  Note:
     *
     *    When a current stack becomes empty, stackTop is set to point to
     *    the topmost element on the previous block group so as to satisfy
     *    the invariants described above.
     */
    bdescr *firstStack;
    bdescr *currentStack;
    stackElement *stackBottom, *stackTop, *stackLimit;

    /**
     * stackSize: records the current size of the stack.
     * maxStackSize: records its high water mark.
     *
     * Invariants:
     *
     *   stackSize <= maxStackSize
     *
     * Note:
     *
     *   When return_cb == NULL stackSize is just an estimate measure of the
     *   depth of the graph. The reason is that some heap objects have only a
     *   single child and may not result in a new element being pushed onto the
     *   stack. Therefore, at the end of retainer profiling, maxStackSize is
     *   some value no greater than the actual depth of the graph.
     */
    int stackSize, maxStackSize;

    bdescr *firstStaticObjects, *staticObjects;
} traverseState;

/**
 * Callback called when heap traversal visits a closure.
 *
 * The callback can assume that the closure's profiling data has been
 * initialized to zero if this is the first visit during this pass.
 *
 * Returning 'false' will instruct the heap traversal code to skip processing
 * this closure's children. If you don't need to traverse any closure more than
 * once you can simply return 'first_visit'.
 */
typedef bool (*visitClosure_cb) (
    StgClosure *c,
    const StgClosure *cp,
    const stackData data,
    const bool first_visit,
    stackAccum *accum,
    stackData *child_data);

/**
 * Callback called when processing of a closure 'c' is complete, i.e. when
 * all it's children have been processed. Note: This includes leaf nodes
 * without children.
 *
 * @param c     The closure who's processing just completed.
 * @param acc   The current value of the accumulator for 'c' on the
 *              stack. It's about to be removed, hence the 'const'
 *              qualifier. This is the same accumulator 'visit_cb' got
 *              passed when 'c' was visited.
 *
 * @param c_parent    The parent closure of 'c'
 * @param acc_parent  The accumulator associated with 'c_parent', currently
 *                    on the stack.
 */
typedef void (*returnClosure_cb) (
    StgClosure *c,
    const stackAccum acc,
    StgClosure *c_parent,
    stackAccum *acc_parent);

/**
 * Get data stored in closure header. The lowest bit is always returned as
 * zero. The closure must have been visited, i.e. traverseIsClosureDataValid
 * must return true otherwise this is undefined. Note that the closure data is
 * reset to zero on the first visit in this pass by 'traverseWorkStack'.
 */
EXTERN_INLINE StgWord traverseGetClosureData(const StgClosure *c);
EXTERN_INLINE StgWord traverseGetClosureData(const StgClosure *c)
{
    const StgWord hdr = c->header.prof.hp.trav;
    return hdr & (STG_WORD_MAX ^ 1);
}


/**
 * Set the data in the closure header and mark the closure as visited. The
 * lowest bit is reserved for the generic heap traversal code and must not be
 * set.
 */
EXTERN_INLINE void traverseSetClosureData(StgClosure *c, StgWord w);
EXTERN_INLINE void traverseSetClosureData(StgClosure *c, StgWord w)
{
    ASSERT((w & 1) == 0);
    c->header.prof.hp.trav = w | 1;
}


/**
 * Check if closure was visited in the current pass, i.e. if the data in the
 * closure header is valid. All newly-allocated closures start out as
 * "unvisited". Setting the closure data with traverseSetClosureData makes them
 * "visited".
 *
 * Resetting of the closures to the "unvisited" state is a coordinated effort
 * between closeTraverseStack() and the heap profiler (ProfHeap.c), which resets
 * all HEAP_ALLOCED closure's headers while closeTraverseStack() handles all
 * static closures.
 */
EXTERN_INLINE bool traverseIsClosureDataValid(const StgClosure *c);
EXTERN_INLINE bool traverseIsClosureDataValid(const StgClosure *c)
{
    return (c->header.prof.hp.trav & 1) == 1;
}


/**
 * Traverse all closures on the traversal work-stack as well as their children,
 * calling 'visit_cb' each time a closure is visited. See 'visitClosure_cb' for
 * details. When all of a closures children have been processed and 'return_cb'
 * is non-NULL 'return_cb' is called. See 'returnClosure_cb' for details.
 */
void traverseWorkStack(
    traverseState *ts,
    visitClosure_cb visit_cb,
    returnClosure_cb return_cb);

void traversePushRoot(traverseState *ts, StgClosure *c, StgClosure *cp, stackData data);
void traversePushClosure(traverseState *ts, StgClosure *c, StgClosure *cp, stackElement *sep, stackData data);

/**
 * Allocate and reset the traversal work-stack.
 */
void initializeTraverseStack(traverseState *ts);

/**
 * Free the traversal work-stack and associated data and reset the closure data
 * for all static objects that were visited during the traversal.
 */
void closeTraverseStack(traverseState *ts);

int getTraverseStackMaxSize(traverseState *ts);


// for GC.c
W_ traverseWorkStackBlocks(traverseState *ts);

#endif /* PROFILING */
