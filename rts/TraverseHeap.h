/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019
 * Author: Daniel Gröber
 *
 * Generalised profiling heap traversal.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(PROFILING)

#include <rts/Types.h>
#include "RetainerSet.h"

#include "BeginPrivate.h"

void resetStaticObjectForProfiling(StgClosure *static_objects);

/* See Note [Profiling heap traversal visited bit]. */
extern StgWord flip;

#define isTravDataValid(c) \
  ((((StgWord)(c)->header.prof.hp.trav.lsb & 1) ^ flip) == 0)

typedef struct traverseState_ traverseState;

typedef union stackData_ {
     /**
      * Most recent retainer for the corresponding closure on the stack.
      */
    retainer c_child_r;
} stackData;

typedef struct stackElement_ stackElement;

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
     *   stackSize is just an estimate measure of the depth of the graph. The
     *   reason is that some heap objects have only a single child and may not
     *   result in a new element being pushed onto the stack. Therefore, at the
     *   end of retainer profiling, maxStackSize is some value no greater than
     *   the actual depth of the graph.
     */
    int stackSize, maxStackSize;
} traverseState;

/**
 * Callback called when heap traversal visits a closure.
 *
 * The callback can assume that the closure's profiling data has been
 * initialized to zero if this is the first visit during a pass.
 *
 * See Note [Profiling heap traversal visited bit].
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
    stackData *child_data);

void traverseWorkStack(traverseState *ts, visitClosure_cb visit_cb);
void traversePushClosure(traverseState *ts, StgClosure *c, StgClosure *cp, stackData data);
bool traverseMaybeInitClosureData(StgClosure *c);

void initializeTraverseStack(traverseState *ts);
void closeTraverseStack(traverseState *ts);
int getTraverseStackMaxSize(traverseState *ts);

W_ traverseWorkStackBlocks(traverseState *ts);

#include "EndPrivate.h"

#endif /* PROFILING */
