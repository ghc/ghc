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

typedef enum {
    // Object with fixed layout. Keeps an information about that
    // element was processed. (stackPos.next.step)
    posTypeStep,
    // Description of the pointers-first heap object. Keeps information
    // about layout. (stackPos.next.ptrs)
    posTypePtrs,
    // Keeps SRT bitmap (stackPos.next.srt)
    posTypeSRT,
    // Keeps a new object that was not inspected yet. Keeps a parent
    // element (stackPos.next.parent)
    posTypeFresh,
    // This stackElement is empty
    posTypeEmpty
} nextPosType;

typedef union {
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
    struct {
        StgClosure *srt;
    } srt;

    // parent of the current closure, used only when posTypeFresh is set
    StgClosure *cp;
} nextPos;

/**
 * Position pointer into a closure. Determines what the next element to return
 * for a stackElement is.
 */
typedef struct stackPos_ {
    nextPosType type;
    nextPos next;
} stackPos;

typedef union stackData_ {
     /**
      * Most recent retainer for the corresponding closure on the stack.
      */
    retainer c_child_r;
} stackData;

extern const stackData nullStackData;

typedef union stackAccum_ {
    StgWord subtree_sizeW;
} stackAccum;

/**
 * An element of the traversal work-stack. Besides the closure itself this also
 * stores it's parent, associated data and an accumulator.
 *
 * When 'info.type == posTypeFresh' a 'stackElement' represents just one
 * closure, namely 'c' and 'cp' being it's parent. Otherwise 'info' specifies an
 * offset into the children of 'c'. This is to support returning a closure's
 * children one-by-one without pushing one element per child onto the stack. See
 * traverseGetChildren() and traversePop().
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
    /** Note [Profiling heap traversal visited bit]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * If the RTS is compiled with profiling enabled StgProfHeader can be used
     * by profiling code to store per-heap object information. Specifically the
     * 'hp_hdr' field is used to store heap profiling information.
     *
     * The generic heap traversal code reserves the least significant bit of the
     * heap profiling word to decide whether we've already visited a given
     * closure in the current pass or not. The rest of the field is free to be
     * used by the calling profiler.
     *
     * By doing things this way we implicitly assume that the LSB is not used by
     * the user. This is true at least for the word aligned pointers which the
     * retainer profiler currently stores there and should be maintained by new
     * users for example by shifting the real data up by one bit.
     *
     * Since we don't want to have to scan the entire heap a second time just to
     * reset the per-object visited bit before/after the real traversal we make
     * the interpretation of this bit dependent on the value of a global
     * variable, 'flip' and "flip" this variable when we want to invalidate all
     * objects.
     *
     * When the visited bit is equal to the value of 'flip' the closure data is
     * valid otherwise not (see isTravDataValid). Both the value of the closure
     * and global 'flip' value start out as zero, so all closures are considered
     * valid. Before every traversal we invert the value of 'flip' (see
     * traverseInvalidateClosureData) invalidating all closures.
     *
     * There are some complications with this approach, namely: static objects
     * and mutable data. There we do just go over all existing objects to reset
     * the bit manually. See 'resetStaticObjectForProfiling' and
     * 'resetMutableObjects'.
     */
    StgWord flip;

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
    void (*return_cb)(StgClosure *c, const stackAccum acc,
                      StgClosure *c_parent, stackAccum *acc_parent);
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
    stackAccum *accum,
    stackData *child_data);

StgWord getTravData(const StgClosure *c);
void setTravData(const traverseState *ts, StgClosure *c, StgWord w);
bool isTravDataValid(const traverseState *ts, const StgClosure *c);

void traverseWorkStack(traverseState *ts, visitClosure_cb visit_cb);
void traversePushRoot(traverseState *ts, StgClosure *c, StgClosure *cp, stackData data);
void traversePushClosure(traverseState *ts, StgClosure *c, StgClosure *cp, stackElement *sep, stackData data);
bool traverseMaybeInitClosureData(const traverseState* ts, StgClosure *c);
void traverseInvalidateClosureData(traverseState* ts);

void initializeTraverseStack(traverseState *ts);
void closeTraverseStack(traverseState *ts);
int getTraverseStackMaxSize(traverseState *ts);

// for GC.c
W_ traverseWorkStackBlocks(traverseState *ts);
void resetStaticObjectForProfiling(const traverseState *ts, StgClosure *static_objects);

#include "EndPrivate.h"

#endif /* PROFILING */
