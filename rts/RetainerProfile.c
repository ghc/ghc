/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer profiling.
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

// Turn off inlining when debugging - it obfuscates things
#if defined(DEBUG)
#define INLINE
#else
#define INLINE inline
#endif

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "RetainerProfile.h"
#include "RetainerSet.h"
#include "Schedule.h"
#include "Printer.h"
#include "Weak.h"
#include "sm/Sanity.h"
#include "Profiling.h"
#include "Stats.h"
#include "ProfHeap.h"
#include "Apply.h"
#include "StablePtr.h" /* markStablePtrTable */
#include "StableName.h" /* rememberOldStableNameAddresses */
#include "sm/Storage.h" // for END_OF_STATIC_LIST

/* Note [What is a retainer?]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
The definition of what sorts of things are counted as retainers is a bit hard to
pin down. Intuitively, we want to identify closures which will help the user
identify memory leaks due to thunks. In practice we also end up lumping mutable
objects in this group for reasons that have been lost to time.

The definition of retainer is implemented in isRetainer(), defined later in this
file.
*/


/*
  Note: what to change in order to plug-in a new retainer profiling scheme?
    (1) type retainer in ../includes/StgRetainerProf.h
    (2) retainer function R(), i.e., getRetainerFrom()
    (3) the two hashing functions, hashKeySingleton() and hashKeyAddElement(),
        in RetainerSet.h, if needed.
    (4) printRetainer() and printRetainerSetShort() in RetainerSet.c.
 */

/* -----------------------------------------------------------------------------
 * Declarations...
 * -------------------------------------------------------------------------- */

static uint32_t retainerGeneration;  // generation

static uint32_t numObjectVisited;    // total number of objects visited
static uint32_t timesAnyObjectVisited;  // number of times any objects are
                                        // visited

/*
  The rs field in the profile header of any object points to its retainer
  set in an indirect way: if flip is 0, it points to the retainer set;
  if flip is 1, it points to the next byte after the retainer set (even
  for NULL pointers). Therefore, with flip 1, (rs ^ 1) is the actual
  pointer. See retainerSetOf().
 */

StgWord flip = 0;     // flip bit
                      // must be 0 if DEBUG_RETAINER is on (for static closures)

#define setRetainerSetToNull(c)   \
  (c)->header.prof.hp.rs = (RetainerSet *)((StgWord)NULL | flip)

static void retainStack(StgClosure *, retainer, StgPtr, StgPtr);
static void retainClosure(StgClosure *, StgClosure *, retainer);
#if defined(DEBUG_RETAINER)
static void belongToHeap(StgPtr p);
#endif

#if defined(DEBUG_RETAINER)
/*
  cStackSize records how many times retainStack() has been invoked recursively,
  that is, the number of activation records for retainStack() on the C stack.
  maxCStackSize records its max value.
  Invariants:
    cStackSize <= maxCStackSize
 */
static uint32_t cStackSize, maxCStackSize;

static uint32_t sumOfNewCost;        // sum of the cost of each object, computed
                                // when the object is first visited
static uint32_t sumOfNewCostExtra;   // for those objects not visited during
                                // retainer profiling, e.g., MUT_VAR
static uint32_t costArray[N_CLOSURE_TYPES];

uint32_t sumOfCostLinear;            // sum of the costs of all object, computed
                                // when linearly traversing the heap after
                                // retainer profiling
uint32_t costArrayLinear[N_CLOSURE_TYPES];
#endif

/* -----------------------------------------------------------------------------
 * Retainer stack - header
 *   Note:
 *     Although the retainer stack implementation could be separated *
 *     from the retainer profiling engine, there does not seem to be
 *     any advantage in doing that; retainer stack is an integral part
 *     of retainer profiling engine and cannot be use elsewhere at
 *     all.
 * -------------------------------------------------------------------------- */

typedef enum {
    posTypeStep,
    posTypePtrs,
    posTypeSRT,
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
} nextPos;

typedef struct {
    nextPosType type;
    nextPos next;
} stackPos;

typedef struct {
    StgClosure *c;
    retainer c_child_r;
    stackPos info;
} stackElement;

/*
  Invariants:
    firstStack points to the first block group.
    currentStack points to the block group currently being used.
    currentStack->free == stackLimit.
    stackTop points to the topmost byte in the stack of currentStack.
    Unless the whole stack is empty, stackTop must point to the topmost
    object (or byte) in the whole stack. Thus, it is only when the whole stack
    is empty that stackTop == stackLimit (not during the execution of push()
    and pop()).
    stackBottom == currentStack->start.
    stackLimit == currentStack->start + BLOCK_SIZE_W * currentStack->blocks.
  Note:
    When a current stack becomes empty, stackTop is set to point to
    the topmost element on the previous block group so as to satisfy
    the invariants described above.
 */
static bdescr *firstStack = NULL;
static bdescr *currentStack;
static stackElement *stackBottom, *stackTop, *stackLimit;

/*
  currentStackBoundary is used to mark the current stack chunk.
  If stackTop == currentStackBoundary, it means that the current stack chunk
  is empty. It is the responsibility of the user to keep currentStackBoundary
  valid all the time if it is to be employed.
 */
static stackElement *currentStackBoundary;

/*
  stackSize records the current size of the stack.
  maxStackSize records its high water mark.
  Invariants:
    stackSize <= maxStackSize
  Note:
    stackSize is just an estimate measure of the depth of the graph. The reason
    is that some heap objects have only a single child and may not result
    in a new element being pushed onto the stack. Therefore, at the end of
    retainer profiling, maxStackSize + maxCStackSize is some value no greater
    than the actual depth of the graph.
 */
#if defined(DEBUG_RETAINER)
static int stackSize, maxStackSize;
#endif

// number of blocks allocated for one stack
#define BLOCKS_IN_STACK 1

/* -----------------------------------------------------------------------------
 * Add a new block group to the stack.
 * Invariants:
 *  currentStack->link == s.
 * -------------------------------------------------------------------------- */
static INLINE void
newStackBlock( bdescr *bd )
{
    currentStack = bd;
    stackTop     = (stackElement *)(bd->start + BLOCK_SIZE_W * bd->blocks);
    stackBottom  = (stackElement *)bd->start;
    stackLimit   = (stackElement *)stackTop;
    bd->free     = (StgPtr)stackLimit;
}

/* -----------------------------------------------------------------------------
 * Return to the previous block group.
 * Invariants:
 *   s->link == currentStack.
 * -------------------------------------------------------------------------- */
static INLINE void
returnToOldStack( bdescr *bd )
{
    currentStack = bd;
    stackTop = (stackElement *)bd->free;
    stackBottom = (stackElement *)bd->start;
    stackLimit = (stackElement *)(bd->start + BLOCK_SIZE_W * bd->blocks);
    bd->free = (StgPtr)stackLimit;
}

/* -----------------------------------------------------------------------------
 *  Initializes the traverse stack.
 * -------------------------------------------------------------------------- */
static void
initializeTraverseStack( void )
{
    if (firstStack != NULL) {
        freeChain(firstStack);
    }

    firstStack = allocGroup(BLOCKS_IN_STACK);
    firstStack->link = NULL;
    firstStack->u.back = NULL;

    newStackBlock(firstStack);
}

/* -----------------------------------------------------------------------------
 * Frees all the block groups in the traverse stack.
 * Invariants:
 *   firstStack != NULL
 * -------------------------------------------------------------------------- */
static void
closeTraverseStack( void )
{
    freeChain(firstStack);
    firstStack = NULL;
}

/* -----------------------------------------------------------------------------
 * Returns true if the whole stack is empty.
 * -------------------------------------------------------------------------- */
static INLINE bool
isEmptyRetainerStack( void )
{
    return (firstStack == currentStack) && stackTop == stackLimit;
}

/* -----------------------------------------------------------------------------
 * Returns size of stack
 * -------------------------------------------------------------------------- */
W_
retainerStackBlocks( void )
{
    bdescr* bd;
    W_ res = 0;

    for (bd = firstStack; bd != NULL; bd = bd->link)
      res += bd->blocks;

    return res;
}

/* -----------------------------------------------------------------------------
 * Returns true if stackTop is at the stack boundary of the current stack,
 * i.e., if the current stack chunk is empty.
 * -------------------------------------------------------------------------- */
static INLINE bool
isOnBoundary( void )
{
    return stackTop == currentStackBoundary;
}

/* -----------------------------------------------------------------------------
 * Initializes *info from ptrs and payload.
 * Invariants:
 *   payload[] begins with ptrs pointers followed by non-pointers.
 * -------------------------------------------------------------------------- */
static INLINE void
init_ptrs( stackPos *info, uint32_t ptrs, StgPtr payload )
{
    info->type              = posTypePtrs;
    info->next.ptrs.pos     = 0;
    info->next.ptrs.ptrs    = ptrs;
    info->next.ptrs.payload = payload;
}

/* -----------------------------------------------------------------------------
 * Find the next object from *info.
 * -------------------------------------------------------------------------- */
static INLINE StgClosure *
find_ptrs( stackPos *info )
{
    if (info->next.ptrs.pos < info->next.ptrs.ptrs) {
        return (StgClosure *)info->next.ptrs.payload[info->next.ptrs.pos++];
    } else {
        return NULL;
    }
}

/* -----------------------------------------------------------------------------
 *  Initializes *info from SRT information stored in *infoTable.
 * -------------------------------------------------------------------------- */
static INLINE void
init_srt_fun( stackPos *info, const StgFunInfoTable *infoTable )
{
    info->type = posTypeSRT;
    if (infoTable->i.srt) {
        info->next.srt.srt = (StgClosure*)GET_FUN_SRT(infoTable);
    } else {
        info->next.srt.srt = NULL;
    }
}

static INLINE void
init_srt_thunk( stackPos *info, const StgThunkInfoTable *infoTable )
{
    info->type = posTypeSRT;
    if (infoTable->i.srt) {
        info->next.srt.srt = (StgClosure*)GET_SRT(infoTable);
    } else {
        info->next.srt.srt = NULL;
    }
}

/* -----------------------------------------------------------------------------
 * Find the next object from *info.
 * -------------------------------------------------------------------------- */
static INLINE StgClosure *
find_srt( stackPos *info )
{
    StgClosure *c;
    if (info->type == posTypeSRT) {
        c = info->next.srt.srt;
        info->next.srt.srt = NULL;
        return c;
    }
}

/* -----------------------------------------------------------------------------
 *  push() pushes a stackElement representing the next child of *c
 *  onto the traverse stack. If *c has no child, *first_child is set
 *  to NULL and nothing is pushed onto the stack. If *c has only one
 *  child, *c_child is set to that child and nothing is pushed onto
 *  the stack.  If *c has more than two children, *first_child is set
 *  to the first child and a stackElement representing the second
 *  child is pushed onto the stack.

 *  Invariants:
 *     *c_child_r is the most recent retainer of *c's children.
 *     *c is not any of TSO, AP, PAP, AP_STACK, which means that
 *        there cannot be any stack objects.
 *  Note: SRTs are considered to  be children as well.
 * -------------------------------------------------------------------------- */
static INLINE void
push( StgClosure *c, retainer c_child_r, StgClosure **first_child )
{
    stackElement se;
    bdescr *nbd;      // Next Block Descriptor

#if defined(DEBUG_RETAINER)
    // debugBelch("push(): stackTop = 0x%x, currentStackBoundary = 0x%x\n", stackTop, currentStackBoundary);
#endif

    ASSERT(get_itbl(c)->type != TSO);
    ASSERT(get_itbl(c)->type != AP_STACK);

    //
    // fill in se
    //

    se.c = c;
    se.c_child_r = c_child_r;

    // fill in se.info
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

        // For CONSTR_2_0 and MVAR, we use se.info.step to record the position
        // of the next child. We do not write a separate initialization code.
        // Also we do not have to initialize info.type;

        // two children (fixed), no SRT
        // need to push a stackElement, but nothing to store in se.info
    case CONSTR_2_0:
        *first_child = c->payload[0];         // return the first pointer
        // se.info.type = posTypeStep;
        // se.info.next.step = 2;            // 2 = second
        break;

        // three children (fixed), no SRT
        // need to push a stackElement
    case MVAR_CLEAN:
    case MVAR_DIRTY:
        // head must be TSO and the head of a linked list of TSOs.
        // Shoule it be a child? Seems to be yes.
        *first_child = (StgClosure *)((StgMVar *)c)->head;
        // se.info.type = posTypeStep;
        se.info.next.step = 2;            // 2 = second
        break;

        // three children (fixed), no SRT
    case WEAK:
        *first_child = ((StgWeak *)c)->key;
        // se.info.type = posTypeStep;
        se.info.next.step = 2;
        break;

        // layout.payload.ptrs, no SRT
    case TVAR:
    case CONSTR:
    case CONSTR_NOCAF:
    case PRIM:
    case MUT_PRIM:
    case BCO:
        init_ptrs(&se.info, get_itbl(c)->layout.payload.ptrs,
                  (StgPtr)c->payload);
        *first_child = find_ptrs(&se.info);
        if (*first_child == NULL)
            return;   // no child
        break;

        // StgMutArrPtr.ptrs, no SRT
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        init_ptrs(&se.info, ((StgMutArrPtrs *)c)->ptrs,
                  (StgPtr)(((StgMutArrPtrs *)c)->payload));
        *first_child = find_ptrs(&se.info);
        if (*first_child == NULL)
            return;
        break;

        // StgMutArrPtr.ptrs, no SRT
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        init_ptrs(&se.info, ((StgSmallMutArrPtrs *)c)->ptrs,
                  (StgPtr)(((StgSmallMutArrPtrs *)c)->payload));
        *first_child = find_ptrs(&se.info);
        if (*first_child == NULL)
            return;
        break;

    // layout.payload.ptrs, SRT
    case FUN_STATIC:
    case FUN:           // *c is a heap object.
    case FUN_2_0:
        init_ptrs(&se.info, get_itbl(c)->layout.payload.ptrs, (StgPtr)c->payload);
        *first_child = find_ptrs(&se.info);
        if (*first_child == NULL)
            // no child from ptrs, so check SRT
            goto fun_srt_only;
        break;

    case THUNK:
    case THUNK_2_0:
        init_ptrs(&se.info, get_itbl(c)->layout.payload.ptrs,
                  (StgPtr)((StgThunk *)c)->payload);
        *first_child = find_ptrs(&se.info);
        if (*first_child == NULL)
            // no child from ptrs, so check SRT
            goto thunk_srt_only;
        break;

        // 1 fixed child, SRT
    case FUN_1_0:
    case FUN_1_1:
        *first_child = c->payload[0];
        ASSERT(*first_child != NULL);
        init_srt_fun(&se.info, get_fun_itbl(c));
        break;

    case THUNK_1_0:
    case THUNK_1_1:
        *first_child = ((StgThunk *)c)->payload[0];
        ASSERT(*first_child != NULL);
        init_srt_thunk(&se.info, get_thunk_itbl(c));
        break;

    case FUN_0_1:      // *c is a heap object.
    case FUN_0_2:
    fun_srt_only:
        init_srt_fun(&se.info, get_fun_itbl(c));
        *first_child = find_srt(&se.info);
        if (*first_child == NULL)
            return;     // no child
        break;

    // SRT only
    case THUNK_STATIC:
        ASSERT(get_itbl(c)->srt != 0);
    case THUNK_0_1:
    case THUNK_0_2:
    thunk_srt_only:
        init_srt_thunk(&se.info, get_thunk_itbl(c));
        *first_child = find_srt(&se.info);
        if (*first_child == NULL)
            return;     // no child
        break;

    case TREC_CHUNK:
        *first_child = (StgClosure *)((StgTRecChunk *)c)->prev_chunk;
        se.info.next.step = 0;  // entry no.
        break;

        // cannot appear
    case PAP:
    case AP:
    case AP_STACK:
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
        // invalid objects
    case IND:
    case INVALID_OBJECT:
    default:
        barf("Invalid object *c in push(): %d", get_itbl(c)->type);
        return;
    }

    if (stackTop - 1 < stackBottom) {
#if defined(DEBUG_RETAINER)
        // debugBelch("push() to the next stack.\n");
#endif
        // currentStack->free is updated when the active stack is switched
        // to the next stack.
        currentStack->free = (StgPtr)stackTop;

        if (currentStack->link == NULL) {
            nbd = allocGroup(BLOCKS_IN_STACK);
            nbd->link = NULL;
            nbd->u.back = currentStack;
            currentStack->link = nbd;
        } else
            nbd = currentStack->link;

        newStackBlock(nbd);
    }

    // adjust stackTop (acutal push)
    stackTop--;
    // If the size of stackElement was huge, we would better replace the
    // following statement by either a memcpy() call or a switch statement
    // on the type of the element. Currently, the size of stackElement is
    // small enough (5 words) that this direct assignment seems to be enough.

    // ToDo: The line below leads to the warning:
    //    warning: 'se.info.type' may be used uninitialized in this function
    // This is caused by the fact that there are execution paths through the
    // large switch statement above where some cases do not initialize this
    // field. Is this really harmless? Can we avoid the warning?
    *stackTop = se;

#if defined(DEBUG_RETAINER)
    stackSize++;
    if (stackSize > maxStackSize) maxStackSize = stackSize;
    // ASSERT(stackSize >= 0);
    // debugBelch("stackSize = %d\n", stackSize);
#endif
}

/* -----------------------------------------------------------------------------
 *  popOff() and popOffReal(): Pop a stackElement off the traverse stack.
 *  Invariants:
 *    stackTop cannot be equal to stackLimit unless the whole stack is
 *    empty, in which case popOff() is not allowed.
 *  Note:
 *    You can think of popOffReal() as a part of popOff() which is
 *    executed at the end of popOff() in necessary. Since popOff() is
 *    likely to be executed quite often while popOffReal() is not, we
 *    separate popOffReal() from popOff(), which is declared as an
 *    INLINE function (for the sake of execution speed).  popOffReal()
 *    is called only within popOff() and nowhere else.
 * -------------------------------------------------------------------------- */
static void
popOffReal(void)
{
    bdescr *pbd;    // Previous Block Descriptor

#if defined(DEBUG_RETAINER)
    // debugBelch("pop() to the previous stack.\n");
#endif

    ASSERT(stackTop + 1 == stackLimit);
    ASSERT(stackBottom == (stackElement *)currentStack->start);

    if (firstStack == currentStack) {
        // The stack is completely empty.
        stackTop++;
        ASSERT(stackTop == stackLimit);
#if defined(DEBUG_RETAINER)
        stackSize--;
        if (stackSize > maxStackSize) maxStackSize = stackSize;
        /*
          ASSERT(stackSize >= 0);
          debugBelch("stackSize = %d\n", stackSize);
        */
#endif
        return;
    }

    // currentStack->free is updated when the active stack is switched back
    // to the previous stack.
    currentStack->free = (StgPtr)stackLimit;

    // find the previous block descriptor
    pbd = currentStack->u.back;
    ASSERT(pbd != NULL);

    returnToOldStack(pbd);

#if defined(DEBUG_RETAINER)
    stackSize--;
    if (stackSize > maxStackSize) maxStackSize = stackSize;
    /*
      ASSERT(stackSize >= 0);
      debugBelch("stackSize = %d\n", stackSize);
    */
#endif
}

static INLINE void
popOff(void) {
#if defined(DEBUG_RETAINER)
    // debugBelch("\tpopOff(): stackTop = 0x%x, currentStackBoundary = 0x%x\n", stackTop, currentStackBoundary);
#endif

    ASSERT(stackTop != stackLimit);
    ASSERT(!isEmptyRetainerStack());

    // <= (instead of <) is wrong!
    if (stackTop + 1 < stackLimit) {
        stackTop++;
#if defined(DEBUG_RETAINER)
        stackSize--;
        if (stackSize > maxStackSize) maxStackSize = stackSize;
        /*
          ASSERT(stackSize >= 0);
          debugBelch("stackSize = %d\n", stackSize);
        */
#endif
        return;
    }

    popOffReal();
}

/* -----------------------------------------------------------------------------
 *  Finds the next object to be considered for retainer profiling and store
 *  its pointer to *c.
 *  Test if the topmost stack element indicates that more objects are left,
 *  and if so, retrieve the first object and store its pointer to *c. Also,
 *  set *cp and *r appropriately, both of which are stored in the stack element.
 *  The topmost stack element then is overwritten so as for it to now denote
 *  the next object.
 *  If the topmost stack element indicates no more objects are left, pop
 *  off the stack element until either an object can be retrieved or
 *  the current stack chunk becomes empty, indicated by true returned by
 *  isOnBoundary(), in which case *c is set to NULL.
 *  Note:
 *    It is okay to call this function even when the current stack chunk
 *    is empty.
 * -------------------------------------------------------------------------- */
static INLINE void
pop( StgClosure **c, StgClosure **cp, retainer *r )
{
    stackElement *se;

#if defined(DEBUG_RETAINER)
    // debugBelch("pop(): stackTop = 0x%x, currentStackBoundary = 0x%x\n", stackTop, currentStackBoundary);
#endif

    do {
        if (isOnBoundary()) {     // if the current stack chunk is depleted
            *c = NULL;
            return;
        }

        se = stackTop;

        switch (get_itbl(se->c)->type) {
            // two children (fixed), no SRT
            // nothing in se.info
        case CONSTR_2_0:
            *c = se->c->payload[1];
            *cp = se->c;
            *r = se->c_child_r;
            popOff();
            return;

            // three children (fixed), no SRT
            // need to push a stackElement
        case MVAR_CLEAN:
        case MVAR_DIRTY:
            if (se->info.next.step == 2) {
                *c = (StgClosure *)((StgMVar *)se->c)->tail;
                se->info.next.step++;             // move to the next step
                // no popOff
            } else {
                *c = ((StgMVar *)se->c)->value;
                popOff();
            }
            *cp = se->c;
            *r = se->c_child_r;
            return;

            // three children (fixed), no SRT
        case WEAK:
            if (se->info.next.step == 2) {
                *c = ((StgWeak *)se->c)->value;
                se->info.next.step++;
                // no popOff
            } else {
                *c = ((StgWeak *)se->c)->finalizer;
                popOff();
            }
            *cp = se->c;
            *r = se->c_child_r;
            return;

        case TREC_CHUNK: {
            // These are pretty complicated: we have N entries, each
            // of which contains 3 fields that we want to follow.  So
            // we divide the step counter: the 2 low bits indicate
            // which field, and the rest of the bits indicate the
            // entry number (starting from zero).
            TRecEntry *entry;
            uint32_t entry_no = se->info.next.step >> 2;
            uint32_t field_no = se->info.next.step & 3;
            if (entry_no == ((StgTRecChunk *)se->c)->next_entry_idx) {
                *c = NULL;
                popOff();
                return;
            }
            entry = &((StgTRecChunk *)se->c)->entries[entry_no];
            if (field_no == 0) {
                *c = (StgClosure *)entry->tvar;
            } else if (field_no == 1) {
                *c = entry->expected_value;
            } else {
                *c = entry->new_value;
            }
            *cp = se->c;
            *r = se->c_child_r;
            se->info.next.step++;
            return;
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
                popOff();
                break;
            }
            *cp = se->c;
            *r = se->c_child_r;
            return;

            // layout.payload.ptrs, SRT
        case FUN:         // always a heap object
        case FUN_STATIC:
        case FUN_2_0:
            if (se->info.type == posTypePtrs) {
                *c = find_ptrs(&se->info);
                if (*c != NULL) {
                    *cp = se->c;
                    *r = se->c_child_r;
                    return;
                }
                init_srt_fun(&se->info, get_fun_itbl(se->c));
            }
            goto do_srt;

        case THUNK:
        case THUNK_2_0:
            if (se->info.type == posTypePtrs) {
                *c = find_ptrs(&se->info);
                if (*c != NULL) {
                    *cp = se->c;
                    *r = se->c_child_r;
                    return;
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
            if (*c != NULL) {
                *cp = se->c;
                *r = se->c_child_r;
                return;
            }
            popOff();
            break;

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
            // invalid objects
        case IND:
        case INVALID_OBJECT:
        default:
            barf("Invalid object *c in pop(): %d", get_itbl(se->c)->type);
            return;
        }
    } while (true);
}

/* -----------------------------------------------------------------------------
 * RETAINER PROFILING ENGINE
 * -------------------------------------------------------------------------- */

void
initRetainerProfiling( void )
{
    initializeAllRetainerSet();
    retainerGeneration = 0;
}

/* -----------------------------------------------------------------------------
 *  This function must be called before f-closing prof_file.
 * -------------------------------------------------------------------------- */
void
endRetainerProfiling( void )
{
#if defined(SECOND_APPROACH)
    outputAllRetainerSet(prof_file);
#endif
}

/* -----------------------------------------------------------------------------
 *  Returns the actual pointer to the retainer set of the closure *c.
 *  It may adjust RSET(c) subject to flip.
 *  Side effects:
 *    RSET(c) is initialized to NULL if its current value does not
 *    conform to flip.
 *  Note:
 *    Even though this function has side effects, they CAN be ignored because
 *    subsequent calls to retainerSetOf() always result in the same return value
 *    and retainerSetOf() is the only way to retrieve retainerSet of a given
 *    closure.
 *    We have to perform an XOR (^) operation each time a closure is examined.
 *    The reason is that we do not know when a closure is visited last.
 * -------------------------------------------------------------------------- */
static INLINE void
maybeInitRetainerSet( StgClosure *c )
{
    if (!isRetainerSetFieldValid(c)) {
        setRetainerSetToNull(c);
    }
}

/* -----------------------------------------------------------------------------
 * Returns true if *c is a retainer.
 * -------------------------------------------------------------------------- */
static INLINE bool
isRetainer( StgClosure *c )
{
    switch (get_itbl(c)->type) {
        //
        //  True case
        //
        // TSOs MUST be retainers: they constitute the set of roots.
    case TSO:
    case STACK:

        // mutable objects
    case MUT_PRIM:
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    case TVAR:
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case BLOCKING_QUEUE:

        // thunks are retainers.
    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_2_0:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_SELECTOR:
    case AP:
    case AP_STACK:

        // Static thunks, or CAFS, are obviously retainers.
    case THUNK_STATIC:

        // WEAK objects are roots; there is separate code in which traversing
        // begins from WEAK objects.
    case WEAK:
        return true;

        //
        // False case
        //

        // constructors
    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
        // functions
    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_2_0:
    case FUN_1_1:
    case FUN_0_2:
        // partial applications
    case PAP:
        // indirection
    // IND_STATIC used to be an error, but at the moment it can happen
    // as isAlive doesn't look through IND_STATIC as it ignores static
    // closures. See trac #3956 for a program that hit this error.
    case IND_STATIC:
    case BLACKHOLE:
    case WHITEHOLE:
        // static objects
    case FUN_STATIC:
        // misc
    case PRIM:
    case BCO:
    case ARR_WORDS:
    case COMPACT_NFDATA:
        // STM
    case TREC_CHUNK:
        // immutable arrays
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        return false;

        //
        // Error case
        //
        // Stack objects are invalid because they are never treated as
        // legal objects during retainer profiling.
    case UPDATE_FRAME:
    case CATCH_FRAME:
    case CATCH_RETRY_FRAME:
    case CATCH_STM_FRAME:
    case UNDERFLOW_FRAME:
    case ATOMICALLY_FRAME:
    case STOP_FRAME:
    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case RET_FUN:
        // other cases
    case IND:
    case INVALID_OBJECT:
    default:
        barf("Invalid object in isRetainer(): %d", get_itbl(c)->type);
        return false;
    }
}

/* -----------------------------------------------------------------------------
 *  Returns the retainer function value for the closure *c, i.e., R(*c).
 *  This function does NOT return the retainer(s) of *c.
 *  Invariants:
 *    *c must be a retainer.
 *  Note:
 *    Depending on the definition of this function, the maintenance of retainer
 *    sets can be made easier. If most retainer sets are likely to be created
 *    again across garbage collections, refreshAllRetainerSet() in
 *    RetainerSet.c can simply do nothing.
 *    If this is not the case, we can free all the retainer sets and
 *    re-initialize the hash table.
 *    See refreshAllRetainerSet() in RetainerSet.c.
 * -------------------------------------------------------------------------- */
static INLINE retainer
getRetainerFrom( StgClosure *c )
{
    ASSERT(isRetainer(c));

    return c->header.prof.ccs;
}

/* -----------------------------------------------------------------------------
 *  Associates the retainer set *s with the closure *c, that is, *s becomes
 *  the retainer set of *c.
 *  Invariants:
 *    c != NULL
 *    s != NULL
 * -------------------------------------------------------------------------- */
static INLINE void
associate( StgClosure *c, RetainerSet *s )
{
    // StgWord has the same size as pointers, so the following type
    // casting is okay.
    RSET(c) = (RetainerSet *)((StgWord)s | flip);
}

/* -----------------------------------------------------------------------------
   Call retainClosure for each of the closures covered by a large bitmap.
   -------------------------------------------------------------------------- */

static void
retain_large_bitmap (StgPtr p, StgLargeBitmap *large_bitmap, uint32_t size,
                     StgClosure *c, retainer c_child_r)
{
    uint32_t i, b;
    StgWord bitmap;

    b = 0;
    bitmap = large_bitmap->bitmap[b];
    for (i = 0; i < size; ) {
        if ((bitmap & 1) == 0) {
            retainClosure((StgClosure *)*p, c, c_child_r);
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

static INLINE StgPtr
retain_small_bitmap (StgPtr p, uint32_t size, StgWord bitmap,
                     StgClosure *c, retainer c_child_r)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            retainClosure((StgClosure *)*p, c, c_child_r);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

/* -----------------------------------------------------------------------------
 *  Process all the objects in the stack chunk from stackStart to stackEnd
 *  with *c and *c_child_r being their parent and their most recent retainer,
 *  respectively. Treat stackOptionalFun as another child of *c if it is
 *  not NULL.
 *  Invariants:
 *    *c is one of the following: TSO, AP_STACK.
 *    If *c is TSO, c == c_child_r.
 *    stackStart < stackEnd.
 *    RSET(c) and RSET(c_child_r) are valid, i.e., their
 *    interpretation conforms to the current value of flip (even when they
 *    are interpreted to be NULL).
 *    If *c is TSO, its state is not ThreadComplete,or ThreadKilled,
 *    which means that its stack is ready to process.
 *  Note:
 *    This code was almost plagiarzied from GC.c! For each pointer,
 *    retainClosure() is invoked instead of evacuate().
 * -------------------------------------------------------------------------- */
static void
retainStack( StgClosure *c, retainer c_child_r,
             StgPtr stackStart, StgPtr stackEnd )
{
    stackElement *oldStackBoundary;
    StgPtr p;
    const StgRetInfoTable *info;
    StgWord bitmap;
    uint32_t size;

#if defined(DEBUG_RETAINER)
    cStackSize++;
    if (cStackSize > maxCStackSize) maxCStackSize = cStackSize;
#endif

    /*
      Each invocation of retainStack() creates a new virtual
      stack. Since all such stacks share a single common stack, we
      record the current currentStackBoundary, which will be restored
      at the exit.
    */
    oldStackBoundary = currentStackBoundary;
    currentStackBoundary = stackTop;

#if defined(DEBUG_RETAINER)
    // debugBelch("retainStack() called: oldStackBoundary = 0x%x, currentStackBoundary = 0x%x\n", oldStackBoundary, currentStackBoundary);
#endif

    ASSERT(get_itbl(c)->type == STACK);

    p = stackStart;
    while (p < stackEnd) {
        info = get_ret_itbl((StgClosure *)p);

        switch(info->i.type) {

        case UPDATE_FRAME:
            retainClosure(((StgUpdateFrame *)p)->updatee, c, c_child_r);
            p += sizeofW(StgUpdateFrame);
            continue;

        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case CATCH_FRAME:
        case CATCH_STM_FRAME:
        case CATCH_RETRY_FRAME:
        case ATOMICALLY_FRAME:
        case RET_SMALL:
            bitmap = BITMAP_BITS(info->i.layout.bitmap);
            size   = BITMAP_SIZE(info->i.layout.bitmap);
            p++;
            p = retain_small_bitmap(p, size, bitmap, c, c_child_r);

        follow_srt:
            if (info->i.srt) {
                retainClosure(GET_SRT(info),c,c_child_r);
            }
            continue;

        case RET_BCO: {
            StgBCO *bco;

            p++;
            retainClosure((StgClosure *)*p, c, c_child_r);
            bco = (StgBCO *)*p;
            p++;
            size = BCO_BITMAP_SIZE(bco);
            retain_large_bitmap(p, BCO_BITMAP(bco), size, c, c_child_r);
            p += size;
            continue;
        }

            // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
        case RET_BIG:
            size = GET_LARGE_BITMAP(&info->i)->size;
            p++;
            retain_large_bitmap(p, GET_LARGE_BITMAP(&info->i),
                                size, c, c_child_r);
            p += size;
            // and don't forget to follow the SRT
            goto follow_srt;

        case RET_FUN: {
            StgRetFun *ret_fun = (StgRetFun *)p;
            const StgFunInfoTable *fun_info;

            retainClosure(ret_fun->fun, c, c_child_r);
            fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(ret_fun->fun));

            p = (P_)&ret_fun->payload;
            switch (fun_info->f.fun_type) {
            case ARG_GEN:
                bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
                size = BITMAP_SIZE(fun_info->f.b.bitmap);
                p = retain_small_bitmap(p, size, bitmap, c, c_child_r);
                break;
            case ARG_GEN_BIG:
                size = GET_FUN_LARGE_BITMAP(fun_info)->size;
                retain_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info),
                                    size, c, c_child_r);
                p += size;
                break;
            default:
                bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
                size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
                p = retain_small_bitmap(p, size, bitmap, c, c_child_r);
                break;
            }
            goto follow_srt;
        }

        default:
            barf("Invalid object found in retainStack(): %d",
                 (int)(info->i.type));
        }
    }

    // restore currentStackBoundary
    currentStackBoundary = oldStackBoundary;
#if defined(DEBUG_RETAINER)
    // debugBelch("retainStack() finished: currentStackBoundary = 0x%x\n", currentStackBoundary);
#endif

#if defined(DEBUG_RETAINER)
    cStackSize--;
#endif
}

/* ----------------------------------------------------------------------------
 * Call retainClosure for each of the children of a PAP/AP
 * ------------------------------------------------------------------------- */

static INLINE StgPtr
retain_PAP_payload (StgClosure *pap,    /* NOT tagged */
                    retainer c_child_r, /* NOT tagged */
                    StgClosure *fun,    /* tagged */
                    StgClosure** payload, StgWord n_args)
{
    StgPtr p;
    StgWord bitmap;
    const StgFunInfoTable *fun_info;

    retainClosure(fun, pap, c_child_r);
    fun = UNTAG_CLOSURE(fun);
    fun_info = get_fun_itbl(fun);
    ASSERT(fun_info->i.type != PAP);

    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        p = retain_small_bitmap(p, n_args, bitmap,
                                pap, c_child_r);
        break;
    case ARG_GEN_BIG:
        retain_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info),
                            n_args, pap, c_child_r);
        p += n_args;
        break;
    case ARG_BCO:
        retain_large_bitmap((StgPtr)payload, BCO_BITMAP(fun),
                            n_args, pap, c_child_r);
        p += n_args;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        p = retain_small_bitmap(p, n_args, bitmap, pap, c_child_r);
        break;
    }
    return p;
}

/* -----------------------------------------------------------------------------
 *  Compute the retainer set of *c0 and all its desecents by traversing.
 *  *cp0 is the parent of *c0, and *r0 is the most recent retainer of *c0.
 *  Invariants:
 *    c0 = cp0 = r0 holds only for root objects.
 *    RSET(cp0) and RSET(r0) are valid, i.e., their
 *    interpretation conforms to the current value of flip (even when they
 *    are interpreted to be NULL).
 *    However, RSET(c0) may be corrupt, i.e., it may not conform to
 *    the current value of flip. If it does not, during the execution
 *    of this function, RSET(c0) must be initialized as well as all
 *    its descendants.
 *  Note:
 *    stackTop must be the same at the beginning and the exit of this function.
 *    *c0 can be TSO (as well as AP_STACK).
 * -------------------------------------------------------------------------- */
static void
retainClosure( StgClosure *c0, StgClosure *cp0, retainer r0 )
{
    // c = Current closure                          (possibly tagged)
    // cp = Current closure's Parent                (NOT tagged)
    // r = current closures' most recent Retainer   (NOT tagged)
    // c_child_r = current closure's children's most recent retainer
    // first_child = first child of c
    StgClosure *c, *cp, *first_child;
    RetainerSet *s, *retainerSetOfc;
    retainer r, c_child_r;
    StgWord typeOfc;

#if defined(DEBUG_RETAINER)
    // StgPtr oldStackTop;
#endif

#if defined(DEBUG_RETAINER)
    // oldStackTop = stackTop;
    // debugBelch("retainClosure() called: c0 = 0x%x, cp0 = 0x%x, r0 = 0x%x\n", c0, cp0, r0);
#endif

    // (c, cp, r) = (c0, cp0, r0)
    c = c0;
    cp = cp0;
    r = r0;
    goto inner_loop;

loop:
    //debugBelch("loop");
    // pop to (c, cp, r);
    pop(&c, &cp, &r);

    if (c == NULL) {
#if defined(DEBUG_RETAINER)
        // debugBelch("retainClosure() ends: oldStackTop = 0x%x, stackTop = 0x%x\n", oldStackTop, stackTop);
#endif
        return;
    }

    //debugBelch("inner_loop");

inner_loop:
    c = UNTAG_CLOSURE(c);

    // c  = current closure under consideration,
    // cp = current closure's parent,
    // r  = current closure's most recent retainer
    //
    // Loop invariants (on the meaning of c, cp, r, and their retainer sets):
    //   RSET(cp) and RSET(r) are valid.
    //   RSET(c) is valid only if c has been visited before.
    //
    // Loop invariants (on the relation between c, cp, and r)
    //   if cp is not a retainer, r belongs to RSET(cp).
    //   if cp is a retainer, r == cp.

    typeOfc = get_itbl(c)->type;

#if defined(DEBUG_RETAINER)
    switch (typeOfc) {
    case IND_STATIC:
    case CONSTR_NOCAF:
    case THUNK_STATIC:
    case FUN_STATIC:
        break;
    default:
        if (retainerSetOf(c) == NULL) {   // first visit?
            costArray[typeOfc] += cost(c);
            sumOfNewCost += cost(c);
        }
        break;
    }
#endif

    // special cases
    switch (typeOfc) {
    case TSO:
        if (((StgTSO *)c)->what_next == ThreadComplete ||
            ((StgTSO *)c)->what_next == ThreadKilled) {
#if defined(DEBUG_RETAINER)
            debugBelch("ThreadComplete or ThreadKilled encountered in retainClosure()\n");
#endif
            goto loop;
        }
        break;

    case IND_STATIC:
        // We just skip IND_STATIC, so its retainer set is never computed.
        c = ((StgIndStatic *)c)->indirectee;
        goto inner_loop;
        // static objects with no pointers out, so goto loop.
    case CONSTR_NOCAF:
        // It is not just enough not to compute the retainer set for *c; it is
        // mandatory because CONSTR_NOCAF are not reachable from
        // scavenged_static_objects, the list from which is assumed to traverse
        // all static objects after major garbage collections.
        goto loop;
    case THUNK_STATIC:
        if (get_itbl(c)->srt == 0) {
            // No need to compute the retainer set; no dynamic objects
            // are reachable from *c.
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
            // resetStaticObjectForRetainerProfiling() from the
            // garbage collector to reset the retainer sets in all the
            // reachable static objects.
            goto loop;
        }
    case FUN_STATIC: {
        StgInfoTable *info = get_itbl(c);
        if (info->srt == 0 && info->layout.payload.ptrs == 0) {
            goto loop;
        } else {
            break;
        }
    }
    default:
        break;
    }

    // The above objects are ignored in computing the average number of times
    // an object is visited.
    timesAnyObjectVisited++;

    // If this is the first visit to c, initialize its retainer set.
    maybeInitRetainerSet(c);
    retainerSetOfc = retainerSetOf(c);

    // Now compute s:
    //    isRetainer(cp) == true => s == NULL
    //    isRetainer(cp) == false => s == cp.retainer
    if (isRetainer(cp))
        s = NULL;
    else
        s = retainerSetOf(cp);

    // (c, cp, r, s) is available.

    // (c, cp, r, s, R_r) is available, so compute the retainer set for *c.
    if (retainerSetOfc == NULL) {
        // This is the first visit to *c.
        numObjectVisited++;

        if (s == NULL)
            associate(c, singleton(r));
        else
            // s is actually the retainer set of *c!
            associate(c, s);

        // compute c_child_r
        c_child_r = isRetainer(c) ? getRetainerFrom(c) : r;
    } else {
        // This is not the first visit to *c.
        if (isMember(r, retainerSetOfc))
            goto loop;          // no need to process child

        if (s == NULL)
            associate(c, addElement(r, retainerSetOfc));
        else {
            // s is not NULL and cp is not a retainer. This means that
            // each time *cp is visited, so is *c. Thus, if s has
            // exactly one more element in its retainer set than c, s
            // is also the new retainer set for *c.
            if (s->num == retainerSetOfc->num + 1) {
                associate(c, s);
            }
            // Otherwise, just add R_r to the current retainer set of *c.
            else {
                associate(c, addElement(r, retainerSetOfc));
            }
        }

        if (isRetainer(c))
            goto loop;          // no need to process child

        // compute c_child_r
        c_child_r = r;
    }

    // now, RSET() of all of *c, *cp, and *r is valid.
    // (c, c_child_r) are available.

    // process child

    // Special case closures: we process these all in one go rather
    // than attempting to save the current position, because doing so
    // would be hard.
    switch (typeOfc) {
    case STACK:
        retainStack(c, c_child_r,
                    ((StgStack *)c)->sp,
                    ((StgStack *)c)->stack + ((StgStack *)c)->stack_size);
        goto loop;

    case TSO:
    {
        StgTSO *tso = (StgTSO *)c;

        retainClosure((StgClosure*) tso->stackobj,           c, c_child_r);
        retainClosure((StgClosure*) tso->blocked_exceptions, c, c_child_r);
        retainClosure((StgClosure*) tso->bq,                 c, c_child_r);
        retainClosure((StgClosure*) tso->trec,               c, c_child_r);
        if (   tso->why_blocked == BlockedOnMVar
               || tso->why_blocked == BlockedOnMVarRead
               || tso->why_blocked == BlockedOnBlackHole
               || tso->why_blocked == BlockedOnMsgThrowTo
            ) {
            retainClosure(tso->block_info.closure, c, c_child_r);
        }
        goto loop;
    }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)c;
        retainClosure((StgClosure*) bq->link,           c, c_child_r);
        retainClosure((StgClosure*) bq->bh,             c, c_child_r);
        retainClosure((StgClosure*) bq->owner,          c, c_child_r);
        goto loop;
    }

    case PAP:
    {
        StgPAP *pap = (StgPAP *)c;
        retain_PAP_payload(c, c_child_r, pap->fun, pap->payload, pap->n_args);
        goto loop;
    }

    case AP:
    {
        StgAP *ap = (StgAP *)c;
        retain_PAP_payload(c, c_child_r, ap->fun, ap->payload, ap->n_args);
        goto loop;
    }

    case AP_STACK:
        retainClosure(((StgAP_STACK *)c)->fun, c, c_child_r);
        retainStack(c, c_child_r,
                    (StgPtr)((StgAP_STACK *)c)->payload,
                    (StgPtr)((StgAP_STACK *)c)->payload +
                             ((StgAP_STACK *)c)->size);
        goto loop;
    }

    push(c, c_child_r, &first_child);

    // If first_child is null, c has no child.
    // If first_child is not null, the top stack element points to the next
    // object. push() may or may not push a stackElement on the stack.
    if (first_child == NULL)
        goto loop;

    // (c, cp, r) = (first_child, c, c_child_r)
    r = c_child_r;
    cp = c;
    c = first_child;
    goto inner_loop;
}

/* -----------------------------------------------------------------------------
 *  Compute the retainer set for every object reachable from *tl.
 * -------------------------------------------------------------------------- */
static void
retainRoot(void *user STG_UNUSED, StgClosure **tl)
{
    StgClosure *c;

    // We no longer assume that only TSOs and WEAKs are roots; any closure can
    // be a root.

    ASSERT(isEmptyRetainerStack());
    currentStackBoundary = stackTop;

    c = UNTAG_CLOSURE(*tl);
    maybeInitRetainerSet(c);
    if (c != &stg_END_TSO_QUEUE_closure && isRetainer(c)) {
        retainClosure(c, c, getRetainerFrom(c));
    } else {
        retainClosure(c, c, CCS_SYSTEM);
    }

    // NOT TRUE: ASSERT(isMember(getRetainerFrom(*tl), retainerSetOf(*tl)));
    // *tl might be a TSO which is ThreadComplete, in which
    // case we ignore it for the purposes of retainer profiling.
}

/* -----------------------------------------------------------------------------
 *  Compute the retainer set for each of the objects in the heap.
 * -------------------------------------------------------------------------- */
static void
computeRetainerSet( void )
{
    StgWeak *weak;
    uint32_t g, n;
    StgPtr ml;
    bdescr *bd;
#if defined(DEBUG_RETAINER)
    RetainerSet *rtl;
    RetainerSet tmpRetainerSet;
#endif

    markCapabilities(retainRoot, NULL); // for scheduler roots

    // This function is called after a major GC, when key, value, and finalizer
    // all are guaranteed to be valid, or reachable.
    //
    // The following code assumes that WEAK objects are considered to be roots
    // for retainer profilng.
    for (n = 0; n < n_capabilities; n++) {
        // NB: after a GC, all nursery weak_ptr_lists have been migrated
        // to the global lists living in the generations
        ASSERT(capabilities[n]->weak_ptr_list_hd == NULL);
        ASSERT(capabilities[n]->weak_ptr_list_tl == NULL);
    }
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (weak = generations[g].weak_ptr_list; weak != NULL; weak = weak->link) {
            // retainRoot((StgClosure *)weak);
            retainRoot(NULL, (StgClosure **)&weak);
        }
    }

    // Consider roots from the stable ptr table.
    markStablePtrTable(retainRoot, NULL);
    // Remember old stable name addresses.
    rememberOldStableNameAddresses ();

    // The following code resets the rs field of each unvisited mutable
    // object (computing sumOfNewCostExtra and updating costArray[] when
    // debugging retainer profiler).
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        // NOT true: even G0 has a block on its mutable list
        // ASSERT(g != 0 || (generations[g].mut_list == NULL));

        // Traversing through mut_list is necessary
        // because we can find MUT_VAR objects which have not been
        // visited during retainer profiling.
        for (n = 0; n < n_capabilities; n++) {
          for (bd = capabilities[n]->mut_lists[g]; bd != NULL; bd = bd->link) {
            for (ml = bd->start; ml < bd->free; ml++) {

                maybeInitRetainerSet((StgClosure *)*ml);

#if defined(DEBUG_RETAINER)
                rtl = retainerSetOf((StgClosure *)*ml);
                if (rtl == NULL) {
                    // first visit to *ml
                    // This is a violation of the interface rule!
                    RSET(ml) = (RetainerSet *)((StgWord)(&tmpRetainerSet) | flip);

                    switch (get_itbl((StgClosure *)ml)->type) {
                    case IND_STATIC:
                        // no cost involved
                        break;
                    case CONSTR_NOCAF:
                    case THUNK_STATIC:
                    case FUN_STATIC:
                        barf("Invalid object in computeRetainerSet(): %d", get_itbl((StgClosure*)ml)->type);
                        break;
                    default:
                        // dynamic objects
                        costArray[get_itbl((StgClosure *)ml)->type] += cost((StgClosure *)ml);
                        sumOfNewCostExtra += cost((StgClosure *)ml);
                        break;
                    }
                }
#endif
            }
          }
        }
    }
}

/* -----------------------------------------------------------------------------
 *  Traverse all static objects for which we compute retainer sets,
 *  and reset their rs fields to NULL, which is accomplished by
 *  invoking maybeInitRetainerSet(). This function must be called
 *  before zeroing all objects reachable from scavenged_static_objects
 *  in the case of major garbage collections. See GarbageCollect() in
 *  GC.c.
 *  Note:
 *    The mut_once_list of the oldest generation must also be traversed?
 *    Why? Because if the evacuation of an object pointed to by a static
 *    indirection object fails, it is put back to the mut_once_list of
 *    the oldest generation.
 *    However, this is not necessary because any static indirection objects
 *    are just traversed through to reach dynamic objects. In other words,
 *    they are not taken into consideration in computing retainer sets.
 *
 * SDM (20/7/2011): I don't think this is doing anything sensible,
 * because it happens before retainerProfile() and at the beginning of
 * retainerProfil() we change the sense of 'flip'.  So all of the
 * calls to maybeInitRetainerSet() here are initialising retainer sets
 * with the wrong flip.  Also, I don't see why this is necessary.  I
 * added a maybeInitRetainerSet() call to retainRoot(), and that seems
 * to have fixed the assertion failure in retainerSetOf() I was
 * encountering.
 * -------------------------------------------------------------------------- */
void
resetStaticObjectForRetainerProfiling( StgClosure *static_objects )
{
#if defined(DEBUG_RETAINER)
    uint32_t count;
#endif
    StgClosure *p;

#if defined(DEBUG_RETAINER)
    count = 0;
#endif
    p = static_objects;
    while (p != END_OF_STATIC_OBJECT_LIST) {
        p = UNTAG_STATIC_LIST_PTR(p);
#if defined(DEBUG_RETAINER)
        count++;
#endif
        switch (get_itbl(p)->type) {
        case IND_STATIC:
            // Since we do not compute the retainer set of any
            // IND_STATIC object, we don't have to reset its retainer
            // field.
            p = (StgClosure*)*IND_STATIC_LINK(p);
            break;
        case THUNK_STATIC:
            maybeInitRetainerSet(p);
            p = (StgClosure*)*THUNK_STATIC_LINK(p);
            break;
        case FUN_STATIC:
        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_NOCAF:
            maybeInitRetainerSet(p);
            p = (StgClosure*)*STATIC_LINK(get_itbl(p), p);
            break;
        default:
            barf("resetStaticObjectForRetainerProfiling: %p (%s)",
                 p, get_itbl(p)->type);
            break;
        }
    }
#if defined(DEBUG_RETAINER)
    // debugBelch("count in scavenged_static_objects = %d\n", count);
#endif
}

/* -----------------------------------------------------------------------------
 * Perform retainer profiling.
 * N is the oldest generation being profilied, where the generations are
 * numbered starting at 0.
 * Invariants:
 * Note:
 *   This function should be called only immediately after major garbage
 *   collection.
 * ------------------------------------------------------------------------- */
void
retainerProfile(void)
{
#if defined(DEBUG_RETAINER)
  uint32_t i;
  uint32_t totalHeapSize;   // total raw heap size (computed by linear scanning)
#endif

#if defined(DEBUG_RETAINER)
  debugBelch(" < retainerProfile() invoked : %d>\n", retainerGeneration);
#endif

  stat_startRP();

  // We haven't flipped the bit yet.
#if defined(DEBUG_RETAINER)
  debugBelch("Before traversing:\n");
  sumOfCostLinear = 0;
  for (i = 0;i < N_CLOSURE_TYPES; i++)
    costArrayLinear[i] = 0;
  totalHeapSize = checkHeapSanityForRetainerProfiling();

  debugBelch("\tsumOfCostLinear = %d, totalHeapSize = %d\n", sumOfCostLinear, totalHeapSize);
  /*
  debugBelch("costArrayLinear[] = ");
  for (i = 0;i < N_CLOSURE_TYPES; i++)
    debugBelch("[%u:%u] ", i, costArrayLinear[i]);
  debugBelch("\n");
  */

  ASSERT(sumOfCostLinear == totalHeapSize);

/*
#define pcostArrayLinear(index) \
  if (costArrayLinear[index] > 0) \
    debugBelch("costArrayLinear[" #index "] = %u\n", costArrayLinear[index])
  pcostArrayLinear(THUNK_STATIC);
  pcostArrayLinear(FUN_STATIC);
  pcostArrayLinear(CONSTR_NOCAF);
*/
#endif

  // Now we flips flip.
  flip = flip ^ 1;

#if defined(DEBUG_RETAINER)
  stackSize = 0;
  maxStackSize = 0;
  cStackSize = 0;
  maxCStackSize = 0;
#endif
  numObjectVisited = 0;
  timesAnyObjectVisited = 0;

#if defined(DEBUG_RETAINER)
  debugBelch("During traversing:\n");
  sumOfNewCost = 0;
  sumOfNewCostExtra = 0;
  for (i = 0;i < N_CLOSURE_TYPES; i++)
    costArray[i] = 0;
#endif

  /*
    We initialize the traverse stack each time the retainer profiling is
    performed (because the traverse stack size varies on each retainer profiling
    and this operation is not costly anyhow). However, we just refresh the
    retainer sets.
   */
  initializeTraverseStack();
#if defined(DEBUG_RETAINER)
  initializeAllRetainerSet();
#else
  refreshAllRetainerSet();
#endif
  computeRetainerSet();

#if defined(DEBUG_RETAINER)
  debugBelch("After traversing:\n");
  sumOfCostLinear = 0;
  for (i = 0;i < N_CLOSURE_TYPES; i++)
    costArrayLinear[i] = 0;
  totalHeapSize = checkHeapSanityForRetainerProfiling();

  debugBelch("\tsumOfCostLinear = %d, totalHeapSize = %d\n", sumOfCostLinear, totalHeapSize);
  ASSERT(sumOfCostLinear == totalHeapSize);

  // now, compare the two results
  /*
    Note:
      costArray[] must be exactly the same as costArrayLinear[].
      Known exceptions:
        1) Dead weak pointers, whose type is CONSTR. These objects are not
           reachable from any roots.
  */
  debugBelch("Comparison:\n");
  debugBelch("\tcostArrayLinear[] (must be empty) = ");
  for (i = 0;i < N_CLOSURE_TYPES; i++)
    if (costArray[i] != costArrayLinear[i])
      // nothing should be printed except MUT_VAR after major GCs
      debugBelch("[%u:%u] ", i, costArrayLinear[i]);
  debugBelch("\n");

  debugBelch("\tsumOfNewCost = %u\n", sumOfNewCost);
  debugBelch("\tsumOfNewCostExtra = %u\n", sumOfNewCostExtra);
  debugBelch("\tcostArray[] (must be empty) = ");
  for (i = 0;i < N_CLOSURE_TYPES; i++)
    if (costArray[i] != costArrayLinear[i])
      // nothing should be printed except MUT_VAR after major GCs
      debugBelch("[%u:%u] ", i, costArray[i]);
  debugBelch("\n");

  // only for major garbage collection
  ASSERT(sumOfNewCost + sumOfNewCostExtra == sumOfCostLinear);
#endif

  // post-processing
  closeTraverseStack();
#if defined(DEBUG_RETAINER)
  closeAllRetainerSet();
#else
  // Note that there is no post-processing for the retainer sets.
#endif
  retainerGeneration++;

  stat_endRP(
    retainerGeneration - 1,   // retainerGeneration has just been incremented!
#if defined(DEBUG_RETAINER)
    maxCStackSize, maxStackSize,
#endif
    (double)timesAnyObjectVisited / numObjectVisited);
}

/* -----------------------------------------------------------------------------
 * DEBUGGING CODE
 * -------------------------------------------------------------------------- */

#if defined(DEBUG_RETAINER)

#define LOOKS_LIKE_PTR(r) ((LOOKS_LIKE_STATIC_CLOSURE(r) || \
        (HEAP_ALLOCED(r))) && \
        ((StgWord)(*(StgPtr)r)!=(StgWord)0xaaaaaaaaaaaaaaaaULL))

static uint32_t
sanityCheckHeapClosure( StgClosure *c )
{
    ASSERT(LOOKS_LIKE_GHC_INFO(c->header.info));
    ASSERT(LOOKS_LIKE_PTR(c));

    if ((((StgWord)RSET(c) & 1) ^ flip) != 0) {
        if (get_itbl(c)->type == CONSTR &&
            !strcmp(GET_PROF_TYPE(get_itbl(c)), "DEAD_WEAK") &&
            !strcmp(GET_PROF_DESC(get_itbl(c)), "DEAD_WEAK")) {
            debugBelch("\tUnvisited dead weak pointer object found: c = %p\n", c);
            costArray[get_itbl(c)->type] += cost(c);
            sumOfNewCost += cost(c);
        } else
            debugBelch(
                    "Unvisited object: flip = %d, c = %p(%d, %s, %s), rs = %p\n",
                    flip, c, get_itbl(c)->type,
                    get_itbl(c)->prof.closure_type, GET_PROF_DESC(get_itbl(c)),
                    RSET(c));
    } else {
        // debugBelch("sanityCheckHeapClosure) S: flip = %d, c = %p(%d), rs = %p\n", flip, c, get_itbl(c)->type, RSET(c));
    }

    return closure_sizeW(c);
}

static uint32_t
heapCheck( bdescr *bd )
{
    StgPtr p;
    static uint32_t costSum, size;

    costSum = 0;
    while (bd != NULL) {
        p = bd->start;
        while (p < bd->free) {
            size = sanityCheckHeapClosure((StgClosure *)p);
            sumOfCostLinear += size;
            costArrayLinear[get_itbl((StgClosure *)p)->type] += size;
            p += size;
            // no need for slop check; I think slops are not used currently.
        }
        ASSERT(p == bd->free);
        costSum += bd->free - bd->start;
        bd = bd->link;
    }

    return costSum;
}

static uint32_t
smallObjectPoolCheck(void)
{
    bdescr *bd;
    StgPtr p;
    static uint32_t costSum, size;

    bd = g0s0->blocks;
    costSum = 0;

    // first block
    if (bd == NULL)
        return costSum;

    p = bd->start;
    while (p < alloc_Hp) {
        size = sanityCheckHeapClosure((StgClosure *)p);
        sumOfCostLinear += size;
        costArrayLinear[get_itbl((StgClosure *)p)->type] += size;
        p += size;
    }
    ASSERT(p == alloc_Hp);
    costSum += alloc_Hp - bd->start;

    bd = bd->link;
    while (bd != NULL) {
        p = bd->start;
        while (p < bd->free) {
            size = sanityCheckHeapClosure((StgClosure *)p);
            sumOfCostLinear += size;
            costArrayLinear[get_itbl((StgClosure *)p)->type] += size;
            p += size;
        }
        ASSERT(p == bd->free);
        costSum += bd->free - bd->start;
        bd = bd->link;
    }

    return costSum;
}

static uint32_t
chainCheck(bdescr *bd)
{
    uint32_t costSum, size;

    costSum = 0;
    while (bd != NULL) {
        // bd->free - bd->start is not an accurate measurement of the
        // object size.  Actually it is always zero, so we compute its
        // size explicitly.
        size = sanityCheckHeapClosure((StgClosure *)bd->start);
        sumOfCostLinear += size;
        costArrayLinear[get_itbl((StgClosure *)bd->start)->type] += size;
        costSum += size;
        bd = bd->link;
    }

    return costSum;
}

static uint32_t
checkHeapSanityForRetainerProfiling( void )
{
    uint32_t costSum, g, s;

    costSum = 0;
    debugBelch("START: sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
    if (RtsFlags.GcFlags.generations == 1) {
        costSum += heapCheck(g0s0->to_blocks);
        debugBelch("heapCheck: sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
        costSum += chainCheck(g0s0->large_objects);
        debugBelch("chainCheck: sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
    } else {
        for (g = 0; g < RtsFlags.GcFlags.generations; g++)
        for (s = 0; s < generations[g].n_steps; s++) {
            /*
              After all live objects have been scavenged, the garbage
              collector may create some objects in
              scheduleFinalizers(). These objects are created through
              allocate(), so the small object pool or the large object
              pool of the g0s0 may not be empty.
            */
            if (g == 0 && s == 0) {
                costSum += smallObjectPoolCheck();
                debugBelch("smallObjectPoolCheck(): sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
                costSum += chainCheck(generations[g].steps[s].large_objects);
                debugBelch("chainCheck(): sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
            } else {
                costSum += heapCheck(generations[g].steps[s].blocks);
                debugBelch("heapCheck(): sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
                costSum += chainCheck(generations[g].steps[s].large_objects);
                debugBelch("chainCheck(): sumOfCostLinear = %d, costSum = %d\n", sumOfCostLinear, costSum);
            }
        }
    }

    return costSum;
}

void
findPointer(StgPtr p)
{
    StgPtr q, r, e;
    bdescr *bd;
    uint32_t g, s;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (s = 0; s < generations[g].n_steps; s++) {
            // if (g == 0 && s == 0) continue;
            bd = generations[g].steps[s].blocks;
            for (; bd; bd = bd->link) {
                for (q = bd->start; q < bd->free; q++) {
                    if (*q == (StgWord)p) {
                        r = q;
                        while (!LOOKS_LIKE_GHC_INFO(*r)) r--;
                        debugBelch("Found in gen[%d], step[%d]: q = %p, r = %p\n", g, s, q, r);
                        // return;
                    }
                }
            }
            bd = generations[g].steps[s].large_objects;
            for (; bd; bd = bd->link) {
                e = bd->start + cost((StgClosure *)bd->start);
                for (q = bd->start; q < e; q++) {
                    if (*q == (StgWord)p) {
                        r = q;
                        while (*r == 0 || !LOOKS_LIKE_GHC_INFO(*r)) r--;
                        debugBelch("Found in gen[%d], large_objects: %p\n", g, r);
                        // return;
                    }
                }
            }
        }
    }
}

static void
belongToHeap(StgPtr p)
{
    bdescr *bd;
    uint32_t g, s;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (s = 0; s < generations[g].n_steps; s++) {
            // if (g == 0 && s == 0) continue;
            bd = generations[g].steps[s].blocks;
            for (; bd; bd = bd->link) {
                if (bd->start <= p && p < bd->free) {
                    debugBelch("Belongs to gen[%d], step[%d]", g, s);
                    return;
                }
            }
            bd = generations[g].steps[s].large_objects;
            for (; bd; bd = bd->link) {
                if (bd->start <= p && p < bd->start + getHeapClosureSize((StgClosure *)bd->start)) {
                    debugBelch("Found in gen[%d], large_objects: %p\n", g, bd->start);
                    return;
                }
            }
        }
    }
}
#endif /* DEBUG_RETAINER */

#endif /* PROFILING */
