/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer profiling.
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RetainerProfile.h"
#include "RetainerSet.h"
#include "TraverseHeap.h"
#include "Profiling.h"
#include "Stats.h"
#include "StablePtr.h" /* markStablePtrTable */
#include "StableName.h" /* rememberOldStableNameAddresses */
#include "sm/Storage.h"

/* Note [What is a retainer?]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
Retainer profiling is a profiling technique that gives information why
objects can't be freed and lists the consumers that hold pointers to
the heap objects. It does not list all the objects that keep references
to the other, because then we would keep too much information that will
make the report unusable, for example the cons element of the list would keep
all the tail cells. As a result we are keeping only the objects of the
certain types, see 'isRetainer()' function for more discussion.

More formal definition of the retainer can be given the following way.

An object p is a retainer object of the object l, if all requirements
hold:

  1. p can be a retainer (see `isRetainer()`)
  2. l is reachable from p
  3. There are no other retainers on the path from p to l.

Exact algorithm and additional information can be found the historical
document 'docs/storage-mgt/rp.tex'. Details that are related to the
RTS implementation may be out of date, but the general
information about the retainers is still applicable.
*/


/*
  Note: what to change in order to plug-in a new retainer profiling scheme?
    (1) type retainer in include/StgRetainerProf.h
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

/* -----------------------------------------------------------------------------
 * Retainer stack - header
 *   Note:
 *     Although the retainer stack implementation could be separated *
 *     from the retainer profiling engine, there does not seem to be
 *     any advantage in doing that; retainer stack is an integral part
 *     of retainer profiling engine and cannot be use elsewhere at
 *     all.
 * -------------------------------------------------------------------------- */

traverseState g_retainerTraverseState;

W_
retainerStackBlocks(void)
{
    return traverseWorkStackBlocks(&g_retainerTraverseState);
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
    outputAllRetainerSet(prof_file);
}

/* -----------------------------------------------------------------------------
 * Returns true if *c is a retainer.
 * In general the retainers are the objects that may be the roots of the
 * collection. Basically this roots represents programmers threads
 * (TSO) with their stack and thunks.
 *
 * In addition we mark all mutable objects as a retainers, the reason for
 * that decision is lost in time.
 * -------------------------------------------------------------------------- */
STATIC_INLINE bool
isRetainer( const StgClosure *c )
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
    case CONTINUATION:
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
    case ANN_FRAME:
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
 * -------------------------------------------------------------------------- */
STATIC_INLINE retainer
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
STATIC_INLINE void
associate( StgClosure *c, RetainerSet *s )
{
    // StgWord has the same size as pointers, so the following type
    // casting is okay.
    setTravData(&g_retainerTraverseState, c, (StgWord)s);
}

bool isRetainerSetValid( const StgClosure *c )
{
    return isTravDataValid(&g_retainerTraverseState, c);
}

inline RetainerSet*
retainerSetOf( const StgClosure *c )
{
    ASSERT(isRetainerSetValid(c));
    return (RetainerSet*)getTravData(c);
}

static bool
retainVisitClosure( StgClosure *c, const StgClosure *cp, const stackData data, const bool first_visit, stackAccum *acc, stackData *out_data )
{
    (void) first_visit;
    (void) acc;

    retainer r = data.c_child_r;
    RetainerSet *s, *retainerSetOfc;
    retainerSetOfc = retainerSetOf(c);

    timesAnyObjectVisited++;

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
        out_data->c_child_r = isRetainer(c) ? getRetainerFrom(c) : r;
    } else {
        // This is not the first visit to *c.
        if (isMember(r, retainerSetOfc))
            return 0;          // no need to process children

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
            return 0;          // no need to process children

        // compute c_child_r
        out_data->c_child_r = r;
    }

    // now, RSET() of all of *c, *cp, and *r is valid.
    // (c, c_child_r) are available.

    return 1; // do process children
}

/**
 *  Push every object reachable from *tl onto the traversal work stack.
 */
static void
retainRoot(void *user, StgClosure **tl)
{
    traverseState *ts = (traverseState*) user;
    StgClosure *c;

    // We no longer assume that only TSOs and WEAKs are roots; any closure can
    // be a root.

    c = UNTAG_CLOSURE(*tl);
    traverseMaybeInitClosureData(&g_retainerTraverseState, c);
    if (c != &stg_END_TSO_QUEUE_closure && isRetainer(c)) {
        traversePushRoot(ts, c, c, (stackData)getRetainerFrom(c));
    } else {
        traversePushRoot(ts, c, c, (stackData)CCS_SYSTEM);
    }

    // NOT TRUE: ASSERT(isMember(getRetainerFrom(*tl), retainerSetOf(*tl)));
    // *tl might be a TSO which is ThreadComplete, in which
    // case we ignore it for the purposes of retainer profiling.
}

/* -----------------------------------------------------------------------------
 *  Compute the retainer set for each of the objects in the heap.
 * -------------------------------------------------------------------------- */
static void
computeRetainerSet( traverseState *ts )
{
    StgWeak *weak;
    uint32_t g, n;

    traverseInvalidateClosureData(ts);

    markCapabilities(retainRoot, (void*)ts); // for scheduler roots

    // This function is called after a major GC, when key, value, and finalizer
    // all are guaranteed to be valid, or reachable.
    //
    // The following code assumes that WEAK objects are considered to be roots
    // for retainer profiling.
    for (n = 0; n < getNumCapabilities(); n++) {
        // NB: after a GC, all nursery weak_ptr_lists have been migrated
        // to the global lists living in the generations
        ASSERT(getCapability(n)->weak_ptr_list_hd == NULL);
        ASSERT(getCapability(n)->weak_ptr_list_tl == NULL);
    }
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        for (weak = generations[g].weak_ptr_list; weak != NULL; weak = weak->link) {
            // retainRoot((StgClosure *)weak);
            retainRoot((void*)ts, (StgClosure **)&weak);
        }
    }

    // Consider roots from the stable ptr table.
    markStablePtrTable(retainRoot, (void*)ts);
    // Remember old stable name addresses.
    rememberOldStableNameAddresses ();

    traverseWorkStack(ts, &retainVisitClosure);
}

/* -----------------------------------------------------------------------------
 * Perform retainer profiling.
 * N is the oldest generation being profiled, where the generations are
 * numbered starting at 0.
 * Invariants:
 * Note:
 *   This function should be called only immediately after major garbage
 *   collection.
 * ------------------------------------------------------------------------- */
void
retainerProfile(void)
{
  stat_startRP();

  numObjectVisited = 0;
  timesAnyObjectVisited = 0;

  /*
    We initialize the traverse stack each time the retainer profiling is
    performed (because the traverse stack size varies on each retainer profiling
    and this operation is not costly anyhow). However, we just refresh the
    retainer sets.
   */
  initializeTraverseStack(&g_retainerTraverseState);
  initializeAllRetainerSet();
  computeRetainerSet(&g_retainerTraverseState);

  // post-processing
  closeTraverseStack(&g_retainerTraverseState);
  retainerGeneration++;

  stat_endRP(
    retainerGeneration - 1,   // retainerGeneration has just been incremented!
    getTraverseStackMaxSize(&g_retainerTraverseState),
    (double)timesAnyObjectVisited / numObjectVisited);
}

#endif /* PROFILING */
