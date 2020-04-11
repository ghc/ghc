/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019
 * Author: Daniel Gröber
 *
 * Heap root profiling.
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include <string.h>

#include "PosixSource.h"
#include "Rts.h"

#include "RtsFlags.h"
#include "Profiling.h"
#include "rts/TraverseHeap.h"
#include "ProfHeapInternal.h"
#include "Hash.h"
#include "StablePtr.h"
#include "Printer.h"

#include "RootProfile.h"

#define MAX_ROOTS ((StgInt)(sizeof(StgWord) * 8 - 2))

#if defined(DEBUG)
int g_rootProfileDebugLevel = 0;
static void debug(int level, const char *s, ...)
{
    va_list ap;

    if(level > g_rootProfileDebugLevel)
        return;

    va_start(ap, s);
    vprintf(s, ap);
    fflush(stdout);
    va_end(ap);
}

static const char *i2b(StgWord bin_idx)
{
    static char str[MAX_ROOTS+1];

    StgWord i;
    for(i=0; i < MAX_ROOTS; i++) {
        if(bin_idx & (1ul<<i))
            str[MAX_ROOTS-i-1] = '1';
        else
            str[MAX_ROOTS-i-1] = '0';
    }
    str[i] = '\0';

    return str;
}
#else
#define debug(...)
#endif

static struct rootProfState {
    StgWord n_roots;
    StgWord max_combined_descr_len;
    StgStablePtr roots[MAX_ROOTS];
    const char* descs[MAX_ROOTS];
    stackElement ses[MAX_ROOTS];
} g_rootProfState;

static StgWord current_root;

static StgWord
sumDescSizes(StgWord n_roots, const char** descs)
{
    StgWord i;
    StgWord sum = 0;
    for(i = 0; i < n_roots; i++) {
        sum += strlen(descs[i]);
    }
    return sum;
}

const char *
rootProfileMkClosureLabel(Arena *arena, const void *key)
{
    struct rootProfState *ps = &g_rootProfState;
    const StgWord bin_idx = (StgWord)key;
    const StgWord n_roots = ps->n_roots;
    const char** descs = ps->descs;

    /* This just allocates for the maximum possible size of the concatinated
     * root descriptions together with ", " seperators. This is quite wasteful
     * but since we can only really have a small number of them anyways what
     * does it matter. */
    const StgWord len = ps->max_combined_descr_len + n_roots + 1;
    /*                               (intercalate ",") -^      ^-null byte */
    char *identity = arenaAlloc(arena, len);
    char *ptr = identity;

    if(key == NULL) {
        strcpy(identity, "NOROOT");
        return identity;
    }

    StgWord i;
    for(i = 0; i < n_roots; i++) {
        if(bin_idx & (1ul<<i)) {
            strcpy(ptr, descs[i]);
            ptr += strlen(descs[i]);
            strcpy(ptr, ",");
            ptr += strlen(",");
            ptr[0] = '\0';
        }
    }
    if(i > 0)
        ptr[-1] = '\0';

    return identity;
}

const void *rootProfileGetClosureIdentity(traverseState *ts, const StgClosure *c)
{
    (void) ts;
    ASSERT(traverseIsClosureDataValid(ts, c));
    return (void*)(traverseGetClosureData(c)>>2);
}

static bool
rootVisit(traverseState *ts, StgClosure *c, const StgClosure *cp,
          const stackData data, const bool first_visit,
          stackAccum *acc, stackData *child_data)
{
    (void) cp;
    (void) data;
    (void) acc;
    (void) child_data;

    debug(2, "visit %p <- %p %s\n", c, cp, info_type(c));

    if(first_visit) {
        debug(2, "  bin %s\n", i2b(1ul<<current_root));
        traverseSetClosureData(ts, c, (1ul<<current_root)<<2);
        return true;
    } else {
        StgWord bin_idx = traverseGetClosureData(c)>>2;
        if((bin_idx & (1ul<<current_root))) {
            return false;
        } else {
            StgWord new_bin = (1ul<<current_root) | bin_idx;
            debug(2, "  bin %s |= %s\n", i2b(bin_idx), i2b((1ul<<current_root)));
            traverseSetClosureData(ts, c, new_bin<<2);
            return true; // have to update the children's bin_idx too
        }
    }

    return first_visit;
}

StgInt setRootProfPtrs(StgInt n, HsStablePtr *sps, const char** descs)
{
    struct rootProfState *ps = &g_rootProfState;
    StgWord i;

    if(n < 0)
        return MAX_ROOTS;

    if(n > MAX_ROOTS)
        n = MAX_ROOTS;

    for (i = 0; i < ps->n_roots; i++) {
        freeStablePtr(ps->roots[i]);
        free((void*)ps->descs[i]);
    }

    for (i = 0; i < (StgWord)n; i++) {
        ps->roots[i] = sps[i];
        ps->descs[i] = descs[i];
    }
    ps->n_roots = i;
    ps->max_combined_descr_len = sumDescSizes(ps->n_roots, ps->descs);

    return n;
}

static void
rootProfileVisitRoots(traverseState *ts, visitClosure_cb visit_cb, returnClosure_cb return_cb)
{
    struct rootProfState *ps = &g_rootProfState;

    traverseInvalidateAllClosureData(ts);
    for(StgWord i = 0; i < ps->n_roots; i++) {
        StgClosure *c = (StgClosure*)deRefStablePtr(ps->roots[i]);
        debug(2, "\npush %s, root=%lu, %p\n", ps->descs[i], i, c);

        current_root = i;
        traversePushClosure(ts, c, c, NULL, nullStackData);
        traverseWorkStack(ts, visit_cb, return_cb);
    }
}

bool rootProfileWasClosureVisited(traverseState *ts, const StgClosure *c)
{
    return traverseIsClosureDataValid(ts, c);
}

/**
 * Perform root profiling.
 *
 * Note:
 *   This function should be called only immediately after major garbage
 *   collection.
 **/
void
rootProfile(traverseState *ts, Time t, Census *census)
{
    (void) t;
    (void) census;

    struct rootProfState *ps = &g_rootProfState;

    if(ps->n_roots == 0)
        return;

    /* The following double traversal is pretty inefficient. However it's the
     * simplest way to get this working for now.
     *
     * Essentially the problem is this: Say currently flip=1, if we traverse a
     * subset of the heap now then the visited bit of all reachable closures
     * will get set to one and the unreachable ones remain at zero. After the
     * traversal the flip bit is flipped, so now it's flip=0.
     *
     * If the user changes the set of roots before the next traversal we now
     * have some nodes in our set of reachable closures with visited=1 and
     * visited=0, the ones with '0' would be interpteted as "currently valid" so
     * their heap profiling header is not reset, giving us stale data.
     *
     * We solve this here by traversing twice. This always resets the visited
     * bit back to zero in the end.
     */

    debug(2, "\n=== root profile ===\n");

    rootProfileVisitRoots(ts, &rootVisit, NULL);
}

void endRootProfiling(traverseState *ts)
{
    rootProfileVisitRoots(ts, &rootVisit, NULL);
}

#endif /* PROFILING */
