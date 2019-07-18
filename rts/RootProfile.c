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

#include "Profiling.h"
#include "TraverseHeap.h"
#include "ProfHeapInternal.h"
#include "Hash.h"
#include "StablePtr.h"

#include "RootProfile.h"

#define  MAX_ROOTS 20
#define  MAX_BINS  0x100000 // 2^20 // makes 'sizes' array about 8 MiB

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
        if(bin_idx & (1<<i))
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
    StgStablePtr roots[MAX_ROOTS];
    const char* descs[MAX_ROOTS];
    stackElement ses[MAX_ROOTS];
    StgWord sizes[MAX_BINS];
} g_rootProfState;

traverseState g_rootTraverseState;

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

static const char *
mkBinIdentity(struct rootProfState *ps, Arena *arena, StgWord bin_idx,
              StgWord max_combined_descr_len)
{
    const StgWord n_roots = ps->n_roots;
    const char** descs = ps->descs;

    /* This just allocates for the maximum possible size of the concatinated
     * root descriptions together with ", " seperators. This is quite wasteful
     * but since we can only really have a small number of them anyways what
     * does it matter. */
    const StgWord len = max_combined_descr_len + n_roots + 1;
    /*                           (intercalate "-") -^      ^-null byte */
    char *identity = arenaAlloc(arena, len);
    char *ptr = identity;

    StgWord i;
    for(i = 0; i < n_roots; i++) {
        if(bin_idx & (1ul<<i)) {
            strcpy(ptr, descs[i]);
            ptr += strlen(descs[i]);
            strcpy(ptr, "-");
            ptr += strlen("-");
            ptr[0] = '\0';
        }
    }
    if(i > 0)
        ptr[-1] = '\0';

    return identity;
}

static bool
rootVisit(StgClosure *c, const StgClosure *cp,
          const stackData data, const bool first_visit,
          stackAccum *acc, stackData *child_data)
{
    (void) cp;
    (void) data;
    (void) acc;
    (void) child_data;

    traverseState *ts = &g_rootTraverseState;
    struct rootProfState *ps = &g_rootProfState;

    debug(2, "visit %p\n", c);

    if(first_visit) {
        StgWord sizeW = (StgWord)closure_sizeW(c) - sizeofW(StgProfHeader);
        debug(2, "  bin %s (%lu) += %lu\n",
              i2b(1ul<<current_root), ps->sizes[1ul<<current_root], sizeW);
        ps->sizes[1ul<<current_root] += sizeW;
        setTravData(ts, c, (1ul<<current_root)<<1);
        return true;
    } else {
        StgWord bin_idx = getTravData(c)>>1;
        if((bin_idx & (1ul<<current_root))) {
            return false;
        } else {
            StgWord sizeW = (StgWord)closure_sizeW(c) - sizeofW(StgProfHeader);
            StgWord new_bin = (1ul<<current_root) | bin_idx;
            debug(2, "  bin %s (%lu) -= %lu\n",
                  i2b(bin_idx), ps->sizes[bin_idx], sizeW);
            debug(2, "  bin %s (%lu) += %lu\n",
                  i2b(new_bin), ps->sizes[new_bin], sizeW);
            ps->sizes[bin_idx] -= sizeW;
            ps->sizes[new_bin] += sizeW;
            setTravData(ts, c, new_bin<<1);
            return true; // have to update the children's bin_idx too
        }
    }

    return first_visit;
}

StgWord setRootProfPtrs(StgWord n, HsStablePtr *sps, const char** descs)
{
    struct rootProfState *ps = &g_rootProfState;
    StgWord i;

    if(n > MAX_ROOTS)
        return MAX_ROOTS;

    for (i = 0; i < ps->n_roots; i++) {
        freeStablePtr(ps->roots[i]);
        free((void*)ps->descs[i]);
    }

    for (i = 0; i < n; i++) {
        ps->roots[i] = sps[i];
        ps->descs[i] = descs[i];
    }
    ps->n_roots = i;

    return 0;
}

/**
 * Perform root profiling.
 *
 * Note:
 *   This function should be called only immediately after major garbage
 *   collection.
 **/
void
rootProfile(Time t, Census *census)
{
    (void) t;
    traverseState *ts = &g_rootTraverseState;
    struct rootProfState *ps = &g_rootProfState;
    initializeTraverseStack(ts);

    if(ps->n_roots == 0)
        return;

    const StgWord n_roots = ps->n_roots;
    const StgWord n_bins = (1<<n_roots) - 1;

    memset(ps->sizes, 0, sizeof(ps->sizes));

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

    for(StgWord i = 0; i < ps->n_roots; i++) {
        StgClosure *c = (StgClosure*)deRefStablePtr(ps->roots[i]);
        debug(2, "\npush %s, root=%lu, %p\n", ps->descs[i], i, c);

        current_root = i;
        traversePushClosure(ts, c, c, NULL, nullStackData);
        traverseWorkStack(ts, &rootVisit);
    }
    traverseInvalidateClosureData(ts);


    for(StgWord i = 0; i < ps->n_roots; i++) {
        StgClosure *c = (StgClosure*)deRefStablePtr(ps->roots[i]);
        traversePushClosure(ts, c, c, NULL, nullStackData);
    }

    traverseWorkStack(ts, NULL);
    traverseInvalidateClosureData(ts);

    debug(2, "\n\n\n=== result ===\n");

    for(StgWord i = 1; i <= n_bins; i++) {
        if(ps->sizes[i] == 0)
            continue;

        const char* identity =
            mkBinIdentity(ps, census->arena, i,
                          sumDescSizes(n_roots, ps->descs));

        debug(1, "bin %s = %lu\n", identity, ps->sizes[i]);

        counter *ctr = lookupHashTable(census->hash, i);
        if(!ctr) {
            ctr = heapInsertNewCounter(census, i);
            ctr->identity = identity;
        }

        ctr->c.resid = ps->sizes[i];
    }

    closeTraverseStack(ts);
}

#endif /* PROFILING */
